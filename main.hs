{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (Value(..), decode, object, (.=), (.:?), (.:), withObject)
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString.Char8 qualified as BS
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Network.HTTP.Simple
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Web.Scotty
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Vector as V
-- Chave para autenticação na API de futebol

apiKey :: String
apiKey = "a8b13670a43b43079197e65af09c6065"

-- Função principal que aplica todos os filtros
aplicarFiltros :: Text -> Text -> Value -> Value
aplicarFiltros filtro local dadosOriginais = 
  case parseMaybe parseMatches dadosOriginais of
    Just matches -> 
      let matchesFiltrados = ordenarPorRodada (filtrarPorLocal local (filtrarPorStatus filtro matches))
      in object ["matches" .= matchesFiltrados]
    _ -> dadosOriginais

-- Parser para extrair array de matches
parseMatches :: Value -> Parser [Value]
parseMatches = withObject "response" $ \o -> do
  Array matches <- o .: "matches"
  return (V.toList matches)

-- Filtrar por status do jogo (finalizados/futuros)
filtrarPorStatus :: Text -> [Value] -> [Value]
filtrarPorStatus "finalizados" matches = filter jogoFinalizado matches
filtrarPorStatus "futuros" matches = filter jogoFuturo matches  
filtrarPorStatus _ matches = matches -- "todos" ou qualquer outro

-- Filtrar por local (casa/fora)
filtrarPorLocal :: Text -> [Value] -> [Value]
filtrarPorLocal "casa" matches = filter jogoCasa matches
filtrarPorLocal "fora" matches = filter jogoFora matches
filtrarPorLocal _ matches = matches -- "todos"

-- Ordenar por rodada (matchday)
ordenarPorRodada :: [Value] -> [Value]
ordenarPorRodada = sortBy (comparing extrairRodada)

-- Funções auxiliares simples
jogoFinalizado :: Value -> Bool
jogoFinalizado jogo = 
  case parseMaybe parseScore jogo of
    Just (Just _, Just _) -> True -- Ambos os placares existem
    _ -> False

jogoFuturo :: Value -> Bool  
jogoFuturo = not . jogoFinalizado

jogoCasa :: Value -> Bool
jogoCasa jogo = 
  case parseMaybe parseHomeTeam jogo of
    Just nome -> "Internacional" `T.isInfixOf` T.pack nome || "Internacional" == T.pack nome
    _ -> False

jogoFora :: Value -> Bool
jogoFora jogo = 
  case parseMaybe parseAwayTeam jogo of  
    Just nome -> "Internacional" `T.isInfixOf` T.pack nome || "Internacional" == T.pack nome
    _ -> False

extrairRodada :: Value -> Int
extrairRodada jogo = 
  case parseMaybe parseMatchday jogo of
    Just rodada -> rodada
    _ -> 0

-- Parsers para extrair dados do JSON

parseScore :: Value -> Parser (Maybe Int, Maybe Int)
parseScore = withObject "match" $ \o -> do
  score <- o .:? "score"
  case score of
    Just s -> do
      fullTime <- s .:? "fullTime" 
      case fullTime of
        Just ft -> do
          home <- ft .:? "home"
          away <- ft .:? "away"  
          return (home, away)
        _ -> return (Nothing, Nothing)
    _ -> return (Nothing, Nothing)

parseHomeTeam :: Value -> Parser String
parseHomeTeam = withObject "match" $ \o -> do
  homeTeam <- o .: "homeTeam"
  homeTeam .: "shortName"

parseAwayTeam :: Value -> Parser String  
parseAwayTeam = withObject "match" $ \o -> do
  awayTeam <- o .: "awayTeam"
  awayTeam .: "shortName"

parseMatchday :: Value -> Parser Int
parseMatchday = withObject "match" $ \o -> o .: "matchday"

main :: IO ()
main = scotty 3000 $ do
  middleware $ staticPolicy (addBase "static")

  get "/" $ file "static/index.html"

  -- Rota para obter dados do time com base no nome, tipo de dado e ano

  get "/api/team/:name/:dataType/:year" $ do
    dataType <- pathParam "dataType" :: ActionM Text
    year <- pathParam "year" :: ActionM Text
    
    -- Parâmetros opcionais de filtro
    filtroMaybe <- queryParamMaybe "filtro"
    localMaybe <- queryParamMaybe "local"
    let filtro = case filtroMaybe of Just f -> f; Nothing -> "todos"
    let local = case localMaybe of Just l -> l; Nothing -> "todos"    -- Monta a URL da requisição com base no tipo de dado e ano, http-conduit fará a requisição para a API logo em seguida


    let requestUrl = case dataType of
          "brasileirao" -> "http://api.football-data.org/v4/teams/6684/matches?competitions=2013&season=" ++ T.unpack year
          "libertadores" -> "http://api.football-data.org/v4/teams/6684/matches?competitions=2152&season=" ++ T.unpack year

    -- 'X Auth-Token' = cabeçalho necessário para autenticação na API pois a chave é passada via cabeçalho na biblioteca http-conduit

    request <- parseRequest requestUrl
    let requestAuth = setRequestHeader "X-Auth-Token" [BS.pack apiKey] request

    -- Realiza a requisição HTTP e obtém o corpo da resposta

    response <- httpLBS requestAuth
    let body = getResponseBody response

    -- Tratamento de erro na decodificação do JSON da resposta da API

    case (decode body :: Maybe Value) of
      Just val -> do
        -- Filtrar os dados com base nos parâmetros 
        let dadosFiltrados = aplicarFiltros filtro local val
        json dadosFiltrados
      Nothing -> json $ object ["erro" .= ("não consegui ler os dados" :: String)]

  -- Rota para obter dados do time com base no nome e tipo de dado (padrão ano 2025)

  get "/api/team/:name/:dataType" $ do
    dataType <- pathParam "dataType" :: ActionM Text
    
    -- Parâmetros opcionais de filtro  
    filtroMaybe <- queryParamMaybe "filtro"
    localMaybe <- queryParamMaybe "local"
    let filtro = case filtroMaybe of Just f -> f; Nothing -> "todos"
    let local = case localMaybe of Just l -> l; Nothing -> "todos"    -- Monta a URL da requisição com base no tipo de dado (ano padrão 2025), http-conduit fará a requisição para a API logo em seguida

    let requestUrl = case dataType of
          "libertadores" -> "http://api.football-data.org/v4/teams/6684/matches?competitions=2152&season=2025"
          "jogadores" -> "http://api.football-data.org/v4/teams/6684"

    -- 'X Auth-Token' = cabeçalho necessário para autenticação na API pois a chave é passada via cabeçalho na biblioteca http-conduit

    request <- parseRequest requestUrl
    let requestAuth = setRequestHeader "X-Auth-Token" [BS.pack apiKey] request

    -- Realiza a requisição HTTP e obtém o corpo da resposta

    response <- httpLBS requestAuth
    let body = getResponseBody response

    -- Tratamento de erro na decodificação do JSON da resposta da API

    case (decode body :: Maybe Value) of
      Just val -> do
        -- Filtrar apenas se não for jogadores para usar os filtros na parte de jogos
        let dadosFiltrados = if dataType == "jogadores" 
                            then val 
                            else aplicarFiltros filtro local val
        json dadosFiltrados
      Nothing -> json $ object ["erro" .= ("não consegui ler os dados" :: String)]
