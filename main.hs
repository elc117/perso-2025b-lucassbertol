{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (Value, decode, object, (.=))
import Data.ByteString.Char8 qualified as BS
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Network.HTTP.Simple
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Web.Scotty

-- Chave para autenticação na API de futebol

apiKey :: String
apiKey = "a8b13670a43b43079197e65af09c6065"

main :: IO ()
main = scotty 3000 $ do
  middleware $ staticPolicy (addBase "static")

  get "/" $ file "static/index.html"

-- Rota para obter dados do time com base no nome, tipo de dado e ano

  get "/api/team/:name/:dataType/:year" $ do
    dataType <- pathParam "dataType" :: ActionM Text  
    year <- pathParam "year" :: ActionM Text

-- Monta a URL da requisição com base no tipo de dado e ano, http-conduit fará a requisição para a API logo em seguida
-- T.unpack = converte Text para String pois 'IO String'
-- Busca de campeonato brasileiro filtrado por ano (24-25)

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
      Just val -> json val
      Nothing -> json $ object ["erro" .= ("não consegui ler os dados" :: String)]

-- Rota para obter dados do time com base no nome e tipo de dado (padrão ano 2025)

  get "/api/team/:name/:dataType" $ do
    dataType <- pathParam "dataType" :: ActionM Text

  -- Monta a URL da requisição com base no tipo de dado (ano padrão 2025), http-conduit fará a requisição para a API logo em seguida
  -- Busca de partidas libertadores e elenco do time (2025)

    let requestUrl = case dataType of
          "brasileirao" -> "http://api.football-data.org/v4/teams/6684/matches?competitions=2013&season=2025"
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
      Just val -> json val
      Nothing -> json $ object ["erro" .= ("não consegui ler os dados" :: String)]
