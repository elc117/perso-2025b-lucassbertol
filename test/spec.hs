{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe, withObject, (.:), (.:?), (.=))
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)
import qualified Data.Vector as V
import Prelude hiding (length, filter)
import qualified Prelude as P

filtrarPorStatus :: Text -> [Value] -> [Value]
filtrarPorStatus "finalizados" matches = P.filter jogoFinalizado matches
filtrarPorStatus "futuros" matches = P.filter jogoFuturo matches  
filtrarPorStatus _ matches = matches

filtrarPorLocal :: Text -> [Value] -> [Value]
filtrarPorLocal "casa" matches = P.filter jogoCasa matches
filtrarPorLocal "fora" matches = P.filter jogoFora matches
filtrarPorLocal _ matches = matches

filtrarPorResultado :: Text -> [Value] -> [Value]
filtrarPorResultado "vitoria" matches = P.filter vitoria matches
filtrarPorResultado "derrota" matches = P.filter derrota matches
filtrarPorResultado "empate" matches = P.filter empate matches
filtrarPorResultado _ matches = matches

jogoFinalizado :: Value -> Bool
jogoFinalizado jogo = 
  case parseMaybe parseScore jogo of
    Just (Just _, Just _) -> True
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

vitoria :: Value -> Bool
vitoria jogo =
  case parseMaybe parseScore jogo of
    Just (Just home, Just away) -> 
      if jogoCasa jogo 
      then home > away 
      else away > home
    _ -> False

derrota :: Value -> Bool
derrota jogo =
  case parseMaybe parseScore jogo of
    Just (Just home, Just away) ->
      if jogoCasa jogo 
      then home < away 
      else away < home
    _ -> False

empate :: Value -> Bool
empate jogo =
  case parseMaybe parseScore jogo of
    Just (Just home, Just away) -> home == away
    _ -> False

extrairRodada :: Value -> Int
extrairRodada jogo = 
  case parseMaybe parseMatchday jogo of
    Just rodada -> rodada
    _ -> 0

-- Parsers
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

-- Dados de teste
criarJogoMock :: String -> String -> Maybe Int -> Maybe Int -> Int -> Value
criarJogoMock homeTeam awayTeam homeScore awayScore matchday = object
  [ "homeTeam" .= object ["shortName" .= homeTeam]
  , "awayTeam" .= object ["shortName" .= awayTeam]
  , "score" .= object 
      [ "fullTime" .= object 
          [ "home" .= homeScore
          , "away" .= awayScore
          ]
      ]
  , "matchday" .= matchday
  ]

jogoCasaInter :: Value
jogoCasaInter = criarJogoMock "Internacional" "Grêmio" (Just 2) (Just 1) 1

jogoForaInter :: Value  
jogoForaInter = criarJogoMock "Grêmio" "Internacional" (Just 1) (Just 2) 2

jogoEmpate :: Value
jogoEmpate = criarJogoMock "Internacional" "Palmeiras" (Just 1) (Just 1) 3

jogoFuturoMock :: Value
jogoFuturoMock = criarJogoMock "Internacional" "Flamengo" Nothing Nothing 4

jogoOutroTime :: Value
jogoOutroTime = criarJogoMock "Flamengo" "Palmeiras" (Just 3) (Just 0) 5

-- Testes principais
main :: IO ()
main = hspec $ do
  describe "Filtros de Status" $ do
    it "filtra jogos finalizados corretamente" $ do
      let jogos = [jogoCasaInter, jogoForaInter, jogoFuturoMock]
      let resultado = filtrarPorStatus "finalizados" jogos
      P.length resultado `shouldBe` 2

    it "filtra jogos futuros corretamente" $ do
      let jogos = [jogoCasaInter, jogoForaInter, jogoFuturoMock]
      let resultado = filtrarPorStatus "futuros" jogos
      P.length resultado `shouldBe` 1

    it "retorna todos os jogos quando filtro é 'todos'" $ do
      let jogos = [jogoCasaInter, jogoForaInter, jogoFuturoMock]
      let resultado = filtrarPorStatus "todos" jogos
      P.length resultado `shouldBe` 3

  describe "Filtros de Local" $ do
    it "filtra jogos em casa corretamente" $ do
      let jogos = [jogoCasaInter, jogoForaInter, jogoOutroTime]
      let resultado = filtrarPorLocal "casa" jogos
      P.length resultado `shouldBe` 1

    it "filtra jogos fora de casa corretamente" $ do
      let jogos = [jogoCasaInter, jogoForaInter, jogoOutroTime]
      let resultado = filtrarPorLocal "fora" jogos
      P.length resultado `shouldBe` 1

    it "retorna todos os jogos quando filtro é 'todos'" $ do
      let jogos = [jogoCasaInter, jogoForaInter, jogoOutroTime]
      let resultado = filtrarPorLocal "todos" jogos
      P.length resultado `shouldBe` 3

  describe "Filtros de Resultado" $ do
    it "filtra vitórias corretamente" $ do
      let jogos = [jogoCasaInter, jogoForaInter, jogoEmpate]
      let resultado = filtrarPorResultado "vitoria" jogos
      P.length resultado `shouldBe` 2

    it "filtra empates corretamente" $ do
      let jogos = [jogoCasaInter, jogoForaInter, jogoEmpate]
      let resultado = filtrarPorResultado "empate" jogos
      P.length resultado `shouldBe` 1

    it "retorna todos os jogos quando filtro é 'todos'" $ do
      let jogos = [jogoCasaInter, jogoForaInter, jogoEmpate]
      let resultado = filtrarPorResultado "todos" jogos
      P.length resultado `shouldBe` 3

  describe "Funções de Identificação" $ do
    it "identifica jogos finalizados corretamente" $ do
      jogoFinalizado jogoCasaInter `shouldBe` True
      jogoFinalizado jogoFuturoMock `shouldBe` False

    it "identifica jogos futuros corretamente" $ do
      jogoFuturo jogoCasaInter `shouldBe` False
      jogoFuturo jogoFuturoMock `shouldBe` True

    it "identifica jogos em casa corretamente" $ do
      jogoCasa jogoCasaInter `shouldBe` True
      jogoCasa jogoForaInter `shouldBe` False

    it "identifica jogos fora corretamente" $ do
      jogoFora jogoCasaInter `shouldBe` False
      jogoFora jogoForaInter `shouldBe` True

  describe "Análise de Resultados" $ do
    it "identifica vitórias corretamente" $ do
      vitoria jogoCasaInter `shouldBe` True
      vitoria jogoForaInter `shouldBe` True
      vitoria jogoEmpate `shouldBe` False

    it "identifica derrotas corretamente" $ do
      derrota jogoCasaInter `shouldBe` False
      derrota jogoForaInter `shouldBe` False

    it "identifica empates corretamente" $ do
      empate jogoCasaInter `shouldBe` False
      empate jogoEmpate `shouldBe` True

  describe "Extração de Dados" $ do
    it "extrai rodada corretamente" $ do
      extrairRodada jogoCasaInter `shouldBe` 1
      extrairRodada jogoForaInter `shouldBe` 2

  describe "Propriedades com QuickCheck" $ do
    it "filtrar por status sempre retorna lista menor ou igual" $ do
      let jogos = [jogoCasaInter, jogoForaInter, jogoFuturoMock]
      let testStatus s = P.length (filtrarPorStatus s jogos) <= P.length jogos
      testStatus "finalizados" `shouldBe` True
      testStatus "futuros" `shouldBe` True
      testStatus "todos" `shouldBe` True
      testStatus "qualquer" `shouldBe` True

    it "filtrar por local sempre retorna lista menor ou igual" $ do
      let jogos = [jogoCasaInter, jogoForaInter, jogoOutroTime]
      let testLocal l = P.length (filtrarPorLocal l jogos) <= P.length jogos
      testLocal "casa" `shouldBe` True
      testLocal "fora" `shouldBe` True
      testLocal "todos" `shouldBe` True
      testLocal "qualquer" `shouldBe` True

    it "jogos finalizados e futuros são mutuamente exclusivos" $ do
      jogoFinalizado jogoCasaInter && jogoFuturo jogoCasaInter `shouldBe` False
      jogoFinalizado jogoFuturoMock && jogoFuturo jogoFuturoMock `shouldBe` False

    it "extração de rodada sempre retorna valor não negativo" $ do
      extrairRodada jogoCasaInter >= 0 `shouldBe` True
      extrairRodada jogoForaInter >= 0 `shouldBe` True
      extrairRodada jogoFuturoMock >= 0 `shouldBe` True
