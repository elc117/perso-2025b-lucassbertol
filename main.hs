{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (Value, decode, object, (.=))
import Data.ByteString.Char8 qualified as BS
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Network.HTTP.Simple
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Web.Scotty

apiKey :: String
apiKey = "a8b13670a43b43079197e65af09c6065"

main :: IO ()
main = scotty 3000 $ do
  middleware $ staticPolicy (addBase "static")

  get "/" $ file "static/index.html"

  get "/api/team/:name/:dataType/:year" $ do
    dataType <- pathParam "dataType" :: ActionM Text  
    year <- pathParam "year" :: ActionM Text
    
    let requestUrl = case dataType of
          "brasileirao" -> "http://api.football-data.org/v4/teams/6684/matches?competitions=2013&season=" ++ T.unpack year
          "libertadores" -> "http://api.football-data.org/v4/teams/6684/matches?competitions=2152&season=" ++ T.unpack year

    request <- parseRequest requestUrl
    let requestAuth = setRequestHeader "X-Auth-Token" [BS.pack apiKey] request

    response <- httpLBS requestAuth
    let body = getResponseBody response

    case (decode body :: Maybe Value) of
      Just val -> json val
      Nothing -> json $ object ["erro" .= ("não consegui ler os dados" :: String)]


  get "/api/team/:name/:dataType" $ do
    dataType <- pathParam "dataType" :: ActionM Text


    let requestUrl = case dataType of
          "brasileirao" -> "http://api.football-data.org/v4/teams/6684/matches?competitions=2013&season=2025"
          "libertadores" -> "http://api.football-data.org/v4/teams/6684/matches?competitions=2152&season=2025"
          "jogadores" -> "http://api.football-data.org/v4/teams/6684"
          
    request <- parseRequest requestUrl
    let requestAuth = setRequestHeader "X-Auth-Token" [BS.pack apiKey] request

    response <- httpLBS requestAuth
    let body = getResponseBody response

    case (decode body :: Maybe Value) of
      Just val -> json val
      Nothing -> json $ object ["erro" .= ("não consegui ler os dados" :: String)]
