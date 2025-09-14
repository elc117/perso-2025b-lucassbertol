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

  get "/api/team/:name/:dataType" $ do
    teamName <- pathParam "name" :: ActionM Text
    dataType <- pathParam "dataType" :: ActionM Text

    let teamId = case teamName of
          "internacional" -> "6684"
          _ -> "6684"

    let requestUrl = case dataType of
          "brasileirao" -> "http://api.football-data.org/v4/teams/6684/matches?competitions=2013"
          "libertadores" -> "http://api.football-data.org/v4/teams/6684/matches?competitions=2152"
          "estatisticas gerais" -> "http://api.football-data.org/v4/teams/6684"

    request <- parseRequest requestUrl
    let requestAuth = setRequestHeader "X-Auth-Token" [BS.pack apiKey] request

    response <- httpLBS requestAuth
    let body = getResponseBody response

    case (decode body :: Maybe Value) of
      Just val -> json val
      Nothing -> json $ object ["erro" .= ("não consegui ler os dados" :: String)]
