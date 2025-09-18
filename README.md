## Identificação

- **Autor:** Lucas Salvini Bertol  
- **Curso:** Sistemas de Informação  
- **Disciplina:** Paradigmas de Programação  

---

## Tema / Objetivo

O *interScore* é uma aplicação web para consulta de informações do Sport Club Internacional, desenvolvida com **backend em Haskell** usando **`Scotty`** e frontend simples em HTML/JavaScript. O projeto consome dados da [API Football-Data.org](https://www.football-data.org/) para exibir informações sobre jogos do Brasileirão, Libertadores, calendário e elenco atual do clube, com filtros específicos em cada ocasão.

---

## Processo de desenvolvimento

### 1. Configuração Inicial da API

**1.1** Estabelecer comunicação com a [API](https://www.football-data.org/)

```haskell
main = scotty 3000 $ do
  get "/api/test" $ do
    text "API funcionando"
```

**1.2** Implementação da autenticação

```haskell
apiKey :: String
apiKey = "a8b13670a43b43079197e65af09c6065"

request <- parseRequest requestUrl
let requestAuth = setRequestHeader "X-Auth-Token" [BS.pack apiKey] request
```

### 2. Parsing e Manipulação de JSON

**2.1** Extrair dados específicos do JSON cru da API

```haskell
case (decode body :: Maybe Value) of
  Just val -> json val  -- Retornava tudo, sem filtros
```

**2.2** Desenvolvimento de [parsers](https://www.google.com/search?q=o+que+s%C3%A3o+parsers+em+haskell+e+como+usar&client=firefox-b-lm&sca_esv=c86351f7882d4df3&sxsrf=AE3TifM0AtBSS3MUugnIy85Tfn-Jtytscg%3A1758156836195&ei=JFjLaPXWC9PN1sQP4OST2AQ&ved=0ahUKEwj1-LjNjOGPAxXTppUCHWDyBEsQ4dUDCBA&uact=5&oq=o+que+s%C3%A3o+parsers+em+haskell+e+como+usar&gs_lp=Egxnd3Mtd2l6LXNlcnAiKW8gcXVlIHPDo28gcGFyc2VycyBlbSBoaXNrbGVsIGUgY29tbyB1c2FyMgUQIRigAUjvGFDUBFjCFnACeACQAQCYAa0CoAHfEqoBBzAuOS4yLjG4AQPIAQD4AQGYAg6gAs8TwgIIEAAYsAMY7wXCAgUQIRifBZgDAIgGAZAGA5IHBzIuOC4zLjGgB7AwsgcHMC44LjMuMbgHvhPCBwYyLTEzLjHIB0k&sclient=gws-wiz-serp)
específicos

Pesquisando sobre manipulação de JSON em Haskell, descobri o conceito de **parsing** que é uma técnica que permite transformar dados brutos em estruturas organizadas e type-safe.

Implementei parsers específicos para cada tipo de dado (placares, times, datas).

**2.3** Por que usar parse? Para nunca quebrar o programa!

```haskell
parseMatches :: Value -> Parser [Value] -- extrai lista de partidas
parseMatches = withObject "response" $ \o -> do -- lambda que recebe o objeto do JSON
  Array matches <- o .: "matches"
  return (V.toList matches) -- vetor pra lista normal
```

```haskell
parseScore :: Value -> Parser (Maybe Int, Maybe Int) -- extrai placares | maybe int pois dados podem nao existir
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
        _ -> return (Nothing, Nothing) -- se nao existir
    _ -> return (Nothing, Nothing)
```

### 3. Sistema de Filtros

**3.1** Como aplicar múltiplos filtros de forma funcional?

  - Na `main.hs`, a filtragem ocorre antes de os dados serem enviados para o frontend

```haskell
-- primeira versão
filtrarJogos :: Text -> [Value] -> [Value]
filtrarJogos "finalizados" matches 
filtrarJogos "futuros" matches 
```

**3.2** Composição de funções

```haskell
-- 1. Parse: Extrai array de matches 
-- 2. filtrarPorStatus "finalizados": Remove jogos SCHEDULED
-- 3. filtrarPorLocal "casa": Remove jogos onde Inter joga fora  
-- 4. ordenarPorRodada: Ordena por matchday (5, 15)

aplicarFiltros :: Text -> Text -> Value -> Value
aplicarFiltros filtro local dadosOriginais = 
  case parseMaybe parseMatches dadosOriginais of
    Just matches -> 
      let matchesFiltrados = ordenarPorRodada (filtrarPorLocal local (filtrarPorStatus filtro matches))
      in object ["matches" .= matchesFiltrados]
    _ -> dadosOriginais
```

### 4. Roteamento Dinâmico

**4.1** Criar rotas dinâmicas para diferentes competições e anos

```haskell
-- Tentativa com rotas hardcoded
get "/api/brasileirao/2024" $ do
get "/api/brasileirao/2025" $ do
get "/api/libertadores/2024" $ do
```

**4.2** Parametrização de rotas

```haskell
-- Trecho do código com rotas parametrizadas
get "/api/team/:name/:dataType/:year" $ do
  dataType <- pathParam "dataType" :: ActionM Text
  year <- pathParam "year" :: ActionM Text
  
  let requestUrl = case dataType of
        "brasileirao" -> "http://api.football-data.org/v4/teams/6684/matches?competitions=2013&season=" ++ T.unpack year
        "libertadores" -> "http://api.football-data.org/v4/teams/6684/matches?competitions=2152&season=" ++ T.unpack year
```

### 5. Filtros por Local (Casa/Fora)

**5.1** Identificar quando o Internacional joga em casa ou fora

```haskell
-- Primeira tentativa
jogoCasa :: Value -> Bool
jogoCasa jogo = 
  case parseMaybe parseHomeTeam jogo of
    Just "Internacional" -> True
    _ -> False
```

**5.2** Descobri que por algum motivo as vezes a API retornava "Internacional" e outras vezes "Sport Club Inernacional"

```haskell
-- Versão final - busca por substring
jogoCasa :: Value -> Bool
jogoCasa jogo = 
  case parseMaybe parseHomeTeam jogo of
    Just nome -> "Internacional" `T.isInfixOf` T.pack nome || "Internacional" == T.pack nome
    _ -> False
```

## Orientações para execução

```bash
# Instalar dependências Haskell
cabal install scotty aeson http-simple vector

# Executar o servidor
runhaskell main.hs

# Acessar http://localhost:3000
```

# Resultado final: demonstrar execução em GIF animado ou vídeo curto (máximo 60s)

---

# Referências e créditos: 

ELC117 – Paradigmas de Programação. Slides da aula: Backend Web com Scotty (Haskell). Disponível em: https://liascript.github.io/course/?https://raw.githubusercontent.com/elc117/demo-scotty-codespace-2025b/main/README.md#1

Football-Data. (n.d.). Quickstart – Football Data API Documentation. Retrieved September 14, 2025, from https://www.football-data.org/documentation/quickstart

HASKELL. Web.Scotty — Module documentation (scotty-0.22). Disponível em: https://hackage.haskell.org/package/scotty-0.22/docs/Web-Scotty.html

HASKELL. Scotty — Hackage. Disponível em: https://hackage.haskell.org/package/scotty

Build a Haskell Server with Scotty framework. 2024. Disponível em: https://www.youtube.com/watch?v=psTTKGj9G6Y



