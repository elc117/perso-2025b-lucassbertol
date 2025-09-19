# Identificação

- **Autor:** Lucas Salvini Bertol  
- **Curso:** Sistemas de Informação  
- **Disciplina:** Paradigmas de Programação  

---

# Tema / Objetivo

O *interScore* é uma aplicação web para consulta de informações do Sport Club Internacional, desenvolvida com **backend em Haskell** usando **`Scotty`** e frontend simples em HTML/JavaScript. O projeto consome dados da [API Football-Data.org](https://www.football-data.org/) para exibir informações sobre jogos do Brasileirão, Libertadores, calendário e elenco atual do clube, com filtros específicos em cada ocasão.

---

# Processo de desenvolvimento

### 1. Configuração Inicial da API

**1.1** Estabelecer comunicação com a [API](https://www.football-data.org/)

- Primeiramente realizei um teste com uma rota simples:

```haskell
main = scotty 3000 $ do
  get "/api/test" $ do
    text "API funcionando"
```

- Vi que era necessário uma autenticação para usar a API, segui o tutorial do [site](https://www.football-data.org/) deles para conseguir a Key e fazer a autenticação

**1.2** Implementação da autenticação

```haskell
apiKey :: String
apiKey = "a8b13670a43b43079197e65af09c6065"

request <- parseRequest requestUrl
let requestAuth = setRequestHeader "X-Auth-Token" [BS.pack apiKey] request
```

### 2. Roteamento Dinâmico

**2.1** Criar rotas dinâmicas para diferentes competições e anos

- Criei mais rotas para teste com diferentes competições e anos. As rotas se encontram especificadas no website da API.

```haskell
-- tentativa com rotas hardcoded
get "/api/brasileirao/2024" $ do
get "/api/brasileirao/2025" $ do
get "/api/libertadores/2024" $ do
```

**2.2** Parametrização de rotas

- Inseri parâmetros nas rotas, facilitando e compactando o código para suprir minhas necessidades

```haskell
-- trecho do código com rotas parametrizadas
get "/api/team/:name/:dataType/:year" $ do
  dataType <- pathParam "dataType" :: ActionM Text
  year <- pathParam "year" :: ActionM Text
  
  let requestUrl = case dataType of
        "brasileirao" -> "http://api.football-data.org/v4/teams/6684/matches?competitions=2013&season=" ++ T.unpack year
        "libertadores" -> "http://api.football-data.org/v4/teams/6684/matches?competitions=2152&season=" ++ T.unpack year
```


### 3. Manipulação de JSON e parsing

**3.1** Extrair dados específicos do JSON cru da API

- A integração com football-data.org apresentou desafios na manipulação de dados JSON extensos e complexos. A estrutura retornada pela API possui múltiplos níveis de resposta, com campos opcionais que podem ou não estar presentes dependendo da escolha do usuário.

**3.2** Desenvolvimento de parsers específicos

- Pesquisando sobre manipulação de JSON em Haskell, descobri o conceito de **parsing** que é uma técnica que permite transformar dados brutos em estruturas organizadas e type-safe.

- Criei parsers específicos para cada tipo de dado necessário. Essa estratégia permitiu maior flexibilidade e facilita a manutenção do código no futuro

- Vi uma analogia quando pesquisava por **parsing** que achei interessante:
  - É como chamar um entregador:
  - JSON = Prédio com apartamentos
  - Parser = Entregador
  - Você = Quem pede: "Entregador, vá até apartamento 'score.fullTime.home' e me traga o que está lá"


```haskell
-- parser principal para extrair array de partidas
parseMatches :: Value -> Parser [Value]
parseMatches = withObject "response" $ \o -> do
  Array matches <- o .: "matches"
  return (V.toList matches)

-- parsers específicos para diferentes campos
parseScore :: Value -> Parser (Maybe Int, Maybe Int)
parseHomeTeam :: Value -> Parser String
parseAwayTeam :: Value -> Parser String
parseMatchday :: Value -> Parser Int
```

- Tabém aderi ao uso de `parseMaybe` em algumas ocasiões, para que falhas na extração de dados não acabem quebrando o sistema inteiro

```haskell
jogoFinalizado :: Value -> Bool
jogoFinalizado jogo = 
  case parseMaybe parseScore jogo of
    Just (Just _, Just _) -> True -- ambos os placares existem
    _ -> False                    -- qualquer falha = jogo não finalizado em vez de erro
```

- O parser mais complexo desenvolvido foi o `parseScore`, com o uso de operadores opcionais (`.:?`), em vez dos obrigatóorios (`.:`), para funcionar com jogos ja terminados e jogos futuros, que no caso estão sem os placares.
  
```haskell
parseScore :: Value -> Parser (Maybe Int, Maybe Int)
parseScore = withObject "match" $ \o -> do
  score <- o .:? "score"          -- busca opcional
  case score of
    Just s -> do
      fullTime <- s .:? "fullTime"  -- jogo finalizado (ou não) 
      case fullTime of
        Just ft -> do
          home <- ft .:? "home"     -- (casa ou fora)
          away <- ft .:? "away"     
          return (home, away)
        _ -> return (Nothing, Nothing)
    _ -> return (Nothing, Nothing)
```

### 4. Sistema de Filtros

- Na `main.hs`, a filtragem ocorre antes de os dados serem enviados para o frontend
- Os dados passam por varias etapas sequenciais 

```haskell
aplicarFiltros :: Text -> Text -> Value -> Value   -- função principal de aplicar filtros
aplicarFiltros filtro local dadosOriginais = 
  case parseMaybe parseMatches dadosOriginais of
    Just matches -> 
      let matchesFiltrados = ordenarPorRodada (filtrarPorLocal local (filtrarPorStatus filtro matches))    -- dados passam por 3 filtros
      in object ["matches" .= matchesFiltrados]   -- json de resposta construido
    _ -> dadosOriginais
```

- Outros filtros desenvolvidos:
```haskell
filtrarPorStatus :: Text -> [Value] -> [Value]
jogoFinalizado :: Value -> Bool
jogoFuturo :: Value -> Bool
filtrarPorLocal :: Text -> [Value] -> [Value]
jogoCasa :: Value -> Bool
jogoFora :: Value -> Bool
ordenarPorRodada :: [Value] -> [Value]
extrairRodada :: Value -> Int
```

**Exemplo de saída flitrada:**
```json
{
  "matches": [
    {"matchday": 5, "homeTeam": "Internacional", "status": "FINISHED"},
    {"matchday": 15, "homeTeam": "Internacional", "status": "FINISHED"}
  ]
}
``` 

### 5. Filtros por Local (Casa/Fora)

**5.1** Caso curioso no filtro de identificar quando o Internacional joga em casa ou fora

```haskell
-- primeira tentativa
jogoCasa :: Value -> Bool
jogoCasa jogo = 
  case parseMaybe parseHomeTeam jogo of
    Just "Internacional" -> True
    _ -> False
```

- Descobri que por algum motivo as vezes a API retornava "Internacional" e outras vezes "Sport Club Inernacional" causando erro no teste acima
- A solução foi implementar `T.isInfixOf`, que "detecta se tem uma palavra dentro da palavra escolhida", no meu caso, verifica se tem Internacional dentro do escolhido

```haskell
-- final - busca por substring
jogoCasa :: Value -> Bool
jogoCasa jogo = 
  case parseMaybe parseHomeTeam jogo of
    Just nome -> "Internacional" `T.isInfixOf` T.pack nome || "Internacional" == T.pack nome
    _ -> False
```

---

# Orientações para execução

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



