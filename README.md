# Identificação

- **Autor:** Lucas Salvini Bertol  
- **Curso:** Sistemas de Informação  
- **Disciplina:** Paradigmas de Programação  

---

# Tema / Objetivo

O *interScore* é uma aplicação web para consulta de informações do Sport Club Internacional, desenvolvida com **backend em Haskell** usando **`Scotty`** e frontend simples em HTML/JavaScript. O projeto consome dados da [API Football-Data.org](https://www.football-data.org/) para exibir informações sobre jogos do Brasileirão, Libertadores, calendário e elenco atual do clube, com filtros específicos em cada ocasião.

A aplicação oferece funcionalidades de:
- **Consulta de partidas** do Brasileirão e Libertadores por ano
- **Filtragem avançada** por status (finalizados/futuros), local (casa/fora) e resultado (vitória/empate/derrota)
- **Visualização de calendário** com próximas partidas
- **Informações do elenco** atual do clube

---

# Processo de desenvolvimento

### 1. Configuração Inicial da API

**1.1** Estabelecer comunicação com a [API](https://www.football-data.org/)

- Primeiramente realizei um teste com uma rota simples para verificar se o servidor Scotty estava funcionando:

```haskell
main = scotty 3000 $ do
  get "/api/test" $ do
    text "API funcionando"
```

- Durante os primeiros testes, descobri que a API football-data.org retorna erro 403 sem autenticação. Vi que era necessário uma chave de API para usar o serviço, então segui o tutorial do [site](https://www.football-data.org/) deles para conseguir a Key e implementar a autenticação adequada.

**1.2** Implementação da autenticação

```haskell
apiKey :: String
apiKey = "a8b13670a43b43079197e65af09c6065"

request <- parseRequest requestUrl
let requestAuth = setRequestHeader "X-Auth-Token" [BS.pack apiKey] request
```

- A biblioteca `http-conduit` facilitou muito essa implementação com `setRequestHeader`.

### 2. Roteamento Dinâmico

**2.1** Criar rotas dinâmicas para diferentes competições e anos

- Inicialmente criei rotas hardcoded para cada combinação de competição/ano, o que ficaria inviável no futuro

```haskell
-- tentativa inicial com rotas hardcoded descartada
get "/api/brasileirao/2024" $ do
get "/api/brasileirao/2025" $ do
get "/api/libertadores/2024" $ do
get "/api/libertadores/2025" $ do
```

**2.2** Parametrização de rotas

- A solução foi usar parâmetros de rota do Scotty, compactando bastante o código:

```haskell
-- versão final com rotas parametrizadas
get "/api/team/:name/:dataType/:year" $ do
  dataType <- pathParam "dataType" :: ActionM Text
  year <- pathParam "year" :: ActionM Text
  
  let requestUrl = case dataType of
        "brasileirao" -> "http://api.football-data.org/v4/teams/6684/matches?competitions=2013&season=" ++ T.unpack year
        "libertadores" -> "http://api.football-data.org/v4/teams/6684/matches?competitions=2152&season=" ++ T.unpack year
```

- Também implementei query parameters opcionais para os filtros usando `queryParamMaybe`, permitindo URLs como `/api/team/internacional/brasileirao/2024?filtro=finalizados&resultado=vitoria`.

### 3. Manipulação de JSON e parsing

**3.1** Extrair dados específicos do JSON cru da API

- A integração com football-data.org retornava JSONs extensos e complexos. A estrutura retornada pela API possui múltiplos níveis, com campos opcionais que podem ou não estar presentes dependendo do status do jogo (finalizado vs. futuro).

- Exemplo da estrutura JSON complexa retornada pela API:
```json
{
  "matches": [
    {
      "homeTeam": {"shortName": "Internacional", "crest": "url"},
      "awayTeam": {"shortName": "Grêmio", "crest": "url"},
      "score": {
        "fullTime": {"home": 2, "away": 1}  
      },
      "matchday": 15
    }
  ]
}
```

**3.2** Desenvolvimento de parsers específicos

- Pesquisando sobre manipulação de JSON em Haskell, descobri o conceito de **parsing** que é uma técnica que permite transformar dados brutos em estruturas organizadas e type-safe.

- Criei parsers específicos para cada tipo de dado necessário. Essa estratégia modular permitiu maior flexibilidade e facilita a manutenção do código no futuro:

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

- Vi uma analogia quando pesquisava por **parsing** que achei interessante e me ajudou a entender:
  - É como chamar um entregador:
  - JSON = Prédio com apartamentos
  - Parser = Entregador que conhece o prédio
  - Você = Quem pede: "Entregador, vá até apartamento 'score.fullTime.home' e me traga o que está lá"

- Também descobri e utilizei `parseMaybe` em várias situações. O tipo `Maybe` é fundamental em Haskell para lidar com dados que "podem não existir", evitando quebras do código inesperadas:

```haskell
jogoFinalizado :: Value -> Bool
jogoFinalizado jogo = 
  case parseMaybe parseScore jogo of
    Just (Just _, Just _) -> True -- ambos os placares existem
    _ -> False                    -- qualquer falha = jogo não finalizado em vez de erro
```

- O parser mais complexo desenvolvido foi o `parseScore`, utilizando operadores opcionais (`.:?`) em vez dos obrigatórios (`.:`), para funcionar tanto com jogos finalizados quanto com jogos futuros, por exemplo, para diferenciar um jogo terminado em 0x0 de um jogo que ainda não ocorreu (Sem gols também).
  
```haskell
parseScore :: Value -> Parser (Maybe Int, Maybe Int)
parseScore = withObject "match" $ \o -> do
  score <- o .:? "score"          -- busca opcional - pode não existir
  case score of
    Just s -> do
      fullTime <- s .:? "fullTime"  -- jogo pode estar finalizado ou não
      case fullTime of
        Just ft -> do
          home <- ft .:? "home"     -- placar casa
          away <- ft .:? "away"     -- placar visitante
          return (home, away)
        _ -> return (Nothing, Nothing)  -- jogo não finalizado
    _ -> return (Nothing, Nothing)      -- sem dados de placar
```

### 4. Sistema de Filtros

**4.1** Como foi feita as filtragems

- Inicialmente, a filtragem ocorria apenas após no front end, mas, por questões de otimização e facilidade, resolvi passar a filtragem para o backend, a redução do tempo de espera do rretorno dos dados da API foi nítido.

- Na `main.hs`, a filtragem ocorre completamente no backend antes de os dados serem enviados para o frontend.

- Os dados passam por várias etapas sequenciais de filtragem, cada uma refinando o resultado:

```haskell
aplicarFiltros :: Text -> Text -> Text -> Value -> Value   -- função principal de aplicar filtros
aplicarFiltros filtro local resultado dadosOriginais = 
  case parseMaybe parseMatches dadosOriginais of
    Just matches -> 
      let matchesFiltrados = ordenarPorRodada (filtrarPorResultado resultado (filtrarPorLocal local (filtrarPorStatus filtro matches)))
      in object ["matches" .= matchesFiltrados]   -- json de resposta reconstruído
    _ -> dadosOriginais  -- retorna dados originais em caso de erro no parsing
```

**4.2** Implementação de filtros específicos

- Desenvolvi vários filtros com funcionalidades diferentes

```haskell
-- Filtro por status do jogo
filtrarPorStatus :: Text -> [Value] -> [Value]
filtrarPorStatus "finalizados" matches = filter jogoFinalizado matches
filtrarPorStatus "futuros" matches = filter jogoFuturo matches  
filtrarPorStatus _ matches = matches -- "todos" ou qualquer outro valor

-- Funções auxiliares que implementam a lógica de negócio
jogoFinalizado :: Value -> Bool
jogoFuturo :: Value -> Bool
filtrarPorLocal :: Text -> [Value] -> [Value]
jogoCasa :: Value -> Bool
jogoFora :: Value -> Bool
ordenarPorRodada :: [Value] -> [Value]
extrairRodada :: Value -> Int
```

- A função `ordenarPorRodada` utiliza `sortBy` e `comparing` para ordenar pelas datas:

```haskell
ordenarPorRodada :: [Value] -> [Value]
ordenarPorRodada = sortBy (comparing extrairRodada)
```

**Exemplo de transformação de dados:**

Entrada e saída respectivamente: 

```json
{"matches": [{"matchday": 15, "status": "FINISHED"}, {"matchday": 5, "status": "SCHEDULED"}]}

{"matches": [{"matchday": 5, "status": "FINISHED"}, {"matchday": 15, "status": "FINISHED"}]}
```

### 5. Filtros por Local (Casa/Fora)

**5.1** Desafio da inconsistência de nomes

- Durante os testes, descobri um problema interessante: a API às vezes retornava "Internacional" e outras vezes "Sport Club Internacional", causando falhas no meu filtro inicial:

```haskell
-- primeira tentativa (falhava com nomes inconsistentes)
jogoCasa :: Value -> Bool
jogoCasa jogo = 
  case parseMaybe parseHomeTeam jogo of
    Just "Internacional" -> True  -- só funcionava para nome exato
    _ -> False
```

- A solução foi implementar `T.isInfixOf`, que verifica se uma substring existe dentro de outra string, tornando a busca mais robusta:

```haskell
-- solução final - busca por substring
jogoCasa :: Value -> Bool
jogoCasa jogo = 
  case parseMaybe parseHomeTeam jogo of
    Just nome -> "Internacional" `T.isInfixOf` T.pack nome || "Internacional" == T.pack nome
    _ -> False
```

- Esta solução funciona tanto para "Internacional" quanto para "Sport Club Internacional" ou qualquer variação que contenha "Internacional".

### 6. Filtros por Resultado (Vitória/Empate/Derrota)

**6.1** Lógica de resultados

- Implementar filtros por resultado foi mais complexo que imaginei, pois o resultado depende da perspectiva: uma vitória por 2x1 em casa é diferente de uma vitória por 1x2 fora de casa.

```haskell
vitoria :: Value -> Bool
vitoria jogo =
  case parseMaybe parseScore jogo of
    Just (Just home, Just away) -> 
      if jogoCasa jogo 
      then home > away   -- vitória em casa: placar casa > visitante
      else away > home   -- vitória fora: placar visitante > casa
    _ -> False           -- sem placar válido = não é vitória

derrota :: Value -> Bool  
derrota jogo =
  case parseMaybe parseScore jogo of
    Just (Just home, Just away) ->
      if jogoCasa jogo 
      then home < away   -- derrota em casa: placar casa < visitante
      else away < home   -- derrota fora: placar visitante < casa
    _ -> False

empate :: Value -> Bool
empate jogo =
  case parseMaybe parseScore jogo of
    Just (Just home, Just away) -> home == away  -- placares iguais
    _ -> False
```

### 7. "Conexão" Backend ↔ Frontend (Haskell ↔ JavaScript)

**7.1** Arquitetura de comunicação

- O Haskell atua como servidor web completo na porta 3000, servindo tanto os arquivos estáticos (HTML, CSS, JS) quanto a API REST:

```haskell
main = scotty 3000 $ do
  middleware $ staticPolicy (addBase "static")  -- serve arquivos da pasta static
  get "/" $ file "static/index.html"            -- página principal
  
  -- rotas da API
  get "/api/team/:name/:dataType/:year" $ do
  get "/api/team/:name/:dataType" $ do
```

**7.2** Fluxo de dados completo

- O fluxo de uma requisição completa funciona assim:
  1. **Frontend (JavaScript)**: Usuário clica em "Buscar" → JavaScript faz requisição HTTP
  2. **Backend (Haskell)**: Recebe requisição → Extrai parâmetros → Faz requisição para API externa
  3. **API Externa**: Retorna JSON bruto com todos os dados
  4. **Backend (Haskell)**: Aplica parsers → Aplica filtros → Reconstrói JSON filtrado
  5. **Frontend (JavaScript)**: Recebe JSON filtrado → Renderiza na tela

- Exemplo prático de requisição:

**No `index.html`:**
```javascript
async function buscarDadosComAno(campeonato, ano, resultado = "todos") {
  const time = "internacional";
  // requisição com parâmetros de filtro na URL
  const resp = await fetch(`/api/team/${time}/${campeonato}/${ano}?filtro=finalizados&resultado=${resultado}`);
  const data = await resp.json();
  
  // transforma dados para exibição
  const dadosRelevantes = {
    Competicao: data.matches[0]?.competition?.name || "N/A",
    Jogos: data.matches.map(jogo => ({
      Rodada: jogo.matchday,
      Casa: jogo.homeTeam.shortName,
      // ... outros campos
    }))
  };
  
  // renderiza na tela
  document.getElementById("saida").innerHTML = formatarParaTexto(dadosRelevantes);
}
```

**No `main.hs`:**
```haskell
get "/api/team/:name/:dataType/:year" $ do
  dataType <- pathParam "dataType" :: ActionM Text
  year <- pathParam "year" :: ActionM Text
  
  -- extrai parâmetros opcionais de filtro
  filtroMaybe <- queryParamMaybe "filtro"
  resultadoMaybe <- queryParamMaybe "resultado"
  let filtro = case filtroMaybe of Just f -> f; Nothing -> "todos"
  let resultado = case resultadoMaybe of Just r -> r; Nothing -> "todos"
  
  -- monta URL da API externa
  let requestUrl = case dataType of
        "brasileirao" -> "http://api.football-data.org/v4/teams/6684/matches?competitions=2013&season=" ++ T.unpack year
        
  -- faz requisição autenticada
  request <- parseRequest requestUrl
  let requestAuth = setRequestHeader "X-Auth-Token" [BS.pack apiKey] request
  response <- httpLBS requestAuth
  
  -- processa resposta
  case (decode body :: Maybe Value) of
    Just val -> do
      let dadosFiltrados = aplicarFiltros filtro local resultado val
      json dadosFiltrados  -- retorna JSON filtrado
    Nothing -> json $ object ["erro" .= ("não consegui ler os dados" :: String)]
```

**7.3** Tratamento de diferentes tipos de dados

- A aplicação lida com diferentes tipos de conteúdo

```haskell
-- para jogos (aplicar filtros)
let dadosFiltrados = if dataType == "jogadores" 
                    then val  -- jogadores não precisam de filtro de jogos
                    else aplicarFiltros filtro local resultado val
```

### 8. Interface e Experiência do Usuário

**8.1** Design responsivo da aplicação

- Implementei diferentes estados visuais para melhorar a experiência:
  - **Estado vazio**: Quando nenhuma categoria está selecionada
  - **Estado de filtros**: Quando uma categoria é selecionada mas ainda não foi feita busca  
  - **Estado de carregamento**: Durante requisições (especialmente para jogadores)
  - **Estado com conteúdo**: Após receber e processar dados

**8.2** Formatação dinâmica de dados

- Criei funções JavaScript especializadas para formatar diferentes tipos de conteúdo:

```javascript
// para jogos com placares
function formatarParaTexto(dados) {
  if (dados.Jogos && dados.Jogos.length > 0) {
    dados.Jogos.forEach(jogo => {
      texto += `<div class="card">`;
      texto += `Rodada ${jogo.Rodada}<br>`;
      texto += `${jogo.Data}<br><br>`;
      texto += `<img src="${jogo.EmblemaCasa}" width="20" height="20"> ${jogo.Casa} | ${jogo.PlacarCasa} <br>`;
      texto += `<img src="${jogo.EmblemaVisitante}" width="20" height="20"> ${jogo.Visitante} | ${jogo.PlacarVisitante} <br>`;
      texto += `</div>`;
    });
  }
}

// para calendário (jogos futuros sem placar)
function formatarPartidasCalendario(dados) {
  dados.Jogos.forEach(jogo => {
    texto += `<div class="card">`;
    texto += `${jogo.Competicao}<br>`;
    texto += `${jogo.Data}<br><br>`;
    texto += `<img src="${jogo.EmblemaCasa}" width="20" height="20"> ${jogo.Casa} <br>`;
    texto += `<img src="${jogo.EmblemaVisitante}" width="20" height="20"> ${jogo.Visitante} <br>`;
    texto += `</div>`;
  });
}
```

# Orientações para execução

**Instalação de dependências:**
```bash
# Instalar todas as dependências necessárias
cabal install --lib scotty http-simple http-conduit aeson wai-middleware-static vector text bytestring hspec QuickCheck
```

**Execução do servidor:**
```bash
# Executar o servidor principal
runhaskell main.hs
```

**Execução dos testes:**
```bash
# Executar suite de testes
runhaskell spec.hs
```
---

# Resultado final


---

# Referências e créditos

- ELC117 – Paradigmas de Programação. Slides da aula: Backend Web com Scotty (Haskell). Disponível em: https://liascript.github.io/course/?https://raw.githubusercontent.com/elc117/demo-scotty-codespace-2025b/main/README.md#1

- Football-Data. (n.d.). Quickstart – Football Data API Documentation. Retrieved September 14, 2025, from https://www.football-data.org/documentation/quickstart

- HASKELL. Web.Scotty — Module documentation (scotty-0.22). Disponível em: https://hackage.haskell.org/package/scotty-0.22/docs/Web-Scotty.html

- HASKELL. Scotty — Hackage. Disponível em: https://hackage.haskell.org/package/scotty

- Build a Haskell Server with Scotty framework. 2024. Disponível em: https://www.youtube.com/watch?v=psTTKGj9G6Y

- HACKAGE. Data.Maybe: The Maybe type and associated operations. GHC Base Libraries, 2024. Disponível em: https://hackage.haskell.org/package/base/docs/Data-Maybe.html.

- Haskell JSON parsing with Aeson. Real World Haskell. Disponível em: http://book.realworldhaskell.org/

- HACKAGE. Data.Aeson.Types: JSON parsing with Maybe types. Aeson Documentation, 2024. Disponível em: https://hackage.haskell.org/package/aeson/docs/Data-Aeson-Types.html.

# IA / Prompts

- **Scotty**
  - Como fazer rotas com mais de um parâmetro com scotty
  - Como funciona query parameters em scotty
  - Como utilizar arquivos estáticos html css com scotty
 
- **API**
  - Como colocar autenticação na API em haskell para não dar erro 403 forbidden

- **Manipulação de JSON**
  - Quais bibliotecas em haskell e o que pode me ajudar com manipulação de dados em JSON
  - Parsing em haskell
  - Como lidar com campos opcionais em JSON
  - Informações úteis sobre a bilbioteca AESON
  - Diferença entre .: e .:? no Aeson e quando usar cada um
  - Error: couldn't parse JSON field 'score'
  - Como fazer logging para saber o que está dando erro no retorno do JSON
  - Por que preciso usar T.pack e T.unpack 

- **Filtragem**
  - Como ver se uma substring está em uma string em haskell
  - Como fazer um filtro que funciona para tudo e também para critérios específicos
  - Como verificar se um jogo está finalizado quando placar pode ser null
  - Como funciona o maybe em haskell
  - Como ordenar dados por data

- **Função de testes:**
  - Como fazer uma função de testes simples em haskell e quais bibliotecas usar
 
- **CSS/JS**
  - Melhorias do CSS da webpage e algumas partes do JS (Como a formatação dos dados) aperfeiçoadas por ia e refinadas após, por mim, para deixar como eu queria

**Bibliotecas utilizadas:**
- `scotty`: Framework web minimalista para Haskell
- `aeson`: Biblioteca para parsing/encoding JSON
- `http-conduit`: Cliente HTTP com suporte a SSL
- `wai-middleware-static`: Middleware para servir arquivos estáticos
- `hspec`: Framework de testes para Haskell
- `QuickCheck`: Biblioteca para testes baseados em propriedades
