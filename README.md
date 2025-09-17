# Relatório do Trabalho – Paradigmas de Programação

## Identificação

- **Autor:** Lucas Salvini Bertol  
- **Curso:** Sistemas de Informação  
- **Disciplina:** Paradigmas de Programação  

---

## Tema / Objetivo

O objetivo do trabalho foi **desenvolver um backend em Haskell** utilizando a biblioteca **Scotty** e de minha escolha foi integrar com a API [football-data.org](https://www.football-data.org/).  

Esse backend foi projetado para consultar informações do **Sport Club Internacional**, como:  
- Partidas do Brasileirão (com opção de filtro por ano)  
- Partidas da Libertadores  
- Próximos jogos (calendário, com filtro por jogos "em casa" ou "fora")  
- Elenco de jogadores  

O frontend foi construído com **HTML, CSS e JavaScript**, consumindo as rotas expostas pelo backend em Haskell, exibindo os dados de forma organizada e acessível ao usuário.  

---

## Processo de Desenvolvimento

Durante o desenvolvimento, foram explorados conceitos fundamentais de programação funcional aplicados ao **backend em Haskell**, como:  
- Uso de **monads** para manipulação de efeitos (I/O e HTTP).  
- Criação de rotas com **Scotty**.  
- Manipulação de JSON com a biblioteca **Aeson**.  

- **Rotas dinâmicas:**  
  No começo implementei apenas uma rota fixa (`/api/team/brasileirao`).  
  - Depois percebi a necessidade de parametrizar ano e tipo de dado (competição/jogadores).  
  - Resolvi isso adicionando rotas com parâmetros (`/api/team/:name/:dataType/:year`).

- **Tratammento de erros:**
  - Decode muitas vezes retornava com `nothing`
  - Com o Scotty, consegui devolver uma resposta JSON de erro no lugar de deixar o servidor não funcionar:
  - `case (decode body :: Maybe Value) of`\
    `Just val -> json val`\
    `Nothing  -> json $ object ["erro" .= ("não consegui ler os dados" :: String)]`


---

## Orientações para Execução

### 1. Instalar dependências
Certifique-se de ter instalado o **GHC** e o **Stack** ou **Cabal**.  
Além disso, são necessárias as bibliotecas:  
- `scotty`  
- `aeson`  
- `http-conduit` (ou `http-simple`)  
- `wai-middleware-static`

### 2. runhaskell main.hs

---

# Resultado final: demonstrar execução em GIF animado ou vídeo curto (máximo 60s)

---

# Referências e créditos (incluindo alguns prompts, se aplicável): 

ELC117 – Paradigmas de Programação. Slides da aula: Backend Web com Scotty (Haskell). Disponível em: https://liascript.github.io/course/?https://raw.githubusercontent.com/elc117/demo-scotty-codespace-2025b/main/README.md#1

Football-Data. (n.d.). Quickstart – Football Data API Documentation. Retrieved September 14, 2025, from https://www.football-data.org/documentation/quickstart

HASKELL. Web.Scotty — Module documentation (scotty-0.22). Disponível em: https://hackage.haskell.org/package/scotty-0.22/docs/Web-Scotty.html

HASKELL. Scotty — Hackage. Disponível em: https://hackage.haskell.org/package/scotty

