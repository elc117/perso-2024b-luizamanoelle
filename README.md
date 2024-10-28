## Identificação

- **Nome:** Luiza Manoelle dos Santos
- **Curso:** Sistemas de Informação - 2024
- **Disciplina:** Paradigmas de Programação

## Tema/Objetivo

O projeto é um site web de receitas, que visa explorar o uso da linguagem Haskell em desenvolvimento web através da criação de uma página de receitas interativa. A ideia é implementar uma interface onde os usuários possam:

1. **Visualizar receitas**: Consultar receitas já cadastradas, com detalhes sobre o nome, ingredientes e a classificação (doce ou salgado).
2. **Buscar receitas**: Permitir a pesquisa por nome e categoria de receitas (doces ou salgadas), facilitando a localização das preferências do usuário.
3. **Cadastrar novas receitas**: Adicionar receitas diretamente na página, de forma que sejam registradas em um arquivo e possam ser acessadas nas próximas consultas.

Durante o desenvolvimento, um desafio adicional foi aprimorar a interface, tornando-a mais amigável e intuitiva para o usuário. Esse esforço de design trouxe um valor significativo ao projeto, melhorando a experiência geral de uso.

Um objetivo importante do projeto foi testar os limites e a flexibilidade da linguagem Haskell no desenvolvimento de um servidor web.

## Processo de Desenvolvimento

### Instalação e Configuração

Configurar o ambiente Haskell e instalar `Scotty` foi um dos principais desafios do projeto. Inicialmente, tentei utilizar outras bibliotecas, mas desisti devido à complexidade de configurá-las e compilá-las no ambiente local. Ao final, escolhi trabalhar com `Scotty` pela compatibilidade com o objetivo do projeto.

Para superar os obstáculos iniciais de configuração, consultei várias fontes e vídeos até encontrar uma solução eficiente utilizando o **GHCup**, disponível no site oficial do Haskell ([haskell.org/ghcup](https://www.haskell.org/ghcup/)). O GHCup facilitou a instalação e configuração do GHC (Glasgow Haskell Compiler), permitindo que eu instalasse o ambiente de Haskell e rodasse o projeto localmente.

Com o ambiente configurado, para rodar o projeto, naveguei até o diretório onde os arquivos estavam armazenados e utilizei o comando `runhaskell` para executar o código diretamente, sem precisar compilar. A biblioteca `Scotty` requer a extensão `OverloadedStrings`, que facilita o uso do tipo `Text` para manipulação de dados na API, tornando o desenvolvimento mais direto e eficiente.

### Importações

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

A extensão `OverloadedStrings` permite o uso direto de literais de texto (`String`) como o tipo `Data.Text.Lazy.Text`, necessário para `Scotty`.

```haskell
import Web.Scotty
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.Monoid ((<>))
import Data.List (uncons)

```

1. **`Web.Scotty`**: Biblioteca principal para o desenvolvimento da API. Ela fornece funções como `get`, `post`, `put`, `delete`, que facilitam a definição de rotas e ações para diferentes métodos HTTP.
2. **`Data.Text.Lazy` e `Data.Text.Lazy.IO`**:  Essas bibliotecas são usadas para manipular textos de maneira eficiente, especialmente textos longos. `Data.Text.Lazy` permite usar o tipo `Text`, que é mais performático que `String`, e `Data.Text.Lazy.IO` lida com operações de entrada e saída (I/O).
3. **`Data.Monoid`**: O operador `(<>)` concatena textos.
4. **`Data.List`**: Função `uncons` para dividir listas em cabeça e cauda. (Usada para buscar receitas pelo nome por exemplo)

### Etapas e Comentários

O desenvolvimento foi estruturado com a divisão em várias etapas, a fim de permitir um melhor acompanhamento das funcionalidades e garantir flexibilidade para ajustes.

1. **Configuração Inicial e Layout**: O primeiro passo foi configurar o ambiente com o Scotty e criar a interface inicial do site A abordagem inicial focou na simplicidade para garantir que a interface fosse amigável e intuitiva.
2. **Desenvolvimento das Rotas Principais**:
    - **Rota de Listagem** (`/receitas`): Exibe todas as receitas com uma divisão visual entre categorias.
    - **Rota de Categoria** (`/receitas/doces` e `/receitas/salgadas`): As rotas foram configuradas para exibir receitas com filtragem por categoria.
    - **Rota de Busca** (`/receita/:nome`): Implementada com um sistema de verificação que retorna a receita caso o nome buscado esteja armazenado, retornando uma mensagem de erro caso contrário.
3. **Armazenamento e Adição de Receitas**: O arquivo `recipes.txt` foi utilizado para armazenar as receitas. Uma rota `POST` foi criada para receber o formulário de nova receita e adicionar o conteúdo ao arquivo.
4. **Testes e Ajustes de Estilo**: Após a implementação inicial das funcionalidades, ajustes de estilo e layout foram realizados para melhorar a visualização, adicionando uma estilização CSS dentro das rotas HTML.

### Dificuldades e Soluções

Durante o desenvolvimento, alguns desafios surgiram:

- **Processamento de Textos em Haskell**: A manipulação de strings e textos foi um ponto de atenção devido à necessidade de dividir e processar grandes blocos de texto. Funções como `splitOn` e `filter` foram empregadas para manipulação dos dados.
- **Busca por Nome em Rota Dinâmica**: Inicialmente, houve dificuldades em implementar a busca por uma receita específica usando uma rota dinâmica. A solução foi utilizar `T.toLower` para evitar problemas de case-sensitive e melhorar a precisão na busca.
- **Erro ao esquecer de implementar `POST` para adicionar receitas:** No desenvolvimento inicial, utilizei apenas a rota `GET` para todas as funcionalidades, incluindo a exibição e o cadastro de receitas. Esqueci de adicionar o `POST` necessário para processar o formulário de adição de receitas, o que impediu o salvamento correto das novas receitas no arquivo. A solução foi criar a rota `POST "/nova-receita"` que, ao receber os dados do formulário, escreve a nova receita em `recipes.txt`.
- **Dificuldade em filtrar categorias de receitas:** Para exibir apenas as receitas "doces" ou "salgadas", precisei desenvolver a função `filtrarReceitasPorCategoria`. Testei várias abordagens para fazer a comparação de texto da categoria com o cabeçalho "Categoria:", até que consegui uma solução satisfatória utilizando `T.isInfixOf`.

## Fontes Consultadas

Para o desenvolvimento deste projeto, as seguintes fontes foram consultadas:

Aqui estão as referências em formato Markdown:

- [Como Fazer um WebApp em Haskell - YouTube](https://www.youtube.com/watch?v=jjuSXbv1nW8&t=72s)
- [Guia de Introdução ao Cabal](https://cabal.readthedocs.io/en/latest/getting-started.html#creating-a-new-application)
- [Wiki Haskell: Como Escrever um Programa em Haskell](https://wiki.haskell.org/How_to_write_a_Haskell_program)
- [Pacote Scotty no Hackage](https://hackage.haskell.org/package/scotty)
- [Construindo um Website com Haskell - Adit.io](https://www.adit.io/posts/2013-04-15-making-a-website-with-haskell.html)
- [24 Dias de Hackage: Scotty - ocharles.org.uk](https://blog.ocharles.org.uk/blog/posts/2013-12-05-24-days-of-hackage-scotty.html)
- [Como Ler um Arquivo como String em Haskell - Stack Overflow](https://stackoverflow.com/questions/48687921/how-can-i-read-a-file-to-a-string-in-haskell)
- [Documentação do Data.Text.Lazy.IO](https://downloads.haskell.org/ghc/latest/docs/libraries/text-2.1.1-cbd6/Data-Text-Lazy-IO.html)
- [Exemplo de Código no Gist do GitHub](https://gist.github.com/dino-/28b09c465c756c44b2c91d777408e166)
- [Guia Completo de Listas em Haskell - haskelltutorials.com](https://www.haskelltutorials.com/guides/haskell-lists-ultimate-guide.html)
- [Documentação do Data.List no Hackage](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-List.html)
- [O Que é um Monoid - Serokell Blog](https://serokell.io/blog/whats-that-typeclass-monoid)
- [Repositório Scotty no GitHub](https://github.com/scotty-web/scotty)

## Histórico de Versões e Erros

| Versão | Data | Descrição | Erros e Soluções |
| --- | --- | --- | --- |
| 1.0 | 2024-10-15 | Implementação inicial com listagem e categorias para receitas | Problemas de rotas com Scotty e erros de parsing de string |
| 1.1 | 2024-10-20 | Adição de rota de busca específica `/receita/:nome` e formatação de layout | Ajuste de case-sensitive na busca por nome |
| 1.2 | 2024-10-25 | Adição do formulário para nova receita com armazenamento no arquivo `recipes.txt` | Correção no tratamento de entrada inválida no formulário |
| 1.3 | 2024-10-25 | Adição de comentários no código e melhorias na documentação. | A função `buscarReceita` não lidava corretamente com receitas que não existiam, causando falhas no servidor ao buscar receitas. |
| 1.4 | 2024-10-25 | Implementação de formatação de receitas para melhor apresentação |  Dificuldade em formatar as listas para colocar no link de receitas; dificuldade em achar o título. |
| 1.5 | 2024-10-28 | Melhorias de layout e estilização em HTML e CSS | Melhoria na legibilidade dos textos de receitas; Após inserir uma receita, a visualização imediata da mesma foi implementada para melhor usabilidade. |

## Melhorias na Interface

Neste projeto, foram implementadas diversas melhorias na interface para proporcionar uma experiência mais atraente e amigável ao usuário. As principais mudanças em relação ao padrão original incluem:

1. **Estilo Visual Atraente**: O design da página foi aprimorado com um esquema de cores mais vibrante e uma tipografia mais moderna, utilizando a fonte Arial para uma melhor legibilidade.
2. **Layout Responsivo**: A interface foi organizada em um layout flexível e responsivo, utilizando `flexbox` e `grid` para garantir uma apresentação adequada em diferentes tamanhos de tela.
3. **Cards para Receitas**: As receitas são exibidas em cards, com bordas arredondadas e sombras sutis, proporcionando um efeito de destaque. Ao passar o mouse, os cards aumentam levemente, melhorando a interatividade.
4. **Botões Estilizados**: Os botões foram estilizados com cores chamativas e efeitos de transição, tornando a navegação mais intuitiva e visualmente agradável.
5. **Formulários Melhorados**: O formulário para adicionar novas receitas possui campos com estilos aprimorados, incluindo bordas arredondadas e um esquema de cores que combina com o restante da interface.
6. **Seções Claras**: O uso de títulos e seções bem definidas melhora a organização das informações, facilitando a navegação e a busca por receitas específicas.

Essas melhorias visam criar uma interface mais envolvente e facilitar a interação do usuário com o site.
