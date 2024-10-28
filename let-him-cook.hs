{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.Monoid ((<>))
import Data.List (uncons)

main :: IO ()
main = scotty 3000 $ do
    -- Rota principal
    get "/" $ do
        text "Bem-vindo ao aplicativo de receitas! Acesse /receitas para ver todas as receitas, /receitas/doces para receitas doces, /receitas/salgadas para receitas salgadas, /receita/:nome para buscar uma receita específica, ou /nova-receita para adicionar uma nova receita."

    -- Exibe todas as receitas
    get "/receitas" $ do
        receitas <- liftIO (TIO.readFile "recipes.txt")
        html $ T.concat ["<h1>Receitas</h1><pre>", receitas, "</pre>"]

    -- Exibe receitas doces
    get "/receitas/doces" $ do
        receitas <- liftIO (TIO.readFile "recipes.txt")
        let receitasDoces = filtrarReceitasPorCategoria "doce" receitas
        html $ T.concat ["<h1>Receitas Doces</h1><pre>", receitasDoces, "</pre>"]

    -- Exibe receitas salgadas
    get "/receitas/salgadas" $ do
        receitas <- liftIO (TIO.readFile "recipes.txt")
        let receitasSalgadas = filtrarReceitasPorCategoria "salgada" receitas
        html $ T.concat ["<h1>Receitas Salgadas</h1><pre>", receitasSalgadas, "</pre>"]

    -- Busca uma receita específica
    get "/receita/:nome" $ do
        nomeReceita <- pathParam "nome"
        receitas <- liftIO (TIO.readFile "recipes.txt")
        let receitaEncontrada = buscarReceita (T.toLower nomeReceita) receitas
        case receitaEncontrada of
            Just receita -> html $ T.concat ["<h1>", T.toTitle nomeReceita, "</h1><pre>", receita, "</pre>"]
            Nothing -> text "Receita não encontrada."

    -- Formulário para adicionar nova receita
    get "/nova-receita" $ do
        html $ T.concat
            [ "<h1>Adicionar Nova Receita</h1>"
            , "<form method='POST' action='/nova-receita'>"
            , "Nome: <input type='text' name='nome'><br>"
            , "Categoria (doce/salgada): <input type='text' name='categoria'><br>"
            , "Ingredientes: <textarea name='ingredientes'></textarea><br>"
            , "Instruções: <textarea name='instrucoes'></textarea><br>"
            , "<input type='submit' value='Adicionar Receita'>"
            , "</form>"
            ]

    -- Processa a nova receita enviada pelo formulário
    post "/nova-receita" $ do
        nome <- formParam "nome"
        categoria <- formParam "categoria"
        ingredientes <- formParam "ingredientes"
        instrucoes <- formParam "instrucoes"
        
        let novaReceita = T.concat
                [ "Categoria: ", categoria, "\n"
                , nome, "\n"
                , "Ingredientes:\n", ingredientes, "\n"
                , "Instruções:\n", instrucoes, "\n\n---\n\n"
                ]
        
        -- Adiciona a nova receita ao arquivo
        liftIO $ TIO.appendFile "recipes.txt" novaReceita
        
        html $ T.concat ["<h1>Receita Adicionada</h1><p>", nome, " foi adicionada com sucesso!</p>"]

-- Função que busca uma receita no arquivo de receitas
buscarReceita :: T.Text -> T.Text -> Maybe T.Text
buscarReceita nomeReceita receitas = 
    let listaReceitas = T.splitOn "---" receitas
        receitaFiltrada = filter (\r -> T.toLower nomeReceita `T.isInfixOf` T.toLower r) listaReceitas
    in fmap fst (uncons receitaFiltrada)

-- Função que filtra receitas por categoria (doces ou salgadas)
filtrarReceitasPorCategoria :: T.Text -> T.Text -> T.Text
filtrarReceitasPorCategoria categoria receitas = 
    let listaReceitas = T.splitOn "---" receitas
        receitasFiltradas = filter (\r -> T.isInfixOf (T.concat ["Categoria: ", categoria]) r) listaReceitas
    in T.intercalate "---" receitasFiltradas
