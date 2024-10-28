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
        html $ T.concat
            [
            "<!DOCTYPE html><html><head><title>Receitas</title>",
            "<style>",
            "body { font-family: Arial, sans-serif; background-color: #fce4ec; color: #333; display: flex; justify-content: center; align-items: center; height: 100vh; margin: 0; }",
            ".container { max-width: 600px; padding: 40px; background-color: #ffffff; border-radius: 8px; box-shadow: 0px 4px 15px rgba(0, 0, 0, 0.2); text-align: center; }",
            "h1 { color: #d81b60; font-size: 28px; margin-bottom: 20px; }",
            ".card { background-color: #f8bbd0; margin: 15px 0; padding: 15px; border-radius: 8px; box-shadow: 0px 2px 8px rgba(0, 0, 0, 0.1); transition: transform 0.2s; }",
            ".card:hover { transform: scale(1.05); }",
            ".button { display: inline-block; padding: 10px 15px; margin: 10px 0; font-size: 16px; font-weight: bold; color: white; background-color: #d81b60; border: none; border-radius: 5px; text-decoration: none; transition: background-color 0.3s; }",
            ".button:hover { background-color: #c2185b; }",
            "</style></head><body>",
            "<div class='container'>",
            "<h1>Let Him Cook!</h1>",
            "<div class='card'><a class='button' href='/receitas'>Ver todas as receitas</a></div>",
            "<div class='card'><a class='button' href='/receitas/doces'>Receitas Doces</a></div>",
            "<div class='card'><a class='button' href='/receitas/salgadas'>Receitas Salgadas</a></div>",
            "<div class='card'><a class='button' href='/nova-receita'>Adicionar Nova Receita</a></div>",
            "<p>Para buscar uma receita específica, acesse <code>/receita/&lt;nome&gt;</code>, substituindo &lt;nome&gt; pelo nome da receita.</p>",
            "</div></body></html>"
            ]

    -- Exibe todas as receitas
    get "/receitas" $ do
        receitas <- liftIO (TIO.readFile "recipes.txt")
        let listaReceitas = T.splitOn "---" receitas
            receitasFormatadas = map formatarReceita listaReceitas
        html $ T.concat
             [ "<!DOCTYPE html><html><head><title>Todas as Receitas</title>"
            , "<style>"
            , "body { font-family: Arial, sans-serif; background-color: #fce4ec; color: #333; padding: 20px; }"
            , "h1 { color: #d81b60; font-size: 28px; text-align: center; }"
            , ".receitas-container { display: grid; grid-template-columns: 1fr; gap: 20px; max-width: 800px; margin: 0 auto; }"
            , ".receita-card { background-color:  #f8bbd0; padding: 20px; border-radius: 8px; box-shadow: 0px 4px 12px rgba(0, 0, 0, 0.1); }"
            , ".receita-titulo { font-size: 22px; font-weight: bold; color: #333; margin-bottom: 10px; }"
            , ".receita-categoria { font-size: 14px; color: #555; margin-bottom: 10px; font-style: italic; }"
            , ".receita-ingredientes, .receita-instrucoes { margin-top: 10px; }"
            , ".section-titulo { font-size: 18px; font-weight: bold; margin-bottom: 5px; }"
            , "</style></head><body>"
            , "<h1>Todas as Receitas</h1>"
            , "<div class='receitas-container'>"
            , T.concat receitasFormatadas
            , "</div></body></html>"
            ]
            
    -- Exibe receitas doces
    get "/receitas/doces" $ do
            receitas <- liftIO (TIO.readFile "recipes.txt")
            let receitasDoces = filtrarReceitasPorCategoria "doce" receitas
                listaReceitas = T.splitOn "---" receitasDoces
                receitasFormatadas = map formatarReceita listaReceitas
            html $ T.concat
                [ "<!DOCTYPE html><html><head><title>Receitas Doces</title>"
                , "<style>"
                , "body { font-family: Arial, sans-serif; background-color: #fce4ec; color: #333; padding: 20px; }"
                , "h1 { color: #d81b60; font-size: 28px; text-align: center; }"
                , ".receitas-container { display: grid; grid-template-columns: 1fr; gap: 20px; max-width: 800px; margin: 0 auto; }"
                , ".receita-card { background-color: #f8bbd0; padding: 20px; border-radius: 8px; box-shadow: 0px 4px 12px rgba(0, 0, 0, 0.1); }"
                , ".receita-titulo { font-size: 22px; font-weight: bold; color: #333; margin-bottom: 10px; }"
                , ".receita-categoria { font-size: 14px; color: #555; margin-bottom: 10px; font-style: italic; }"
                , ".receita-ingredientes, .receita-instrucoes { margin-top: 10px; }"
                , ".section-titulo { font-size: 18px; font-weight: bold; margin-bottom: 5px; }"
                , "</style></head><body>"
                , "<h1>Receitas Doces</h1>"
                , "<div class='receitas-container'>"
                , T.concat receitasFormatadas
                , "</div></body></html>"
                ]


-- Exibe receitas salgadas
    get "/receitas/salgadas" $ do
            receitas <- liftIO (TIO.readFile "recipes.txt")
            let receitasSalgadas = filtrarReceitasPorCategoria "salgada" receitas
                listaReceitasSalgadas = T.splitOn "---" receitasSalgadas
                receitasFormatadas = map formatarReceita listaReceitasSalgadas
            html $ T.concat
                [ "<!DOCTYPE html><html><head><title>Receitas Salgadas</title>"
                , "<style>"
                , "body { font-family: Arial, sans-serif; background-color: #fce4ec; color: #333; padding: 20px; }"
                , "h1 { color: #d81b60; font-size: 28px; text-align: center; }"
                , ".receitas-container { display: grid; grid-template-columns: 1fr; gap: 20px; max-width: 800px; margin: 0 auto; }"
                , ".receita-card { background-color: #f8bbd0; padding: 20px; border-radius: 8px; box-shadow: 0px 4px 12px rgba(0, 0, 0, 0.1); }"
                , ".receita-titulo { font-size: 22px; font-weight: bold; color: #333; margin-bottom: 10px; }"
                , ".receita-categoria { font-size: 14px; color: #555; margin-bottom: 10px; font-style: italic; }"
                , ".receita-ingredientes, .receita-instrucoes { margin-top: 10px; }"
                , ".section-titulo { font-size: 18px; font-weight: bold; margin-bottom: 5px; }"
                , "</style></head><body>"
                , "<h1>Receitas Salgadas</h1>"
                , "<div class='receitas-container'>"
                , T.concat receitasFormatadas
                , "</div></body></html>"
                ]


    -- Busca uma receita específica
    get "/receita/:nome" $ do
        nomeReceita <- pathParam "nome"
        receitas <- liftIO (TIO.readFile "recipes.txt")
        let receitaEncontrada = buscarReceita (T.toLower nomeReceita) receitas
        case receitaEncontrada of
            Just receita -> html $ T.concat 
                [ "<div style='display: flex; align-items: center; justify-content: center; min-height: 100vh; font-family: Arial, sans-serif;'>"
                , "<div style='max-width: 700px; text-align: center;'>"
                , "<h1 style='color: #e75480; font-size: 2.5em; border-bottom: 3px solid #ffb6c1; padding-bottom: 10px;'>"
                , T.toTitle nomeReceita
                , "</h1>"
                , "<pre style='text-align: left; background-color: #ffe4e1; padding: 20px; border-radius: 8px; border: 2px solid #ffb6c1; font-size: 1.2em;'>"
                , receita
                , "</pre>"
                , "</div></div>"
                ]
            Nothing -> text "Receita não encontrada."

    -- Formulário para adicionar nova receita
    get "/nova-receita" $ do
        html $ T.concat
            [ "<!DOCTYPE html><html><head><title>Adicionar Nova Receita</title>"
            , "<style>"
            , "body { font-family: Arial, sans-serif; background-color: #fce4ec; color: #333; display: flex; justify-content: center; align-items: center; height: 100vh; margin: 0; }"
            , ".container { max-width: 600px; padding: 40px; background-color: #ffffff; border-radius: 8px; box-shadow: 0px 4px 15px rgba(0, 0, 0, 0.2); }"
            , "h1 { color: #d81b60; font-size: 28px; margin-bottom: 20px; text-align: center; }"
            , "label { display: block; margin-bottom: 5px; font-weight: bold; }"
            , "input[type='text'], textarea { width: 100%; padding: 10px; margin-bottom: 20px; border: 1px solid #ccc; border-radius: 5px; }"
            , "input[type='submit'] { background-color:  #d81b60; color: white; padding: 10px; border: none; border-radius: 5px; font-size: 16px; cursor: pointer; transition: background-color 0.3s; }"
            , "input[type='submit']:hover { background-color:#c2185b; }"
            , "</style></head><body>"
            , "<div class='container'>"
            , "<h1>Adicionar Nova Receita</h1>"
            , "<form method='POST' action='/nova-receita'>"
            , "<label for='nome'>Nome:</label> <input type='text' name='nome' required><br>"
            , "<label for='categoria'>Categoria (doce/salgada):</label> <input type='text' name='categoria' required><br>"
            , "<label for='ingredientes'>Ingredientes:</label> <textarea name='ingredientes' rows='5' required></textarea><br>"
            , "<label for='instrucoes'>Instruções:</label> <textarea name='instrucoes' rows='5' required></textarea><br>"
            , "<input type='submit' value='Adicionar Receita'>"
            , "</form>"
            , "</div></body></html>"
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
                , "Instrucoes:\n", instrucoes, "\n\n---\n\n"
                ]
        
        -- Adiciona a nova receita ao arquivo
        liftIO $ TIO.appendFile "recipes.txt" novaReceita
        
        html $ T.concat [ "<!DOCTYPE html><html><head><title>Receita Adicionada</title>"
            , "<style>"
            , "body { font-family: Arial, sans-serif; background-color: #f1f2f6; color: #333; display: flex; justify-content: center; align-items: center; height: 100vh; margin: 0; }"
            , ".container { max-width: 600px; padding: 40px; background-color: #ffffff; border-radius: 8px; box-shadow: 0px 4px 15px rgba(0, 0, 0, 0.2); text-align: center; }"
            , "h1 { color: #4a4e69; font-size: 28px; margin-bottom: 20px; }"
            , "</style></head><body>"
            , "<div class='container'>"
            , "<h1>Receita Adicionada</h1>"
            , "<p>A receita <strong>", nome, "</strong> foi adicionada com sucesso!</p>"
            , "</div></body></html>"
            ]
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

-- Função que formata cada receita individualmente
formatarReceita :: T.Text -> T.Text
formatarReceita receita = 
    let linhas = T.lines receita
        categoria = case filter (T.isPrefixOf "Categoria: ") linhas of
            (x:_) -> T.drop 10 x
            _ -> ""
        nome = case dropWhile (not . T.isPrefixOf "Categoria: ") linhas of
            (_:x:_) -> x  -- A linha imediatamente após "Categoria: <categoria>" é o nome da receita
            _ -> "Receita Desconhecida"
        -- Ingredientes como lista com bolinhas
        ingredientes = filter (not . T.null) $ takeWhile (/= "Instrucoes:") $ drop 1 $ dropWhile (/= "Ingredientes:") linhas
        ingredientesList = T.concat ["<ul><li>", T.intercalate "</li><li>" ingredientes, "</li></ul>"]
        
        -- Instruções como lista numerada
        instrucoes = filter (not . T.null) $ drop 1 $ dropWhile (/= "Instrucoes:") linhas
        instrucoesList = T.concat ["<ol><li>", T.intercalate "</li><li>" instrucoes, "</li></ol>"]
        
    in T.concat
        [ "<div class='receita-card'>"
        , "<div class='receita-titulo'>", nome, "</div>"
        , "<div class='receita-categoria'>Categoria: ", categoria, "</div>"
        , "<div class='receita-ingredientes'><div class='section-titulo'>Ingredientes</div>", ingredientesList, "</div>"
        , "<div class='receita-instrucoes'><div class='section-titulo'>Instruções</div>", instrucoesList, "</div>"
        , "</div>"
        ]