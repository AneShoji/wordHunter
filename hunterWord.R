
## ---------------------------------------------------------------------------------------------- ## 
##                                       BIBLIOTECA                                               ##
## ---------------------------------------------------------------------------------------------- ##
library(shiny)
library(tm)
library(bslib)      # ---- biblioteca de thema
library(shinyjs)
library(base)
library(rlang)
library(dplyr)
library(stringr)
library(tm)
library(tau)
library(stopwords)
library(textstem)
library(tibble)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(shinycssloaders)
library(wordcloud)
library(wordcloud2)
library(ggraph)
library(igraph)
library(pdftools)


## ----------------- Opção para aumentar o limite de MB ----------------------------------------- ##
##
##  1 MB = 1024 KB
##  1 GB = 1024 MB
##  KB = GB x 1024² :: 9*1024² = 9MB
##  maxRequestSize -> options(shiny.maxRequestSize = 10*1024^2)
## ---------------------------------------------------------------------------------------------- ##

options(shiny.maxRequestSize = 10 * 1024 ^ 2)

## ----------------------------------- Tema ----------------------------------------------------- ##

thematic::thematic_shiny() ## plot themes

## ---------------------------------------------------------------------------------------------- ##
##                                      FUNÇÕES                                                   ##
## ---------------------------------------------------------------------------------------------- ##


## ---------------------------------------------------------------------------------------------- ##
##  Função que valida se o documento carregado é pdf, utilizando 'req' e 'validate'. 
##----------------------------------------------------------------------------------------------- ##

validacao <- function(file) {
    extensao <- tools::file_ext(file)   # -- variavel que recebe a extensao do arquivo #
    req(file)   # -- require functions Ensure that values are available ("truthy") before proceeding with a calculation or action
    validate(
        need(extensao == "pdf",
             message = "Somente arquivos em .pdf são aceitos. Tente arquivos do tipo PDF"))
    return(file)
}

## ---------------------------------------------------------------------------------------------- ##
##  Função de extração do texto do arquivo pdf, utilizando o pacote TM - Text Mining 
##  utilizando o método 'readPDF' que utilizam como mecanismo de extração protocolos 
## do tipo 'xpdf' e 'ghostscript'. Extrai o texto e o metadados       
## ---------------------------------------------------------------------------------------------- ##

extracao <- function(caminho, idioma) {
    language <- idioma
    mecanismo <- "xpdf"
    leitor <- readPDF(engine = mecanismo)
    texto <- leitor(elem = list(uri = caminho), language = language, id = 'id1')
    return(texto)
}

## ---------------------------------------------------------------------------------------------- ##
##  Função que extrai o conteudo do vetor entre as seções Abstract e References, como forma de 
##  limpar o texto.
## ---------------------------------------------------------------------------------------------- ##

extracao_conteudo <- function(texto_extraido, idioma){
    if(idioma=="en"){
        textoExtarido <-extracao_conteudo_en(texto_extraido)
        return(textoExtarido)
    }
    if(idioma=="pt"){
        textoExtarido <-extracao_conteudo_pt(texto_extraido)
        return(textoExtarido)
    }
}

## --- FUNÇÃO PARA EXTRAIR O CONTEÚDO ENTRE AS PALAVRAS ABSTRACT E REFERENCES --------------------##

extracao_conteudo_en <- function(texto_extraido) {
    conteudo <- texto_extraido$content
    
    ## -- validar se no texto tem a palavra Abstract
    valorA <- is_empty(which(conteudo %in% "Abstract"))
    
    if(valorA){
        ## -- não tem a palavra Abstract
        posicao_1 <- 1
    }else{
        ## --  Buscando a posicao da palavra abstract no vetor
        titulo <- c("Abstract", "ABSTRACT")
        vetorAbstract <- match(conteudo, titulo)
        
        for (i in 1:length(vetorAbstract)) {
            if (!is.na(vetorAbstract[i])) {
                posicao_1 <- i
            }
        }
    }
    posicao_1
    validate(need(!is.null(posicao_1), message = "Não foi possível gerar o gráfico, o título da seção selecionado não foi encontrado." ))
    validate(need(!is_empty(posicao_1), message = "Não foi possível gerar o gráfico, o título da seção selecionado não foi encontrado." ))
    
    ## -- validar se no texto tem a palavra References
    valorR <- is_empty(which(conteudo %in% "References"))
    
    if(valorR){
        ## -- não tem a palavra References 
        posicao_2 <- length(conteudo)
    }else{
        ## --  Buscando a posicao da palavra references no vetor
        titulo2 <- c("References", "REFERENCES","Bibliography")
        vetorReferences <- match(conteudo, titulo2)
        
        for (i in 1:length(vetorReferences)) {
            if (!is.na(vetorReferences[i])) {
                posicao_2 <- i
            }
        }
    }
    posicao_2
    validate(need(!is.null(posicao_2), message = "Não foi possível gerar o gráfico, o título da seção selecionado não foi encontrado." ))
    validate(need(!is_empty(posicao_2), message = "Não foi possível gerar o gráfico, o título da seção selecionado não foi encontrado." ))
    
    ## -- separando o conteúdo entre a palavra Abstract e References
    conteudo <- conteudo[c(posicao_1:posicao_2)]
    return(conteudo)
}

## --- FUNÇÃO PARA EXTRAIR O CONTEÚDO ENTRE AS PALAVRAS RESUMO E REFERÊNCIAS ---------------------##

extracao_conteudo_pt <- function(texto_extraido) {
    conteudo <- texto_extraido$content
    
    ## -- validar se no texto tem a palavra Abstract
    valorA <- is_empty(which(conteudo %in% "Resumo"))
    if (valorA) {
        ## -- não tem a palavra Abstract
        posicao_1 <- 1
    } else{
        ## --  Buscando a posicao da palavra abstract no vetor
        titulo <- c("Resumo")
        vetorAbstract <- match(conteudo, titulo)
        
        for (i in 1:length(vetorAbstract)) {
            if (!is.na(vetorAbstract[i])) {
                posicao_1 <- i
            }
        }
    }
    posicao_1
    validate(need(!is.null(posicao_1), message = "Não foi possível gerar o gráfico, o título da seção selecionado não foi encontrado."))
    validate(need(!is_empty(posicao_1), message = "Não foi possível gerar o gráfico, o título da seção selecionado não foi encontrado."))
    
    ## -- validar se no texto tem a palavra References
    valorR <- is_empty(which(conteudo %in% "Referências"))
    
    if (valorR) {
        ## -- não tem a palavra References
        posicao_2 <- length(conteudo)
    } else{
        ## --  Buscando a posicao da palavra references no vetor
        titulo2 <- c("Referência", "Bibliografia")
        vetorReferences <- match(conteudo, titulo2)
        
        for (i in 1:length(vetorReferences)) {
            if (!is.na(vetorReferences[i])) {
                posicao_2 <- i
            }
        }
    }
    posicao_2
    validate(need(!is.null(posicao_2), message = "Não foi possível gerar o gráfico, o título da seção selecionado não foi encontrado."))
    validate(need(!is_empty(posicao_2), message = "Não foi possível gerar o gráfico, o título da seção selecionado não foi encontrado."))
    
    ## -- separando o conteúdo entre a palavra Abstract e References
    conteudo <- conteudo[c(posicao_1:posicao_2)]
    return(conteudo)
}

## ---------------------------------------------------------------------------------------------- ##
##  Função que realiza o processamento do texto, limpa o texto: remove stopwords, números, pontuaçào
##  espaços de tabulação, novalinha, ..., stopwords, espaços extras em branco.
## ---------------------------------------------------------------------------------------------- ##


## ----------------- VALIDA QUAL O IDIOMA PARA CHAMAR A FUNÇÃO DE PREPROCESSAMENTO ---------------##
preprocessamento <- function(conteudo, idioma){
    if(idioma=="en"){
       return(preprocessamento_en(conteudo)) 
    }
    if(idioma=="pt"){
       return(preprocessamento_pt(conteudo)) 
    }
}

## --------------- PROCESSAMENTO PARA IDIOMA INGLES - EN -----------------------------------------##
preprocessamento_en <- function(conteudo) {
    
    # --- Lista Palavras extra para remoção --- #
    palavras <- c("et al.","et", "al.", "www","jss","tel.","fax","doi","eq.","Fig.","fig.", "jss")
    
    texto <-
        conteudo %>%
        removeWords(palavras) %>%
        removeWords(stopwords("en")) %>%
        removeWords(stopwords(source = "smart")) %>%
        str_replace_all("[0-9]", " ") %>%
        str_replace_all("[;,():+@*./=~\\[\\]\\{\\}%\\|]", " ") %>%
        str_replace_all("[\t\n\r\f\v]", " ") %>%
        stripWhitespace() %>%
        removeWords(stopwords("en")) %>%
        removeWords(stopwords(source = "smart")) %>%
        removeWords(palavras) %>%
        stripWhitespace()
    return(texto)
}

## --------------- PROCESSAMENTO PARA IDIOMA PORTUGUÊS - PT---------------------------------------##
preprocessamento_pt <- function(conteudo) {
    
    vogais <-c("a","as","e","o","os","A","E","O","As","Os","ao","aos","Ao","Aos","-a","-as","-o","-os","ou"
               ,"até","Até","antes","Antes"
               ,"com","Com","como","Como"
               ,"da","das","de","do","dos","Da","Das","Do","Dos","De","demais","Demais"
               ,"deste","destes","Deste","Destes","desta","destas","Desta","Destas"
               ,"desse","Desse","Desses","desses","disso","Disso","dissos","Dissos"
               ,"depois","Depois"
               ,"em","Em","entre", "Entre","esta","estas","este","estes","isto","istos","Esta","Estas","Este","Estes","Isto","Istos"
               ,"esse","esses","Esse","Esses"
               ,"na","nas","no","nos","Na","Nas","No","Nos","neste","nestes","Neste","Nestes"
               ,"uma","umas","um","uns","Uma","Umas","Um","Uns"
               ,"que","Que","quando","Quando","quanto", "Quanto","quantos","Quantos","qual","Qual","quais","Quais"
               ,"outro","outros","outra","outras","Outro","Outros","Outra","Outras"
               ,"mas","Mas","mais","Mais","muita","muitas","Muita","Muitas","muito","muitos","Muito","Muitos","menos","Menos"
               ,"para","por","Para","Por"
               ,"pelo","pelos","Pelo","Pelos"
               ,"-se","se","Se","sem","Sem","seu","seus","Seu","Seus","sua","suas","Sua","Suas"
               ,"et al.","et al"
               ,"todo","todos","Todo","Todos","tudo","Tudo","toda","todas","Toda","Todas")
    
    verbos <- c("pode","Pode","podem","Podem","ser","Ser","tem","Tem","fazer","Fazer"
              ,"deve","foi","Foi","foram","Foram","feito","feitos","feita","feitas","Feito","Feitos","Feita","Feitas"
              ,"teve","Teve"
              ,"são","São")
    
    preposicao <- c("bem","Bem","bom","Bom","bons","Bons"
                    ,"cada","Cada","conforme","Comforme","caso","Caso","casos","Casos"
                    ,"entanto","Entanto", "embora", "Embora"
                    ,"forma","formas","Forma", "Formas"
                    ,"mesmo","Mesmo"
                    ,"seguida","seguidas","Seguida","Seguidas","sobre","Sobre"
                    ,"pouco","Pouco","pouca","Pouca","poucos","Poucos","poucas","Poucas"
                    ,"primeiramente","Primeiramente","portanto","Portanto","pois","Pois"
    )
    
    # --- Lista Palavras extra para remoção --- #
    palavras <- c("et al.","et", "al.", "www","jss","tel.","fax","doi","eq.","Fig.","fig.", "jss")
    
    texto <-
        conteudo %>%
        tau::remove_stopwords(palavras) %>%
        tau::remove_stopwords(vogais)%>%
        tau::remove_stopwords(verbos) %>%
        tau::remove_stopwords(preposicao)%>%
        str_replace_all("[0-9]", " ") %>%
        str_replace_all("[;,():+@*./=~\\[\\]\\{\\}%\\|]", " ") %>%
        str_replace_all("[\t\n\r\f\v]", " ") %>%
        stripWhitespace() %>%
        str_to_lower(locale = "pt")%>%
        removeWords(stopwords(language = "pt", source = "snowball")) %>%
        stripWhitespace()
    return(texto)
}

## ---------------------------------------------------------------------------------------------- ##
##  Função que realiza a lematização das palavras com o dicionário lexico.
## ---------------------------------------------------------------------------------------------- ##

lemmatizacaoLex <- function(texto) {
    lemma_dictionary_lex <- make_lemma_dictionary(texto, engine = 'lexicon')
    lex_lem <- lemmatize_strings(texto, dictionary = lemma_dictionary_lex)
    return(lex_lem)
}

## ---------------------------------------------------------------------------------------------- ##
##  Função que converte o processado - limpo -  e converte em dataframe.
## ---------------------------------------------------------------------------------------------- ##

transformando_dataframe <- function(textoPreProcessado) {
    texto_em_dataframe <- enframe(textoPreProcessado, name = NULL, value = "text")
    return(texto_em_dataframe)
}

## ---------------------------------------------------------------------------------------------- ##
##  Função que realiza o preprocessamento - limpeza e tokenização - do dataframe
## ---------------------------------------------------------------------------------------------- ##

preprocessamento_dataframe <- function(dataframeTransformado) {
    texto_tokenizado <- dataframeTransformado %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words)
    return(texto_tokenizado)
}


## ---------------------------------------------------------------------------------------------- ##
##  Função que a contagem (contador) e filtragem das palavras com interação do usuário
## ---------------------------------------------------------------------------------------------- ##

filtragem_palavras <- function(tokens, frequencia) {
    palavras <- dplyr::count(tokens, word, sort = TRUE, name = "frequency")
    palavra_filtrada <- dplyr::filter(palavras, (frequency >= frequencia))
    return(palavra_filtrada)
}

## ---------------------------------------------------------------------------------------------- ##
##  Função que realiza a lematização das palavras com o dicionário hunspell
## ---------------------------------------------------------------------------------------------- ##

lemmatizacaoHun <- function(texto) {
    lemma_dictionary_hs <- make_lemma_dictionary(texto, engine = 'hunspell')
    hun_len <- lemmatize_strings(texto, dictionary = lemma_dictionary_hs)
    return(hun_len)
}

## ---------------------------------------------------------------------------------------------- ##
##  função para buscar no arquivo os itens de seção
## ---------------------------------------------------------------------------------------------- ##

lista <- function(caminho) {
    toc <- pdf_toc(caminho)
    
    ## -- criando o vetor -- ##
    toc_lista <- c()
    condicao <- length(toc) > 0
    if (condicao) {
        ##-- atribuindo os valores para a lista
        toc_lista <- toc[["children"]][[1]][["children"]]
        
        condicao2 <- length(toc_lista) > 0
        if(condicao2){
            ## -- criando o vetor dentro do escopo do if -- ##
            listaTitulos <- c()
            
            tamanhoToc <- length(toc_lista)
            for (i in 1:tamanhoToc) {
                listaTitulos <- c(listaTitulos,
                                  toc[["children"]][[1]][["children"]][[i]][["title"]])
            }
            listaTitulos <- stringi::stri_remove_empty(listaTitulos)
            return(listaTitulos)
        }
    }else{
        validate(need(!isTRUE(condicao), message = "Não será possível gerar o gráfico, os títulos da seções selecionado não foram encontrados." ))
        return("Lista Vazia")
    }
}

## ---------------------------------------------------------------------------------------------- ##
##  função para validar condição da lista de seção 
## ---------------------------------------------------------------------------------------------- ##
validarLista <- function(caminho){
    toc <- pdf_toc(caminho)
    condicao <- length(toc) > 0
    return(condicao)
}

## ---------------------------------------------------------------------------------------------- ##
##  função para extrair o conteudo de palavras entre selecao do usuário 
## ---------------------------------------------------------------------------------------------- ##

extracao_conteudo_titulo <- function(texto_extraido, listaTitulos, titulo1) {
    
    validate(need(!is_empty(titulo1)
                  ,message = "Não foi possível gerar o gráfico, o título da seção selecionado não foi encontrado." ))
    
    conteudo <- texto_extraido$content
    
    p1 <- which(listaTitulos %in% titulo1)
    p2 <- p1 + 1
    titulo2 <- listaTitulos[p2]
    
    ## --  Buscando a posicao da palavra da variavel titulo1 no vetor conteudo
    vetorTitulo1 <- str_match(conteudo, titulo1)
    
    pos1 <- c()
    
    for(i in 1:length(vetorTitulo1)){
        if(is.na(vetorTitulo1[i])){
            p_na <- c(i)
        }else{
            pos1 <- c(pos1, i)
        }
    }
    
    posicao1 <- pos1[1] + 1
    # -- validando o valor da posicao1
    validate(need(!is.null(posicao1), message = "Não foi possível gerar o gráfico, o título da seção selecionado não foi encontrado." ))
    validate(need(!is_empty(posicao1), message = "Não foi possível gerar o gráfico, o título da seção selecionado não foi encontrado." ))
    
    ## --  Buscando a posicao da palavra da variavel titulo2 no vetor conteudo
    vetorTitulo2 <- str_match(conteudo, titulo2)
    
    pos2 <- c()
    
    for(j in 1:length(vetorTitulo2)) {
        if(is.na(vetorTitulo2[j])) {
            p_na <- c(j)
        }else{
            pos2 <- c(pos2, j)
        }
    }
    posicao2 <- pos2[1]
    # -- validando o valor de posica2
    validate(need(!is.null(posicao2), message = "Não foi possível gerar o gráfico, o título da seção selecionado não foi encontrado."))
    validate(need(!is_empty(posicao2), message = "Não foi possível gerar o gráfico, o título da seção selecionado não foi encontrado."))
    
    ## -- separando o conteúdo entre os titulos
    conteudo <- conteudo[c(posicao1:posicao2)]
    return(conteudo)
}

## ---------------------------------------------------------------------------------------------- ##
##                                           DEFINE UI                                            ##
## ---------------------------------------------------------------------------------------------- ##

ui <- fluidPage(
    
    # -- define o tema do app -- #
    theme = bs_theme(version = 5, bootswatch = "spacelab")
    
    # -- define o uso de javascript -- #
    ,useShinyjs()
    
    ,titlePanel(title = "The Word Hunter - O Caçador de Palavras",
                windowTitle = "The Word Hunter")
    ,hr()
    
    ,fluidRow(
        column(
            width = 12

            ,fluidRow(
                column(
                    width = 3
                    ,div(class = "container"
                        ,id = "language"
                        ,radioButtons(
                            inputId = "idioma"
                            ,label = h5("Para começar, selecione o idioma do artigo: ")
                            ,choices = c("Inglês" = "en", "Português" = "pt")
                            ,inline = TRUE
                            ,selected = 0 ))) # -- fecha coluna
                ,br()
                ,column(
                    width = 6
                        ,conditionalPanel(
                            "input.idioma == 'en' ||input.idioma == 'pt'"
                            ,div(class = "container"
                                ,id = "pdf"
                                ,fileInput(
                                    inputId = "arquivoPdf"
                                    ,label = h5("Agora, selecione um arquivo digital '.pdf' :")
                                    ,multiple = FALSE
                                    ,accept = "application/pdf"
                                    ,width = "70%"
                                    ,buttonLabel = "Carregar documento"
                                    ,placeholder = "Nenhum arquivo selecionado - Max:10MB")
                                )
                            ,div(class = "container", actionButton("limpar", "Limpar dados para carregar outro arquivo.", width = "70%"))
                            )
                    ) # -- fecha coluna
            ) # --- fecha fluidrow da parte de seleçao de idioma e arquivo pdf
            
            ,br(),hr()
            ,fluidRow(
                column(
                    width = 12
                    
                    ,tabsetPanel(
                        id = "plotsTabs",
                        type = "pills"
                        
                        ,br()
                        ## ---  Aba de informações sobre o artigo  --- ##
                        ,tabPanel("Informaçõebre o artigo"
                            ,br()
                            ,fluidRow(
                                  column(
                                      width = 12
                                      ,div(class = "well"
                                          ,h6(p("As informações são constantes dos metadados do arquivo. "))
                                          ,p("Autor: ", textOutput("autor", inline = TRUE))
                                          ,p("Título: ", textOutput("titulo", inline = TRUE))
                                          ,p("Número de páginas: ", textOutput("numpages", inline = TRUE))
                                          ,p("idioma selecionado: ", textOutput("lingua", inline = TRUE))
                                          ,p("Palavras-Chaves: ", textOutput("keywords", inline = TRUE))
                                          ,p("Criador: ", textOutput("creator", inline = TRUE))
                                          ,p("Assunto: ", textOutput("subject", inline = TRUE))
                                          ,p("DOI: ", textOutput("doi", inline = TRUE))
                                          ,p("Outros: ", textOutput("cmd1", inline = TRUE))
                                          ,p("Outros: ", textOutput("cmd2", inline = TRUE))
                                          ,hr()
                                          ,p("Lista de seções: ",textOutput("listasecao", inline = TRUE))
                            ))))  # --- fecha tabpanel sobre info do artigo
                                              
                        ## ---  Gráfico de Palavras  - Usando o dicionário Lexico  --- #
                        ,tabPanel("Gráficos de Palavras - Lexicon"
                            ,br()
                            ,fluidRow(
                                    column(
                                        width = 6
                                        ,br()
                                        ,h5("Gráfico gerado utilizando dicionário Lexicon no processo de lematização!")) # -- fecha coluna
                                    ,br()
                                    
                                    ,column(
                                         width = 6
                                         ,br()
                                         ,sliderInput(
                                            inputId = "qtdNLex"
                                            ,label = "Deslize para alterar a quantidade de palavras exibidas. Frequência mínima:1 e máxima:100."
                                             ,min = 1 ,max = 100 ,value = 10 ,width = "70%")) # --- fecha fluidrow
                                    ,br()
                                    ,fluidRow(
                                      column(
                                          width = 12
                                          ,br()
                                          ,hr()
                                          ,withSpinner(
                                              type = 8
                                              ,plotOutput("graficoLex", width = "80%", height = "600px"))
                                          ,br(),hr()
                                          )))) # -- tabpanel grafico Lexico
                                  
                         # ---  Gráfico de Palavras - usando o dicionário Hunspell  --- #
                        ,tabPanel("Gráficos de Palavras - Hunspell"
                                  ,fluidRow(
                                      column(
                                          width = 6
                                          ,br()
                                          ,h5("Gráfico foi gerado utilizando dicionário dicionário Hunspell o processo de lematização!")
                                          ,br()) # -- fecha coluna
                                          
                                      ,column(
                                          width = 6
                                          ,br()
                                          ,sliderInput(
                                              inputId = "qtdNHun"
                                              ,label = "Deslize para alterar a quantidade de palavras exibidas. Frequência mínima:1 e máxima:100."
                                              ,min = 1 ,max = 100 ,value = 10 ,width = "70%" )
                                    )) # -- fecha fluidrow
                                  ,fluidRow(
                                      column(
                                          12
                                          ,hr()
                                          ,withSpinner(
                                              type = 8
                                              ,plotOutput("graficoHun", width = "80%", height = "600px"))
                                          ,br()
                                          ,hr()
                            ))) # -- fecha tabpanel do grafico hun
                        
                        ## ---  Gráfico de Palavras   --- #
                        ,tabPanel("'Word Cloud' - Nuvens de Palavras"
                            
                            ,fluidRow(
                                column(
                                    width = 12
                                    ,br()
                                    ,div(h4("Nuvem de palavras!"))
                                    ,h6(class = "well" ,"Passe a seta do mouse sobre a palavra para ver a frequência da palavra, o número de vezes que foi contabilizada.")

                                    ,withSpinner(
                                        type = 8
                                        ,wordcloud2Output("nuvem", width = "100%", height = "600px"))
                                    ,br()
                                    ,hr()
                                ))) #  -- tabpanel - grafico de palavras
                        
                        ## ---  Associacao de Palavras   --- #
                        ,tabPanel("Associação de palavras"
                            ,br()
                            ,div(h4("Associação de palavras!"))
                            ,fluidRow(
                                column(
                                    width = 12
                                    ,h6(class = "well" ,"Zoom: clique e arraste para selecionar a área a ser aumentada, e dois clique na área selecionadas. Zoom normal: dois clique fora da aŕea selecionada")
                                    ,wellPanel(id = "tPanel",style ="overflow-y: scroll; overflow-x: scroll;"
                                        ,withSpinner(
                                            type = 8
                                            
                                            ,plotOutput("ngramsRelacaoPalavras", width = "100%", height = "800px"
                                                        ,dblclick = "ngrams_dblClick", brush = brushOpts( id = "ngrams_brush" ,resetOnNew = TRUE)
                                      )))
                                    
                                    ,br()
                                    ,hr()
                                ))) # --- tabpanel - associacao de palavras
                        
                        ## --- Grafico por secao   --- #
                        ,tabPanel(
                            "Informação por seção"
                            ,fluidRow(
                                column(
                                    width = 4
                                    ,br()
                                    ,uiOutput("selectBox")
                                    ,br()
                                    ,conditionalPanel(
                                        "input.selectBox !== 'Lista Vazia'"
                                        ,uiOutput("gerargrafico"))
                                    ,conditionalPanel(
                                        "input.selectBox == 'Lista Vazia'"
                                        ,h5("Aviso:")
                                        ,p("Não foi encontrado os metadados com os títulos.")
                                        ,p("Por esse motivo, não será possível gerar o gráfico por seções"))
                                    )# -- fecha coluna --#
                                ,column(
                                    width = 4
                                    ,h5("Aviso:")
                                    ,p("Títulos de seções com mais de duas palavras podem não ser encontradas pelo algoritmo.")
                                    ,p("Esse gráfico utiliza o dicionário Lexicon para lematizar as palavras!"))
                                ,column(
                                    width = 4
                                    ,br()
                                    ,sliderInput(
                                        inputId = "qtdNLexSec"
                                        ,label = "Deslize para alterar a quantidade de palavras exibidas. Frequência mínima:10 e máxima:100."
                                        ,min = 1, max = 100, value = 1, width = "70%")))
                            ,fluidRow(
                                column(
                                    width = 12
                                    ,br()
                                    ,withSpinner(
                                        type = 8
                                        ,plotOutput("graficoTitulo", width = "100%", height = "700px"))
                                    ,hr()
                                    ))) # --- tabpanel - informacao por secao/titulo
                    ))))))

## ---------------------------------------------------------------------------------------------- ##
##                                       DEFINE SERVER LOGIC                                      ##
## ---------------------------------------------------------------------------------------------- ##

server <- function(input, output, session) {
    
# ------------------------------------------------------------------------------------------------ #    
#   a função retorna o caminho temporário do arquivo pdf
# ------------------------------------------------------------------------------------------------ #
    
    caminhoArquivo <- reactive({
        filePath <- validacao(input$arquivoPdf$datapath)
        return(filePath)
    })

# ------------------------------------------------------------------------------------------------ #
#   a função retorna o conteudo extraido - texto e metadados - do arquivo pdf
# ------------------------------------------------------------------------------------------------ #   
    
    textoExtraido <- reactive({
        texto_extraido <- extracao(caminhoArquivo(),input$idioma)
        return(texto_extraido)
    })    
    
# -----------------------------------------------------------------------------------------------  #
#   a função retorna o conteúdo extraído do texto, das seções Abstract e References
# ------------------------------------------------------------------------------------------------ #           
    
    conteudoExtraido <- reactive({
        conteudo_extraido <- extracao_conteudo(textoExtraido(),input$idioma)
        return(conteudo_extraido)
    })        

# -----------------------------------------------------------------------------------------------  #
#   a função retorna o texto processado, limpo dos elementos que não tem relevancia
# ------------------------------------------------------------------------------------------------ #  
    
    textoPreprocessado <- reactive({
        texto_preprocessado <- preprocessamento(conteudoExtraido(), input$idioma)
        return(texto_preprocessado)
    })   
        
# -----------------------------------------------------------------------------------------------  #
#   processamento do texto limpo, utilizando o dicionário lexicon
# ------------------------------------------------------------------------------------------------ #  
    
    # -- retorna o texto processado lemmatizado com Lexicon
    textoLemmatizadoLex <- reactive({
        texto_lemmatizado <- lemmatizacaoLex(textoPreprocessado())
        return(texto_lemmatizado)
    })
   
    ##  -- retorna o texto lemmatizado em data frame
    textoDataframeLem <- reactive({
        texto_to_dataframe_lem <- transformando_dataframe(textoLemmatizadoLex())
        return(texto_to_dataframe_lem)
    })
        
    ##  -- retorna o texto tokenizado com lem
    textoTokenizadoLem <- reactive({
        tokenizacao_texto_lem <- preprocessamento_dataframe(textoDataframeLem())
        return(tokenizacao_texto_lem)
    })
        
# -----------------------------------------------------------------------------------------------  #
#   processamento do texto limpo, utilizando o dicionário hunspell
# ------------------------------------------------------------------------------------------------ #      

    ## -- retorna o texto processado com hunspell
    textoLemmatizadoHun <- reactive({
        texto_hunspel <- lemmatizacaoHun(textoPreprocessado())
        return(texto_hunspel)
    })
    
    
# ------------------------------------------------------------------------------------------------ #
#                                          OUTPUT                                                  #
# ------------------------------------------------------------------------------------------------ #    
    
    
# ------------------------------------------------------------------------------------------------ #    
#   output do gráfico utilizando o dicionário lexicon
# ------------------------------------------------------------------------------------------------ #    

    output$graficoLex <- renderPlot({
        palavras_filtradas <- filtragem_palavras(textoTokenizadoLem(), input$qtdNLex)

        ggplot(palavras_filtradas) +
            geom_col(aes(x = frequency, y = word, fill = frequency, group = frequency)) +
            geom_label(aes(x = frequency, y = word, label = frequency), size = 3) +
            labs(x = "Frequência", y = "Palavras") +
            geom_label(aes(x = frequency, y = word, label = word), size = 3,
                       position = position_stack(vjust = 0.5))

    }, res = 141)
        
# ------------------------------------------------------------------------------------------------ #    
#   output do gráfico utilizando o dicionário hunspell
# ------------------------------------------------------------------------------------------------ #  
    
    output$graficoHun <- renderPlot({
        texto_to_dataframeh <- transformando_dataframe(textoLemmatizadoHun())
        tokenizacao_textoh <- preprocessamento_dataframe(texto_to_dataframeh)
        palavras_filtradas <- filtragem_palavras(tokenizacao_textoh, input$qtdNHun)
        ggplot(palavras_filtradas)+
            geom_col(aes(x=frequency, y=word, fill=frequency, group=frequency))+
            geom_label(aes(x=frequency, y=word, label=frequency),size=3)+
            labs(x="Frequência", y="Palavras")+
            geom_label(aes(x=frequency, y=word, label=word),
                       size=3,position = position_stack(vjust = 0.5))
    }, res = 141)
    
# ------------------------------------------------------------------------------------------------ #    
#   nuvem de plavras
# ------------------------------------------------------------------------------------------------ #          
    
    output$nuvem <- renderWordcloud2({
        palavras_filtradas <- filtragem_palavras(textoTokenizadoLem(), 10)
        nuvem <- wordcloud2(palavras_filtradas, size = 1, color = "random-light",
                            backgroundColor = "white")
    })
        
# ------------------------------------------------------------------------------------------------ #    
#   Associacoa de palavras 
# ------------------------------------------------------------------------------------------------ #         
    
    ranges <- reactiveValues(x = NULL, y = NULL)
        
    output$ngramsRelacaoPalavras <- renderPlot({
        bigram_counts <- textoDataframeLem() %>%
            unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
            separate(bigram, c("word1", "word2"), sep = " ") %>%
            filter(!word1 %in% stop_words$word,!word2 %in% stop_words$word) %>%
            dplyr::count(word1, word2, sort = TRUE)
        
        bigram_graph <- bigram_counts %>%
            filter(n > 5) %>%
            graph_from_data_frame()
        
        set.seed(2020)
        ggraph(bigram_graph, layout = "graphopt") +
            geom_edge_link(arrow = arrow(length = unit(5, 'mm')), end_cap = circle(3, 'mm')) +
            geom_node_point() +
            geom_node_text(aes(label = name), size = 8, vjust = 0.5, hjust = 0.5) +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
            theme_graph(background = "grey98")
    })
        
# ------------------------------------------------------------------------------------------------ #    
#    ZOOM -- When a double-click happens, check if there's a brush on the plot.
#    If so, zoom to the brush bounds; if not, reset the zoom.
# ------------------------------------------------------------------------------------------------ #         
    
    observeEvent(input$ngrams_dblClick, {
        brush <- input$ngrams_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    
# ------------------------------------------------------------------------------------------------ #    
#   Informação por seção  
# ------------------------------------------------------------------------------------------------ #      
    
## -------------------------------- lista de titulos do arquivo ---------------------------------- #
    listaOpcao <- reactive({
        filePath <- validacao(input$arquivoPdf$datapath)
        lista <- lista(filePath)
        return(lista)
    })
    
    output$selectBox <- renderUI({
            selectInput(
            "selectBox",label = "Selecione um subtítulo do artigo",
            choices = listaOpcao(),selected = "...",width = "400px")
    })
    
    output$gerargrafico <- renderUI({
        actionButton("button", "Gerar Gráfico",width = "400px", height = "10px")
    })
    
    output$noButton <- renderText({"Não foi possivel exibir os titulos do artigo"})

# ---------------------------- grafico por secao ------------------------------------------------- #
    observeEvent(input$button, {
        output$graficoTitulo <- renderPlot({
            conteudoTitulo <- extracao_conteudo_titulo(textoExtraido(), listaOpcao(), input$selectBox)
            textoPreprocessado <- preprocessamento(conteudoTitulo, input$idioma)
            textoLemmatizado <- lemmatizacaoLex(textoPreprocessado)
            textoDataframelem <- transformando_dataframe(textoLemmatizado)
            textoTokenizadoLem <- preprocessamento_dataframe(textoDataframelem)
            palavras_filtradas <- filtragem_palavras(textoTokenizadoLem, input$qtdNLexSec)
            ggplot(palavras_filtradas) +
                geom_col(aes(x = frequency, y = word, fill = frequency, group = frequency)) +
                geom_label(aes(x = frequency, y = word, label = frequency), size = 3) +
                labs(x = "Frequência", y = "Palavras") +
                geom_label(aes(x = frequency, y = word, label = word),size = 3,
                           position = position_stack(vjust = 0.5))
        }, res = 141)
    })
    
# -----------------------------------------------------------------------------------------------  #
#   a função retorna o autor do arquivo 
# ------------------------------------------------------------------------------------------------ #             
    
    output$autor <- renderText({
        infoP <- pdftools::pdf_info(caminhoArquivo())
        infoR <- Rpoppler::PDF_info(caminhoArquivo())
        
        condicao <- length(infoP$keys$Author) > 0 
        condicao1 <- length(infoR$Author) > 0
        if(condicao){
            return(infoP$keys$Author)
        }
        if(condicao1){
            return(infoR$Author)
        }
        else{
            return("Informação inexistente.")
        }
    })     
    
# -----------------------------------------------------------------------------------------------  #
#   a função retorna o título do arquivo 
# ------------------------------------------------------------------------------------------------ #  
    
    output$titulo <- renderText({
        infoP <- pdftools::pdf_info(caminhoArquivo())
        infoR <- Rpoppler::PDF_info(caminhoArquivo())
        
        condicao <- length(infoP$keys$Title) > 0 
        condicao1 <- length(infoR$Title) > 0
        if(condicao){
            return(infoP$keys$Title)
        }
        if(condicao1){
            return(infoR$Title)
        }
        else{
            return("Informação inexistente.")
        }
    })

# -----------------------------------------------------------------------------------------------  #
#   a função retorna o núemro de páginas do arquivo 
# ------------------------------------------------------------------------------------------------ #       
    
    output$numpages <- renderText({
        info <- Rpoppler::PDF_info(caminhoArquivo())
        return(info$Pages)
    })
    
# -----------------------------------------------------------------------------------------------  #
#   a função retorna o o idioma selecionado 
# ------------------------------------------------------------------------------------------------ #                  
    
    output$lingua <- renderText({
        return(input$idioma)
    })    
    
# -----------------------------------------------------------------------------------------------  #
#   a função retorna o keywords do arquivo 
# ------------------------------------------------------------------------------------------------ #  
    
    output$keywords <- renderText({
        infoP <- pdftools::pdf_info(caminhoArquivo())
        infoR <- Rpoppler::PDF_info(caminhoArquivo())
        
        condicao <- length(infoP$keys$Keywords) > 0 
        condicao1 <- length(infoR$Keywords) > 0
        if(condicao){
            return(infoP$keys$Keywords)
        }
        if(condicao1){
            return(infoR$Keywords)
        }
        else{
            return("Informação inexistente.")
        }
    })

# -----------------------------------------------------------------------------------------------  #
#   a função retorna o creator do arquivo 
# ------------------------------------------------------------------------------------------------ # 
        
    output$creator <- renderText({
        infoP <- pdftools::pdf_info(caminhoArquivo())
        infoR <- Rpoppler::PDF_info(caminhoArquivo())
        
        condicao <- length(infoP$keys$Creator) > 0 
        condicao1 <- length(infoR$Creator) > 0
        if(condicao){
            return(infoP$keys$Creator)
        }
        if(condicao1){
            return(infoR$Creator)
        }
        else{
            return("Informação inexistente.")
        }
    })   
    
# -----------------------------------------------------------------------------------------------  #
#   a função retorna o assunto  e outros topicos de páginas do arquivo 
# ------------------------------------------------------------------------------------------------ #       
    
    output$subject <- renderText({
        info <- Rpoppler::PDF_info(caminhoArquivo())
        return(info$Subject)
    })
    
    output$cmd1 <- renderText({
        infoP <- pdftools::pdf_info(caminhoArquivo())
        return(infoP$keys$`CrossMarkDomains[1]`)
    })
    
    output$cmd2 <- renderText({
        infoP <- pdftools::pdf_info(caminhoArquivo())
        return(infoP$keys$`CrossMarkDomains[2]`)
    })
    
    output$doi <- renderText({
        infoP <- pdftools::pdf_info(caminhoArquivo())
        return(infoP$keys$doi)
    })
    
# -----------------------------------------------------------------------------------------------  #
#   a função retorna texto sobre a validação de conteudo na lista de seções
# ------------------------------------------------------------------------------------------------ #            
    
    output$listasecao <- renderText({
        c <- validarLista(caminhoArquivo())
        if(c == TRUE){
            return("Lista de secção encontrada")
        }
        else{
            return("Lista de secção vazia")
        }
        
    })
    
# -----------------------------------------------------------------------------------------------  #
#   a função para atualizar a pagina
# ------------------------------------------------------------------------------------------------ #        
        
    observeEvent(input$limpar, {
        refresh()
    })
      
}

## ---------------------------------------------------------------------------------------------- ##
##                                   Run the application                                          ##
## ---------------------------------------------------------------------------------------------- ##

shinyApp(ui = ui, server = server)
