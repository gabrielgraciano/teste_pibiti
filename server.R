colorful <- c('#0c7bdc', '#FFc20a', "#20BFB2", "#63BE76", "#E66100",
              "#0c7bdc", "#d41159", "#4b0092", "#64B5DA", "#c9e5f2")

blue <- c("#A1D2CE", "#8AD1CB", "#78CAD2", "#62A8AC", 
          "#4CA6AA", "#5497A7", "#50858B", "#254A4F")
medium_cyan <- '#3db9bf'
dark_cyan <- '#077e84'
medium_ocre <- "#bf9f56"
dark_orange <- "#8c6a1c"
brown <- "#4c380c"
red_nail <- '#ce4b37'
red_broken_nail <- '#300802'

floorN <- function(x, N){ 
  N*floor(x/N) 
}

ceilN <- function(x, N) {
  N*ceiling(x/N)
}

values <- reactiveValues()
values$showMedia <- FALSE
normal_params <- reactiveValues()
normal_params$u <- 0
normal_params$dp <- 1
normal_params$min <- 1
normal_params$max <- 4
normal_params$sit <- 1

testeT1_params <- reactiveValues()
testeT1_params$u <- 0
testeT1_params$dp <- 1
testeT1_params$X <- 0.18
testeT1_params$n <- 10
testeT1_params$alpha <- 0.05

testeT2_params <- reactiveValues()
testeT2_params$n <- 12
testeT2_params$u1 <- 10
testeT2_params$u2 <- 9
testeT2_params$dp1 <- 2.2
testeT2_params$dp2 <- 2.2
p1 <- rnorm(12, 10, 2.2)
p2 <- rnorm(12, 9, 2.2)
testeT2_params$p1 <- p1
testeT2_params$p2 <- p2
testeT2_params$D <- p1 - p2

testeQui_params <- reactiveValues()
testeQui_params$matrix <- rbind(c(48, 8, 56), c(30, 21, 51), c(78, 29, 107))
testeQui_params$expected <- rbind(c(41, 15, 56), c(37, 14, 51), c(78, 29, 107))
testeQui_params$colnames <- c("Melhora", "Não", "Total")
testeQui_params$rownames <- c("Droga", "Placebo", "Total")
testeQui_params$chi <- 9.3

elementos <- sort(round(runif(20, min = 1, max = 10)))

getmode <- function(v) {
  uv <- unique(v)
  tab <- tabulate(match(v, uv))
  uv[tab == max(tab)]
}

vectorToString <- function(v) {
  string <- v[1]
  if (length(v) > 1) {
    string <- "{"
    for (i in (1:length(v))) {
      if (i != length(v)) {
        string <- paste0(string, v[i], ", ")
      }
      else {
        string <- paste0(string, v[i], "}")
      }
    }
  }
  return (string)
}

dataqui <- reactiveValues(data=dados_saude_alimentacao,
                          custom=FALSE, 
                          factors=var_factors, 
                          numbers=var_numerics,
                          shortLevels=shortLevels,
                          uploaded = FALSE)


# server function ----

function(input, output, session) {
  
  ## Projeto ----
  
  callModule(inicio_projeto_server, "inicio_projeto", session = session)
  
  ## Contato ----
  
  callModule(inicio_contato_server, "inicio_contato", session = session)
  
  ## Equipe ----
  
  callModule(inicio_equipe_server, "inicio_equipe", session = session)
  
  ## Conjunto de dados ----
  
  output$botaoBaixarParalisia <- downloadHandler(
    
    filename = function() {
      'dados.paralisia.csv'
    },
    content = function(file) {
      write_excel_csv2(dados_paralisia, file, na="")
    }
  )
  
  output$botaoBaixarAlimentacao <- downloadHandler(
    filename = function() {
      'dados_saude_alimentacao.csv'
      },
    content = function(file) {
      write_excel_csv2(dados_saude_alimentacao, file, na="")
    }
  )
  
  output$dicio_alimentacao <- renderTable({
    DicioAlimentacao
  }, colnames = FALSE, striped = TRUE, bordered = TRUE, 
  width = "100%", align = "c")
  
  
  observeEvent(input$botaoDicioAlimentacao, {
    showModal(modalDialog(
      title = "Dicionário - Alimentação",
      tableOutput("dicio_alimentacao"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  
  output$dicio_paralisia <- renderTable({
    DicioParalisia
  }, colnames = FALSE, striped = TRUE, bordered = TRUE, 
  width = "100%", align = "c")
  
  
  observeEvent(input$botaoDicioParalisia, {
    showModal(modalDialog(
      title = "Dicionário - Paralisia Cerebral",
      tableOutput("dicio_paralisia"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  
## Descritiva ----
  
  observeEvent(input$linkTiposVariaveis, { 
    updateNavbarPage(session, "mainNav", selected = "tipos_variaveis")
  })
  observeEvent(input$linkDistrFreq, { 
    updateNavbarPage(session, "mainNav", selected = "tabTabFreqs")
  })
  observeEvent(input$linkMedidasResumo, { 
    updateNavbarPage(session, "mainNav", selected = "tabMedidasResumo")
  })
  
  output$tabelaExemploVariaveis <- renderTable(example_dataframe, bordered = TRUE)
  
  output$imagemTiposVariaveis <- renderImage({
    return(list(
      src = "www/images/garu_variaveis.png",
      filetype = "image/png",
      alt = "Tipos de Variaveis"
    ))
  }, deleteFile = FALSE)
  
  output$calcFrequencia <- renderText({
    "Frequência absoluta: Número de ocorrências de determinada categoria"
  })
  
  output$calcProporcao <- renderText({
    "Proporção: Frequência absoluta dividida pelo número total de ocorrências"
  })
  
  output$tabelaFreqRelacionamento <- renderTable(tab_frequencia_relacionamento,
                                                 bordered = TRUE, striped = TRUE)
  
  output$tabelaFreqAnoLetivo <- renderTable(tab_frequencia_ano_letivo,
                                            bordered = TRUE, striped = TRUE)
  
  output$tabelaFreqPeso <- renderTable(tab_frequencia_peso,
                                       bordered = TRUE, striped = TRUE)
  
  
  
  observeEvent(input$geraElementos, {
    elementos <<- sort(round(runif(input$slider_qtd_elementos, min = input$slider_min_max_valor[1], max = input$slider_min_max_valor[2])))
  })
  
  getElementos <- reactive({
    return(elementos)
  })
  
  output$printElementos <- renderText({
    input$geraElementos
    elementos
  })
  
  #Média
  
  observeEvent(input$mediaMostrarMais, {
    shinyjs::hide("mediaMostrarMais")
    shinyjs::show("mediaMostrarMenos")
    shinyjs::show("mediaTexto")
    shinyjs::show("mediaExplain")
    shinyjs::show("htMedia")
    shinyjs::show("exMedia")
  })
  
  observeEvent(input$mediaMostrarMenos, {
    shinyjs::hide("mediaMostrarMenos")
    shinyjs::show("mediaMostrarMais")
    shinyjs::hide("mediaTexto")
    shinyjs::hide("mediaExplain")
    shinyjs::hide("htMedia")
    shinyjs::hide("exMedia")
  })
  
  output$mediaTitle <- renderText({
    input$geraElementos
    paste0("<h4> Média: <strong> ", round(mean(elementos), 2), "</strong> </h4>")
  })
  
  output$showMedia <- reactive({
    return(values$showMedia)
  })
  
  output$mediaExplain <- renderUI({
    withMathJax(helpText("Média:  $$\\frac{1}{n} \\sum_{i=1}^{n} x_{i}$$"))
  })
  
  output$exMedia <- renderText({
    input$geraElementos
    text <- "Média = ("
    for (i in (1:length(elementos))) {
      if (i != length(elementos)) {
        text <- paste0(text, elementos[i], " + ")
      } else {
        text <- paste0(text, elementos[i])
      }
      
    }
    
    text <- paste0(text, ")/", length(elementos), " = ", round(mean(elementos), 4))
    text
  })
  
  #Mediana
  
  observeEvent(input$medianaMostrarMais, {
    shinyjs::hide("medianaMostrarMais")
    shinyjs::show("medianaMostrarMenos")
    shinyjs::show("medianaTexto")
    shinyjs::show("medianaExplain")
    shinyjs::show("htMediana")
    shinyjs::show("exMediana")
  })
  
  observeEvent(input$medianaMostrarMenos, {
    shinyjs::hide("medianaMostrarMenos")
    shinyjs::show("medianaMostrarMais")
    shinyjs::hide("medianaTexto")
    shinyjs::hide("medianaExplain")
    shinyjs::hide("htMediana")
    shinyjs::hide("exMediana")
  })
  
  output$medianaTitle <- renderText({
    input$geraElementos
    paste0("<h4> Mediana: <strong> ", round(median(elementos), 2), "</strong> </h4>")
  })
  
  output$medianaExplain <- renderUI({
    withMathJax(helpText("Mediana (n ímpar): $$x_{\\frac{n+1}{2}}$$"),
                helpText("Mediana (n par): $$\\frac{x_{\\frac{n}{2}} + x_{\\frac{n}{2} + 1}}{2}$$")
    )
  })
  
  output$exMediana <- renderText({
    input$geraElementos
    text <- "Mediana = "
    if (length(elementos)%%2 == 1) {
      text <- paste0(text, median(elementos))
    } else {
      text <- paste0(text, "(", elementos[length(elementos)/2], " + ", elementos[(length(elementos)/2) + 1], ")/2 = ", round(median(elementos), 4))
    }
    text
  })
  
  #Moda
  
  observeEvent(input$modaMostrarMais, {
    shinyjs::hide("modaMostrarMais")
    shinyjs::show("modaMostrarMenos")
    shinyjs::show("modaTexto")
    shinyjs::show("modaExplain")
    shinyjs::show("htModa")
    shinyjs::show("exModa")
  })
  
  observeEvent(input$modaMostrarMenos, {
    shinyjs::hide("modaMostrarMenos")
    shinyjs::show("modaMostrarMais")
    shinyjs::hide("modaTexto")
    shinyjs::hide("modaExplain")
    shinyjs::hide("htModa")
    shinyjs::hide("exModa")
  })
  
  output$modaTitle <- renderText({
    input$geraElementos
    paste0("<h4> Moda: <strong> ", vectorToString(getmode(elementos)), "</strong> </h4>")
  })
  
  output$exModa <- renderText({
    input$geraElementos
    text <- paste0("Moda = ", vectorToString(getmode(elementos)))
    text
  })
  
  #Mínimo
  
  observeEvent(input$minimoMostrarMais, {
    shinyjs::hide("minimoMostrarMais")
    shinyjs::show("minimoMostrarMenos")
    shinyjs::show("minimoTexto")
    shinyjs::show("minimoExplain")
    shinyjs::show("htMinimo")
    shinyjs::show("exMinimo")
  })
  
  observeEvent(input$minimoMostrarMenos, {
    shinyjs::hide("minimoMostrarMenos")
    shinyjs::show("minimoMostrarMais")
    shinyjs::hide("minimoTexto")
    shinyjs::hide("minimoExplain")
    shinyjs::hide("htMinimo")
    shinyjs::hide("exMinimo")
  })
  
  output$minimoTitle <- renderText({
    input$geraElementos
    paste0("<h4> Mínimo: <strong> ", min(elementos), "</strong> </h4>")
  })
  
  output$exMinimo <- renderText({
    input$geraElementos
    text <- paste0("Mínimo = ", min(elementos))
    text
  })
  
  #Máximo
  
  observeEvent(input$maximoMostrarMais, {
    shinyjs::hide("maximoMostrarMais")
    shinyjs::show("maximoMostrarMenos")
    shinyjs::show("maximoTexto")
    shinyjs::show("maximoExplain")
    shinyjs::show("htMaximo")
    shinyjs::show("exMaximo")
  })
  
  observeEvent(input$maximoMostrarMenos, {
    shinyjs::hide("maximoMostrarMenos")
    shinyjs::show("maximoMostrarMais")
    shinyjs::hide("maximoTexto")
    shinyjs::hide("maximoExplain")
    shinyjs::hide("htMaximo")
    shinyjs::hide("exMaximo")
  })
  
  output$maximoTitle <- renderText({
    input$geraElementos
    paste0("<h4> Máximo: <strong> ", max(elementos), "</strong> </h4>")
  })
  
  output$exMaximo <- renderText({
    input$geraElementos
    text <- paste0("Máximo = ", max(elementos))
    text
  })
  
  #Variância
  
  observeEvent(input$varianciaMostrarMais, {
    shinyjs::hide("varianciaMostrarMais")
    shinyjs::show("varianciaMostrarMenos")
    shinyjs::show("varianciaTexto")
    shinyjs::show("varianciaExplain")
    shinyjs::show("htVariancia")
    shinyjs::show("exVariancia")
  })
  
  observeEvent(input$varianciaMostrarMenos, {
    shinyjs::hide("varianciaMostrarMenos")
    shinyjs::show("varianciaMostrarMais")
    shinyjs::hide("varianciaTexto")
    shinyjs::hide("varianciaExplain")
    shinyjs::hide("htVariancia")
    shinyjs::hide("exVariancia")
  })
  
  output$varianciaTitle <- renderText({
    input$geraElementos
    paste0("<h4> Variância: <strong> ", round(var(elementos), 2), "</strong> </h4>")
  })
  
  output$varianciaExplain <- renderUI({
    withMathJax(helpText("Variância: $$\\frac{\\sum_{i=1}^{n} (x_{i} - \\bar{x})^{2}}{n}$$")
    )
  })
  
  output$exVariancia <- renderText({
    input$geraElementos
    text <- "Variância = ("
    for (i in (1:length(elementos))) {
      if (i != length(elementos)) {
        text <- paste0(text, "(",elementos[i], " - ", round(mean(elementos), 4), ")^2 + ")
      }
      else {
        text <- paste0(text, "(",elementos[i], " - ", round(mean(elementos), 4), ")^2)/", length(elementos), " = ", round(var(elementos), 4))
      }
    }
    text
  })
  
  #Desvio Padrão
  
  observeEvent(input$dpMostrarMais, {
    shinyjs::hide("dpMostrarMais")
    shinyjs::show("dpMostrarMenos")
    shinyjs::show("dpTexto")
    shinyjs::show("dpExplain")
    shinyjs::show("htDp")
    shinyjs::show("exDp")
  })
  
  observeEvent(input$dpMostrarMenos, {
    shinyjs::hide("dpMostrarMenos")
    shinyjs::show("dpMostrarMais")
    shinyjs::hide("dpTexto")
    shinyjs::hide("dpExplain")
    shinyjs::hide("htDp")
    shinyjs::hide("exDp")
  })
  
  output$dpTitle <- renderText({
    input$geraElementos
    paste0("<h4> Desvio Padrão: <strong> ", round(sd(elementos), 2), "</strong> </h4>")
  })
  
  output$dpExplain <- renderUI({
    withMathJax(helpText("Desvio Padrão: $$\\sqrt{var(X)}$$")
    )
  })
  
  output$exDp <- renderText({
    input$geraElementos
    text <- paste0("Desvio Padrão = sqrt(", round(var(elementos), 2), ") = ", round(sd(elementos), 4))
    text
  })
  
  #Erro Padrão
  
  observeEvent(input$epMostrarMais, {
    shinyjs::hide("epMostrarMais")
    shinyjs::show("epMostrarMenos")
    shinyjs::show("epTexto")
    shinyjs::show("epExplain")
    shinyjs::show("htEp")
    shinyjs::show("exEp")
  })
  
  observeEvent(input$epMostrarMenos, {
    shinyjs::hide("epMostrarMenos")
    shinyjs::show("epMostrarMais")
    shinyjs::hide("epTexto")
    shinyjs::hide("epExplain")
    shinyjs::hide("htEp")
    shinyjs::hide("exEp")
  })
  
  output$epTitle <- renderText({
    paste0("<h4> Erro Padrão </h4>")
  })
  
  output$epExplain <- renderUI({
    withMathJax(helpText("Erro Padrão: $$\\frac{\\sigma}{\\sqrt{n}}$$")
    )
  })
  
  output$exEp <- renderText({
    input$geraElementos
    tam_amostra <- ceiling(sqrt(length(elementos)))
    amostra <- sample(elementos, tam_amostra, replace = FALSE)
    text <- paste0("Separando uma amostra exemplo dos elementos gerados: \n", vectorToString(amostra),
                   "\nCalcula-se o seu desvio padrão = sqrt(var(", vectorToString(amostra), ")) = ", 
                   round(sd(amostra), 4), "\nE divide-se pela raiz do tamanho da amostra: \n", 
                   round(sd(amostra), 4), "/sqrt(", tam_amostra, ") = ", round((sd(amostra)/sqrt(tam_amostra)), 4))
    text
  })
  
  #Quantis
  
  observeEvent(input$quantilMostrarMais, {
    shinyjs::hide("quantilMostrarMais")
    shinyjs::show("quantilMostrarMenos")
    shinyjs::show("quantilTexto")
    shinyjs::show("quantilExplain")
    shinyjs::show("htQuantil")
    shinyjs::show("exQuantil")
  })
  
  observeEvent(input$quantilMostrarMenos, {
    shinyjs::hide("quantilMostrarMenos")
    shinyjs::show("quantilMostrarMais")
    shinyjs::hide("quantilTexto")
    shinyjs::hide("quantilExplain")
    shinyjs::hide("htQuantil")
    shinyjs::hide("exQuantil")
  })
  
  output$quantilTitle <- renderText({
    paste0("<h4> Quartis </h4>")
  })
  
  output$exQuantil <- renderText({
    input$geraElementos
    isEven <- (length(elementos)%%2 == 0)
    text <- paste0("Temos: ", vectorToString(elementos), "\n")
    
    if (isEven) {
      text <- paste0(text, 
                     paste0("0.25 quartil = ", elementos[ceiling(0.25*length(elementos))], ", o "),
                     ceiling(0.25*length(elementos)), "º elemento, pois divide 25% dos elementos abaixo dele.\n",
                     "0.5 quartil (mediana) = (", elementos[length(elementos)/2] , " + ", elementos[(length(elementos)/2) + 1], 
                     ")/2 = ", median(elementos), "\n",
                     "0.75 quartil = ", elementos[ceiling(0.75*length(elementos))], ", o ",
                     ceiling(0.75*length(elementos)), "º elemento, pois divide 75% dos elementos abaixo dele.\n"
      )
    } else {
      text <- paste0(text, 
                     "0.25 quartil = (",  elementos[floor(0.25*length(elementos))] , " + ", elementos[ceiling(0.25*length(elementos))], 
                     ")/2 = ", as.vector(quantile(elementos))[2], "\n",
                     "0.5 quartil (mediana) = ", elementos[ceiling(0.5*length(elementos))], ", o ",
                     ceiling(0.5*length(elementos)), "º elemento, pois divide metade dos elementos abaixo dele.\n",
                     "0.75 quartil = (",  elementos[floor(0.75*length(elementos))] , " + ", elementos[ceiling(0.75*length(elementos))],
                     ")/2 = ", as.vector(quantile(elementos))[4], "\n"
      )
    }
    
    text
  })
  
  #Distância interquartílica
  
  observeEvent(input$dist_quantilMostrarMais, {
    shinyjs::hide("dist_quantilMostrarMais")
    shinyjs::show("dist_quantilMostrarMenos")
    shinyjs::show("dist_quantilTexto")
    shinyjs::show("dist_quantilExplain")
    shinyjs::show("dist_htQuantil")
    shinyjs::show("dist_exQuantil")
  })
  
  observeEvent(input$dist_quantilMostrarMenos, {
    shinyjs::hide("dist_quantilMostrarMenos")
    shinyjs::show("dist_quantilMostrarMais")
    shinyjs::hide("dist_quantilTexto")
    shinyjs::hide("dist_quantilExplain")
    shinyjs::hide("dist_htQuantil")
    shinyjs::hide("dist_exQuantil")
  })
  
  output$dist_quantilTitle <- renderText({
    input$geraElementos
    paste0("<h4> Distância Interquartílica: <strong>", round(median(elementos) - as.vector(quantile(elementos))[2], 2), "</strong> </h4>")
  })
  
  
  output$dist_exQuantil <- renderText({
    input$geraElementos
    isEven <- (length(elementos)%%2 == 0)
    text <- paste0("Temos: ", vectorToString(elementos), "\n")
    
    if (isEven) {
      text <- paste0(text, 
                     paste0("0.5 quartil - 0.25 quartil = ", median(elementos), ' - ', elementos[ceiling(0.25*length(elementos))], ' = ', 
                            median(elementos) - elementos[ceiling(0.25*length(elementos))], '\n')
                     
      )
    } else {
      text <- paste0(text,
                     "0.5 quartil - 0.25 quartil = ", median(elementos), ' - ', as.vector(quantile(elementos))[2], ' = ',
                     median(elementos) - as.vector(quantile(elementos))[2], '. \n'
                     
      )
    }
    
    text
  })
  
  
  output$graficoElementos <- renderPlot({
    input$geraElementos
    tab <- as.data.frame(table(elementos))
    tab$elementos <- as.numeric(as.character(tab$elementos))
    mean <- round(mean(elementos), 2)
    mode <- getmode(elementos)
    tab$isMode <- ifelse(tab$elementos %in% mode, TRUE, FALSE)
    ggplot(data=tab, aes(x=elementos, y=Freq, fill = isMode)) + 
      geom_bar(stat="identity", position=position_dodge(), color = "black", width = 0.9) +
      geom_text(aes(label=Freq), vjust=-0.5, size=3.5)+
      annotate("segment", x = mean, xend = mean, y = 0, yend = max(tab$Freq) + 0.5, colour = "black", size=1) +
      #geom_segment(aes(x=mean, xend=mean, y=0, yend=max(tab$Freq) + 0.5)) +
      geom_label(aes(x=mean, y=max(tab$Freq) + 0.5, label = paste0("Média = ", mean))) +
      annotate("text", x = mode, y = 0.5, label = "Moda" , color="black", size=4 , angle=0) +
      #geom_label(aes(x=mode, y=0, label = paste0("Moda"))) +
      theme_minimal()+
      scale_fill_manual(values = c("#FFC20A", "#0C7BDC"),
                        labels = c("", "Moda")) +
      scale_x_continuous(breaks = round(seq(min(tab$elementos), max(tab$elementos), by = 1))) +
      theme(legend.position="none") +
      theme(legend.title = element_blank()) +
      labs(x="Valores", y = "Quantidade de elementos com esse valor")
  })
  
  
## Gráficos ----
  
  output$uiGrafQual <- renderUI({
    tagList(
      h3(strong("Gráficos para variáveis qualitativas")),
      selectInput("varGrafQual", "Variável",
                  choices = colnames(dados_saude_alimentacao)[var_factors]),
      h5(strong("Gráfico de Barras")),
      p("O gráfico de barras se dá por retângulos (barras) em que em uma de suas direções (mais frequentemente 
      na vertical) representa-se a frequência absoluta (contagem) ou relativa (porcentagem) de uma 
      variável qualitativa. Cada barra representa um possível valor e todas são paralelas umas às outras."),
      h5(strong("Gráfico de Pizza")),
      p("O gráfico de setores, ou gráfico de pizza, consiste de um círculo dividido em fatias que são proporcionais 
      às frequências relativas da variável."),
      br(),
      helpText("Fonte: Morettin, P. and Bussab, W. (2000). Estatística Básica (7a. ed.). Editora Saraiva.")
    )
  })
  
  output$uiGrafQuant <- renderUI({
    tagList(
      h3(strong("Gráficos para variáveis quantitativas")),
      selectInput("varGrafQuant", "Variável", 
                  choices = colnames(dados_saude_alimentacao)[var_numerics]),
      h5(strong("Gráfico de Barras")),
      p("Quando trabalhamos com variáveis quantitativas discretas, ou seja, que assumem uma quantidade limitada 
        de valores, podemos usar um gráfico similar ao de barras usado para variáveis qualitativas ordinais. 
        Dispomos os valores possíveis, em ordem, em barras e a altura é relativa a frequência."),
      h5(strong("Histograma")),
      p("Para variáveis quantitativas contínuas, fica inviável representar uma barra para cada observação. Imagine: 
      teria uma barra para o valor 4.82, uma para o 4.91, e assim em diante. Portanto, transformamos a variável 
      quantitativa em uma qualitativa ordinal por meio da criação de intervalos. A variável Peso, por exemplo, 
      é dividida em faixas de peso, para assim construir o histograma."),
      p("O histograma é um gráfico de barras adjacentes com bases proporcionais aos intervalos definidos, e a área de cada 
      barra é proporcional à sua respectiva frequência."),
      h5(strong("Boxplot")),
      HTML("<p>O boxplot é um gráfico muito utilizado pois a partir dele pode-se obter muitas informações sobre a dispersão, 
         posição, assimetria, caudas e valores discrepantes entre os dados. Ele se dá por um retângulo, onde cada limite é um quantil (o limite inferior é o 
         primeiro quartil (q1), e o limite superior é o terceiro quartil (q3).) O traço no meio representa a <strong>mediana</strong> 
         dos dados (q2). A partir do retângulo, para cima, segue uma linha até o ponto mais remoto que não exceda 
         LS = q3 + 1,5dq, chamado de limite superior. Similarmente, segue uma linha abaixo até o ponto que não exceda 
         LI = q1 - 1,5dq, chamando limite inferior. Os valores acima do limite superior e abaixo do limite inferior 
         são plotados como pontos e chamados de outliers ou valores atípicos.</p>"),
      br(),
      helpText("Observação: dq é a distância interquartil, que se dá por dq = q3 - q1."),
      br(),
      helpText("Fonte: Morettin, P. and Bussab, W. (2000). Estatística Básica (7a. ed.). Editora Saraiva.")
    )
  })
  
  output$graficosQual <- renderUI({
    req(input$varGrafQual)
    req(input$varGrafQual %in% colnames(dados_saude_alimentacao)[var_factors])
    if (input$varGrafQual %in% c("Sexo", "Relacionamento", 
                                 "Pratica esportes", 
                                 "Toma vitaminas")) {
      tagList(
        column(6, h4(strong("Gráfico de Barras")),
               plotOutput("grafBarras")),
        column(6, h4(strong("Gráfico de Pizza")),
               plotOutput("grafPizza"))
      )
    } else {
      tagList(
        h4(strong("Gráfico de Barras")),
        plotOutput("grafBarras"),
        helpText("O gráfico de pizza não é exibido pois a variável é qualitativa ordinal.")
      )
    }
  })
  
  output$graficosQuant <- renderUI({
    req(input$varGrafQuant)
    req(input$varGrafQuant %in% colnames(dados_saude_alimentacao))
    tagList(
      fluidRow(
        column(6, htmlOutput("tituloGraf1"),
               plotOutput("histograma"),
               conditionalPanel(
                 condition = "input.varGrafQuant != 'Ano letivo' && input.varGrafQuant != 'Percepção de Saúde'",
                 sliderInput("histNumBins", "Número de Barras", min = 5, max = 20, value = 10, step = 1)
               ),
               htmlOutput("htGraf1")
        ),
        column(6, h4(strong("Boxplot")),
               plotOutput("boxplot"))
      ),
      fluidRow(column(8, uiOutput("histAlt")))
    )
  })
  
  getQualPlotData <- reactive({
    req(input$varGrafQual)
    req(input$varGrafQual %in% colnames(dados_saude_alimentacao))
    dados_saude_alimentacao[[input$varGrafQual]]
  })
  
  getQuantPlotData <- reactive({
    req(input$varGrafQuant)
    req(input$varGrafQuant %in% colnames(dados_saude_alimentacao))
    dados_saude_alimentacao[[input$varGrafQuant]]
  })
  
  output$grafBarras <- renderPlot({
    selData <- getQualPlotData()
    selData <- as.data.frame(table(selData))
    colnames(selData) <- c("cat", "freq")
    selData$freq <- selData$freq / sum(selData$freq)
    
    ggplot(data = selData, aes(x = cat, y = freq, fill = cat)) +
      geom_bar(stat = "identity", colour = "black", width = 0.8) +
      geom_text(aes(x = cat, y = freq, label = round(freq, 2)), 
                colour = "black", vjust = -0.5) +
      scale_fill_manual(values = colorful) +
      guides(fill = "none") +
      theme_minimal() +
      scale_x_discrete(name = input$varGrafQual) +
      scale_y_continuous(name = "Frequência relativa", 
                         limits = c(0, max(selData$freq) + 0.05)) +
      theme(axis.title.y = element_text(size = 16),
            axis.text.y = element_text(size = 12))
  })
  
  output$grafPizza <- renderPlot({
    selData <- getQualPlotData()
    selData <- as.data.frame(table(selData))
    colnames(selData) <- c("cat", "freq")
    
    ggplot(selData, aes(x = "", y = freq, fill = cat)) +
      geom_bar(width = 1, stat = "identity", colour = "black") +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = colorful, 
                        name = input$varGrafQual) + 
      xlab(" ") +
      ylab(" ") + 
      theme_minimal() + 
      theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
      geom_text(aes(label = scales::percent(freq / sum(freq))), 
                position = position_stack(vjust = 0.5))
  })
  
  output$histograma <- renderPlot({
    req(input$varGrafQuant)
    req(input$varGrafQuant %in% colnames(dados_saude_alimentacao))
    selData <- getQuantPlotData()
    selData <- data.frame(Var = selData)
    
    if (input$varGrafQuant %in% c("Ano letivo", "Percepção de Saúde")) {
      selData$Var <- as.factor(selData$Var)
      ggplot(selData, aes(x = Var, fill = Var)) + 
        geom_bar(stat = "count", colour = "black", width = 0.8) +
        geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
        theme_minimal() +
        guides(fill = "none") +
        scale_x_discrete(name = input$varGrafQuant) +
        scale_y_continuous(name = "Frequência absoluta", limits = c(0, max(table(selData$Var)) + 5))
    } else {
      amplitude <- max(selData$Var, na.rm = TRUE) - min(selData$Var, na.rm = TRUE)
      N <- 5 * 10^(floor(log10(amplitude)) - 1)
      x_breaks <- seq(floor(min(selData$Var, na.rm = TRUE) / N) * N, ceiling(max(selData$Var, na.rm = TRUE) / N) * N, length.out = input$histNumBins + 1)
      freqdata <- data.frame(seq = (x_breaks[-1] + x_breaks[-length(x_breaks)]) / 2, freq = hist(selData$Var, breaks = x_breaks, plot = FALSE)$density)
      
      ggplot(freqdata, aes(x = seq, y = freq)) +
        geom_bar(stat = "identity", colour = "black", fill = "#0C7BDC", width = diff(x_breaks)[1]) +
        theme_minimal() +
        scale_x_continuous(breaks = x_breaks) +
        ylab("Frequência Relativa") +
        xlab(input$varGrafQuant) +
        geom_text(aes(label = scales::percent(freq)), vjust = -0.5)
    }
  })
  
  output$tituloGraf1 <- renderText({
    req(input$varGrafQuant)
    if (input$varGrafQuant %in% c("Ano letivo", "Percepção de Saúde")) {
      "<h4><strong>Gráfico de Barras</strong></h4>"
    } else {
      "<h4><strong>Histograma</strong></h4>"
    }
  })
  
  output$htGraf1 <- renderText({
    req(input$varGrafQuant)
    if (input$varGrafQuant %in% c("Ano letivo", "Percepção de Saúde")) {
      paste0("<span class = 'help-block'> Pode-se utilizar o gráfico de barras pois a variável ", 
             input$varGrafQuant, " é quantitativa discreta.</span>")
    } else {
      paste0("<span class = 'help-block'> Utiliza-se somente o histograma pois a variável ", 
             input$varGrafQuant, " é quantitativa contínua.</span>")
    }
  })
  
  output$boxplot <- renderPlot({
    req(input$varGrafQuant)
    req(input$varGrafQuant %in% colnames(dados_saude_alimentacao))
    selData <- getQuantPlotData()
    selData <- data.frame(cat = selData)
    
    ggplot(selData, aes(x = "", y = cat)) + 
      geom_boxplot(fill = "#0C7BDC", colour = "black") + 
      xlab("") +
      ylab(input$varGrafQuant) +
      theme_minimal()
  })
  
  output$histAlt <- renderUI({
    req(input$varGrafQuant)
    req(input$varGrafQuant %in% colnames(dados_saude_alimentacao))
    if (input$varGrafQuant %in% c("Ano letivo", "Percepção de Saúde")) {
      tagList(
        fluidRow(
          column(8, 
                 h4(strong("Histograma")),
                 plotOutput("histogramaAlt"))
        )
      )
    }
  })
  
  output$histogramaAlt <- renderPlot({
    req(input$varGrafQuant)
    req(input$varGrafQuant %in% colnames(dados_saude_alimentacao))
    selData <- getQuantPlotData()
    selData <- data.frame(Var = selData)
    
    if (input$varGrafQuant == "Ano letivo") {
      selData$Var <- cut(selData$Var, breaks =  c(-Inf, 1, 2, 3, 4), labels = c("1", '2', "3",  "4"))
      selData <- as.data.frame(table(selData$Var))
      colnames(selData) <- c("Var", "Freq")
      selData$Freq <- selData$Freq / sum(selData$Freq)
      
      ggplot(selData, aes(x = Var, y = Freq)) + 
        geom_histogram(stat = "identity", colour = "black", fill = "#0C7BDC", width = 1) +
        geom_text(aes(label = scales::percent(Freq)), vjust = -0.5) +
        scale_y_continuous(limits = c(0, max(selData$Freq) + 0.05)) +
        theme_minimal() + 
        ylab("Frequência Relativa") + 
        xlab(input$varGrafQuant)
    } else {
      selData$Var <- cut(selData$Var, breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10'))
      selData <- as.data.frame(table(selData))
      colnames(selData) <- c("Var", "Freq")
      selData$Freq <- selData$Freq / sum(selData$Freq)
      
      ggplot(selData, aes(x = Var, y = Freq)) + 
        geom_bar(stat = "identity", colour = "black", fill = "#0C7BDC", width = 1) +
        geom_text(aes(label = scales::percent(Freq)), vjust = -0.5) +
        scale_y_continuous(limits = c(0, max(selData$Freq) + 0.05)) +
        theme_minimal() + 
        ylab("Frequência Relativa") + 
        xlab(input$varGrafQuant)
    }
  })
  
  output$biUI <- renderUI({
    enable1 <- sum(shortLevels) >= 2
    enable2 <- sum(var_numerics) >= 2
    enable3 <- sum(shortLevels) >= 1 & sum(var_numerics) >= 1
    
    part1 <- if (enable1) {
      tagList(
        h4(strong("Duas variáveis qualitativas")),
        selectInput("varGrafBiQual1", "Variável 1", choices = colnames(dados_saude_alimentacao)[shortLevels], 
                    selected = colnames(dados_saude_alimentacao)[shortLevels][1]),
        selectInput("varGrafBiQual2", "Variável 2", choices = colnames(dados_saude_alimentacao)[shortLevels], 
                    selected = colnames(dados_saude_alimentacao)[shortLevels][2]),
        p("Imagine que queiramos visualizar a distribuição das observações em 
                        relação a duas variáveis qualitativas. Podemos, primeiramente, visualizar 
                        a tabela de frequências relativas: "),
        tableOutput("tabGrafVarQual"),
        p("Podemos agora, construir um gráfico de barras muito similar ao visto 
                        anteriormente, mas com as barras divididas de acordo com a outra variável 
                        variável qualitativa escolhida"),
        plotOutput("grafBarraBiQual")
      )
    } else {
      tagList(h4(strong("Duas variáveis qualitativas")),
              p("Seu banco de dados utilizado não tem variáveis qualitativas o suficiente 
                         para isso. Por favor escolha outro ou tente com o banco de dados nativo."))
    }
    
    part2 <- if (enable2) {
      tagList(
        h4(strong("Duas variáveis quantitativas")),
        selectInput("varGrafBiQuant1", "Variável 1", choices = colnames(dados_saude_alimentacao)[var_numerics], selected = colnames(dados_saude_alimentacao)[var_numerics][1]),
        selectInput("varGrafBiQuant2", "Variável 2", choices = colnames(dados_saude_alimentacao)[var_numerics], selected = colnames(dados_saude_alimentacao)[var_numerics][2]),
        p("O gráfico de dispersão bivariado é utilizado para observar a correlação entre duas variáveis quantitativas. Por exemplo, 
se a nuvem de pontos se assemelha a uma reta crescente ou descrescente, há indícios de correlação linear entre as duas variáveis. 
No entanto, vale ressaltar que há outros tipos de correlação, não lineares."),
        plotOutput("scatterGraf")
      )
    } else {
      tagList(h4(strong("Duas variáveis quantitativas")),
              p("Seu banco de dados utilizado não tem variáveis quantitativas o suficiente 
                         para isso. Por favor escolha outro ou tente com o banco de dados nativo."))
    }
    
    part3 <- if (enable3) {
      tagList(
        h4(strong("Variáveis qualitativas e quantitativas")),
        selectInput("varQualBoxplot1", "Variável Qualitativa", choices = colnames(dados_saude_alimentacao)[shortLevels], 
                    selected = colnames(dados_saude_alimentacao)[shortLevels][1]),
        selectInput("varQualBoxplot2", "Variável Quantitativa", choices = colnames(dados_saude_alimentacao)[var_numerics], 
                    selected = colnames(dados_saude_alimentacao)[var_numerics][1]),
        p("Também é possível visualizar a relação existente entre uma variável qualitativa e uma variável quantitativa. Isto pode ser feito por meio do cálculo de 
medidas resumo, e construção de histogramas e boxplots, para a variável quantitativa em cada categoria da qualitativa. Compare cada variável qualitativa com uma outra quantitativa contínua nos boxplots abaixo:"),
        checkboxInput("multiBoxplotUseNA", "Usar NA", value=FALSE),
        plotOutput("multiBoxplot")
      )
    } else {
      tagList(h4(strong("Variáveis qualitativas e quantitativas")),
              p("Seu banco de dados utilizado não tem variáveis qualitativas/quantitativas o suficiente 
                         para isso. Por favor escolha outro ou tente com o banco de dados nativo."))
    }
    
    tagList(
      h3(strong("Gráficos Bivariados")),
      fluidRow(
        column(12, part1),
        column(12, part2),
        column(12, part3)
      )
    )
  })
  
  getBiQualTable <- reactive({
    req(input$varGrafBiQual1, input$varGrafBiQual2)
    req(input$varGrafBiQual1 %in% colnames(dados_saude_alimentacao))
    req(input$varGrafBiQual2 %in% colnames(dados_saude_alimentacao))
    df <- data.frame(Var1 = dados_saude_alimentacao[[input$varGrafBiQual1]], 
                     Var2 = dados_saude_alimentacao[[input$varGrafBiQual2]])
    
    tab <- table(df$Var1, df$Var2) / length(df$Var1)
    tab <- round(tab, 2)
    tab
  })
  
  output$tabGrafVarQual <- renderTable({
    getBiQualTable()
  }, rownames = TRUE, bordered = TRUE, striped = TRUE)
  
  output$grafBarraBiQual <- renderPlot({
    df_src <- getBiQualTable()
    df <- as.data.frame(as.table(df_src))
    
    ggplot(df, aes(x = Var2, y = Freq, fill = Var1)) + 
      geom_bar(stat = "identity", color = "black", width = 0.9) + 
      geom_text(aes(label = scales::percent(Freq)), position = position_stack(vjust = 0.5)) +
      labs(x = input$varGrafBiQual1, y = "Frequência relativa") + 
      scale_fill_discrete(name = input$varGrafBiQual2) + 
      theme_minimal()
  })
  
  output$scatterGraf <- renderPlot({
    req(input$varGrafBiQuant1, input$varGrafBiQuant2)
    req(input$varGrafBiQuant1 %in% colnames(dados_saude_alimentacao))
    req(input$varGrafBiQuant2 %in% colnames(dados_saude_alimentacao))
    selData <- data.frame(Var1 = dados_saude_alimentacao[[input$varGrafBiQuant1]], 
                          Var2 = dados_saude_alimentacao[[input$varGrafBiQuant2]])
    
    ggplot(selData, aes(x = Var1, y = Var2)) + 
      geom_point(color = "#0C7BDC", size = 3L) +
      theme_minimal() + 
      labs(x = input$varGrafBiQuant1, y = input$varGrafBiQuant2)
  })
  
  output$multiBoxplot <- renderPlot({
    req(input$varQualBoxplot1, input$varQualBoxplot2)
    req(input$varQualBoxplot1 %in% colnames(dados_saude_alimentacao))
    req(input$varQualBoxplot2 %in% colnames(dados_saude_alimentacao))
    selData <- data.frame(Var1 = dados_saude_alimentacao[[input$varQualBoxplot1]], 
                          Var2 = dados_saude_alimentacao[[input$varQualBoxplot2]])
    
    if (!input$multiBoxplotUseNA) {
      selData <- selData[!is.na(selData$Var1), ]
    }
    
    ggplot(selData, aes(x = Var1, y = Var2, fill = Var1)) + 
      geom_boxplot() + 
      theme_minimal() + 
      labs(x = "", y = input$varQualBoxplot2) + 
      scale_fill_discrete(name = input$varQualBoxplot1)
  })
  
## Probabilidade ----
  
  values <- reactiveValues()
  values$showMedia <- FALSE
  normal_params <- reactiveValues()
  normal_params$u <- 0
  normal_params$dp <- 1
  normal_params$min <- 1
  normal_params$max <- 4
  normal_params$sit <- 1
  
  testeT1_params <- reactiveValues()
  testeT1_params$u <- 0
  testeT1_params$dp <- 1
  testeT1_params$X <- 0.18
  testeT1_params$n <- 10
  testeT1_params$alpha <- 0.05
  
  testeT2_params <- reactiveValues()
  testeT2_params$n <- 12
  testeT2_params$u1 <- 10
  testeT2_params$u2 <- 9
  testeT2_params$dp1 <- 2.2
  testeT2_params$dp2 <- 2.2
  p1 <- rnorm(12, 10, 2.2)
  p2 <- rnorm(12, 9, 2.2)
  testeT2_params$p1 <- p1
  testeT2_params$p2 <- p2
  testeT2_params$D <- p1 - p2
  
  testeQui_params <- reactiveValues()
  testeQui_params$matrix <- rbind(c(48, 8, 56), c(30, 21, 51), c(78, 29, 107))
  testeQui_params$expected <- rbind(c(41, 15, 56), c(37, 14, 51), c(78, 29, 107))
  testeQui_params$colnames <- c("Melhora", "Não", "Total")
  testeQui_params$rownames <- c("Droga", "Placebo", "Total")
  testeQui_params$chi <- 9.3

  bernoulliPlot <- reactive({
    p <- input$bernoulli_p
    bern <- data.frame(c(0, 1), c(1-p, p))
    colnames(bern) <- c("x", "p(x)")
    bern$x <- as.factor(as.character(bern$x))
    g <- ggplot(data = bern, aes(x=x, y=`p(x)`)) + 
      geom_bar(stat="identity", fill = medium_cyan, color = "white", width = 0.3) + 
      labs(title = paste0("Distribuição Bernoulli de Probabilidades com p = ", p),
           y = "Probabilidade", 
           x = "x") + 
      scale_y_continuous(breaks = seq(0, 1, by=0.1), limits = c(0, 1)) + 
      theme_classic() + 
      theme(axis.ticks.x = element_blank(),
            axis.title.x = element_text(size = 20),
            axis.text.x = element_text(size = 14), 
            axis.title.y = element_text(size = 14),
            axis.text.y = element_text(size = 12),
            title = element_text(size = 16))
    
    g
  })
  
  binomPlot <- reactive({
    p <- input$binom_p
    n <- input$binom_n
    x_breaks <- c(0:n)
    if (n > 25)
      x_breaks <- seq(5, n, by = 5)
    
    values <- data.frame(x = 0:n, y = dbinom(0:n, n, p))
    print(x_breaks)
    g <- ggplot(data = values, aes(x=x, y=y)) + 
      geom_bar(stat = "identity", width = 0.5, color = "white", fill = medium_cyan) + 
      labs(title = paste0("Distribuição Binomial de Probabilidades com p = ", p, " e n = ", n)) + 
      ylab("Probabilidade") + 
      scale_x_continuous(name = "k", breaks = x_breaks) +
      theme_classic() + 
      theme(axis.ticks.x = element_blank(),
            axis.title.x = element_text(size = 20),
            axis.text.x = element_text(size = 14), 
            axis.title.y = element_text(size = 14),
            axis.text.y = element_text(size = 12),
            title = element_text(size = 16))
    
    return(g)
  })
  
  buildBernoulliTable <- reactive({
    p <- input$bernoulli_p
    bern <- data.frame(c(0, 1), c(1-p, p), c("Probabilidade de que seja 0 (fracasso)", "Probabilidade de que seja 1 (sucesso)"))
    colnames(bern) <- c("x", "p(x)", "Explicação")
    bern$`p(x)` <- scales::percent(bern$`p(x)`)
    bern
  })
  
  buildBinomTable <- reactive({
    p <- input$binom_p
    n <- input$binom_n
    binomTab <- data.frame(0:n, round(dbinom(0:n, n, p), 4))
    colnames(binomTab) <- c("k", "p(k)")
    if (n > 25) {
      binomTab <- binomTab[binomTab$`p(k)` > 0,]
    }
    binomTab$exp <- ""
    binomTab$`p(k)` <- scales::percent(binomTab$`p(k)`)
    for (i in 1:nrow(binomTab)) {
      binomTab$exp[i] <- paste0("Probabilidade de ter ", binomTab$k[i], " sucesso(s) em ", n, " tentativas.")
    }
    colnames(binomTab)[3] <- "Explicação"
    return (binomTab)
  })
  
  buildPoissonTable <- reactive({
    u <- input$poisson_u
    min <- input$poisson_minmax[1]
    max <- input$poisson_minmax[2]
    
    poisTab <- data.frame(min:max, dpois(min:max, u))
    colnames(poisTab) <- c("k", "p(k)")
    poisTab$exp <- ""
    poisTab$`p(k)` <- scales::percent(poisTab$`p(k)`)
    for (i in 1:nrow(poisTab)) {
      poisTab$exp[i] <- paste0("Probabilidade de ter ", poisTab$k[i], " acontecimento(s) no intervalo de tempo.")
    }
    colnames(poisTab)[3] <- "Explicação"
    return (poisTab)
  })
  
  output$distrTable <- renderTable({
    switch (input$distribuicao,
            "bernoulli" = buildBernoulliTable(),
            "binomial" = buildBinomTable(),
            "poisson" = buildPoissonTable(),
            "normal" = buildBinomTable()
    )
  }, bordered = TRUE, striped = TRUE, colnames = TRUE, width = "100%", align = "c")
  
  poissonPlot <- reactive({
    u <- input$poisson_u
    min <- input$poisson_minmax[1]
    max <- input$poisson_minmax[2]
    x_breaks <- c(min:max)
    if (max - min > 20)
      x_breaks <- seq(min, max, by = 5)
    values <- data.frame(x = min:max, y = dpois(min:max, u))
    g <- ggplot(data = values, aes(x=x, y=y)) + 
      geom_bar(stat = "identity", width = 0.5, color = "white", fill = medium_cyan) +
      ylab("Probabilidade") + 
      scale_x_continuous(name = "k", breaks = x_breaks) +
      theme_classic() + 
      theme(axis.ticks.x = element_blank(),
            axis.title.x = element_text(size = 20),
            axis.text.x = element_text(size = 14), 
            axis.title.y = element_text(size = 14),
            axis.text.y = element_text(size = 12),
            title = element_text(size = 16))
    
    
    return(g)
  })
  
  observeEvent(input$distribuicao, {
    if (input$distribuicao == "Normal") {
      req(input$normal_p)
      click("normal_refresh")
      print("triggered")
    }
  })
  
  observeEvent(input$normal_refresh, {
    normal_params$u <- input$normal_media
    normal_params$dp <- input$normal_dp
    normal_params$min <- input$normal_range[1]
    normal_params$max <- input$normal_range[2]
    min <- normal_params$u - 4*normal_params$dp
    max <- normal_params$u + 4*normal_params$dp
    normal_params$sit <- 1
    if (normal_params$min > min) {
      if (normal_params$max < max) {
        normal_params$sit <- 3
      } else {
        normal_params$sit <- 4
      }
    } else {
      if (normal_params$max < max) {
        normal_params$sit <- 2
      } else {
        normal_params$sit <- 1
      }
    }
    
  })
  
  output$normal_params <- renderUI({
    u <- input$normal_media
    dp <- input$normal_dp
    min <- u - 4*dp
    max <- u + 4*dp
    tags <- tagList(
      sliderInput("normal_range", label = "Intervalo de interesse", 
                  min = min, max = max, value = c(u + dp, max), round = -2, step = 0.05)
      
      
      
    )
    tags
  })
  
  normalPlot <- reactive({
    req(input$normal_range)
    u <- normal_params$u
    dp <- normal_params$dp
    min_p <- normal_params$min
    max_p <- normal_params$max
    print(min_p)
    print(max_p)
    print(normal_params$sit)
    yend <- switch (normal_params$sit,
                    dnorm(u + 2*dp, mean=u, sd=dp),
                    dnorm(max_p, mean=u, sd=dp),
                    dnorm(max_p, mean=u, sd=dp),
                    dnorm(min_p, mean=u, sd=dp)
    ) 
    xpos <- switch(normal_params$sit,
                   u+2*dp + 1.5,
                   ifelse(max_p > u, max_p + 1.5, max_p - 1.5),
                   ifelse(max_p > u, max_p + 1.5, max_p - 1.5),
                   ifelse(min_p > u, min_p + 1.5, min_p - 1.5)
    )
    
    min <- u - 6*dp
    max <- u + 6*dp
    
    prob <- round(pnorm(max_p, u, dp) - pnorm(min_p, u, dp), 3)
    enum <- switch (normal_params$sit,
                    paste0("P(", min_p, " <= x <= ", max_p, ") = "),
                    paste0("P(x <= ", max_p, ") = "),
                    paste0("P(", min_p, " <= x <= ", max_p, ") = "),
                    paste0("P(x >= ", min_p, ") = ")
    )
    enum <- paste0(enum, scales::percent(prob))
    x <- seq(min_p, max_p, length = 300)
    y <- dnorm(x, mean=u, sd=dp)
    area <- data.frame(x, y)
    
    g <- ggplot(data = area, aes(x=x, y=y)) + 
      geom_area(stat="identity", fill = medium_cyan, alpha = 0.7) + 
      stat_function(fun=dnorm, n=101, args = list(mean=u, sd=dp), color=dark_cyan) + 
      scale_x_continuous(limits = c(min, max)) + 
      geom_segment(aes(x=u, xend=u, y=0, yend=dnorm(u, u, dp)), color = "gray", linetype=2, alpha=0.3) + 
      annotate(geom="text", x=u, y=dnorm(u,u,dp) + 0.05, label=paste0("μ = ", u), size=5, color=dark_cyan) + 
      annotate(geom="text", x=xpos, y=yend + 0.01, label=enum) + 
      labs(x = "x", y = "f(x)")
    
    g <- switch (normal_params$sit,
                 g,
                 g + geom_segment(aes(x=max_p, xend=max_p, y=0, yend=dnorm(max_p, u, dp)), color=dark_cyan, linetype=2),
                 g + geom_segment(aes(x=max_p, xend=max_p, y=0, yend=dnorm(max_p, u, dp)), color=dark_cyan, linetype=2) + 
                   geom_segment(aes(x=min_p, xend=min_p, y=0, yend=dnorm(min_p, u, dp)), color=dark_cyan, linetype=2),
                 g + geom_segment(aes(x=min_p, xend=min_p, y=0, yend=dnorm(min_p, u, dp)), color=dark_cyan, linetype=2)
    )
    g
    
    
  })
  
  output$normalExemplo <- renderText({
    req(input$normal_range)
    u <- normal_params$u
    dp <- normal_params$dp
    min_p <- normal_params$min
    max_p <- normal_params$max
    z1 <- (min_p - u)/dp
    z2 <- (max_p - u)/dp
    text <- c("P(", min_p, " <= x <= ", max_p, ") = \nP((", min_p, " - μ)/σ <= (x- μ)/σ <= (", max_p, " - μ)/σ) = \n
              P((", min_p, " - ", u, ")/", dp, " <= Z <= (", max_p, " - ", u, ")/", dp, ") = \n
              P(", z1, " <= Z <= ", z2, ")\n\n
              P(Z >= ", z1, ") = 1 - P(Z <= ", z1, ") = 1 - ", round(pnorm(min_p, u, dp), 3), " = ", round(pnorm(min_p, u, dp, lower.tail=TRUE), 3), "\n
              P(Z <= ", z2, ") = ", round(pnorm(max_p, u, dp), 3), "\n\n
              P(", z1, " <= Z <= ", z2, ") = ", round(pnorm(max_p, u, dp), 3), " - ", round(pnorm(min_p, u, dp, lower.tail=TRUE), 3), " = ", round(pnorm(max_p, u, dp), 3) - round(pnorm(min_p, u, dp, lower.tail=TRUE), 3), " = \n
              ", scales::percent(round(pnorm(max_p, u, dp), 3) - round(pnorm(min_p, u, dp, lower.tail=TRUE), 3)))
    text
  })
  
  output$expExemplo <- renderUI({
    b <- input$exp_b
    min <- input$exp_val[1]
    max <- input$exp_val[2]
    prob <- pexp(max, b) - pexp(min, b)
    withMathJax(paste0("P(", min, " <= x <= ", max, ") = $$\\int_{", min, "}^{", max, "}
                       2e^{-2x}dx = ", prob, "$$"))
  })
  
  expPlot <- reactive({
    b <- input$exp_b
    min <- input$exp_val[1]
    max <- input$exp_val[2]
    x <- seq(min, max, length = 300)
    y <- dexp(x, b)
    area <- data.frame(x, y)
    prob <- pexp(max, b) - pexp(min, b)
    text <- paste0("P(", min, " <= x <= ", max, ") = ", scales::percent(round(prob, 4)))
    g <- ggplot(data = area, aes(x=x, y=y)) + 
      geom_area(stat="identity", fill = medium_ocre) + 
      stat_function(fun=dexp, n=101, args = list(rate = b), color="black") + 
      scale_x_continuous(name="x", limits = c(0, 3)) + 
      scale_y_continuous(name="f(x)", limits=c(0, b + 1)) + 
      geom_segment(aes(x=max, xend=max, y=0, yend=dexp(max, b)), color = brown, alpha= 0.7, linetype=2) +
      annotate(geom="text", x = 2, y=b+0.1, label=text, color=brown, size = 6) 
    
    if (min > 0) {
      g <- g + geom_segment(aes(x=min, xend=min, y=0, yend=dexp(min, b)), color = "brown", alpha=0.7, linetype=2)
    }
    g
  })
  
  quiPlot <- reactive({
    qx <- input$qui_x
    min <- input$qui_values[1]
    max <- input$qui_values[2]
    x <- seq(min, max, length = 300)
    y <- dchisq(x, qx)
    area <- data.frame(x, y)
    prob <- pchisq(max, qx) - pchisq(min, qx)
    text <- paste0("P(", min, " <= x <= ", max, ") = ", scales::percent(round(prob, 4)))
    g <- ggplot(data = area, aes(x=x, y=y)) + 
      geom_area(stat="identity", fill = red_nail) + 
      stat_function(fun=dchisq, n=101, args = list(df = qx), color=red_broken_nail) + 
      scale_x_continuous(name="x", limits = c(0, 16)) + 
      ylab("f(x)") + 
      geom_segment(aes(x=max, xend=max, y=0, yend=dchisq(max, qx)), color = red_broken_nail, alpha= 0.7, linetype=2) +
      annotate(geom="text", x = 12, y=dchisq(max, qx) + 0.1, label=text, color=red_broken_nail, size = 6) 
    
    if (min > 0) {
      g <- g + geom_segment(aes(x=min, xend=min, y=0, yend=dchisq(min, qx)), color = red_broken_nail, alpha=0.7, linetype=2)
    }
    g
  })
  
  tPlot <- reactive({
    gl <- input$t_gl
    min <- -4
    max <- 4
    x <- seq(min, max, length=300)
    y <- dt(x, gl)
    area <- data.frame(x, y)
    
    g <- ggplot(data=area, aes(x=x, y=y)) + 
      stat_function(fun=dnorm, n=101, args=list(mean=0, sd=1), color = medium_cyan, alpha = 0.7) +
      stat_function(fun=dt, n=101, args=list(df=gl), color = "black") + 
      annotate(geom="text", x=2, y=dnorm(1, 0, 1), color=medium_cyan, label = "Distribuição Normal") + 
      annotate(geom="text", x=-0.1, y=dt(-1, gl) - 0.15, color="black", label = paste0("Distribuição T com ", gl, " graus de liberdade")) + 
      labs(x="x", y="f(x)")
    g
  })
  
  
  
  distributionInput <- reactive({
    switch (input$distribuicao,
            "bernoulli" = bernoulliPlot(),
            "binomial" = binomPlot(),
            "poisson" = poissonPlot(),
            "Normal" = normalPlot(),
            "exp" = expPlot(),
            "qui" = quiPlot(),
            "t" = tPlot()
    )
  })
  
  output$distribuicao <- renderPlot({
    g <- distributionInput()
    g
  })
  
  
## Inferência -------
  
  observeEvent(input$testeT1Refresh, {
    testeT1_params$u <- input$testeT1MediaPop
    testeT1_params$dp <- input$testeT1DPPop
    testeT1_params$X <- input$testeT1MediaA
    testeT1_params$n <- input$testeT1TamanhoA
    testeT1_params$alpha <- input$testeT1Alpha
  })
  
  output$testeT1Plot <- renderPlot({
    u <- testeT1_params$u
    dp <- testeT1_params$dp
    X <- testeT1_params$X
    n <- testeT1_params$n
    S <- dp/sqrt(n)
    alpha <- testeT1_params$alpha
    x_min <- u - 4*dp
    x_max <- u + 4*dp
    rc1 <- qnorm(alpha/2, mean=u, sd=S)
    rc2 <- qnorm(1-alpha/2, mean=u, sd=S)
    accept <- FALSE
    if (X > rc1 && X < rc2) {
      accept <- TRUE
    }
    x1 <- c(seq(x_min, rc1, length = 200))
    y1 <- dnorm(x1, mean=u, sd=S)
    data1 <- as.data.frame(cbind(x1, y1))
    colnames(data1) <- c("x", "y")
    
    x2 <- c(seq(rc2, x_max, length=200))
    y2 <- dnorm(x2, mean=u, sd=S)
    data2 <- as.data.frame(cbind(x2, y2))
    colnames(data2) <- c("x", "y")
    
    point <- data.frame(X, dnorm(X, u, S)/2)
    colnames(point) <- c("x", "y")
    
    g <- ggplot(data=data1, aes(x=x, y=y))  + 
      geom_area(stat="identity", fill="#0C7BDC", alpha = 0.4) + 
      geom_area(data=data2, aes(x=x, y=y), stat="identity", fill="#0C7BDC", alpha=0.4) +
      stat_function(fun=dnorm, n=300, args = list(mean=u, sd=S), color = "#E66100") + 
      scale_x_continuous(limits = c(x_min, x_max)) + 
      geom_segment(aes(x=u, xend=u, y=0, yend=dnorm(u, u, S)), color = "gray", linetype=2, alpha=0.6) + 
      annotate(geom="text", x=u, y=1.1*dnorm(u,u,S), label=paste0("μ = ", u), size=5, color=dark_cyan) + 
      geom_point(data=point, aes(x=x, y=y), 
                 color = ifelse(accept, "#FFC20A", "#0C7BDC"), size = 8, shape = 8)+
      theme_minimal()
    g
  })
  
  output$testeT1Texto <- renderText({
    u <- testeT1_params$u
    dp <- testeT1_params$dp
    X <- testeT1_params$X
    n <- testeT1_params$n
    S <- dp/sqrt(n)
    alpha <- testeT1_params$alpha
    rc1 <- qnorm(alpha/2, mean=u, sd=S)
    rc2 <- qnorm(1-alpha/2, mean=u, sd=S)
    accept <- FALSE
    if (X > rc1 && X < rc2) {
      accept <- TRUE
    }
    p <- 2*pnorm(X, u, S, lower.tail = X < u)
    
    text <- paste0("H0: μ = ", u, "\nH1: μ ≠ ", u, 
                   "\n\nX̄ ~ N(μ, σ/sqrt(n))\n",
                   "X̄ ~ N(", u, ", ", round(S, digits = 4), ")\n",
                   "α = P(Cometer Erro Tipo I) = ", alpha, " = ", scales::percent(alpha), "\n",
                   "Região Crítica = RC = ]-∞, ", round(rc1, digits = 4), "] U [", round(rc2, digits = 4), ", ∞[\n"
    )
    
    if (accept) {
      text <- paste0(text, "X̄̄ ∉ RC, logo, H0 não é rejeitada. μ = ", u,"\n")
    } else {
      text <- paste0(text, "X̄ ∈ RC, logo H0 é rejeitada e a hipótese alternativa H1 é aceita. 
                     μ ≠ ", u, "\n")
    }
    
    
    text <- paste0(text, "\nValor-p: ", p)
    text
    
  })
  
  
  observeEvent(input$testeT2Refresh, {
    testeT2_params$n <- input$testeT2TamanhoA
    testeT2_params$u1 <- input$testeT2MediaP1
    testeT2_params$u2 <- input$testeT2MediaP2
    testeT2_params$dp1 <- input$testeT2DPP1
    testeT2_params$dp2 <- input$testeT2DPP2
    p1 <- rnorm(testeT2_params$n, testeT2_params$u1, testeT2_params$dp1)
    p2 <- rnorm(testeT2_params$n, testeT2_params$u2, testeT2_params$dp2)
    testeT2_params$p1 <- p1
    testeT2_params$p2 <- p2
    testeT2_params$D <- p1 - p2
  })
  
  output$testeT2Table <- renderTable({
    tab <- as.data.frame(rbind(testeT2_params$p1[1:10], testeT2_params$p2[1:10], testeT2_params$D[1:10]))
    tab <- round(tab, 2)
    rownames(tab) <- c("Amostra A1 de P1", "Amostra A2 de P2", "Diferença (A1 - A2)")
    tab
  }, colnames = FALSE, rownames = TRUE, bordered = TRUE, striped = TRUE, width = "100%")
  
  output$testeT2Graph1 <- renderPlot({
    n <- testeT2_params$n
    u1 <- testeT2_params$u1
    u2 <- testeT2_params$u2
    dp1 <- testeT2_params$dp1
    dp2 <- testeT2_params$dp2
    
    p1 <- testeT2_params$p1
    p2 <- testeT2_params$p2
    p <- as.data.frame(cbind(c(p1, p2), c(rep("População 1", n), rep("População 2", n))))
    colnames(p) <- c("x", "pop")
    p$x <- as.numeric(as.character(p$x))
    
    g <- ggplot(data=p, aes(x=x)) + 
      geom_density(aes(group = pop, color = pop)) + 
      scale_x_continuous(limits = c(min(u1 - 4*dp1, u2 - 4*dp2), max(u1 + 4*dp1, u2 + 4*dp2))) + 
      scale_color_discrete(name="População") + 
      xlab(" ") +
      ylab("Densidade")+
      theme_minimal()
    g
    
  })
  
  output$testeT2Graph2 <- renderPlot({
    u1 <- testeT2_params$u1
    u2 <- testeT2_params$u2
    dp1 <- testeT2_params$dp1
    dp2 <- testeT2_params$dp2
    D <- as.data.frame(testeT2_params$D)
    colnames(D) <- c("x")
    
    g <- ggplot(data=D, aes(x=x)) + 
      geom_density() + 
      scale_x_continuous(limits = c(min(u1 - u2, u2 - u1) - 4*(max(dp1, dp2)), max(u1 - u2, u2 - u1) + 4*max(dp1, dp2))) + 
      xlab("Diferença") + 
      ylab("Densidade")+
      theme_minimal()
    g
    
    
  })
  
  output$testeT2Calc <- renderText({
    D <- testeT2_params$D
    n <- testeT2_params$n
    t <- mean(D)/(sd(D)/sqrt(n))
    p <- dt(t, df=n-1)
    
    alpha <- input$testeT2Alpha
    print(t.test(D, alternative = "two.sided", mu = 0, conf.level = .95))
    text <- paste0("H0: µ1 = µ2\nH1:µ1 ≠ µ2\n\n")
    
    text <- paste0(text, "Estatística do teste observada = ", round(t, 4))
    
    if (p < alpha) {
      text <- paste0(text, "\nComo p-valor = ", round(p, 4), " < ", "α = ", alpha, ", rejeita-se a hipótese nula H0. Ou seja, µ1 ≠ µ2 ao
                  nível de significância de ", alpha * 100, "%\n")
    } else {
      text <- paste0(text, "\nComo p-valor = ", round(p, 4), " >= ", "α = ", alpha, ", não se rejeita a hipótese nula H0. Ou seja, µ1 = µ2 ao
                  nível de significância de ", alpha * 100, "%\n ")
    }
    text 
  })
  
  output$testeT2Graph3 <- renderPlot({
    D <- testeT2_params$D
    n <- testeT2_params$n
    t <- mean(D)/(sd(D)/sqrt(n))
    p <- dt(t, df=n-1)
    alpha <- input$testeT2Alpha
    accept <- p >= alpha
    
    lower_p <- qt(alpha/2, df=n-1)
    upper_p <- qt(1 - alpha/2, df=n-1)
    
    x1 <- seq(-6, lower_p, length = 200)
    x2 <- seq(upper_p, 6, length = 200)
    y1 <- dt(x1, df=n-1)
    y2 <- dt(x2, df=n-1)
    
    data1 <- data.frame(x1, y1)
    data2 <- data.frame(x2, y2)
    
    colnames(data1) <- c("x", "y")
    colnames(data2) <- c("x", "y")
    
    point <- data.frame(t, dt(t, df=n-1)/2)
    colnames(point) <- c("x", "y")
    g <- ggplot(data=data1, aes(x=x, y=y)) + 
      stat_function(fun=dt, n=101, args=list(df=n-1), color = "black") + 
      geom_area(data=data1, aes(x=x, y=y), fill="#0C7BDC", alpha=0.4) + 
      geom_area(data=data2, aes(x=x, y=y), fill="#0C7BDC", alpha=0.4) + 
      geom_point(data=point, aes(x=x, y=y), 
                 color = ifelse(accept, "#FFC20A", "#0C7BDC"), size = 8, shape = 8) + 
      scale_x_continuous(limits = c(-6, 6)) + 
      ylab("f(x)")+
      ggtitle(paste0("Distribuição T de Student com ", n-1, " graus de liberdade"))+
      theme_minimal()
    g
    
    
    
  })
  
  
  observeEvent(input$testeQuiRefresh, {
    testeQui_params$matrix <- rbind(c(input$testeQuiValor1, input$testeQuiValor2, input$testeQuiValor1 + input$testeQuiValor2), 
                                    c(input$testeQuiValor3, input$testeQuiValor4, input$testeQuiValor3 + input$testeQuiValor4), 
                                    c(input$testeQuiValor1 + input$testeQuiValor3, input$testeQuiValor2 + input$testeQuiValor4, 
                                      input$testeQuiValor1 + input$testeQuiValor2 + input$testeQuiValor3 + input$testeQuiValor4))
    testeQui_params$colnames <- c(input$testeQuiCol1, input$testeQuiCol2, "Total")
    testeQui_params$rownames <- c(input$testeQuiRow1, input$testeQuiRow2, "Total")
    p <- (input$testeQuiValor1 + input$testeQuiValor3)/
      (input$testeQuiValor1 + input$testeQuiValor2 + input$testeQuiValor3 + input$testeQuiValor4)
    testeQui_params$expected <- rbind(c(round(p*(input$testeQuiValor1 + input$testeQuiValor2)), 
                                        round((1-p)*(input$testeQuiValor1 + input$testeQuiValor2)),
                                        input$testeQuiValor1 + input$testeQuiValor2
    ),
    c(round(p*(input$testeQuiValor3 + input$testeQuiValor4)),
      round((1-p)*(input$testeQuiValor3 + input$testeQuiValor4)),
      input$testeQuiValor3 + input$testeQuiValor4
    ),
    c(input$testeQuiValor1 + input$testeQuiValor3, input$testeQuiValor2 + input$testeQuiValor4, 
      input$testeQuiValor1 + input$testeQuiValor2 + input$testeQuiValor3 + input$testeQuiValor4))
    testeQui_params$chi <- 
      ((input$testeQuiValor1 - round(p*(input$testeQuiValor1 + input$testeQuiValor2)))^2)/(round(p*(input$testeQuiValor1 + input$testeQuiValor2))) +
      ((input$testeQuiValor2 - round((1-p)*(input$testeQuiValor1 + input$testeQuiValor2)))^2)/(round((1-p)*(input$testeQuiValor1 + input$testeQuiValor2))) +
      ((input$testeQuiValor3 - round(p*(input$testeQuiValor3 + input$testeQuiValor4)))^2)/(round(p*(input$testeQuiValor3 + input$testeQuiValor4))) +
      ((input$testeQuiValor4 - round((1-p)*(input$testeQuiValor3 + input$testeQuiValor4)))^2)/(round((1-p)*(input$testeQuiValor3 + input$testeQuiValor4)))
  })
  
  output$testeQuiTabela <- renderTable({
    tab <- as.data.frame(testeQui_params$matrix)
    colnames(tab) <- testeQui_params$colnames
    rownames(tab) <- testeQui_params$rownames
    
    tab
  }, colnames = TRUE, rownames = TRUE, bordered = TRUE, striped = TRUE, width = "100%", digits = 0, align = "c")
  
  output$testeQuiEsperado <- renderTable({
    tab <- as.data.frame(testeQui_params$expected)
    colnames(tab) <- testeQui_params$colnames
    rownames(tab) <- testeQui_params$rownames
    
    tab
  }, colnames = TRUE, rownames = TRUE, bordered = TRUE, striped = TRUE, width = "100%", digits = 0, align = "c")
  
  output$testeQuiCalc <- renderUI({
    matrix <- testeQui_params$matrix
    exp <- testeQui_params$expected
    chi <- 0
    for (i in c(1, 2, 4, 5)) {
      chi <- chi + (matrix[i] - exp[i])^2/(exp[i])
    }
    withMathJax(helpText(paste0("$$\\chi^{2} = ", round(chi, 2), "$$")))
  })
  
  output$testeQuiPlot <- renderPlot({
    matrix <- testeQui_params$matrix
    exp <- testeQui_params$expected
    chi <- 0
    rc <- qchisq(1 - input$testeQuiAlpha, 1)
    for (i in c(1, 2, 4, 5)) {
      chi <- chi + (matrix[i] - exp[i])^2/(exp[i])
    }
    
    accept <- chi < rc
    
    x <- seq(rc, 8, length = 300)
    y <- dchisq(x, df=1)
    
    data <- data.frame(x, y)
    colnames(data) <- c("x", "y")
    
    point <- data.frame(chi, dchisq(chi, df=1)/2)
    colnames(point) <- c("x", "y")
    
    g <- ggplot(data, aes(x=x, y=y)) + 
      geom_area(stat="identity", fill = "#0c7bdc", alpha = 0.4) + 
      stat_function(fun=dchisq, n=101, args = list(df = 1), color="black") + 
      geom_point(data=point, aes(x=x, y=y), color = ifelse(accept, "#ffc20a", "#0c7bdc"), size = 8, shape = 8) + 
      scale_x_continuous(limits = c(0, max(7, 1.2*chi))) + 
      xlab("x") +
      ylab("f(x)") + 
      ggtitle("Distribuição Qui-quadrado com 1 grau de liberdade")+
      theme_minimal()
    
    g
  })
  
  output$testeQuiConta <- renderText({
    matrix <- testeQui_params$matrix
    exp <- testeQui_params$expected
    chi <- 0
    rc <- qchisq(1 - input$testeQuiAlpha, 1)
    for (i in c(1, 2, 4, 5)) {
      chi <- chi + (matrix[i] - exp[i])^2/(exp[i])
    }
    
    
    
    accept <- chi < rc
    text <- paste0("H0: As variáveis são independentes\nH1: As variáveis não são independentes\n
    Estatística de teste observada = ", round(chi, 4), "\nRegião Crítica RC = [", round(rc, 4), ", +∞[\n")
    if (accept) {
      text <- paste0(text, "Como o valor-qui está fora da região crítica, aceita-se H0 e portanto 
                     não há influência se ", input$testeQuiRow1, " ou ", input$testeQuiRow2, ".")
    } else {
      text <- paste0(text, "Como o valor-qui está dentro da região crítica, H0 é rejeitada e portanto H1 é aceita, ou seja,
                     há influência quando ", input$testeQuiRow1, " ou ", input$testeQuiRow2, ".")
    }
    
    text
  })
  
  
  #### código gabriel qui quadrado
  output$testando_qui_q <- renderUI({
    tags <- NULL
    if (sum(dataqui$numbers) >= 2) {
      tags <- tagList(
        selectInput("teste_qui_var1", "Variável 1", 
                    choices = colnames(dataqui$data[dataqui$shortLevels])),
        
        selectInput("teste_qui_var2", "Variável 2", 
                    choices = colnames(dataqui$data[dataqui$shortLevels])),
        actionButton("testeQuiRefresh", "Atualizar", icon = icon("refresh"))
      )
    } else {
      tags <- tagList(p("Os dados que você está utilizando não possuem uma quantidade suficiente de variáveis 
      quantitativas (duas). Por favor selecione outro conjunto de dados ou utilize nosso padrão."))
    }
    
    tags
  })
  
  observeEvent(input$testeQuiRefresh, {
    req(input$teste_qui_var1, input$teste_qui_var2)
    
    # Construir a tabela de contingência
    tabela <- table(dataqui$data[[input$teste_qui_var1]], dataqui$data[[input$teste_qui_var2]])
    
    # Calcular as frequências esperadas
    expected <- outer(rowSums(tabela), colSums(tabela)) / sum(tabela)
    
    # Verificar se alguma das frequências esperadas é menor que 5
    mcmc_aplicado <- any(expected < 5)
    if (mcmc_aplicado) {
      # Calcular o teste qui-quadrado com simulação de Monte Carlo
      resultado_qui <- chisq.test(tabela, simulate.p.value = TRUE, B = 10000)
    } else {
      # Calcular o teste qui-quadrado normalmente
      resultado_qui <- chisq.test(tabela)
    }
    
    chi_squared <- resultado_qui$statistic
    df <- (nrow(tabela) - 1) * (ncol(tabela) - 1)  # Calcular os graus de liberdade corretamente
    
    # Calcular a região crítica
    alpha <- input$testeQuiAlpha
    rc <- qchisq(1 - alpha, df)
    
    accept <- chi_squared < rc
    
    # Garantir que chi_squared e rc sejam finitos
    if (!is.finite(chi_squared) || !is.finite(rc)) {
      chi_squared <- 0
      rc <- 0
    }
    
    # Preparar os dados para o gráfico
    x <- seq(0, max(chi_squared * 1.2, rc * 1.2, na.rm = TRUE), length.out = 300)
    y <- dchisq(x, df = df)
    
    data <- data.frame(x, y)
    
    point <- data.frame(x = chi_squared, y = dchisq(chi_squared, df = df))
    
    # Criar a região crítica para sombrear
    critical_region <- data.frame(
      x = c(rc, seq(rc, max(x), length.out = 100)),
      y = c(0, dchisq(seq(rc, max(x), length.out = 100), df = df))
    )
    
    # Renderizar o gráfico
    output$grafico_qui_q <- renderPlot({
      ggplot(data, aes(x = x, y = y)) +
        geom_area(data = critical_region, aes(x = x, y = y), fill = "#0c7bdc", alpha = 0.4) +
        stat_function(fun = dchisq, args = list(df = df), color = "black") +
        geom_point(data = point, aes(x = x, y = y), color = ifelse(accept, "#ffc20a", "#0c7bdc"), size = 8, shape = 8) +
        scale_x_continuous(limits = c(0, max(x))) +
        xlab("x") +
        ylab("f(x)") +
        ggtitle(paste("Distribuição Qui-quadrado com", df, "grau(s) de liberdade")) +
        theme_minimal()
    })
    
    # Calcular e renderizar a tabela de contingência e frequências esperadas
    row_totals <- rowSums(tabela)
    col_totals <- colSums(tabela)
    grand_total <- sum(tabela)
    tabela_completa <- cbind(tabela, row_totals)
    tabela_completa <- rbind(tabela_completa, c(col_totals, grand_total))
    
    colnames(tabela_completa)[ncol(tabela_completa)] <- "Total"
    rownames(tabela_completa)[nrow(tabela_completa)] <- "Total"
    
    output$tabela_qui <- renderTable({
      tabela_completa
    }, colnames = TRUE, rownames = TRUE, bordered = TRUE, striped = TRUE, width = "100%", digits = 0, align = "c")
    
    expected_completa <- cbind(expected, rowSums(expected))
    expected_completa <- rbind(expected_completa, c(colSums(expected), sum(expected)))
    
    colnames(expected_completa)[ncol(expected_completa)] <- "Total"
    rownames(expected_completa)[nrow(expected_completa)] <- "Total"
    
    output$tabela_qui_esperada <- renderTable({
      expected_completa
    }, colnames = TRUE, rownames = TRUE, bordered = TRUE, striped = TRUE, width = "100%", digits = 0, align = "c")
    
    # Renderizar o texto explicativo
    output$teste_qui_q_conta <- renderText({
      tabela <- table(dataqui$data[[input$teste_qui_var1]], dataqui$data[[input$teste_qui_var2]])
      
      # Calcular as frequências esperadas
      expected <- outer(rowSums(tabela), colSums(tabela)) / sum(tabela)
      
      mcmc_aplicado <- any(expected < 5)
      if (mcmc_aplicado) {
        resultado_qui <- chisq.test(tabela, simulate.p.value = TRUE, B = 10000)
      } else {
        resultado_qui <- chisq.test(tabela)
      }
      
      chi_squared <- resultado_qui$statistic
      p_value <- resultado_qui$p.value
      rc <- qchisq(1 - input$testeQuiAlpha, (nrow(tabela) - 1) * (ncol(tabela) - 1))
      
      accept <- chi_squared < rc
      text <- paste0("H0: As variáveis são independentes\nH1: As variáveis não são independentes\n",
                     "Estatística do teste observada = ", round(chi_squared, 4), "\nValor-p = ", format(p_value, digits = 4), 
                     "\nRegião Crítica RC = [", round(rc, 4), ", +∞[\n")
      if (accept) {
        text <- paste0(text, "Como a estatística do teste não pertence à região crítica, não se rejeita H0 e portanto 
                   as variáveis ", input$teste_qui_var1, " e ", input$teste_qui_var2, " são independentes")
      } else {
        text <- paste0(text, "Como a estatística do teste pertence à região crítica, rejeita-se H0, ou seja,
                   as variáveis ", input$teste_qui_var1, " e ", input$teste_qui_var2, " não são independentes")
      }
      
      if (mcmc_aplicado) {
        text <- paste0(text, "\nNota: A correção por Monte Carlo foi aplicada devido a frequências esperadas menores que 5.")
      }
      
      text
    })
  })
  
  
  output$testeCorrExplicacao <- renderUI({
    tags <- NULL
    req(input$tipoTesteCorr)
    if (input$tipoTesteCorr == "spearman") {
      
    } else {
      tags <- tagList(
        withMathJax(helpText("Coeficiente ρ = 
                                                              $$\\frac{1}{n}\\sum_{i=1}^{n}
                                                              (\\frac{x_{i} - \\bar{x}}{dp(X)})
                                                              (\\frac{y_{i} - \\bar{y}}{dp(Y)})$$")),
        p("ρ = -1: Correlação linear perfeita negativa"),
        p("ρ = 0: Não há nenhuma relação linear"),
        p("ρ = 1: Correlação linear perfeita positiva")
      )
    }
    tags
  })
  
  getCorrPlotData <- reactive({
    req(input$testeCorrVar1)
    req(input$testeCorrVar2)
    req(input$testeCorrVar1 %in% colnames(dataqui$data))
    req(input$testeCorrVar2 %in% colnames(dataqui$data))
    return (as.data.frame(cbind(dataqui$data[,colnames(dataqui$data) == input$testeCorrVar1], 
                                dataqui$data[,colnames(dataqui$data) == input$testeCorrVar2])))
  })
  
  output$selectTesteCorrVarUI <- renderUI({
    tags <- NULL
    if (sum(dataqui$numbers) >= 2) {
      tags <- tagList(
        selectInput("testeCorrVar1", "Variável 1", 
                    choices = colnames(dataqui$data)[dataqui$numbers], 
                    selected = colnames(dataqui$data)[dataqui$numbers][1]),
        selectInput("testeCorrVar2", "Variável 2", 
                    choices = colnames(dataqui$data)[dataqui$numbers],
                    selected = colnames(dataqui$data)[dataqui$numbers][2])
      )
    } else {
      tags <- tagList(p("Os dados que você está utilizando não
      possuem uma quantidade suficiente de variáveis 
        quantitativas (duas). Por favor selecione outro conjunto 
                        de dados ou utilize nosso padrão."))
    }
    
    tags
  })
  
  output$testeNormCorr <- renderText({
    req(input$testeCorrVar1, input$testeCorrVar2)
    
    var1_data <- dataqui$data[[input$testeCorrVar1]]
    var2_data <- dataqui$data[[input$testeCorrVar2]]
    
    shapiro_var1 <- shapiro.test(var1_data)
    shapiro_var2 <- shapiro.test(var2_data)
    
    paste(
      "Teste de Shapiro-Wilk para a variável 1 (",input$testeCorrVar1,"):",
      "\nW = ", round(shapiro_var1$statistic, 4),
      "\np-valor = ", format(shapiro_var1$p.value, scientific = TRUE),
      "\n\nTeste de Shapiro-Wilk para a variável 2 (",input$testeCorrVar2,"):",
      "\nW = ", round(shapiro_var2$statistic, 4),
      "\np-valor = ", format(shapiro_var2$p.value, scientific = TRUE)
    )
  })
  
  output$testeNormCorrExplicacao <- renderUI({
    wellPanel(
      "O teste de Shapiro-Wilk verifica se os dados seguem ou não
      uma distribuição normal. ",
      br(),
      strong("H0:"),
      "Os dados seguem distribuição normal.",
      br(),
      strong("H1:"),
      "Os dados não seguem distribuição normal.",
      br(),
      'Se p-valor ≤ α, rejeita-se H0',
      br(),
      'Se p-valor > α, não se rejeita H0'
    )
  })
  
  
  
  
  output$testeCorrPlot <- renderPlot({
    selData <- getCorrPlotData()
    colnames(selData) <- c("Var1", "Var2")
    g <- ggplot(selData, aes(x=Var1, y=Var2)) + 
      geom_point(color=dark_cyan, size = 2) + 
      #geom_smooth(method=lm) + 
      xlab(input$testeCorrVar1) + 
      ylab(input$testeCorrVar2) + 
      theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12))+
      theme_minimal()
    
    g
  })
  
  output$testeCorrConta <- renderText({
    selData <- getCorrPlotData()
    colnames(selData) <- c("Var1", "Var2")
    cor_test <- cor.test(selData$Var1, selData$Var2,
                         method = input$tipoTesteCorr)
    correlacao <- cor_test$estimate
    p_value <- cor_test$p.value
    text <- paste0("Correlação(", input$testeCorrVar1, ", ",
                   input$testeCorrVar2, ") = ", round(correlacao, 4), 
                   "\np-valor: ", format(p_value, scientific = TRUE))
    text
  })
  
  
  # linkTexts ----
  
  observeEvent(input$linkTiposVariaveis, { 
                updateNavbarPage(session, "mainNav", selected = "tipos_variaveis")
              })
  observeEvent(input$linkDistrFreq, { 
                updateNavbarPage(session, "mainNav", selected = "tabTabFreqs")
              })
  observeEvent(input$linkMedidasResumo, { 
                updateNavbarPage(session, "mainNav", selected = "tabMedidasResumo")
              })
  observeEvent(input$linkGrafQual, { 
                updateNavbarPage(session, "mainNav", selected = "tabGrafQual")
              })
  observeEvent(input$linkGrafQuant, { 
                updateNavbarPage(session, "mainNav", selected = "tabGrafQuant")
              })
  observeEvent(input$linkGrafBi, { 
                updateNavbarPage(session, "mainNav", selected = "tabGrafBi")
              })
              
  observeEvent(input$linkTesteT1, { 
                updateNavbarPage(session, "mainNav", selected = "tabTesteT1")
              })
  observeEvent(input$linkTesteT2, { 
                updateNavbarPage(session, "mainNav", selected = "tabTesteT2")
              })
  observeEvent(input$linkTesteQui, { 
                updateNavbarPage(session, "mainNav", selected = "tabTesteQui")
              })
  observeEvent(input$linkTesteCorr, { 
                updateNavbarPage(session, "mainNav", selected = "tabTesteCorr")
              })
  
  
  ## Exercícios ----
  
  exerc_teoricos_server("exerc_teoricos")
  
  ### Paralisia ----
  
  observeEvent(input$tabs, {
    if (input$tabs == 'ex1') {
      numero_do_exercicio(1)  
    } else if (input$tabs == 'ex2') {
      numero_do_exercicio(2)  
    } else if (input$tabs == 'ex3') {
      numero_do_exercicio(3)  
    } else if (input$tabs == 'ex4') {
      numero_do_exercicio(4)  
    } else if (input$tabs == 'ex5') {
      numero_do_exercicio(5)  
    } else if (input$tabs == 'ex6') {
      numero_do_exercicio(6)  
    } else if (input$tabs == 'ex7') {
      numero_do_exercicio(7)  
    } else if (input$tabs == 'ex8') {
      numero_do_exercicio(8)  
    } else if (input$tabs == 'ex9') {
      numero_do_exercicio(9)  
    } else if (input$tabs == 'ex10') {
      numero_do_exercicio(10)  
    }
  })
  
  
  ####01UI para exercício 1 ----
  output$ex1 <- renderUI({
    
    enunciado <- questoes_paralisia[[1]]
    
    fluidRow(
      h3(''),
      column(10,
             wellPanel(
               p(HTML(enunciado))
             ),
             column(8,
                    pickerInput('variavel_quali', 'Quais variáveis são qualitativas?',
                                choices = nomes_exibidos, 
                                multiple = TRUE,
                                selected = 'Nada selecionado'),
                    pickerInput('variavel_quanti', 'Quais variáveis são quantitativas?',
                                choices = nomes_exibidos, 
                                multiple = TRUE,
                                selected = 'Nada selecionado'),
                    actionButton('verif_resp_ex1', 'Verificar'),
                    align = 'center'
             )
      )
      
    )
  })
  
  ####02UI para exercício 2 ----
  output$ex2 <- renderUI({
    
    enunciado <- questoes_paralisia[[2]]
    
    fluidRow(
      h3(''),
      column(10,
             wellPanel(
               p(HTML(enunciado))
             ),
             column(8,
                    pickerInput('variavel_ex2', 'Medidas-resumo',
                                choices = c('Média', 'Mediana', 
                                            'Porcentagem', 'Frequência absoluta',
                                            'Desvio-padrão', 'Frequência relativa', 'Quartis'), 
                                multiple = TRUE,
                                options = list(noneSelectedText = 'Nada selecionado')),
                    actionButton('verif_resp_ex2', 'Verificar'),
                    align = 'center'
             )
      )
    )
  })
  
  ####03UI para exercício 3 ----
  output$ex3 <- renderUI({
    
    enunciado <- questoes_paralisia[[3]]
    
    fluidRow(
      h3(''),
      column(10,
             wellPanel(
               p(HTML(enunciado))
             ),
             column(8,
                    pickerInput('variavel_ex3', 'Medidas-resumo',
                                choices = c('Média', 'Mediana', 'Porcentagem',
                                            'Frequência absoluta', 'Desvio-padrão',
                                            'Frequência relativa'),
                                multiple = TRUE,
                                options = list(noneSelectedText = 'Nada selecionado')),
                    actionButton('verif_resp_ex3', 'Verificar'),
                    align = 'center')
             
      )
    )
  })
  
  ####04UI para exercício 4 ----
  output$ex4 <- renderUI({
    
    enunciado <- questoes_paralisia[[4]]
    
      fluidRow(
      h3(''),
      column(10,
             wellPanel(
               p(HTML(enunciado))
             ),
             column(8,offset = 1,
                    pickerInput('variavel_ex4x', 'Eixo x',
                                choices = c('Não se aplica', nomes_exibidos),
                                options = list(noneSelectedText = 'Nada selecionado')),
                    pickerInput('variavel_ex4y', 'Eixo y',
                                choices = c('Não se aplica', nomes_exibidos),
                                options = list(noneSelectedText = 'Nada selecionado')),
                    pickerInput('variavel_ex4_graf', 'Tipo de gráfico',
                                choices = c('Barras', 'Boxplot', 'Dispersão'),
                                options = list(noneSelectedText = 'Nada selecionado')),
                    actionButton('graf_ex4', 'Gerar gráfico'),
                    align = 'center'
             ),
             br(),
             column(8, offset = 1,
                    br(),
             plotOutput('plot_ex4'),
             align = 'center'
             )
             )
    )
    
  })
  
  ####05UI para exercício 5 ----
  output$ex5 <- renderUI({
    
    enunciado <- questoes_paralisia[[5]]
    
    fluidRow(
      h3(''),
      column(10,
             wellPanel(
               p(HTML(enunciado))
             ),
             column(8, offset = 1,
                    pickerInput('variavel_ex5x', 'Eixo x',
                                choices = c('Não se aplica', nomes_exibidos),
                                options = list(noneSelectedText = 'Nada selecionado')),
                    pickerInput('variavel_ex5y', 'Eixo y',
                                choices = c('Não se aplica', nomes_exibidos),
                                options = list(noneSelectedText = 'Nada selecionado')),
                    pickerInput('variavel_ex5_graf', 'Tipo de gráfico',
                                choices = c('Barras', 'Boxplot', 'Dispersão'),
                                options = list(noneSelectedText = 'Nada selecionado')),
                    actionButton('graf_ex5', 'Gerar gráfico'),
                    align = 'center'
             ),
             column(8, offset = 1,
                    br(),
             plotOutput('plot_ex5'),
             align = 'center'
             )
             )
      
    )
  })
  
  ####06UI para exercício 6 ----
  output$ex6 <- renderUI({
    
    enunciado <- questoes_paralisia[[6]]
    
    fluidRow(
      h3(''),
      column(10,
             wellPanel(
               p(HTML(enunciado))
             ),
             column(8,offset = 1,
                    pickerInput('variavel_ex6x', 'Eixo x',
                                choices = c('Não se aplica', nomes_exibidos),
                                options = list(noneSelectedText = 'Nada selecionado')),
                    pickerInput('variavel_ex6y', 'Eixo y',
                                choices = c('Não se aplica', nomes_exibidos),
                                options = list(noneSelectedText = 'Nada selecionado')),
                    pickerInput('variavel_ex6_graf', 'Tipo de gráfico',
                                choices = c('Barras', 'Boxplot', 'Dispersão'),
                                options = list(noneSelectedText = 'Nada selecionado')),
                    actionButton('graf_ex6', 'Gerar gráfico'),
                    align = 'center'
                    ),
             column(8, offset = 1,
                    plotOutput('plot_ex6'),
                    align = 'center')
             )
      
      
    )
  })
  
  ####07UI para exercício 7 ----
  output$ex7 <- renderUI({
    
    enunciado <- questoes_paralisia[[7]]
    
    fluidRow(
      h3(''),
      column(10,
             wellPanel(
               p(HTML(enunciado)),
               column(8, 
                      br(),
                      
                      gt_output(outputId = 'tabela_pc'),
                      gt_output(outputId = 'tabela_esperada_ex7'),
                      align = 'center'
               )
             )
             
      ),
      
      column(10,
             wellPanel(
               p(HTML('<b>a)</b> Com um nível de significância de 5%, escolha 
             o teste estatístico mais apropriado para verificar se há 
             associação estatisticamente significante 
             entre essas variáveis?<br>')
               ),
               
               column(8, 
                      br(),
                      br(),
                      pickerInput('teste_ex7', 'Escolha um teste:',
                                  choices = c('Qui-Quadrado', 'Qui-Quadrado via simulação de Monte Carlo'),
                                  options = list(noneSelectedText = 'Nada selecionado')),
                      actionButton('botao_teste_ex7', 'Verificar'),
                      br(),
                      br(),
                      verbatimTextOutput('resultado_teste_ex7'),
                      align = 'center'
               ),
             )
      ),
      column(10,
             wellPanel(
               p(HTML('<b>b)</b> De acordo com o resultado do teste,
             é correto afirmar que as variáveis estão associadas ao nível 
                    de significância de 5%?<br?')),
               
               column(8,
                      br(),
                      br(),
                      uiOutput('ex7_parteb'),
                      align = 'center'
               )
             )
      )
    )
  })
  
  ####08UI para exercício 8 ----
  output$ex8 <- renderUI({
    enunciado <- questoes_paralisia[[8]]
    
    fluidRow(
      h3(''),
      
      column(10,
             wellPanel(
               p(HTML(enunciado)),
               column(8,
                      br(),
                      gt_output(outputId = 'tabela_ex8_pc'),
                      gt_output(outputId = 'tabela_esperada_ex8'),
                      align = 'center'
               )
             )
      ),
      column(10,
             wellPanel(
               p(HTML('<b>a)</b> Com um nível de significância de 5%,
                      qual é o teste estatístico 
                      mais apropriado para verificar se há associação 
                      estatisticamente significante entre 
                      essas variáveis?')),
               column(8,
                      br(),
                      br(),
                      pickerInput('teste_ex8', 'Escolha o teste:',
                                  choices = c('Qui-Quadrado', 
                                              'Qui-Quadrado via simulação de Monte Carlo'),
                                  options = list(noneSelectedText =
                                                   'Nada selecionado')),
                      actionButton('botao_teste_ex8', 'Verificar'),
                      br(),
                      br(),
                      verbatimTextOutput('resultado_teste_ex8'),
                      align = 'center'
               )
             ),
      ),
      column(10,
             wellPanel(
               p(HTML('<b>b)</b> De acordo com o resultado do teste, 
               é correto afirmar que as variáveis estão 
                      associadas ao nível de significância de 5%?')),
               column(8,
                      br(),
                      br(),
                      uiOutput('ex8_parteb'),
                      align = 'center'
               )
             )
      )
    )
  })
  
  ####09UI para exercício 9 ----
  "output$ex9 <- renderUI({
    
    enunciado <- questoes_paralisia[[9]]
    
    fluidRow(
      h3(''),
      column(10,
             wellPanel(
               p(HTML(enunciado)),
               column(8,
                      pickerInput('variavel_ex9x', 'Eixo x',
                                  choices = c('Não se aplica', nomes_exibidos),
                                  options = list(noneSelectedText = 'Nada selecionado')),
                      pickerInput('variavel_ex9y', 'Eixo y',
                                  choices = c('Não se aplica', nomes_exibidos),
                                  options = list(noneSelectedText = 'Nada selecionado')),
                      pickerInput('variavel_ex9_graf', 'Tipo de gráfico',
                                  choices = c('Barras', 'Boxplot', 'Dispersão'),
                                  options = list(noneSelectedText = 'Nada selecionado')),
                      actionButton('graf_ex9', 'Gerar gráfico'),
                      plotOutput('plot_ex9'),
                      align = 'center'
               )
             )
      ),
      column(10,
             wellPanel(
               h5('<b>Testes de normalidade da variável quantitativa pelos níveis
             da variável qualitativa:</b>'),
               column(8,
                      verbatimTextOutput('resultado_teste_norm_ex9'),
                      align = 'center'
               )
             )
      ),
      column(10,
             wellPanel(
               p(HTML('<b>a)</b> A partir dos resultados dos testes de normalidade
             exibidos, qual é o teste estatístico mais apropriado
      para verificar se há associação estatisticamente significante entre essas 
      variáveis a um nível de 5%?')),
               column(8,
                      uiOutput('ex9_parteb'),
                      uiOutput('resultado_teste_ex9'),
                      align = 'center'
               )
             )
      ),
      column(10,
             wellPanel(
               p(HTML('<b>b)</b> De acordo com o resultado do teste,
           é correto afirmar que as variáveis estão 
                  associadas ao nível de significância de 5%?')
               ),
               column(8,
                      uiOutput('ex9_partec'),
                      align = 'center'
               )
             )
      )
    )
    
  })"
  
  ####10UI para exercício 10 ----
  
  output$ex10 <- renderUI({
    
    enunciado <- questoes_paralisia[[10]]
    
    fluidRow(
      h3(''),
      column(10,
             wellPanel(
               p(HTML(enunciado)
               ),
               column(8,
                      br(),
                      br(),
                      pickerInput('variavel_ex10x', 'Eixo x',
                                  choices = c('Não se aplica', nomes_exibidos),
                                  options = list(noneSelectedText = 'Nada selecionado')),
                      pickerInput('variavel_ex10y', 'Eixo y',
                                  choices = c('Não se aplica', nomes_exibidos),
                                  options = list(noneSelectedText = 'Nada selecionado')),
                      pickerInput('variavel_ex10_graf', 'Tipo de gráfico',
                                  choices = c('Barras', 'Boxplot', 'Dispersão'),
                                  options = list(noneSelectedText = 'Nada selecionado')),
                      actionButton('graf_ex10', 'Gerar gráfico'),
                      br(),
                      br(),
                      plotOutput('plot_ex10'),
                      br(),
                      align = 'center')
             )
      ),
      br(),
      br(),
      column(10,
             wellPanel(  
               
               h5(HTML('<b>Testes de normalidade da variável quantitativa pelos níveis
             da variável qualitativa: </b>')),
               column(8,
                      br(),
                      br(),
                      verbatimTextOutput('resultado_teste_norm_ex10'),
                      align = 'center'
               )
             )
      ),
      column(10,
             wellPanel(  
               p(HTML('<b>a)</b> A partir dos resultados dos testes de normalidade
             exibidos, qual é o teste estatístico mais apropriado 
             para verificar se há relação estatisticamente significante
             entre elas a um nível de 5%?')),
               
               column(8, 
                      uiOutput('ex10_parteb'),
                      br(),
                      verbatimTextOutput('resultado_teste_ex10'),
                      align = 'center')
             )
      ),
      column(10,
             wellPanel(  
               p(HTML('<b>b)</b> De acordo com o resultado do teste, 
                  é correto afirmar que as variáveis estão
                         relacionadas ao nível de significância de 5%?')
               ),
               column(8,
                      uiOutput('ex10_partec'),
                      align = 'center'
               )
             )
      )
    )
    
  })
  
  #Validação das respostas
  
  
  #Ex1
  observeEvent(input$verif_resp_ex1, {
    
    mensagem <- reactive({
      if (is.null(input$variavel_quali) && is.null(input$variavel_quanti)) {
        return("Você não selecionou as respostas!")} else {
          if (identical(input$variavel_quali, c("sexo", "grupo", "perda_audit", "dist_comun", "dmo")) & 
              identical(input$variavel_quanti, c("idade", "td_liquido", "td_pastoso", "td_solido"))) {
            return("Resposta correta.")} else {
              return("Há algo errado com as suas seleções.")}}
    })
    
    shinyalert(
      title = "",
      text = mensagem(),
      type = ifelse(mensagem() == "Resposta correta.", "success", "warning")
    )},
    ignoreNULL = T)
  
  #Ex2
  observeEvent(input$verif_resp_ex2, {
    
    mensagem <- reactive({
      if (is.null(input$variavel_ex2)) {
        return("Você não selecionou as respostas!")} else {
          if (identical(input$variavel_ex2, c('Média', 'Mediana', 'Desvio-padrão', 'Quartis'))) {
            return("Resposta correta.")} else {
              return("Há algo errado com sua seleção.")}}
    })
    
    shinyalert(
      title = "",
      text = mensagem(),
      type = ifelse(mensagem() == "Resposta correta.", "success", "warning")
    )},
    ignoreNULL = T)
  
  #Ex3
  observeEvent(input$verif_resp_ex3, {
    
    mensagem <- reactive({
      if (is.null(input$variavel_ex3)) {
        return("Você não selecionou as respostas!")} else {
          if (identical(input$variavel_ex3, c('Porcentagem'))) {
            return("Resposta correta.")} else {
              return("Há algo errado com sua seleção.")}}
    })
    
    shinyalert(
      title = "",
      text = mensagem(),
      type = ifelse(mensagem() == "Resposta correta.", "success", "warning")
    )},
    ignoreNULL = T)
  
  #Ex4
  observeEvent(input$graf_ex4, {
    output$plot_ex4 <- renderPlot({
      req(input$variavel_ex4x != input$variavel_ex4y,
          input$variavel_ex4x == 'dmo' || input$variavel_ex4x == 'Não se aplica',
          input$variavel_ex4y == 'dmo' || input$variavel_ex4y == 'Não se aplica',
          input$variavel_ex4_graf == 'Barras')
      
      x_label <- if(input$variavel_ex4x == 'dmo'){
        'Disfunção Motora Oral'
      } else{
        'Frequência absoluta'
      }
      
      y_label <- if(input$variavel_ex4y == 'dmo'){
        'Disfunção Motora Oral'
      } else{
        'Frequência absoluta'
      
      }
      if(input$variavel_ex4x == 'dmo'){
        plot <- ggplot(dados_paralisia, aes_string(x = input$variavel_ex4x, fill = 'dmo'))+
          geom_bar(stat = 'count')+
          theme_minimal()+
          labs(x = x_label, y = y_label,fill = 'DMO')+
          scale_fill_manual(values = c(colorful[1], colorful[2], colorful[3], colorful[4], colorful[5]))
        return(plot)
      }
      if (input$variavel_ex4y == 'dmo'){
        plot <- ggplot(dados_paralisia, aes_string(y = input$variavel_ex4y, fill = 'dmo'))+
          geom_bar(stat = 'count')+
          theme_minimal()+
          labs(x = x_label, y = y_label,fill = 'DMO')+
          scale_fill_manual(values = c(colorful[1], colorful[2], colorful[3], colorful[4], colorful[5]))
        return(plot)
      }
      
    })
    
    
    mensagem <- reactive({
      if (is.null(input$variavel_ex4x) && is.null(input$variavel_ex4y) && is.null(input$variavel_ex4_graf)) {
        return("Você não selecionou as respostas!")}
      else {
        if (identical(input$variavel_ex4x, 'dmo') && identical(input$variavel_ex4y, 'Não se aplica') && identical(input$variavel_ex4_graf, 'Barras')) {
          return("Resposta correta.")}
        else if (identical(input$variavel_ex4x, 'Não se aplica') && identical(input$variavel_ex4y, 'dmo') && identical(input$variavel_ex4_graf, 'Barras')) {
          return("Resposta correta.")}
        else {
          return("Há algo errado com sua seleção.")}}
    })
    
    shinyalert(
      title = "",
      text = mensagem(),
      type = ifelse(mensagem() == "Resposta correta.", "success", "warning")
    )},
    ignoreNULL = T)
  
  #Ex5
  observeEvent(input$graf_ex5, {
    output$plot_ex5 <- renderPlot({
      req(input$variavel_ex5x != input$variavel_ex5y,
          input$variavel_ex5x == 'dist_comun' || input$variavel_ex5x == 'Não se aplica',
          input$variavel_ex5y == 'dist_comun' || input$variavel_ex5y == 'Não se aplica',
          input$variavel_ex5_graf == 'Barras')
      
      x_label <- if(input$variavel_ex5x == 'dist_comun'){
        'Distúrbio de Comunicação'
      } else{
        'Frequência absoluta'
      }
      
      y_label <- if(input$variavel_ex5y == 'dmo'){
        'Distúrbio de Comunicação'
      } else{
        'Frequência absoluta'
        
      }
      if(input$variavel_ex5x == 'dist_comun'){
        plot <- ggplot(dados_paralisia, aes_string(x = input$variavel_ex5x, fill = 'dist_comun'))+
          geom_bar(stat = 'count')+
          theme_minimal()+
          labs(x = x_label, y = y_label,fill = 'Distúrbio de Comunicação')+
          scale_fill_manual(values = c(colorful[1], colorful[2], colorful[3], colorful[4], colorful[5]))
        return(plot)
      }
      if (input$variavel_ex5y == 'dist_comun'){
        plot <- ggplot(dados_paralisia, aes_string(y = input$variavel_ex5y, fill = 'dist_comun'))+
          geom_bar(stat = 'count')+
          theme_minimal()+
          labs(x = x_label, y = y_label,fill = 'Distúrbio de Comunicação')+
          scale_fill_manual(values = c(colorful[1], colorful[2], colorful[3], colorful[4], colorful[5]))
        return(plot)
      }
      
    })
      
      
      
    
    
    
    mensagem <- reactive({
      if (is.null(input$variavel_ex5x) && is.null(input$variavel_ex5y) && is.null(input$variavel_ex5_graf)) {
        return("Você não selecionou as respostas!")}
      else {
        if (identical(input$variavel_ex5x, 'dist_comun') && identical(input$variavel_ex5y, 'Não se aplica') && identical(input$variavel_ex5_graf, 'Barras')) {
          return("Resposta correta.")}
        else if (identical(input$variavel_ex5x, 'Não se aplica') && identical(input$variavel_ex5y, 'dist_comun') && identical(input$variavel_ex5_graf, 'Barras')) {
          return("Resposta correta.")}
        else {
          return("Há algo errado com sua seleção.")}}
    })
    
    shinyalert(
      title = "",
      text = mensagem(),
      type = ifelse(mensagem() == "Resposta correta.", "success", "warning")
    )},
    ignoreNULL = T)
  
  #Ex6
  observeEvent(input$graf_ex6, {
    output$plot_ex6 <- renderPlot({
      req(input$variavel_ex6x != input$variavel_ex6y,
          input$variavel_ex6x == 'td_liquido' || input$variavel_ex6x == 'Não se aplica',
          input$variavel_ex6y == 'td_liquido' || input$variavel_ex6y == 'Não se aplica',
          input$variavel_ex6_graf == 'Boxplot')
      
      x_label <- if(input$variavel_ex6x == 'td_liquido'){
        'Tempo líquido'
      } else{
        ''
      }
      
      y_label <- if(input$variavel_ex6y == 'td_liquido'){
        'Tempo líquido'
      } else{
        ''
        
      }
      if(input$variavel_ex6x == 'td_liquido'){
        plot <- ggplot(dados_paralisia, aes_string(x = input$variavel_ex6x, fill = as.factor(input$variavel_ex6x)))+
          geom_boxplot()+
          theme_minimal()+
          scale_fill_manual(values = c(colorful[1]), labels = 'Tempo')+
          ylab('Tempo Líquido')+
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())+
          labs(x = x_label, y = y_label, fill = 'Tempo Líquido')
        return(plot)
      }
      if (input$variavel_ex6y == 'td_liquido'){
        plot <- ggplot(dados_paralisia, aes_string(y = input$variavel_ex6y, fill = as.factor(input$variavel_ex6y)))+
          geom_boxplot()+
          theme_minimal()+
          scale_fill_manual(values = c(colorful[1]), labels = 'Tempo')+
          ylab('Tempo Líquido')+
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())+
          labs(x = x_label, y = y_label, fill = 'Tempo Líquido')
        return(plot)
      }
      
    })
      
      
    
    
    mensagem <- reactive({
      if (is.null(input$variavel_ex6x) && is.null(input$variavel_ex6y) && is.null(input$variavel_ex6_graf)) {
        return("Você não selecionou as respostas!")}
      else {
        if (identical(input$variavel_ex6x, 'Não se aplica') && identical(input$variavel_ex6y, 'td_liquido') && identical(input$variavel_ex6_graf, 'Boxplot')) {
          return("Resposta correta.")}
        else if (identical(input$variavel_ex6x, 'td_liquido') && identical(input$variavel_ex6y, 'Não se aplica') && identical(input$variavel_ex6_graf, 'Boxplot')) {
          return("Resposta correta.")}
        else {
          return("Há algo errado com sua seleção.")}}
    })
    
    shinyalert(
      title = "",
      text = mensagem(),
      type = ifelse(mensagem() == "Resposta correta.", "success", "warning")
    )},
    ignoreNULL = T)
  
  #Ex7
  output$tabela_pc <- render_gt({
    contagem_tabela <- dados_paralisia %>%
      count(perda_audit, grupo) %>%
      pivot_wider(names_from = grupo, values_from = n, values_fill = list(n = 0)) %>%
      rename(
        `Perda Auditiva` = perda_audit,
        `PC` = PC,
        `SAN` = SAN
      )
    
    # Calcular as porcentagens
    contagem_tabela <- contagem_tabela %>%
      mutate(
        Total_PC = sum(PC),
        Total_SAN = sum(SAN),
        `PC (%)` = paste(PC, "(", round((PC / Total_PC) * 100, 2), "%)", sep = ""),
        `SAN (%)` = paste(SAN, "(", round((SAN / Total_SAN) * 100, 2), "%)", sep = "")
      ) %>%
      select(`Perda Auditiva`, `PC (%)`, `SAN (%)`)
    
    # Criar a tabela gt
    gt(contagem_tabela) %>%
      tab_header(
        title = md("**Tabela Observada**")
      ) %>%
      cols_label(
        `Perda Auditiva` = "Perda Auditiva",
        `PC (%)` = "PC",
        `SAN (%)` = "SAN"
      ) %>%
      cols_align(
        align = "center",
        columns = everything()
      ) %>%
      fmt_markdown(
        columns = everything()
      ) %>%
      tab_options(
        table.width = pct(100),
        heading.align = "center"
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels(everything())
      ) %>%
      tab_footnote(
        footnote = "n (%): frequências absolutas e relativas",
        locations = cells_column_labels(columns = c(`PC (%)`, `SAN (%)`))
      )
  })
  
  
  output$tabela_esperada_ex7 <- render_gt({
    qui_quad_ex7 <- chisq.test(dados_paralisia$perda_audit, dados_paralisia$grupo, simulate.p.value = TRUE, B = 10000)
    tabela_esperada_ex7 <- as.data.frame(qui_quad_ex7$expected)
    
    # Renomear colunas e linhas
    colnames(tabela_esperada_ex7) <- c("PC", "SAN")
    rownames(tabela_esperada_ex7) <- c("Não", "Sim")
    
    # Calcular porcentagens
    total_coluna <- colSums(tabela_esperada_ex7)
    tabela_esperada_ex7 <- tabela_esperada_ex7 %>%
      mutate(PC = sprintf("%.2f (%.2f%%)", PC, PC / total_coluna["PC"] * 100),
             SAN = sprintf("%.2f (%.2f%%)", SAN, SAN / total_coluna["SAN"] * 100))
    
    # Preparar tabela para renderização com gt
    tabela_esperada_ex7 <- tabela_esperada_ex7 %>%
      tibble::rownames_to_column(var = "Perda Auditiva") %>%
      select(`Perda Auditiva`, `PC`, `SAN`)
    
    gt(tabela_esperada_ex7) %>%
      tab_header(
        title = md("**Tabela esperada do Teste Qui-Quadrado**")
      ) %>%
      cols_label(
        `Perda Auditiva` = "Perda auditiva",
        `PC` = "PC",
        `SAN` = "SAN"
      ) %>%
      fmt_markdown(columns = everything()) %>%
      cols_align(
        align = "center",
        columns = everything()
      ) %>%
      tab_options(
        table.width = pct(100),
        heading.align = "center"
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels(everything())
      ) %>%
      tab_footnote(
        footnote = "n (%): frequências esperadas e relativas",
        locations = cells_column_labels(columns = c(`PC`, `SAN`))
      )
  })
  
  
  observeEvent(input$botao_teste_ex7, {
    
    req(input$teste_ex7)
    if(input$teste_ex7 == 'Qui-Quadrado via simulação de Monte Carlo'){
      output$resultado_teste_ex7 <- renderPrint({
        qui_quad_7 <- chisq.test(dados_paralisia$perda_audit, dados_paralisia$grupo, simulate.p.value =  TRUE, B = 10000)
        cat('p-valor do Teste Qui-Quadrado:', qui_quad_7$p.value)
      }
      )
    }
    mensagem <- reactive({
      if(input$teste_ex7 == 'Qui-Quadrado via simulação de Monte Carlo'){
        return('Resposta correta.')}
      else{
        if(is.null(input$teste_ex7)){
          return('Você não selecionou as respostas')}
        else{
          return('Há algo errado com sua seleção.')}
      }
    })
    shinyalert(
      title = '',
      text = mensagem(),
      type = ifelse(mensagem() == 'Resposta correta.', 'success', 'warning')
    )},
    ignoreNULL = T)
  
  output$ex7_parteb <- renderUI({
    req(input$botao_teste_ex7)
    req(input$teste_ex7 == 'Qui-Quadrado via simulação de Monte Carlo')
    if(input$teste_ex7 == 'Qui-Quadrado via simulação de Monte Carlo'){
      fluidRow(
        pickerInput('relacao', 'Há relação entre as variáveis?',
                    choices = c('Sim', 'Não')),
        actionButton('verificar_teste', 'Verificar')
      )
      
    }
  })
  
  observeEvent(input$verificar_teste, {
    mensagem <- reactive({
      if(input$relacao == 'Não'){
        return('Resposta correta.')}
      else{
        if(is.null(input$relacao)){
          return('Você não selecionou as respostas')}
        else{
          return('Há algo errado com sua seleção.')}
      }
    })
    shinyalert(
      title = '',
      text = mensagem(),
      type = ifelse(mensagem() == 'Resposta correta.', 'success', 'warning')
    )}
  )
  
  #Verificar se as tabelas plotadas são iguais
  #Ex8
  output$tabela_ex8_pc <- render_gt({
    contagem_tabela <- dados_paralisia %>%
      count(dist_comun, grupo) %>%
      pivot_wider(names_from = grupo, values_from = n, values_fill = list(n = 0)) %>%
      rename(
        `Distúrbio de Comunicação` = dist_comun,
        `PC` = PC,
        `SAN` = SAN
      )
    
    # Calcular as porcentagens
    contagem_tabela <- contagem_tabela %>%
      mutate(
        Total_PC = sum(PC),
        Total_SAN = sum(SAN),
        `PC (%)` = paste(PC, "(", round((PC / Total_PC) * 100, 2), "%)", sep = ""),
        `SAN (%)` = paste(SAN, "(", round((SAN / Total_SAN) * 100, 2), "%)", sep = "")
      ) %>%
      select(`Distúrbio de Comunicação`, `PC (%)`, `SAN (%)`)
    
    # Criar a tabela gt
    gt(contagem_tabela) %>%
      tab_header(
        title = md("**Tabela Observada**")
      ) %>%
      cols_label(
        `Distúrbio de Comunicação` = "Distúrbio de Comunicação",
        `PC (%)` = "PC",
        `SAN (%)` = "SAN"
      ) %>%
      cols_align(
        align = "center",
        columns = everything()
      ) %>%
      fmt_markdown(
        columns = everything()
      ) %>%
      tab_options(
        table.width = pct(100),
        heading.align = "center"
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels(everything())
      ) %>%
      tab_footnote(
        footnote = "n (%): frequências absolutas e relativas",
        locations = cells_column_labels(columns = c(`PC (%)`, `SAN (%)`))
      )
  })
  
  output$tabela_esperada_ex8 <- render_gt({
    qui_quad_ex8 <- chisq.test(dados_paralisia$dist_comun, dados_paralisia$grupo, simulate.p.value = TRUE, B = 10000)
    tabela_esperada_ex8 <- as.data.frame(qui_quad_ex8$expected)
    
    # Renomear colunas e linhas
    colnames(tabela_esperada_ex8) <- c("PC", "SAN")
    rownames(tabela_esperada_ex8) <- c("Não", "Sim")
    
    # Calcular porcentagens
    total_coluna <- colSums(tabela_esperada_ex8)
    tabela_esperada_ex8 <- tabela_esperada_ex8 %>%
      mutate(PC = sprintf("%.2f (%.2f%%)", PC, PC / total_coluna["PC"] * 100),
             SAN = sprintf("%.2f (%.2f%%)", SAN, SAN / total_coluna["SAN"] * 100))
    
    # Preparar tabela para renderização com gt
    tabela_esperada_ex8 <- tabela_esperada_ex8 %>%
      tibble::rownames_to_column(var = "Distúrbio de Comunicação") %>%
      select(`Distúrbio de Comunicação`, `PC`, `SAN`)
    
    gt(tabela_esperada_ex8) %>%
      tab_header(
        title = md("**Tabela esperada do Teste Qui-Quadrado**")
      ) %>%
      cols_label(
        `Distúrbio de Comunicação` = "Distúrbio de Comunicação",
        `PC` = "PC",
        `SAN` = "SAN"
      ) %>%
      fmt_markdown(columns = everything()) %>%
      cols_align(
        align = "center",
        columns = everything()
      ) %>%
      tab_options(
        table.width = pct(100),
        heading.align = "center"
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels(everything())
      ) %>%
      tab_footnote(
        footnote = "n (%): frequências esperadas e relativas",
        locations = cells_column_labels(columns = c(`PC`, `SAN`))
      )
  })
  
  observeEvent(input$botao_teste_ex8, {
    
    req(input$teste_ex8)
    if(input$teste_ex8 == 'Qui-Quadrado via simulação de Monte Carlo'){
      output$resultado_teste_ex8 <- renderPrint({
        qui_quad_ex8 <- chisq.test(dados_paralisia$dist_comun, dados_paralisia$grupo, simulate.p.value = TRUE, B = 10000)
        cat('p-valor do teste Qui-Quadrado:', qui_quad_ex8$p.value)
      }
      )
    }
    mensagem <- reactive({
      if(input$teste_ex8 == 'Qui-Quadrado via simulação de Monte Carlo'){
        return('Resposta correta.')}
      else{
        if(is.null(input$teste_ex8)){
          return('Você não selecionou as respostas')}
        else{
          return('Há algo errado com sua seleção.')}
      }
    })
    shinyalert(
      title = '',
      text = mensagem(),
      type = ifelse(mensagem() == 'Resposta correta.', 'success', 'warning')
    )},
    ignoreNULL = T)
  
  output$ex8_parteb <- renderUI({
    req(input$botao_teste_ex8)
    req(input$teste_ex8 == 'Qui-Quadrado via simulação de Monte Carlo')
    if(input$teste_ex8 == 'Qui-Quadrado via simulação de Monte Carlo'){
      fluidRow(
        pickerInput('relacao_ex8', 'Há relação entre as variáveis?',
                    choices = c('Sim', 'Não')),
        actionButton('verificar_teste_ex8', 'Verificar')
      )
      
    }
  })
  
  observeEvent(input$verificar_teste_ex8, {
    mensagem <- reactive({
      if(input$relacao_ex8 == 'Não'){
        return('Resposta correta.')}
      else{
        if(is.null(input$relacao_ex8)){
          return('Você não selecionou as respostas')}
        else{
          return('Há algo errado com sua seleção.')}
      }
    })
    shinyalert(
      title = '',
      text = mensagem(),
      type = ifelse(mensagem() == 'Resposta correta.', 'success', 'warning')
    )}
  )
  
  ###Ex9 (deixei a parte do código no arquivo 'exercicios_nao_utilizados', troquei o nome de exibição do exercício 10 para ex 9 e escondi o ex9, pois necessita correção)
  
  
  #Ex10
  observeEvent(input$graf_ex10, {
    output$plot_ex10 <- renderPlot({
      req(input$variavel_ex10x != input$variavel_ex10y, 
          (input$variavel_ex10x == 'td_liquido' || input$variavel_ex10x == 'td_solido'),
          (input$variavel_ex10y == 'td_liquido' || input$variavel_ex10y == 'td_solido'),
          input$variavel_ex10_graf == 'Dispersão')
      
      # Determine the labels based on the selected variables
      x_label <- if (input$variavel_ex10x == 'td_liquido') {
        'Tempo líquido'
      } else {
        'Tempo sólido'
      }
      
      y_label <- if (input$variavel_ex10y == 'td_liquido') {
        'Tempo líquido'
      } else {
        'Tempo sólido'
      }
      
      plot <- ggplot(dados_paralisia,
                     aes_string(x = input$variavel_ex10x, 
                                y = input$variavel_ex10y)) +
        geom_point() +
        labs(x = x_label, y = y_label) +
        theme_minimal()
      
      return(plot)
    })
    
    
    mensagem <- reactive({
      if (is.null(input$variavel_ex10x) && is.null(input$variavel_ex10y) && is.null(input$variavel_ex10_graf)) {
        return("Você não selecionou as respostas!")}
      else {
        if (identical(input$variavel_ex10x, 'td_liquido') && identical(input$variavel_ex10y, 'td_solido') && identical(input$variavel_ex10_graf, 'Dispersão')) {
          return("Resposta correta.")}
        if (identical(input$variavel_ex10x, 'td_solido') && identical(input$variavel_ex10y, 'td_liquido') && identical(input$variavel_ex10_graf, 'Dispersão')) {
          return("Resposta correta.")}
        else {
          return("Há algo errado com sua seleção.")}}
    })
    
    shinyalert(
      title = "",
      text = mensagem(),
      type = ifelse(mensagem() == "Resposta correta.", "success", "warning")
    )},
    ignoreNULL = T)
  
  output$ex10_parteb <- renderUI({
    req(input$variavel_ex10x != input$variavel_ex10y, 
        (input$variavel_ex10x == 'td_liquido' || input$variavel_ex10x == 'td_solido'),
        (input$variavel_ex10y == 'td_liquido' || input$variavel_ex10y == 'td_solido'),
        input$variavel_ex10_graf == 'Dispersão')
    req(input$graf_ex10)
    if(input$variavel_ex10x == 'td_liquido' || input$variavel_ex10x == 'td_solido' && input$variavel_ex10y == 'td_liquido' || input$variavel_ex10y == 'td_solido' && input$variavel_ex10_graf =='Dispersão' && input$variavel_ex10x != input$variavel_ex10y){
      fluidRow(
        br(),
        br(),
        pickerInput('teste_ex10', 'Escolha o teste:',
                    choices = c('Teste de Correlação de Spearman', 'Qui-Quadrado', 't de Student')),
        actionButton('verificar_teste_ex10', 'Verificar')
      )
      
      
      
    }
    
  })
  
  observeEvent(input$graf_ex10, {
    if(input$variavel_ex10x == 'td_liquido' || input$variavel_ex10x == 'td_solido' && input$variavel_ex10y == 'td_liquido' || input$variavel_ex10y == 'td_solido' && input$variavel_ex10_graf =='Dispersão' && input$variavel_ex10x != input$variavel_ex10y){
      output$resultado_teste_norm_ex10 <- renderPrint({
        shapiro_tdliq <- shapiro.test(dados_paralisia$td_liquido)
        shapiro_tdsol <- shapiro.test(dados_paralisia$td_solido)
        
        cat('p-valor do teste de Shapiro-Wilk para tempo líquido',
            shapiro_tdliq$p.value, '\n',
            'p-valor do teste de Shapiro-Wilk para tempo sólido',
            shapiro_tdsol$p.value)
      })
    }
  })
  
  observeEvent(input$verificar_teste_ex10, {
    
    if(input$teste_ex10 == 'Teste de Correlação de Spearman'){
      output$resultado_teste_ex10 <- renderPrint({
        teste_cor_ex10 <- cor.test(dados_paralisia$td_liquido, 
                                   dados_paralisia$td_solido,
                                   exact = F, method = 'spearman')
        cat('p-valor do Teste de Correlação de Spearman', teste_cor_ex10$p.value)
        
      })
    }
    
    
    
    mensagem <- reactive({
      if(input$teste_ex10 == 'Teste de Correlação de Spearman'){
        return('Resposta correta.')
      }
      else{
        if(is.null(input$teste_ex10)){
          return('Você não selecionou as respostas')}
        else{
          return('Há algo errado com sua seleção.')}
      }
    })
    shinyalert(
      title = '',
      text = mensagem(),
      type = ifelse(mensagem() == 'Resposta correta.', 'success', 'warning')
    )}
    
  )
  
  output$ex10_partec <- renderUI({
    req(input$verificar_teste_ex10)
    if(input$teste_ex10 == 'Teste de Correlação de Spearman'){
      fluidRow(
        br(),
        br(),
        pickerInput('relacao_ex10', 'Escolha sua resposta:',
                    choices = c('Sim', 'Não')),
        actionButton('verificar_relacao_ex10', 'Verificar')
        
      )
    }
  })
  
  observeEvent(input$verificar_relacao_ex10, {
    mensagem <- reactive({
      if(input$relacao_ex10 == 'Sim'){
        return('Resposta correta.')
      }
      else{
        return('Há algo errado com sua seleção')
      }
    })
    shinyalert(
      title = '',
      text = mensagem(),
      type = ifelse(mensagem() == 'Resposta correta.', 'success', 'warning')
    )
  })
  
  
  
}