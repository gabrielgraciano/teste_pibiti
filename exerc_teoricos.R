# Questões e gráficos -----

questoes_teoricos <- list(
  
  list(
    pergunta= HTML('Qual das seguintes medidas é afetada por 
                   um valor discrepante no conjunto de dados?'),
    opcoes=c('Média',
             'Mediana',
             'Moda',
             'Primeiro quartil'),
    resposta_correta=1,
    has_plot=NULL
  ),
  
  list(
    pergunta= HTML('Qual dos seguintes conjuntos de dados tem uma média 
                     de 15 e um desvio padrão de 0?'),
    opcoes=c('0, 15, 30',
             '15, 15, 15',
             '0, 0, 0',
             'Não existe um conjunto de dados com desvio padrão de 0'),
    resposta_correta=2,
    has_plot=NULL
  ),
  
  list(
    pergunta= HTML('Qual das seguintes afirmações é verdadeira?'),
    opcoes=c('50% dos valores de um conjunto de dados está entre o 2º e o 3º quartis.
',
             '25% dos valores de um conjunto de dados está entre a mediana e o valor máximo
',
             '25% dos valores de um conjunto de dados está entre a mediana e o valor mínimo
',
             '50% dos valores de um conjunto de dados está entre a mediana e o valor mínimo
'),
    resposta_correta=4,
    has_plot=NULL
  ),
  
  list(
    pergunta= HTML('Suponha que o conjunto de dados contém os pesos, em quilos,
    de uma     amostra aleatória de 100 recém-nascidos. Qual das medidas-resumo 
    a seguir para a variável peso não é dada em quilos?
'),
    opcoes=c('A média dos pesos',
             'O desvio padrão dos pesos',
             'A variância dos pesos',
             'A amplitude dos pesos'),
    resposta_correta=3,
    has_plot=NULL
  ),
  
  list(
    pergunta=HTML('O gráfico a seguir apresenta os planos de pós-graduação dos
                  formandos de Ensino Médio. Suponha que cada aluno escolha uma
                  dessas cinco opções.<br>'),
    complemento=HTML('Qual é o plano de pós-graduação mais 
                  comum para esses formandos?<br>'),
    opcoes=c('Emprego',
             'Faculdade Comunitária',
             'Universidade',
             'Exército'),
    resposta_correta=3,
    has_plot='plano_pg_plot_output'
  ),
  
  list(
    pergunta= HTML('O seguinte gráfico de barras representa os planos de 
    pós-graduação dos formandos de Ensino Médio. Suponha que cada aluno 
    escolha uma dessas cinco opções.<br>)'),
    complemento=HTML('Qual é o plano de pós-graduação menos comum 
    para esses formandos?<br>'),
    opcoes=c('Emprego',
             'Faculdade Comunitária',
             'Universidade',
             'Exército'),
    resposta_correta=4,
    has_plot='plano_pg_plot_output'
  ),
  
  list(
    pergunta= HTML('O seguinte gráfico de barras representa os planos de 
    pós-graduação dos formandos de Ensino Médio. Suponha que cada aluno 
    escolha uma dessas cinco opções.<br>'),
    complemento=HTML('Quantos alunos planejam tirar um ano sabático ou ir para a 
    universidade?<br>'),
    opcoes=c('20',
             '120',
             '100',
             '140'),
    resposta_correta=4,
    has_plot='plano_pg_plot_output'
  ),
  
  list(
    pergunta= HTML('O seguinte gráfico de barras representa os planos
    de pós-graduação dos formandos de Ensino Médio. Suponha que cada 
    aluno escolha uma dessas cinco opções.<br>'),
    complemento=HTML('Qual porcentagem da turma de graduandos está planejando
    frequentar a faculdade comunitária?<br> 
'),
    opcoes=c('90/330',
             '90/240',
             '240/330',
             '120/330'),
    resposta_correta=1,
    has_plot='plano_pg_plot_output'
  ),
  
  list(
    pergunta= HTML('O seguinte gráfico de pizza mostra a proporção de alunos
    matriculados em diferentes cursos de uma universidade.<br>'),
    complemento=HTML('Se alguns alunos estiverem matriculados em mais de um 
    curso, qual tipo de gráfico seria mais apropriado para exibir a 
    porcentagem de alunos em cada curso?<br>
                     '),
    opcoes=c('Um gráfico de pizza motrando o percentual de alunos matriculados 
             por curso, ou seja, o mesmo gráfico da figura.',
             'Um boxplot para cada curso mostrando a distribuição percentual dos
             alunos matriculados.',
             'Um gráfico de barras onde cada barra representa um curso e
                  a altura mostra o percentual de alunos matriculados.',
             'Um histograma para cada curso mostrando a distribuição percentual dos
             alunos matriculados.'),
    resposta_correta=3,
    has_plot='curso_pie_plot_output'
  ),
  
  list(
    pergunta= HTML('Baseada no trabalho de Francisco et al. 2004, a figura 
    abaixo exibe internações por doenças respiratórias em idosos e a intervenção vacinal
    contra influenza no Estado de São Paulo.<br>'),
    complemento=HTML('Com base nesse gráfico, é correto afirmar que:<br>'),
    opcoes=c('A proporção de internações por doenças respiratórias
    é maior nas mulheres idosas.
',
             'Pode-se dizer que parece não haver efeito de intervenção
                da vacina pois os percentuais de internação mantiveram-se
                os mesmos.',
             'Não há sazonalidade na proporção de internações.',
             'Pode-se dizer que parece haver efeito da vacinação,
                pois os percentuais de internação diminuíram entre os idosos.'),
    resposta_correta=4,
    has_plot='idosos_plot_output'
  ),
  
  list(
    pergunta= HTML('Foi realizado um estudo para avaliar a relação
    entre o índice de massa corporal segundo padrão internacional e
    indicadores de adiposidade no diagnóstico de sobrepeso e obesidade
    em 528 escolares, entre 6 e 10 anos, de ambos os sexos (baseado em Giugliano, R
    e Melo, ALP 2004). A seguir tem-se o gráfico de dispersão entre o índice
    de massa corporal e o percentual de gordura corporal em escolares, 
    na faixa etária de 6 a 10 anos.<br>'),
    complemento=HTML('Com relação à figura, é correto afirmar:<br>
                     '),
    opcoes=c('Há indícios de correlação negativa entre IMC e percentual de
    gordura corporal das meninas.
',
             'Há indícios de correlação negativa entre IMC e percentual
                  de gordura corporal dos meninos.
',
             'Não há indícios de correlação positiva entre IMC e
                percentual de gordura corporal das meninas.',
             'Há indícios de correlação positiva entre IMC e percentual 
                  de gordura corporal das meninas.
'),
    resposta_correta=4,
    has_plot='imc_plot_output'
  ),
  
  list(
    pergunta= HTML('Abaixo, são dadas medidas resumo do salário mensal e da 
    carga horária semanal (em horas) de 100 funcionários de um determinado 
    hospital:
<html>
<head>
    <style>
        table {
            width: 80%;
            border-collapse: collapse;
            margin: 15px 0;
            font-size: 16px;
            text-align: left;
        }
        th, td {
            padding: 12px 15px;
        }
        th {
            background-color: #f2f2f2;
        }
        th, td {
            border: 1px solid #ddd;
        }
        th {
            font-weight: bold;
        }
    </style>
</head>
<body>

<table>
  <tr>
    <th>Variável</th>
    <th>Média</th>
    <th>Desvio padrão</th>
  </tr>
  <tr>
    <td>Salário (R$)</td>
    <td>1.950,00</td>
    <td>350,00</td>
  </tr>
  <tr>
    <td>Carga horária semanal (horas)</td>
    <td>35</td>
    <td>15</td>
  </tr>
</table>

</body>
</html>

Com base nessas informações, assinale a alternativa correta.
                     '),
    opcoes=c('O salário possui menor variabilidade que a carga horária
    semanal, pois o coeficiente de variação é menor.
',
             'O salário possui menor variabilidade que a carga horária 
                  semanal, pois tem maior desvio padrão.
',
             'A carga horária semanal possui menor variabilidade que o 
                salário, pois o desvio padrão é menor.',
             'A carga horária semanal possui menor variabilidade que o
                  salário, pois o coeficiente de variação é menor.
'),
    resposta_correta=1,
    has_plot=NULL
  ),
  
  
  list(
    pergunta= HTML('A faixa etária é uma variável _____; um tipo de 
    gráfico apropriado para representar sua distribuição é o _____. A
    pressão arterial é uma variável _____; um tipo de gráfico apropriado 
    para representar sua distribuição é o _____. <br>
    <p>Assinale a alternativa que corretamente preenche a sequência
    de lacunas exibidas acima. 
'),
    opcoes=c('Qualitativa ordinal; de barras; quantitativa discreta;
    de boxplot.',
             'Quantitativa discreta; de barras; quantitativa contínua;
                  histograma.',
             'Qualitativa ordinal; de pizza; quantitativa contínua; 
                  boxplot.',
             'Qualitativa ordinal; de barras; quantitativa contínua; 
                  histograma.'),
    resposta_correta=4,
    has_plot=NULL
  ),
  
  list(
    pergunta= HTML(' Para visualizar a relação entre idade gestacional da 
    mãe (em semanas) e peso ao nascer (em quilogramas) de recém-nascidos,
    qual dos gráficos abaixo é o mais apropriado?<br>
'),
    opcoes=c('Gráfico de barras
',
             'Gráfico de linhas
',
             'Gráfico de dispersão',
             'Boxplot
'),
    resposta_correta=3,
    has_plot=NULL
  )
)

custom_theme <- theme_minimal() +
  theme(
    strip.text.x = element_text(face='bold', size = 12), 
    plot.caption = element_text(face = "bold", hjust = 0),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face='bold', size = 12),
    axis.text = element_text(face='bold', size = 10),
    legend.title = element_blank(),
    legend.text = element_text(face = "bold"),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.border = element_rect(colour = "black", fill = NA)
  )

colorful <- c('#0c7bdc', '#FFc20a', "#20BFB2", "#63BE76", "#E66100",
              "#0c7bdc", "#d41159", "#4b0092", "#64B5DA", "#c9e5f2")

# Gráfico de barras
plano_pg_plot <- data.frame(Plano = c("Universidade", "Faculdade Comunitária", 
                                 "Exército", "Emprego", "Ano sabático"),
                       Numero = c(120, 90, 15, 85, 20)) %>%
  ggplot(aes(x = Plano, y = Numero, fill = Plano)) +
  geom_bar(stat = "identity", color='black') +
  geom_text(aes(label = Numero), vjust = -0.3) +
  labs(caption = 'Nota: Um ano sabático significa que aluno ficará de folga por um ano antes de decidir o que fazer.',
       title = "Planos de pós-graduação dos formandos do Ensino Médio",
       x = "",
       y = "Número de Indivíduos"
  ) +
  scale_fill_manual(values = colorful) +
  custom_theme +
  theme(legend.position = "none")

# Gráfico de pizza
curso_pie_plot <- data.frame(
  course = c("Negócios", "Educação", "Engenharia", 
             "Ciências da Saúde", "Artes e Ciências"),
  percentage = c(25, 23, 14, 16, 22)
) %>%
  ggplot(aes(x = "", y = percentage, fill = course)) +
  geom_bar(width = 1, stat = "identity", color='black') +
  coord_polar("y", start = 0) +
  labs(title = "Percentual de matrículas por curso") +
  scale_fill_manual(values = colorful) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", 
            fontface = "bold")

# Gráfico de linhas
idosos_plot <- ggplot(exerc_idosos, 
                      aes(x = mes, y = perc, color = sexo, 
                          linetype = sexo, group = sexo)) +
  geom_line(linewidth = 0.5) +
  labs(
    x = "Mês",
    y = "%",
    title = "Proporção de internações por doenças respiratórias na opulação idosa,
    segundo sexo, estado de São Paulo, 1995 a 2002"
  ) +
  scale_color_manual(values = c(colorful[1], colorful[2])) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  custom_theme +
  ylim(0,20) +
  scale_x_continuous(breaks = seq(1, 84, by = 6), 
                     labels = exerc_idosos$label[seq(1, 84, by = 6)]) +
  geom_vline(xintercept = 40, linetype = "dotted",
             linewidth=1.2, color='red') +
  annotate("text", x = 40 + 2,
           y = max(exerc_idosos$perc), label = "Intervenção",
           angle = 0, vjust = -0.5, hjust = 0)

# Gráfico combinado
imc_plot <- exerc_imc %>%
  ggplot(aes(x = perc_gordura, y = imc, color = sexo)) +
  geom_point() +
  xlim(0,40) +
  ylim(0,35) +
  facet_grid(cols = vars(sexo)) +
  labs(
    title = "Distribuição do Índice de Massa Corporal(IMC) por
    Percentual de Gordura Corporal",
    x = "\n% de gordura corporal",
    y = "IMC (kg/m²)\n"
  ) +
  scale_color_manual(values = c(colorful[1], colorful[2])) +
  custom_theme +
  theme(legend.position = "none")

# UI do módulo -----

exerc_teoricos_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h3(strong('Exercícios Teóricos')),
    fluidRow(
      column(8,
             wellPanel(
               uiOutput(ns('pergunta_teoricos')),
               div(style="text-align: center;",
                   uiOutput(ns("has_plot_output_teoricos"))),
               uiOutput(ns('complemento_teoricos')),
               uiOutput(ns('opcoes_resposta_teoricos')),
               useShinyjs(),
               actionButton(ns('anterior_teoricos'), 'Questão anterior'),
               actionButton(ns('confirmar_resposta_teoricos'), 'Verificar'),
               actionButton(ns('proximo_teoricos'), 'Próxima Questão'),
               textOutput(ns('resultado_teoricos'))
             )
      ),
      column(4,
             wellPanel(
               gt_output(ns('score_teoricos'))
             )
      ),
      column(12,
             h5('Fonte: 1.001 Problemas de Estatística para Leigos/ Dummies Team; traduzido por Samantha Batista. RJ: Alta Books, 2016. 560 p.')
      )
    )
  )
}


# server do módulo -----

exerc_teoricos_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    questao_atual <- reactiveVal(1)
    feedback <- reactiveVal(NULL)
    counter <- reactiveVal(0)
    botao <- reactiveVal(TRUE)
    acertos <- reactiveVal(0)
    erros <- reactiveVal(0)
    tentativas <- reactiveVal(0)
    
    output$pergunta_teoricos <- renderUI({
      if (questao_atual() <= length(questoes_teoricos)) {
        pergunta <- questoes_teoricos[[questao_atual()]]
        HTML(paste('<h5><b>Questão', questao_atual(), ':</b>',
                   pergunta$pergunta, '</h5><br>'))
      } else {
        HTML('<h3>Questionário finalizado.</h3>')
      }
    })
    
    output$opcoes_resposta_teoricos <- renderUI({
      if (questao_atual() <= length(questoes_teoricos)) {
        pergunta <- questoes_teoricos[[questao_atual()]]
        radioButtons(ns('resposta'), 'Escolha uma opção:', choices = pergunta$opcoes)
      } else {
        NULL
      }
    })
    
    output$plano_pg_plot_output <- renderPlot({ plano_pg_plot })
    output$curso_pie_plot_output <- renderPlot({ curso_pie_plot })
    output$idosos_plot_output <- renderPlot({ idosos_plot })
    output$imc_plot_output <- renderPlot({ imc_plot })
    
    output$has_plot_output_teoricos <- renderUI({
      if (questao_atual() <= length(questoes_teoricos)) {
        pergunta <- questoes_teoricos[[questao_atual()]]
        if (!is.null(pergunta$has_plot)) {
          div(style = "display: flex; justify-content: center;", 
              plotOutput(ns(pergunta$has_plot)))
        } else {
          NULL
        }
      }
    })
    
    output$complemento_teoricos <- renderUI({
      if (questao_atual() <= length(questoes_teoricos)) {
        pergunta <- questoes_teoricos[[questao_atual()]]
        if (!is.null(pergunta$complemento)) {
          HTML(paste0('<p>',pergunta$complemento))
        } else {NULL}
      }
      })
    
    
    observeEvent(input$proximo_teoricos, {
      if (questao_atual() <= length(questoes_teoricos)) {
        questao_atual(questao_atual() + 1)
        feedback(NULL)
      }
    })
    
    observeEvent(input$anterior_teoricos, {
      if (questao_atual() > 1) {
        questao_atual(questao_atual() - 1)
        feedback(NULL)
      }
    })
    
    observeEvent(input$confirmar_resposta_teoricos, {
      if (questao_atual() <= length(questoes_teoricos)) {
        pergunta <- questoes_teoricos[[questao_atual()]]
        resposta_correta <- pergunta$resposta_correta
        tentativas(tentativas() + 1)
        
        if (input$resposta == pergunta$opcoes[resposta_correta]) {
          feedback('Resposta Correta.')
          acertos(acertos() + 1)
        } else {
          feedback('Resposta Incorreta.')
          erros(erros() + 1)
        }
        counter(counter() + 1)
      }
    })
    
    output$score_teoricos <- render_gt({
      data <- data.frame(
        Categoria = c("Acertos", "Erros", "Tentativas"),
        Quantidade = c(acertos(), erros(), tentativas())
      ) 
      
      data %>%
        gt() %>%
        tab_header(title = "Pontuação")
    })
    
    output$resultado_teoricos <- renderText({
      if (questao_atual() <= length(questoes_teoricos)) {
        feedback()
      } else {
        ''
      }
    })
    
    observe({
      if (questao_atual() > length(questoes_teoricos)) {
        shinyalert(
          title = "Resultado Final",
          text = paste("Você fez ", tentativas(), "tentativas.\n",
                       "Acertos:", acertos(), "\n",
                       "Erros:", erros()),
          type = "info"
        )
        botao(FALSE)
      }
    })
    
    observe({
      shinyjs::toggle(ns('confirmar_resposta_teoricos'), condition = botao())
    })
  })
}