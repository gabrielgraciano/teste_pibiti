# UI ----

inicio_projeto_ui <- function(id) {

  ns <- NS(id)
  
  tagList(
    
    tags$head(
      tags$style(HTML("
        .project-text {
          font-size: 14px;
          line-height: 1.6;
        }
        .project-icon {
          font-size: 20px;
          margin-right: 10px;
          color: #007bff;
        }
      "))
    ),
    
    fluidRow(
      column(12, 
             div(
               HTML('<img src="images/garu_3.png" style="max-width:100%; height:auto;">'),
               align = 'center'
               )
             )
    ),
    
    fluidRow(
      column(12, 
             div(
               h3(strong("O Projeto")),
               div(class = "project-text",
                   tags$p(
                     tags$span(class = "project-icon", fa("project-diagram")),
                     "Este é o projeto Garu Estatística. Aqui você pode 
                     encontrar várias funcionalidades, como os principais 
                         conceitos da estatística descritiva e
                         inferencial, além de exercícios.
                         
                     Garu Estatística é um aplicativo gratuito, 
                     desenvolvido em R/Shiny, com o objetivo de aproximar 
                     a Estatística do cotidiano do profissional da 
                     Saúde de modo prático."
                   ),
                   tags$p(
                     tags$span(class = "project-icon", fa("university")),
                     "Desenvolvido por alunos de graduação e pós-graduação
                     da Unifesp, o aplicativo busca ser uma 
                     ferramenta útil e acessível."
                   ),
                   tags$p(
                     tags$span(class = "project-icon", fa("hands-helping")),
                     "Interessados em participar do projeto
                     podem entrar em contato com a coordenação."
                   ),
                   tags$p(
                     tags$span(class = "project-icon", fa("check-circle")),
                     "A equipe se esforça para manter a correção e
                     integridade das informações, mas reconhecemos que
                     podem ocorrer falhas. Estamos sempre abertos a 
                     críticas construtivas."
                   ),
                   tags$p(
                     tags$span(class = "project-icon", fa("book-open")),
                     "O app não é autossuficiente no ensino de estatística.
                     É necessário suporte adicional, e por isso, pode ser 
                     utilizado como instrumento de auxílio nas aulas."
                   ),
                   tags$p(
                     tags$span(class = "project-icon", fa("comments")),
                     "A equipe está aberta a sugestões da comunidade para 
                     a abordagem de problemas sugeridos pelos usuários,
                     dentro dos limites da equipe."
                   )
               )
             )
      )
    )
  )
}


# server ----

inicio_projeto_server <- function(input, output, session) {
  
}