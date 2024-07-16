# UI ----

inicio_contato_ui <- function(id) {
  
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
        .envelope {
          width: 100px;
          height: 100px;
          background: url('images/envelope.png') no-repeat center center;
          background-size: contain;
          position: relative;
          animation: fly 4s ease-in-out infinite;
          margin: 0 auto;
        }

        @keyframes fly {
          0% {
            transform: translateX(0) translateY(0);
          }
          25% {
            transform: translateX(100px) translateY(-50px);
          }
          50% {
            transform: translateX(200px) translateY(0);
          }
          75% {
            transform: translateX(100px) translateY(50px);
          }
          100% {
            transform: translateX(0) translateY(0);
          }
        }
      "))
    ),
    fluidRow(
      column(12, 
             h3(strong('Contato')),
             div(
               style = "padding: 20px; text-align: center;",
               tags$p("Críticas, correções, sugestões ou interesse em participar do projeto devem ser enviadas para:"),
               tags$p(tags$b("garuestatistica@unifesp.br")),
               br(),
               tags$div(
                 id = ns("animation"),
                 style = "margin-top: 20px;",
                 tags$div(class = "envelope")
               )
             )
      )
    )
  )
}

# server ----

inicio_contato_server <- function(input, output, session) {
  
}