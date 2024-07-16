# UI ----

inicio_equipe_ui <- function(id) {
 
   ns <- NS(id)
   
  tagList(
    tags$head(
      tags$style(HTML("
        .participant-container {
          display: flex;
          flex-wrap: wrap;
          justify-content: center;
        }
        .participant {
          margin: 15px;
          text-align: center;
        }
        .participant img {
          width: 150px;
          height: 150px;
          border-radius: 50%;
          object-fit: cover;
        }
        .participant-name {
          margin-top: 10px;
          font-weight: bold;
        }
      "))
    ),
    fluidRow(
      column(12, 
             h3(strong("Equipe")),
             div(
               div(class = "participant-container", 
                   
                   div(class = "participant", 
                       img(src = "images/alessandra.jpg"),
                       div(class = "participant-name",
                           "Alessandra A. S. Menezes")
                   ),
                   div(class = "participant", 
                       img(src = "images/camila.jpg"),
                       div(class = "participant-name",
                           "Camila Bertini Martins")
                   ),
                   div(class = "participant", 
                       img(src = "images/flavia.jpg"),
                       div(class = "participant-name", 
                           "Flávia Cristina Martins Queiroz Mariano")
                   ),
                   div(class = "participant", 
                       img(src = "images/gabriel.jpg"),
                       div(class = "participant-name", 
                           "Gabriel Graciano Dias")
                   ),
                   div(class = "participant", 
                       img(src = "images/joao.jpg"),
                       div(class = "participant-name", 
                           "Joao Henrique de Araujo Morais")
                   ),
                   div(class = "participant", 
                       img(src = "images/participante.jpg"),
                       div(class = "participant-name", 
                           "Paulo Bandiera Paiva")
                   )
               )
             )
      )
    )
  )
  
}


# server ----

inicio_equipe_server <- function(input, output, session) {
  
  }