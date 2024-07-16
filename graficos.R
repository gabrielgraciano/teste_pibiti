graf_qualitativa <-
  tabItem(tabName = 'graf_qualitativa',
          sidebarLayout(
            sidebarPanel(
              uiOutput("uiGrafQual")
            ),
            mainPanel(
              uiOutput("graficosQual"))
          )
  )

graf_quantitativa <-
  tabItem(tabName = 'graf_quantitativa',
          sidebarLayout(
            sidebarPanel(
              uiOutput("uiGrafQuant")
            ),
            mainPanel(
              uiOutput("graficosQuant")
              
            )
          )
  )

graf_bidimensional <-
  tabItem(tabName = 'graf_bidimensional',
          fluidPage(
            uiOutput("biUI")
          )
  )