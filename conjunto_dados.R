conjunto_dados <- 
  tabItem(tabName = 'conjunto_dados',
          fluidPage(
            h3(strong('Conjuntos de Dados')),
            
            fluidRow(
              wellPanel(
                h4(strong("Alimentação")),
                HTML("<p> 'dados_saude_alimentação.csv' é uma versão didática
                do 'Food choices', banco de dados de domínio público disponível 
                em <a href='https://www.kaggle.com/borapajo/food-choices' target='_blank'>Kaggle</a>. 
                A base inclui informações de preferências gastronômicas,
                nutrição e de saúde de estudantes. A variável 'altura'
                e os dados relacionados aos exames laboratoriais (HDL, LDL etc.) 
                não existiam na base de dados original e foram acrescentados,
                de modo fictício, por questões didáticas. Esta base será 
                     utilizada na apresentação de alguns conceitos estatísticos.</p>"),
                fluidRow(
                  column(6, actionButton("botaoDicioAlimentacao", "Abrir Dicionário")),
                  column(6, downloadButton("botaoBaixarAlimentacao", "Download dos dados")),
                  align = 'center'
                )
              )
            ),
            fluidRow(
              wellPanel(
                h4(strong("Paralisia Cerebral")),
                HTML("<p>Para fins didáticos, algumas informações fornecidas pelas
                <a href= 'https://bvsms.saude.gov.br/bvs/publicacoes/diretrizes_atencao_paralisia_cerebral.pdf' 
                target='_blank' > Diretrizes de Atenção à
              Pessoa com Paralisia Cerebral do Ministério da Saúde (2014) </a> 
              e achados de 
                <a href= 'https://www.scielo.br/j/rboto/a/hvQv9C6JY9h7MbrrSY98WpB/abstract/?lang=pt' target='_blank'> 
             Aurélio et al. (2002)</a> 
                na comparação do padrão de deglutição de alimentos entre
                crianças com paralisia cerebral (PC) e crianças sem acometimentos 
                neurológicos (SAN), em Curitiba/PR, foram simulados e inseridos 
                na planilha 'dados_paralisia.csv' aqui fornecida. Este banco de dados será
                utilizado em alguns exercícios.<br></p>"),
                fluidRow(
                  column(6, actionButton("botaoDicioParalisia", "Abrir Dicionário")),
                  column(6, downloadButton("botaoBaixarParalisia", "Download dos dados")),
                  align = 'center'
                )
              )
            )
            
          )
  )