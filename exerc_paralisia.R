# Questões ----

questoes_paralisia <- list(
  
  "1" = "<left>
  <p>Utilize as caixas de seleção para agrupar as variáveis 
  em <b>qualitativas</b> e <b>quantitativas</b>.<p>
  </left>",
  
  "2" = "<left>
  <p>Utilize a caixa de seleção “Medidas-resumo” para selecionar 
  <b>todas</b> as medidas-resumo
  adequadas aos <b>tempos de deglutição de alimentos (líquidos, pastosos e
  sólidos)</b>.<p>
  </left>",
  
  "3" = "<left>
  <p>Utilize a caixa de seleção “Medidas-resumo”
  para selecionar <b>todas</b>  as medidas-resumo adequadas ao <b>distúrbio 
  de comunicação</b>.<p>
  </left>",
  
  "4" = "<left>
  <p>Utilize as caixas de seleção para construir
  um gráfico adequado para visualizar o <b>grau de disfunção motora oral
  (DMO)</b>.<p>
  </left>",
  
  "5" = "<left>
  <p>Utilize as caixas de seleção para construir um gráfico
  adequado para visualizar os <b>distúrbios de comunicação</b>.<p>
  </left>",
  
  "6" = "<left>
  <p>Utilize as caixas de seleção para construir um gráfico 
  adequado para visualizar o <b>tempo de deglutição
  de alimentos líquidos</b>.<p>
  </left>",
  
  "7" = "<left>
  <p>A tabela abaixo apresenta a distribuição de perda auditiva 
  pelos grupos de crianças com paralisia cerebral e as sem acometimentos 
  neurológicos</b>.<p>
  </left>",
  
  "8" = "<left>
  <p>A tabela abaixo apresenta os distúrbios de comunicação
  pelos grupos de crianças SAN e PC.</b><p>
  </left>",
  
  "9" = "<left>
  <p>Utilize as caixas de seleção para construir 
  um gráfico para visualizar a relação do <b>tempo de deglutição de alimentos
  líquidos</b> com os <b> grupos de crianças PC e SAN.</b><p>
  </left>",
  
  "10" = "<left>
  <p>Utilize as caixas de seleção para construir 
  um gráfico para visualizar a relação do <b>tempo de deglutição de alimentos 
  líquidos </b> com o <b>tempo de deglutição de alimentos sólidos.</b><p>
  </left>"
)

# UI ---- 

useShinyjs()
exerc_paralisia <-
  tabItem(tabName = 'exerc_paralisia',
          
          fluidPage(
            
            h3(strong('Exercícios Práticos - Banco de Dados Paralisia Cerebral')),
            
            fluidRow(
              column(10,
                     wellPanel(
                       HTML("<p>Segundo as 
                <a href= 'https://bvsms.saude.gov.br/bvs/publicacoes/diretrizes_atencao_paralisia_cerebral.pdf' 
                target='_blank' > Diretrizes de Atenção à
              Pessoa com Paralisia Cerebral do Ministério da Saúde (2014) </a>,
              a paralisia cerebral (PC) descreve um grupo de desordens da 
              desenvolução do movimento e postura atribuído
              à distúrbio não progressivo durante o desenvolvimento do 
              cérebro fetal ou infantil.
             <br>
             <p>Estima-se que nos países em desenvolvimento, 7 a cada 1000 
             nascidos vivos
             sejam acometidos por PC, em diferentes graus de comprometimento
             dos movimentos e postura.
             <br>
             <p>Algumas informações fornecidas nas diretrizes 
             do Ministério da Saúde e achados de <a href= 'https://www.scielo.br/j/rboto/a/hvQv9C6JY9h7MbrrSY98WpB/abstract/?lang=pt' target='_blank'> 
             Aurélio et al. (2002)</a> na comparação do padrão de deglutição 
             de alimentos entre crianças com PC e crianças sem acometimentos
             neurológicos
             (SAN), encontram-se simulados no banco de dados que será utilizado
             nos exercícios abaixo.</p><br>
             <p> Existem nove variáveis nesse banco de dados, sendo elas:</p>
             <br>
             <p><b>Sexo:</b> menino ou menina
             <p><b>Idade:</b> idade em anos completos
             <p><b>Grupo:</b> grupo das crianças por condição de saúde (SAN ou PC)
             <p><b>Perda auditiva:</b> existência ou não de perda auditiva (Não ou Sim)
             <p><b>Distúrbio de Comunicação:</b> existência ou não de distúrbio de
             comunicação (Não ou Sim)
             <p><b>DMO:</b> grau de Disfunção Motora Oral (DMO), em quatro categorias 
             (Normal, Leve, Moderada ou Severa)
             <p><b>Tempo líquido:</b> tempo, em segundos, para deglutição de 100 ml 
             de suco de laranja
             <p><b>Tempo pastoso:</b> tempo, em segundos, para deglutição de 140 g de
             iogurte de morango homogêneo
             e sem pedaços de fruta
             <p><b>Tempo sólido:</b> tempo, em segundos, para deglutição de 12 g de 
             bolacha recheada de chocolate"
                       )
                     )
              )
              
              
            ),
            
            fluidRow(
              column(10,
                     wellPanel(
                       tabsetPanel(
                         tabPanel('Ex1', uiOutput('ex1')),
                         tabPanel("Ex2", uiOutput('ex2')),
                         tabPanel('Ex3', uiOutput('ex3')),
                         tabPanel('Ex4', uiOutput('ex4')),
                         tabPanel('Ex5', uiOutput('ex5')),
                         tabPanel('Ex6', uiOutput('ex6')),
                         tabPanel('Ex7', uiOutput('ex7')),
                         tabPanel('Ex8', uiOutput('ex8')),
                         #tabPanel('Ex9', uiOutput('ex9')),
                         tabPanel('Ex9', uiOutput('ex10'))
                       )
                     )
              )
            )
          )
  )