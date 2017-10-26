

shinyUI( 
  
  # this is actually .css; this changes the color for the sliders
  # for some reason it didn't work when put in the .css file. 
  tagList(tags$style(HTML(".irs-single, .irs-bar-edge, .irs-bar{background: #00a90a}")), 
  navbarPage("Inventário Florestal (BETA)", 
             

             
            # shinythemes::themeSelector(),
            # theme = "green.css", # seleciona um tema contido na pasta www
              theme = "green_yeti2.css", # seleciona um tema contido na pasta www
            # theme = shinythemes::shinytheme("paper"), # seleciona um tema utilizando pacote
             
 
      # Painel Intro ####          
             tabPanel( "Intro" ,
                       fluidRow(
                         column(6,
                                includeMarkdown("about.md")
                         ),
                         column(6,
                                img(class="img-polaroid",
                                    src="intro_picture.jpg",
                                    width  = 655,
                                    height = 595),
                                tags$small(
                                  "Source: Google images"
                                )
                         )
                       ) # fluid row
             ), # Painel Intro             
                      

      # Upload de dados ####
      tabPanel("Dados",
               sidebarLayout(
                 
                 sidebarPanel(
                   
                   h3("Dados"),
                   
                   radioButtons("df_select", 
                                "Fazer o upload de um arquivo, ou utilizar o dado de exemplo?", 
                                c("Fazer o upload", 
                                  "Utilizar o dado de exemplo em nivel de arvore", 
                                  "Utilizar o dado de exemplo em nivel de parcela"), 
                                selected = "Fazer o upload"),
                   
                   uiOutput("upload") # tipos de arquivos aceitos
                   
                   
                   
                 ), # sidebarPanel
                 
                 mainPanel(
                   DT::dataTableOutput("rawdata")
                 ) # mainPanel
               ) # sidebarLayout
      ), # tabPanel Upload  de dados
      
      
      # Painel Totalização de Parcelas ####
      tabPanel("Totalização de Parcelas",
               
               sidebarLayout(
                 
                 sidebarPanel(
                   
                   uiOutput("tot_parc_ui1"),
                   
                   h4("Inserir valores de área:"),
                   
                   radioButtons("area_radio_new",
                                "Escolher coluna da lista de colunas, ou inserir os valores manualmente?",
                                c("Lista de colunas", "Manualmente"),
                                "Manualmente"),
                   
                   uiOutput("tot_parc_ui2"),
                   
                   actionButton( # botao que o usuario clica, e gera uma acao no server
                     "Loadnew", # Id
                     "Rodar")
                   
                 ), # sidebar panel
                 
                 mainPanel(
                   
                   DT::dataTableOutput("newdata")
                   
                 ) # mainPanel
                 
                 
               ) #sidebar layout
               
               
      ) , # tab panel nivel parcela       
      
      
      # Painel Casual Simples ####
      tabPanel("A. Casual Simples", 
              # tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: green}")),
               
               
               sidebarLayout(
                 
                 sidebarPanel(

                   uiOutput("acs_ui1"),
                   
                   h4("Inserir valores de área:"),
                   
                   radioButtons("area_radio_acs",
                                "Escolher coluna da lista de colunas, ou inserir os valores manualmente?",
                                c("Lista de colunas", "Manualmente"),
                                "Lista de colunas"),
                   
                   uiOutput("acs_ui2"),
                   
                   actionButton( # botao que o usuario clica, e gera uma acao no server
                     "Loadacs", # Id
                     "Rodar Inventario")
                   
                 ), # sidebarPanel
                 mainPanel(
                   
                   DT::dataTableOutput("acs")
                   
                 ) # mainPanel
                 
               ) # sidebarLayout            
               
               
      ), # tabPanel Casual Simples
      
      # Painel Casual Estratificada ####
      tabPanel("A. Casual Estratificada",  
               
               sidebarLayout(
                
                 
                 sidebarPanel(
                   
                   uiOutput("ace_ui"), 
                   
                   actionButton( # botao que o usuario clica, e gera uma acao no server
                     "Loadace", # Id
                     "Rodar Inventario")
                   
                 ), # sidebarPanel
                 mainPanel(
                   tabsetPanel(
                     id="tabsace",
                     tabPanel("Resultados",  DT::dataTableOutput("ace2")),
                     tabPanel("Resultados por estrato", DT::dataTableOutput("ace1") )
                     
                     
                   )#tabsetPanel
                   
                 ) # mainPanel
                 
               ) # sidebarLayout            
               
               
      ), # tabPanel Casual Estratificada
      
      
      
      # Painel Sistematica ####
      tabPanel("A. Sistematica",
               
               sidebarLayout(
                 
                 sidebarPanel(
                   
                   uiOutput("as_ui1"),
                   
                   h4("Inserir valores de área:"),
                   
                   radioButtons("area_radio_as",
                                "Escolher coluna da lista de colunas, ou inserir os valores manualmente?",
                                c("Lista de colunas", "Manualmente"),
                                "Lista de colunas"),
                   
                   uiOutput("as_ui2"),
                   
                   actionButton( # botao que o usuario clica, e gera uma acao no server
                     "Loadas", # Id
                     "Rodar Inventario")
                   
                 ), # sidebarPanel
                 mainPanel(
                   
                   DT::dataTableOutput("as")
                   
                 ) # mainPanel
                 
               ) # sidebarLayout            
               
               
               
      ), # tabPanel Sistematica
      
      
      
      # Painel Download ####

      tabPanel("Download de tabelas", 
               sidebarLayout(
                 
                 sidebarPanel(
                   
                   h3("Download de tabelas"),
                   
                   selectInput("dataset", "Escolha uma tabela:", 
                               choices = c(
                                 "Amostragem Casual Simples", 
                                 "Amostragem Casual Estratificada 1", 
                                 "Amostragem Casual Estratificada 2",
                                 "Amostragem Sistematica",
                                 "Nivel Parcela")),
                   
                   selectInput("datasetformat",
                               "Escolha o formato da tabela:",
                               choices = c("Valor separado por Virgulas (.CSV)" = ".csv",
                                           "Planilha do Excel (.xlsx)" = ".xlsx")
                   ),
                   
                   downloadButton('downloadData', 'Download')
                   
                 ),
                 mainPanel(
                   DT::dataTableOutput('table')      
                 )
               )
      ) # download de tabelas
      
# final da UI  ####    
  )# navbarPage

)#tagList

) # ShinyUI

