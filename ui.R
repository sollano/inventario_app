library(shiny)
suppressPackageStartupMessages(library(DT))
#library(plotly)
library(formattable)
library(readxl)
#library(plyr)
library(tibble)
library(tidyr)
suppressPackageStartupMessages(library(dplyr))
library(lazyeval)
library(ggplot2)
library(ggthemes)
library(openxlsx)
library(rmarkdown)
library(stringr)
library(googledrive)

shinyUI(
  # Intro, taglists e error messages colors ####
  tagList(tags$style(HTML(".irs-single, .irs-bar-edge, .irs-bar{background: #00a90a}")), # this is actually .css; this changes the color for the sliders
          
          # Cor de todas as mensagens da funcao need
          tags$head(
            tags$style(HTML("
                            .shiny-output-error-validation {
                            color: #00a90a;
                            }
                            "))
            ),
          
          # cor das mensagens que eu especificar com "WRONG"
          tags$head(
            tags$style(HTML("
                            .shiny-output-error-WRONG {
                            color: red;
                            }
                            "))
            ),
          
          # cor das mensagens que eu especificar com "AVISO"
          tags$head(
            tags$style(HTML("
                            .shiny-output-error-AVISO {
                            color: orange;
                            }
                            "))
            ),
          
          
          
          # Version ####
          navbarPage("App Inventário Florestal 2.1.0",id="tab",
          # ####           
                     theme = "green_yeti2.css",
                     # theme = "green.css", # seleciona um tema contido na pasta www
                     # theme = shinythemes::shinytheme("paper"), # seleciona um tema utilizando pacote
                     
                     # Painel Intro ####          
                     tabPanel( "Intro" ,
                               fluidRow(
                                 column(5,
                                        includeMarkdown("about.md")
                                 ),
                                 column(6,
                                        img(contentType = "image/jpg",
                                            src="intro_picture.jpg",
                                            width = 770,
                                            #           height = 750)
                                            height = 856)
                                        
                                 )
                               ) # fluid row
                     ), # Painel Intro             
                     
                     
                     # Upload de dados ####
                     tabPanel("Importação",
                              sidebarLayout(
                                
                                sidebarPanel(
                                  
                                  h3("Dados"),
                                  
                                  radioButtons("df_select", 
                                               "Fazer o upload de um arquivo, ou utilizar o dado de exemplo?", 
                                               c("Fazer o upload", 
                                                 "Utilizar o dado de exemplo em nivel de fuste", 
                                                 "Utilizar o dado de exemplo em nivel de arvore", 
                                                 "Utilizar o dado de exemplo em nivel de parcela"), 
                                               selected = "Fazer o upload"),
                                  
                                  
                                  radioButtons("df", 
                                               "Tipo da base de dados:", 
                                               choices = c("Dados em nivel de fuste",
                                                           "Dados em nivel de arvore",
                                                           "Dados em nivel de parcela"),
                                               selected = "Dados em nivel de arvore"),
                                  
                                  
                                  uiOutput("upload"), # tipos de arquivos aceitos
                                  hr(),
                                  uiOutput("upload_csv"), # tipos de arquivos aceitos
                                  uiOutput("upload_xlsx") # tipos de arquivos aceitos
                                  
                                  
                                ), # sidebarPanel
                                
                                mainPanel(
                                  DT::dataTableOutput("rawdata")
                                ) # mainPanel
                              ) # sidebarLayout
                     ),
                     
                     # Mapeamento ####
                     tabPanel("Mapeamento de variáveis",
                              fluidPage(
                                
                                #h1("Shiny", span("Widgets Gallery", style = "font-weight: 300"), 
                                h1("Definição dos nomes das variáveis", 
                                   style = "text-align: center;"),
                                br(),
                                
                                #  h4("Nesta aba serão indicados os nomes das colunas que serão utilizadas nas análises em todo o app"),
                                fluidRow(
                                  
                                  column(4,
                                         wellPanel(
                                           h3("Circunferência (CAP)*"),
                                           p("Selecione o nome da variável referente à CAP:"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_cap")
                                         )), # Coluna dap
                                  
                                  column(4,
                                         wellPanel(
                                           h3("Diâmetro (DAP)*"),
                                           p("Selecione o nome da variável referente à DAP:"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_dap")
                                         )), # Coluna dap
                                  
                                  column(4,
                                         wellPanel(
                                           h3("Altura total*"),
                                           p("Selecione o nome da variável referente à altura total:"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_ht")
                                         )) # Coluna ht
                                  
                                  
                                ), # fluidRow 1
                                
                                
                                fluidRow(
                                  
                                  column(4,
                                         wellPanel(
                                           h3("Arvore"),
                                           p("Selecione o nome da variável referente à árvore:"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_arvore")
                                         )),
                                  
                                  column(4,
                                         wellPanel(
                                           h3("Parcela*"),
                                           p("Selecione o nome da variável referente à parcela:"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_parcelas")
                                         )),
                                  
                                  
                                  column(4,
                                         wellPanel(
                                           h3("Área da parcela"),
                                           p("Selecione o nome da variável referente à área da parcela (m²):"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_area.parcela")
                                         )) # Coluna area.parcela
                                  
                                  
                                ), # fluidRow 2  
                                
                                
                                fluidRow(
                                  
                                  
                                  column(4,
                                         wellPanel(
                                           h3("Área total"),
                                           p("Selecione o nome da variável referente à área total (ha):"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_area.total")
                                         )), # Coluna area.total
                                  
                                  
                                  column(4,
                                         wellPanel(
                                           h3("Estrato"),
                                           p("Selecione o nome da variável referente à estrato:"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_estrato")
                                         )), # Coluna area.total
                                  
                                  
                                  column(4,
                                         wellPanel(
                                           h3("Observações"),
                                           p("Selecione o nome da variável referente à observacao ou qualidade das arvores:"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_obs")
                                         )) # Coluna obs
                                  
                                ), # fluidRow 3
                                
                                fluidRow(
                                  
                                  column(4,
                                         wellPanel(
                                           h3("Altura dominante"),
                                           p("Selecione o nome da variável referente à altura dominante"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_hd")
                                         )), # Coluna altura dominante
                                  
                                  
                                  column(4,
                                         wellPanel(
                                           h3("Idade"),
                                           p("Selecione o nome da variável referente à idade:"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_idade")
                                         )), # Coluna idade
                                  
                                  
                                  column(4,
                                         wellPanel(
                                           h3("Volume com casca"),
                                           p("Selecione o nome da variável referente à volume com casca:"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_vcc")
                                         )) # Coluna vcc
                                  
                                  
                                ),# fluidRow 4
                                
                             fluidRow(
                               column(4,
                                      wellPanel(
                                        h3("Volume sem casca"),
                                        p("Selecione o nome da variável referente à Volume sem casca:"#, 
                                          #style = "font-family: 'Source Sans Pro';"
                                        ),
                                        uiOutput("selec_vsc")
                                      )) # Coluna vsc
                               
                               )#fluidRow 5   
                                
                              ) # fluidPage 
                              
                              
                     ),# tabPanel Mapeamento
                     
                     # tabPanel Preparação ####
                     tabPanel("Preparação", 
                              
                              
                              fluidPage(
                                
                                fluidRow(
                                  
                                  h1("Preparação dos dados",style = "text-align: center;"),
                                  br()
                                ),
                                
                                
                                fluidRow(
                                  
                                  sidebarPanel(
                                    h3("Variaveis para graficos de classe de diametro"),
                                    h4("Intervalo de classe"),
                                    numericInput("int.classe.dap", "Insira o intervalo de classe:", 2, 1, 50, 0.5),
                                    
                                    h4("Diâmetro mínimo"),
                                    numericInput("diam.min", "Insira o diâmetro mínimo:", 1, 1, 100, 1),
                                    
                                    h3("Variaveis para grafico de classe de altura"),
                                    h4("Intervalo de classe"),
                                    numericInput("int.classe.ht", "Insira o intervalo de classe:", 2, 1, 50, 0.5),
                                    
                                    h4("Altura mínima"),
                                    numericInput("ht.min", "Insira o diâmetro mínimo:", 10, 1, 100, 1),
                                    
                                    
                                    h3("Transformar zero em NA"),
                                    radioButtons("zero_to_NA","Transformar zeros em variávies numéricas em NA? (recomendado)",c("Sim"=TRUE,"Nao"=FALSE), inline = TRUE),
                                    
                                    h3("Filtrar dados"),
                                    
                                    uiOutput("rm_data_var"),
                                    uiOutput("rm_data_level"),
                                    uiOutput("rm_vars"),
                                    uiOutput("selec_area_parcela_num"),
                                    uiOutput("selec_area_total_num"),
                                    uiOutput("est_hd1"),
                                    uiOutput("est_hd2"),
                                    uiOutput("est_hd3"),
                                    uiOutput("consist_warning1")
                                    
                                    
                                    
                                  ),# sidebarPanel
                                  
                                  mainPanel( tabsetPanel(
                                    tabPanel("Dado pos preparação",
                                             shiny::htmlOutput("avisos_prep"),
                                             DT::dataTableOutput("prep_table"),
                                             hr(),
                                             tableOutput("teste")
                                             
                                    ),
                                    tabPanel("Dados inconsistentes",
                                             uiOutput("consist_warning2"),
                                             uiOutput("consist_table_help"),
                                             uiOutput("consist_choice"),
                                             DT::dataTableOutput("consist_table")
                                    )
                                    
                                  ))# mainPanel
                                  
                                  
                                )
                                
                              )
                              
                              
                              
                              
                     ), # tabPanel filtrar dados
                     # Totalização de fustes ####
                     tabPanel("Totalização de fustes",
                              
                              fluidPage(
                                
                                h1("Totalização de fustes", style = "text-align: center;"),
                                br(),
                                
                                DT::dataTableOutput("tot_fuste_tab")
                              )
                     ),# tabpanel Totalização de fustes

                     # tabPanel Estimativas de altura e volume ####
                     
                     tabPanel("Altura e volume",
                              
                              fluidPage(
                                
                                h1("Estimativas de altura e volume", style = "text-align: center;"),
                                br(),
                                
                                fluidRow(
                                  column(2,uiOutput("ui_ht_est" ),
                                         radioButtons("ajuste_p_estrato",
                                         "Ajustar modelos por estrato?",
                                         c("Sim"=TRUE,"Nao"=FALSE), selected = FALSE,inline = TRUE)
                                         ),
                                  column(3,uiOutput("ui_estvcc1")),
                                  column(2,uiOutput("ui_estvcc3"),uiOutput("ui_estvcc4")),
                                  
                                  column(3,uiOutput("ui_estvsc1")),
                                  column(2,uiOutput("ui_estvsc3"),uiOutput("ui_estvsc4"))
                                  
                                  
                                ),
                                
                                fluidRow(column(6,uiOutput("aviso_ajuste"))),
                                
                                tabsetPanel(
                                  tabPanel("Tabela de estimativas", DT::dataTableOutput("HtVolEstData_table")),
                                  tabPanel("Gráfico dos resíduos dos modelos hipsométricos",
                                           plotOutput("ht_scatter_plot" ,height = "550px"),
                                           plotOutput("ht_hist_plot" ,height = "550px"),
                                           plotOutput("ht_vs_plot" ,height = "550px")
                                  )
                                )
                                )
                              
                       
                     ), # Estimativas de altura e volume end
                     
                     
                     # tabPanel Distribuições e gráficos ####
                                # tabPanel("Distribuição diamétrica",
                                  tabPanel("Análise descritiva",
                                                   
                                          fluidPage(
                                          #  h1("Distribuição diamétrica (DD)", style = "text-align: center;"),
                                            h1("Análise descritiva", style = "text-align: center;"),
                                            br(),
                                            tabsetPanel(
                                              
                                              tabPanel("Tabela da distribuição diamétrica"                   , DT::dataTableOutput("dd_geral_tab") ),
                                              tabPanel("Gráfico do Nº de indivíduos por ha por classe diamétrica" , plotOutput("dd_graph_indv",height = "550px") ),
                                              tabPanel("Gráfico do volume por ha por classe diamétrica"      , plotOutput("dd_graph_vol" ,height = "550px")),
                                              tabPanel("Gráfico de G por ha por classe diamétrica"           , plotOutput("dd_graph_G"   ,height = "550px")),
                                              
                                              tabPanel("Tabela da distribuição de Altura"                    , DT::dataTableOutput("dist_ht_tabela") ),
                                              tabPanel("Gráfico dos indivíduos por ha por classe de altura"  , plotOutput("dist_ht_plot",height = "550px") ),
                                              
                                              
                                              tabPanel("Tabela de frequência para a variável Qualidade"      , DT::dataTableOutput("obs_tabela") ),
                                              tabPanel("Gráfico de frequencia variável Qualidade"            , plotOutput("obs_plot_1"   ,height = "550px")),
                                              tabPanel("Gráfico da porcentagem da variável Qualidade"        , plotOutput("obs_plot_2"   ,height = "550px"))
                                            )
                                            
                                          )
                                          
                                 ), # tabPanel DD 
                                 
                     # tabPanel inventario florestal ####
                                 tabPanel("Inventário",
                                          
                                          fluidPage(
                                            
                                            h1("Inventário florestal", style = "text-align: center;"),
                                            br(),
                                            
                        # ####
                                            fluidRow(
                                              
                                              column(2,
                                                     sliderInput("alpha_inv", 
                                                                 label = "Selecione o nível de significância:", 
                                                                 min = 0.01, 
                                                                 max = 0.10, 
                                                                 value = 0.10,
                                                                 step = 0.01)
                                              ),
                                              
                                              column( 2,  sliderInput("erro_inv", 
                                                                      label = "Selecione o erro admitido (%):", 
                                                                      min = 1, 
                                                                      max = 20, 
                                                                      value = 10,
                                                                      step = 1)),
                                              
                                              column(2,
                                                     sliderInput("cd_inv", 
                                                                 label = "Selecione o nº de casas decimais:", 
                                                                 min = 0, 
                                                                 max = 10, 
                                                                 value = 4,
                                                                 step = 1)
                                              ),
                                              
                                              column(2,
                                                     radioButtons(
                                                       inputId='pop_inv', # Id
                                                       label='Considerar a população infinita ou finita?', # nome que sera mostrado na UI
                                                       choices=c(Infinita="inf", Finita="fin"), # opcoes e seus nomes
                                                       selected="inf",
                                                       inline = T)
                                              ),
                                              
                                              column(3,
                                                     uiOutput("acs_estrato_rb"), uiOutput("as_estrato_rb"),
                                                     uiOutput("acs_as_warning")
                                                     )
                                              
                                            ),
                        
                                            fluidRow(
                                              radioButtons("yi_inv",
                                                           label="Selecione a variável utilizada nas estatísticas:",
                                                           choices = c("Indv", "G","VCC", "VSC"),
                                                           selected = "VCC",
                                                           inline=T )
                                                     ),
                                            fluidRow(   
                                              tabsetPanel(id="tabset_inv",
                                                tabPanel("Totalização de parcelas",DT::dataTableOutput("tot_parc_tab") ) , 
                                                tabPanel("Amostragem casual simples",DT::dataTableOutput("acs") ), 
                                                tabPanel("Amostragem casual estratificada",DT::dataTableOutput("ace1"),br(),DT::dataTableOutput("ace2") ), 
                                                tabPanel("Amostragem sistemática",DT::dataTableOutput("as") ) )
                                            )
                        # ####
                                            
                                          )
                                 ),# TabPanel Inventario
                                 
                                 
                     # navbarMenu  Download ####
                     tabPanel("Download",
                              # Painel Download Tabelas ####
                              
                              fluidPage(
                                
                                
                                h1("Download dos resultados", style = "text-align: center;"),
                                br(),
                                
                                
                                tabsetPanel(
                                  tabPanel("Download de tabelas", 
                                           fluidPage(
                                             
                                             
                                             h2("Download de tabelas", style = "text-align: center;"),
                                             br(),
                                             
                                             fluidRow(
                                               column(
                                                 10
                                                 ,uiOutput("checkbox_df_download")
                                               )
                                               
                                             ),
                                             br(),
                                             
                                             fluidRow(column(3,downloadButton('downloadData', 'Baixar tabelas selecionadas'), offset=4)),
                                             br(),
                                             h3("Ou, para baixar todas as tabelas disponíveis, clique abaixo:"),
                                             fluidRow(
                                               column(3,downloadButton('downloadAllData', 'Baixar todas as tabelas'), offset=4)
                                             )
                                             
                                             
                                             
                                           )
                                  ), # download tabelas
                                  
                                  # Painel Download Graficos ####
                                  
                                  tabPanel("Download de graficos", 
                                           
                                           
                                           
                                           sidebarLayout(
                                             
                                             sidebarPanel(
                                               
                                               tags$style(type="text/css",
                                                          ".recalculating {opacity: 1.0;}"
                                               ),
                                               
                                               h3("Download de graficos"),
                                               
                                               selectInput("graph_d", "Escolha um grafico:", 
                                                           choices = c(
                                                             "Indv. por ha por CC",
                                                             "Vol. por ha por CC",
                                                             "G por ha por CC",
                                                             "Indv. por ha por classe de ht",
                                                             "Frequencia para var. Qualidade",
                                                             "Porcentagem para var. Qualidade",
                                                             "Dispersao dos residuos em porcentagem para HT",
                                                             "Histograma dos residuos em porcentagem para HT",
                                                             "HT vs HT estimada"
                                                             )),
                                               
                                               selectInput("graphformat",
                                                           "Escolha o formato do gráfico:",
                                                           choices = c("PNG" = ".png",
                                                                       "JPG" = ".jpg",
                                                                       "PDF" = ".pdf") ),
                                               
                                               downloadButton('downloadGraph', 'Download')
                                               
                                             ),
                                             mainPanel(
                                               plotOutput("graph_d_out",height = "550px")
                                             )
                                           )
                                  ) # download graficos
                                  
                                )       
                              ) # fluidPage
                     ) # final navbarMenu download ####    
                     # final da UI  ####    
          ) # navbarPage
            )#tagList
            ) # ShinyUI



