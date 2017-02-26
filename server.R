
library(shiny)
library(DT)
library(formattable)
library(tidyr)
library(dplyr)
library(lazyeval)
library(xlsx)
library(xlsxjars)
library(markdown)

# functions ####

ex1 <- read.csv2("examples/nivel_arvore/ex1.csv")
ex2 <- read.csv2("examples/nivel_parcela/ex_livro_dendro_ACE_def.csv")

source("funs/acs.R"        , encoding="UTF-8")
source("funs/ace.R"        , encoding="UTF-8")
source("funs/as_diffs.R"   , encoding="UTF-8")
source("funs/inv_summary.R", encoding="UTF-8")
source("funs/round_df.R"   , encoding="UTF-8")

# vectors for names ####

especies_names <- c("scientific.name","Scientific.Name","SCIENTIFIC.NAME" ,"scientific_name", "Scientific_Name","SCIENTIFIC_NAME","nome.cientifico", "Nome.Cientifico","NOME.CIENTIFICO","nome_cientifico", "Nome_Cientifico","NOME_CIENTIFICO")
parcelas_names <- c("transect", "Transect", "TRNASECT", "transect.code","Transect.Code","TRANSECT.CODE","transect_code","Transect_Code","TRANSECT_CODE","parcela", "Parcela","PARCELA","cod.parcela","Cod.Parcela","COD.PARCELA", "cod_parcela","Cod_Parcela","COD_PARCELA")
est.vertical_names <- c("canopy", "canopy_09")
est.interno_names <- c("light", "light_09")

DAP_names <- c("DAP","Dap","dap", "dbh", "Dbh","DBH","DBH_11")
HT_names <- c("HT_EST", "HT", "Ht", "ht","Htot","ALTURA","Altura","Altura_Total", "ALTURA_TOTAL")
VCC_names <- c("VCC","Vcc", "vcc", "VOL", "Vol", "VOLUME")
area_parcela_names <- c("AREA_PARCELA","Area_Parcela","area_parcela", "AREAPARCELA", "areaparcela", "transect.area", "Transect.Area", "TRANSECT.AREA","transect_area","Transect_Area","TRANSECT_AREA")
area_total_names <- c("AREA_TOTAL", "AREATOTAL", "area_total", "areatotal","AREA_TALHAO", "AREATALHAO", "area_talhao", "areatalhao","total.area","Total.Area","TOTAL.AREA","total_area","Total_Area","TOTAL_AREA")
idade_names <- c("IDADE", "Idade","idade")
VSC_names <- c("VSC","Vsc", "vsc")
HD_names <- c("HD", "Hd", "hd", "ALTURA_DOMINANTE", "ALT_DOM")
grupos_names <- c(c("TALHAO", "PARCELA"), c("area.code", "transect"))
estratos_names <- c("TALHAO", "Talhao", "talhao","COD_TALHAO","Cod_Talhao","cod_talhao", "COD.TALHAO", "Cod.Talhao","cod.talhao", "area.code", "Area.Code","AREA.CODE", "area_code","Area_Code","AREA_CODE")

# Server ####

shinyServer(function(input, output, session) {
   
  # Importar os dados ####
  
  output$upload <- renderUI({
    
    validate(need(input$df_select == "Fazer o upload", "" )  )
    
    list(    

      radioButtons("df", 
                   "Tipo da base de dados:", 
                   choices = c("Dados em nivel de arvore",
                               "Dados em nivel de parcela"),
                   selected = "Dados em nivel de arvore" ),
      
      fileInput( # input de arquivos
        inputId = "file1", # Id
        
        label = "Selecione o arquivo: (.csv, .txt ou .xlsx)", # nome que sera mostrado na UI
        
        accept=c('text/csv/xlsx','.csv', ".txt", ".xlsx")),
      
      checkboxInput(inputId = "excel",
                    label = "Excel (.xls ou .xslx) ?",
                    value = F),
      
      div("Recomendamos o uso do formato .csv", style = "color:blue"),
      
      
      
      radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
        inputId='sep',  #Id
        label='Separador:', # nome que sera mostrado na UI
        choices=c(Virgula=',', "Ponto e Virgula"=';', Tab='\t'), # opcoes e seus nomes
        selected=';'), # valor que sera selecionado inicialmente
      
      radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
        inputId='dec', # Id
        label='Decimal:', # nome que sera mostrado na UI
        choices=c(Ponto=".", Virgula=","), # opcoes e seus nomes
        selected=","), # valor que sera selecionado inicialmente
      
      
      
      actionButton( # botao que o usuario clica, e gera uma acao no server
        "Load", # Id
        "Carregue o arquivo")
      
      
      
      
    )
    
    
  })
  
  upData <- reactive({ # Criamos uma nova funcao reactive. este sera o objeto filtrado, utilizado nos calculos
    
    if(input$Load==0){return()} # se o botao load nao for pressionado(==0), retornar nada
    else(inFile <- input$file1) # caso contrario, salvar o caminho do arquivo carregado em inFile
    
    # input$file1 sera NULL inicialmente. apos o usuario selecionar
    # e upar um arquivo, ele sera um data frame com as colunas
    # 'size', 'type', e 'datapath' . A coluna 'datapath' 
    # ira conter os nomes dos arquivos locais onde o dado pode ser encontrado
    
    if (is.null(inFile)){return(NULL)} # se o arquivo nao for carregado, retornar null
    else if(input$excel == F)
    {
      raw_data <- read.csv(inFile$datapath, header=TRUE, sep=input$sep, dec=input$dec,quote='"')
    } else {file.copy(inFile$datapath,
                      paste(inFile$datapath, "xlsx", sep="."));
      raw_data <- readxl::read_excel(paste(inFile$datapath, "xlsx", sep="."), 1)  }
    
    # Carregamos o arquivo em um objeto
    
    
    raw_data # tabela final a ser mostrada. 
    
  })
  
  rawData <- reactive({
    

    switch(input$df_select, 
           "Fazer o upload" = if(is.null(input$file1)){return()}else{upData()},
           "Utilizar o dado de exemplo em nivel de arvore" = ex1,
           "Utilizar o dado de exemplo em nivel de parcela" = ex2)
    
  })
  
  output$rawdata <- renderDataTable({ # renderizamos uma DT::DataTable
    
    # salvamos a funcao newData, que contem o arquivo carregado pelo usuario em um objeto
    data <- rawData() 
    
    datatable(data,
              
              options = list(
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                  "}")
              )
              
              ) # Criamos uma DT::datatable com base no objeto
    
    # Este arquivo e reativo, e ira se alterar caso o usuario
    # aperte o botao input$columns
    
  })
  
  
  # Dado utilizado no inventario ####
  
  # switch que muda o dado a ser utilizado
  invData <- reactive({
    
    if(is.null(input$df)){ return()}
    
    if(input$df_select == "Utilizar o dado de exemplo em nivel de parcela"){return(rawData())}
    
    switch(input$df, 
           "Dados em nivel de arvore" = newData(),
           "Dados em nivel de parcela" = rawData() )
    
  })

  # Totalização de Parcelas ####
  
  # dados / funcao inv_summary
  newData <- reactive({
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" ),
             need(input$df_select != "Utilizar o dado de exemplo em nivel de parcela", "Base de dados incompativel"))
    
    if(input$Loadnew){    
      
      dados <- rawData()
      
      x <- inv_summary(df           = dados, 
                       DAP          = input$DAPnew, 
                       HT           = input$HTnew,
                       VCC          = input$VCCnew,
                       area_parcela = input$area_parcelanew,
                       groups       = input$gruposnew,
                       area_total   = input$area_totalnew,
                       idade        = input$idadenew,
                       VSC          = input$VSCnew,
                       Hd           = input$Hdnew)
      
      x
      
    }
    
  })
  
  # UI
  output$tot_parc_ui1 <- renderUI({
    
    data <- rawData()
    
    list(
      
      h3("Totalização de Parcelas"),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'DAPnew', # Id
        "Selecione a coluna do DAP (cm):", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        selected = DAP_names,
        options = list(
          placeholder = 'selecione uma coluna abaixo'# ,
          # onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'HTnew', # Id
        "Selecione a coluna da altura (m):", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        selected = HT_names,
        options = list(
          placeholder = 'selecione uma coluna abaixo'#,
          # onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'VCCnew', # Id
        "Selecione a coluna do volume com casca (m³):", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        selected = VCC_names,
        options = list(
          placeholder = 'selecione uma coluna abaixo'#,
          #onInitialize = I('function() { this.setValue(""); }')
        ) # options
      )
      
      
      
    )
    
  })
  
  output$tot_parc_ui2 <- renderUI({
    
    data <- rawData()
    
    list(
      
      
      switch(input$area_radio_new,
             "Manualmente" =  numericInput("area_parcelanew", 
                                           label = "Insira o valor da área da parcela (m²):",
                                           value = 810),
             
             "Lista de colunas" = selectizeInput("area_parcelanew",
                                                 label = "Selecione a coluna da área da parcela (m²):",
                                                 choices = names(data),
                                                 selected = area_parcela_names,
                                                 options = list(
                                                   placeholder = 'Selecione uma coluna abaixo:'#,
                                                  # onInitialize = I('function() { this.setValue(""); }')
                                                 ) # options    
             )# selectize
      ),
      
      switch(input$area_radio_new,
             "Manualmente" =  numericInput("area_totalnew", 
                                           label = "Insira o valor da área total (ha):",
                                           value = 45),
             
             "Lista de colunas" = selectizeInput("area_totalnew",
                                                 label = "Selecione a coluna da área total (ha)",
                                                 choices = names(data),
                                                 selected = area_total_names,
                                                 options = list(
                                                   placeholder = 'Selecione uma coluna abaixo:'#,
                                                  # onInitialize = I('function() { this.setValue(""); }')
                                                 ) # options    
             )# selectize
      ),
      
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'gruposnew', # Id
        "selecione as variáveis pivô:", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        multiple = TRUE,
        selected = grupos_names,
        options = list(
          placeholder = 'selecione uma coluna abaixo'#,
          #onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      
      h3("Variaveis opcionais:"),
      
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'idadenew', # Id
        "Selecione a coluna da idade:", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        #selected = idade_names,
        options = list(
          placeholder = 'selecione uma coluna abaixo',
          onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'VSCnew', # Id
        "selecione a coluna do volume sem casca (m³):", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        # selected = VSC_names,
        options = list(
          placeholder = 'selecione uma coluna abaixo',
          onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'Hdnew', # Id
        "Selecione a coluna da altura dominante (m):", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        # selected = HD_names,
        options = list(
          placeholder = 'selecione uma coluna abaixo',
          onInitialize = I('function() { this.setValue(""); }')
        ) # options
      )    
      
      
    )
    
    
  })
  
  
  # tabela
  output$newdata <- renderDataTable({ # renderizamos uma DT::DataTable
    
    data <- newData() 
    
    if(input$Loadnew)
    {
      datatable(data,
                
                options = list(
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                    "}")
                )
      ) # Criamos uma DT::datatable com base no objeto
    }
    
  })
  
  
  # ACS ####
  
  # funcao acs aplicada em invData
  tabacs <- reactive({
    
    if(input$Loadacs){
      
      dados <- invData()
      
      x <-     acs(df             = dados,
                   VCC            = input$VCCacs,
                   area_parcela   = input$area_parcelaacs,
                   area_total     = input$area_totalacs, 
                   idade          = input$idadeacs,
                   grupos         = input$gruposacs, 
                   alpha          = input$alphaacs, 
                   Erro           = input$erroacs, 
                   casas_decimais = input$cdacs, 
                   pop            = input$popacs, 
                   tidy           = input$tidyacs)
      
      x}
    
  })
  
  # UI: as opcoes (choices) sao os nomes de invData
  
  
  output$acs_ui1 <- renderUI({
    
    data <- invData()
    
    list(
      
      h3("Amostragem Casual Simples"),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'VCCacs', # Id
        "Selecione a coluna do volume (m³):", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        selected = VCC_names,     
        options = list(
          placeholder = 'selecione uma coluna abaixo'#,
          # onInitialize = I('function() { this.setValue(""); }')
        ) # options
      )
      
    )
  })
  output$acs_ui2 <- renderUI({
    
    data <- invData()
    
    list(
      
      switch(input$area_radio_acs,
             "Manualmente" =  numericInput("area_parcelaacs", 
                                           label = "Insira o valor da área da parcela (m²):",
                                           value = 810),
             
             "Lista de colunas" = selectizeInput("area_parcelaacs",
                                                 label = "Selecione a coluna da área da parcela (m²):",
                                                 choices = names(data),
                                                 selected = area_parcela_names,     
                                                 options = list(
                                                   placeholder = 'Selecione uma coluna abaixo:'#,
                                                   #onInitialize = I('function() { this.setValue(""); }')
                                                 ) # options    
             )# selectize
      ),
      
      switch(input$area_radio_acs,
             "Manualmente" =  numericInput("area_totalacs", 
                                           label = "Insira o valor da área total (ha):",
                                           value = 45),
             
             "Lista de colunas" = selectizeInput("area_totalacs",
                                                 label = "Selecione a coluna da área total (ha):",
                                                 choices = names(data),
                                                 selected = area_total_names,     
                                                 options = list(
                                                   placeholder = 'Selecione uma coluna abaixo:'#,
                                                   #  onInitialize = I('function() { this.setValue(""); }')
                                                 ) # options    
             )# selectize
      ),
      
      h4("Variaveis opcionais:"),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'idadeacs', # Id
        "Selecione a coluna da idade:", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        #selected = idade_names,     
        options = list(
          placeholder = 'selecione uma coluna abaixo',
          onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'gruposacs', # Id
        "Selecione as variáveis pivô:", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        multiple = TRUE,  # permite mais de uma opcao ser selecionada
        selected = NULL,     
        options = list(
          placeholder = 'Selecione as variaveis abaixo',
          onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      sliderInput("erroacs", 
                  label = "Selecione o erro admitido (%):", 
                  min = 1, 
                  max = 20, 
                  value = 10,
                  step = 1),
      
      sliderInput("alphaacs", 
                  label = "Selecione o nível de significância:", 
                  min = 0.01, 
                  max = 0.10, 
                  value = 0.05,
                  step = 0.01),
      
      sliderInput("cdacs", 
                  label = "Selecione o nº de casas decimais:", 
                  min = 0, 
                  max = 10, 
                  value = 4,
                  step = 1),
      
      radioButtons(
        inputId='popacs', # Id
        label='Considerar a população infinita ou finita?', # nome que sera mostrado na UI
        choices=c(Infinita="inf", Finita="fin"), # opcoes e seus nomes
        selected="inf"
      ),
      
      radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
        inputId="tidyacs",  #Id
        label='Selecione o arranjo da tabela:', # nome que sera mostrado na UI
        choices=c(Vertical = T, Horizontal = F), # opcoes e seus nomes
        selected=T) # valor que sera selecionado inicialmente
      
    )
  })
  
  # tabela
  output$acs <- renderDataTable({
    
    acsdt <- tabacs() 
    
    if(input$Loadacs)
    {
      # converte em datatable        # cria formattable
      as.datatable( formattable(acsdt, 
                                list(    # colore a linha 6 da coluna dois de verde ou vemelho, se ela for menor ou maior que o numero da linha 1 coluna 2
                                  area(row=6, col=2) ~  formatter("span", 
                                                                  style = x ~ formattable::style(color = ifelse(x <= acsdt[1,2], "#108e00", "red"))) ,
                                  # colore o erro estimado de verde ou vemelho, se ela for menor ou maior que o erro desejado
                                  area(row=10, col=2) ~ formatter("span", 
                                                                  style = x ~ formattable::style(color = ifelse(x <= input$erroacs, "#108e00", "red")))
                                  
                                  
                                )#list
                         ), #formattable
                    # pre seleciona linhas
                 selection = list(mode = 'multiple', selected = c(6,10,15,16), target = 'row'),
                 options = list(searching = FALSE,
                                paging=FALSE,
                                initComplete = JS( # muda cor do cabecalho
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                  "}")
                 ) 
                 
                 
                 
                 ) #as.datatable
                 
      
    } 
    
  })
  
  # ACE ####
  
  # resultado 1 da funcao ace aplicada em invData
  tabace1 <- reactive({
    
    if(input$Loadace){
      
      dados <- invData()
      
      x <- ace(df             = dados, 
               VCC            = input$VCCace, 
               area_parcela   = input$area_parcelaace, 
               area_estrato   = input$area_estratoace, 
               grupos         = input$gruposace, 
               idade          = input$idadeace, 
               alpha          = input$alphaace, 
               Erro           = input$erroace, 
               casas_decimais = input$cdace, 
               pop            = input$popace, 
               tidy           = input$tidyace)[[1]]
      x
    }
    
  })
  
  # resultado 2 da funcao ace aplicada em invData
  tabace2 <- reactive({
    
    if(input$Loadace){ 
      
      dados <- invData()
      
      x <- ace(df = dados, 
               VCC            = input$VCCace, 
               area_parcela   = input$area_parcelaace , 
               area_estrato   = input$area_estratoace, 
               grupos         = input$gruposace, 
               idade          = input$idadeace, 
               alpha          = input$alphaace, 
               Erro           = input$erroace, 
               casas_decimais = input$cdace, 
               pop            = input$popace, 
               tidy           = input$tidyace)[[2]]
      
      x
    }
    
  })
  
  # UI: as opcoes (choices) sao os nomes de invData
  
  output$ace_ui <- renderUI({
    
    data <- invData()
    
    list(
      
      h3("Amostragem Casual Estratificada"),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'VCCace', # Id
        "Selecione a coluna do volume (m³):", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        selected = VCC_names,     
        options = list(
          placeholder = 'selecione uma coluna abaixo'#,
          # onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'area_parcelaace', # Id
        "Selecione a coluna da área da parcela (m²):", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        selected = area_parcela_names,     
        options = list(
          placeholder = 'selecione uma coluna abaixo'#,
          # onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'area_estratoace', # Id
        "Selecione a coluna da área total (ha):", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        selected = area_total_names,     
        options = list(
          placeholder = 'selecione uma coluna abaixo'#,
          # onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'gruposace', # Id
        "Selecione a(s) coluna(s) para estratificação:", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        multiple = TRUE,  # permite mais de uma opcao ser selecionada
        selected = estratos_names,     
        options = list(
          placeholder = 'Selecione as variaveis abaixo'#,
          # onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      h4("Variaveis opcionais:"),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'idadeace', # Id
        "Selecione a coluna da idade:", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        # selected = idade_names,     
        options = list(
          placeholder = 'selecione uma coluna abaixo',
          onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      
      sliderInput("erroace", 
                  label = "Selecione o erro admitido (%):", 
                  min = 1, 
                  max = 20, 
                  value = 10,
                  step = 1),
      
      sliderInput("alphaace", 
                  label = "Selecione o nível de significância:", 
                  min = 0.01, 
                  max = 0.10, 
                  value = 0.05,
                  step = 0.01),
      
      sliderInput("cdace", 
                  label = "Selecione o nº de casas decimais:", 
                  min = 0, 
                  max = 10, 
                  value = 4,
                  step = 1),
      
      radioButtons(
        inputId='popace', # Id
        label='Considerar a população infinita ou finita?', # nome que sera mostrado na UI
        choices=c(Infinita="inf", Finita="fin"), # opcoes e seus nomes
        selected="inf"
      ),
      
      radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
        inputId="tidyace",  #Id
        label='Selecione o arranjo da tabela:', # nome que sera mostrado na UI
        choices=c(Vertical = T, Horizontal = F), # opcoes e seus nomes
        selected=T) # valor que sera selecionado inicialmente
      
    )
  }) 
  
  # tabela ace1
  output$ace1 <- renderDataTable({
    
    ace1dt <- tabace1() 
    
    if(input$Loadace)
    {
      datatable( ace1dt, # seleciona a linha 5 previamente
                 selection = list(mode = 'multiple', selected = c(13,17,18,19), target = 'row'),
                 options = list(searching = FALSE,
                                paging=FALSE,
                                initComplete = JS( # muda a cor do cabecalho
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                  "}")
                 )   
                 
      )
    } 
    
  })
  
  # tabela ace2
  output$ace2 <- renderDataTable({
    
    ace2dt <- tabace2() 
    
    if(input$Loadace)
    {
      # converte em datatable        # cria formattable
      as.datatable( formattable(ace2dt, 
                                list(
                                  # colore o erro estimado de verde ou vemelho, se ela for menor ou maior que o erro desejado
                                  area(row=5, col=2) ~ formatter("span", 
                                                                  style = x ~ formattable::style(color = ifelse(x <= input$erroace, "#108e00", "red")))
                                  
                                  
                                )#list
      ), #formattable
      # pre seleciona linhas
      selection = list(mode = 'multiple', selected = c(5,10,11), target = 'row'),
      options = list(searching = FALSE,
                     paging=FALSE,
                     initComplete = JS( # muda cor do cabecalho
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                       "}")
      ) 
      
      
      
      )
      
      
      
      
    } 
    
  })
  
  # AS ####
  
  # funcao as aplicado em invData
  tabas <- reactive({
    
    if(input$Loadas){ 
      
      dados <- invData()
      
      x <- as_diffs(df             = dados, 
                    VCC            = input$VCCas,
                    area_parcela   = input$area_parcelaas ,
                    area_total     = input$area_totalas,
                    idade          = input$idadeas,
                    grupos         = input$gruposas,
                    alpha          = input$alphaas,
                    Erro           = input$erroas,
                    casas_decimais = input$cdas,
                    tidy           = input$tidyas)
      
      x
    }
    
  }) 
  
  # UI: as opcoes (choices) sao os nomes de invData
  
  output$as_ui1 <- renderUI({
    
    data <- invData()
    
    list(
      
      h3("Amostragem Sistematica"),
      
      helpText("Utiliza-se o método das diferenças sucessivas, portanto, assume-se que os dados estão organizados de forma ordenada."),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'VCCas', # Id
        "Selecione a coluna do volume (m³):", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        selected = VCC_names,     
        options = list(
          placeholder = 'selecione uma coluna abaixo'#,
          # onInitialize = I('function() { this.setValue(""); }')
        ) # options
      )
      
    )
  })
  output$as_ui2 <- renderUI({
    
    data <- invData()
    
    list(
      
      
      switch(input$area_radio_as,
             "Manualmente" =  numericInput("area_parcelaas", 
                                           label = "Insira o valor da área da parcela (m²):",
                                           value = 810),
             
             "Lista de colunas" = selectizeInput("area_parcelaas",
                                                 label = "Selecione a coluna da área da parcela (m²):",
                                                 choices = names(data),
                                                 selected = area_parcela_names,     
                                                 options = list(
                                                   placeholder = 'Selecione uma coluna abaixo:'#,
                                                   #onInitialize = I('function() { this.setValue(""); }')
                                                 ) # options    
             )# selectize
      ),
      
      switch(input$area_radio_as,
             "Manualmente" =  numericInput("area_totalas", 
                                           label = "Insira o valor da área total (ha):",
                                           value = 45),
             
             "Lista de colunas" = selectizeInput("area_totalas",
                                                 label = "Selecione a coluna da área total (ha):",
                                                 choices = names(data),
                                                 selected = area_total_names,     
                                                 options = list(
                                                   placeholder = 'Selecione uma coluna abaixo:'#,
                                                   #  onInitialize = I('function() { this.setValue(""); }')
                                                 ) # options    
             )# selectize
      ),
      
      
      h4("Variaveis opcionais:"),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'idadeas', # Id
        "Selecione a coluna da idade:", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        #selected = idade_names,     
        options = list(
          placeholder = 'selecione uma coluna abaixo',
          onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'gruposas', # Id
        "Selecione as variáveis pivô:", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        multiple = TRUE,  # permite mais de uma opcao ser selecionada
        options = list(
          placeholder = 'Selecione as variaveis abaixo',
          onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      sliderInput("erroas", 
                  label = "Selecione o erro admitido (%):", 
                  min = 1, 
                  max = 20, 
                  value = 10,
                  step = 1),
      
      sliderInput("alphaas", 
                  label = "Selecione o nível de significância:", 
                  min = 0.01, 
                  max = 0.10, 
                  value = 0.05,
                  step = 0.01),
      
      sliderInput("cdas", 
                  label = "Selecione o nº de casas decimais:", 
                  min = 0, 
                  max = 10, 
                  value = 4,
                  step = 1),
      
      radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
        inputId="tidyas",  #Id
        label='Selecione o arranjo da tabela:', # nome que sera mostrado na UI
        choices=c(Vertical = T, Horizontal = F), # opcoes e seus nomes
        selected=T)
      
    )
  })
  
  # tabela as
  output$as <- renderDataTable({
    
    asdt <- tabas() 
    
    if(input$Loadas)
    {
      
      
      # converte em datatable        # cria formattable
      as.datatable( formattable(asdt, 
                                list(    # colore a linha 6 da coluna dois de verde ou vemelho, se ela for menor ou maior que o numero da linha 1 coluna 2
                                  area(row=6, col=2) ~  formatter("span", 
                                                                  style = x ~ formattable::style(color = ifelse(x <= asdt[1,2], "#108e00", "red"))) ,
                                  # colore o erro estimado de verde ou vemelho, se ela for menor ou maior que o erro desejado
                                  area(row=10, col=2) ~ formatter("span", 
                                                                  style = x ~ formattable::style(color = ifelse(x <= input$erroas, "#108e00", "red")))
                                  
                                  
                                )#list
      ), #formattable
      # pre seleciona linhas
      selection = list(mode = 'multiple', selected = c(6,10,15,16), target = 'row'),
      options = list(searching = FALSE,
                     paging=FALSE,
                     initComplete = JS( # muda cor do cabecalho
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                       "}")
      ) 
      
      
      
      )
    } 
    
  })
  
  
  
  # Download tabelas ####
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Amostragem Casual Simples"         = tabacs(),
           "Amostragem Casual Estratificada 1" = tabace2(),
           "Amostragem Casual Estratificada 2" = tabace1(),
           "Amostragem Sistematica"            = tabas(),
           "Nivel Parcela"                     = newData() )
  })
  
  
  output$table <- renderDataTable({
    
    datadownload <- datasetInput()
    
    datatable( datadownload,
               options = list(searching = FALSE,
                              paging=FALSE,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                "}")
               )  )
    
  }) 
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      
      if(input$datasetformat==".csv")
      {
        paste(input$dataset, '.csv', sep='') 
      }
      else if(input$datasetformat==".xlsx")
      {
        paste(input$dataset, '.xlsx', sep='') 
      }
    },
    
    content = function(file) {
      if(input$datasetformat==".csv")
      {
        write.csv(datasetInput(), file, row.names = F)
      }
      else if(input$datasetformat==".xlsx")
      {
        xlsx::write.xlsx2(as.data.frame( datasetInput() ), file, row.names = F)
      }
      
      
      
    }
  )
})

