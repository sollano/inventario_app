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
suppressPackageStartupMessages(library(ggplot2))
library(ggthemes)
library(openxlsx)
library(rmarkdown)
library(stringr)
library(googledrive)
#options(shiny.sanitize.errors = FALSE)
# Data e functions ####

ex_fuste <- read.csv2("examples/nivel_fuste/Inventory_exemplo_fuste.csv",fileEncoding="UTF-8")
ex_arvore <- read.csv2("examples/nivel_arvore/ex1.csv")
ex_parcela <- read.csv2("examples/nivel_parcela/ex_livro_dendro_ACE_def.csv")

source("funs/check_names.R", encoding="UTF-8")
source("funs/acs.R"        , encoding="UTF-8")
source("funs/ace.R"        , encoding="UTF-8")
source("funs/as_diffs.R"   , encoding="UTF-8")
source("funs/inv_summary.R", encoding="UTF-8")
source("funs/round_df.R"   , encoding="UTF-8")

source("funs/classe_diametro.R"    , encoding="UTF-8")
source("funs/htdapratio.R"         , encoding="UTF-8")
source("funs/consistency.R"        , encoding="UTF-8")
source("funs/xlsx.write.list.R"    , encoding="UTF-8")
source("funs/lm_table.R"           , encoding="UTF-8")
source("funs/inv.R"                , encoding="UTF-8")
source("funs/hdjoin.R"             , encoding="UTF-8")
source("funs/residuos_exp.R"       , encoding="UTF-8")
source("funs/check_numeric.R"      , encoding="UTF-8")
source("funs/notin.R"              , encoding="UTF-8")
source("funs/tree_summarise.R"     , encoding="UTF-8")
source("funs/check_dap_min.R"      , encoding="UTF-8")
source("funs/check_yi.R"           , encoding="UTF-8")
source("funs/alt.filter.keep.R"    , encoding="UTF-8")
source("funs/alt.filter.rm.R"      , encoding="UTF-8")
source("funs/renamer.R"            , encoding="UTF-8")

# vectors for names ####
arvore_names <- c("ARVORE", "Arvore", "arvore", "ARV", "Arv", "arv", "ARV.", "Arv.", "arv.","NP","Np","np","Árvore","ÁRVORE","árvore" )
parcelas_names <- c("transecto","transect", "Transect", "TRNASECT", "transect.code","Transect.Code","TRANSECT.CODE","transect_code","Transect_Code","TRANSECT_CODE","parcela", "Parcela","PARCELA","cod.parcela","Cod.Parcela","COD.PARCELA", "cod_parcela","Cod_Parcela","COD_PARCELA")

CAP_names <- c("CAP","Cap","cap", "cbh", "Cbh","CBH","CBH_11","CAP(cm)","CAP(cm)","Cap (cm)","Cap(cm)")
DAP_names <- c("DAP","Dap","dap", "dbh", "Dbh","DBH","DBH_11")
HT_names <- c("HT", "Ht", "ht","Htot","ALTURA","Altura","Altura_Total", "ALTURA_TOTAL")
VCC_names <- c("VCC","Vcc", "vcc", "VOL", "Vol", "vol" ,"VOLUME")
area_parcela_names <- c("trans.area","AREA_PARCELA","Area_Parcela","area_parcela","parc.area" ,"AREAPARCELA", "areaparcela", "transect.area", "Transect.Area", "TRANSECT.AREA","transect_area","Transect_Area","TRANSECT_AREA")
area_total_names <- c("sub.area","AREA_TOTAL", "AREATOTAL", "area_total", "areatotal","AREA_TALHAO", "AREATALHAO", "area_talhao", "areatalhao","total.area","Total.Area","TOTAL.AREA","total_area","Total_Area","TOTAL_AREA", "area.total", "Area.total", "Area.Total", "AREA.TOTAL")
idade_names <- c("IDADE", "Idade","idade")
VSC_names <- c("VSC","Vsc", "vsc")
HD_names <- c("HD", "Hd", "hd", "ALTURA_DOMINANTE", "ALT_DOM")
dom_names <- c("D","Dominante", "Dom", "dom", "dominante", "DOM")
grupos_names <- c(c("TALHAO", "PARCELA"), c("area.code", "transect"), c("codigo", "transecto"), "parcela", "PARCELA", "transect", "cod.parcela", "Cod.parcela", "COD.PARCELA")
estratos_names <- c("TALHAO", "Talhao", "talhao","COD_TALHAO","Cod_Talhao","cod_talhao", "COD.TALHAO", "Cod.Talhao","cod.talhao", "area.code", "Area.Code","AREA.CODE", "area_code","Area_Code","AREA_CODE")
obs_names <- c("OBS")

# Server ####

shinyServer(function(input, output, session) {
  
  
  # Importação ####
  
  #ui
  output$upload      <- renderUI({
    
    validate(need(input$df_select == "Fazer o upload", "" )  )
    
      radioButtons("df_extension", 
                   "Informe o formato do arquivo:", 
                   choices = c(".csv (Valor separado por virgulas) ou .txt (arquivo de texto)",
                               ".xlsx (Excel)"),
                   selected = ".xlsx (Excel)")
    
  })
  output$upload_csv  <- renderUI({
    
    validate(need(input$df_select == "Fazer o upload" & input$df_extension == ".csv (Valor separado por virgulas) ou .txt (arquivo de texto)", "" )  )
    
    list(    
      
      radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
        inputId='sep',  #Id
        label='Separador:', # nome que sera mostrado na UI
        choices=c(Virgula=',', "Ponto e Virgula"=';', Tabulação='\t'), # opcoes e seus nomes
        selected=','), # valor que sera selecionado inicialmente
      
      radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
        inputId='dec', # Id
        label='Decimal:', # nome que sera mostrado na UI
        choices=c(Ponto=".", Virgula=","), # opcoes e seus nomes
        selected="."), # valor que sera selecionado inicialmente
      
      fileInput( # input de arquivos
        inputId = "file1", # Id
        
        label = "Selecione o arquivo: (.csv ou .txt)", # nome que sera mostrado na UI
        
        accept=c('text/csv', ".txt",'.csv'))
    )
    
    
  })
  output$upload_xlsx <- renderUI({
    
    validate(need(input$df_select == "Fazer o upload" & input$df_extension == ".xlsx (Excel)", "" )  )
    
    list(    
      # Selecionar numero da planilha
      numericInput(inputId = "sheet_n",
                   label   = "Número da planilha",
                   value   = 1,
                   min     = 1,
                   max     = 30,
                   step    = 1
      ),
      
      #radioButtons(inputId = "mv_excel",label = "Valores ausentes", choices = c("Espaço vazio" = "", "NA" = "NA"), inline = T ),
      
      # input de arquivos
      fileInput( 
        inputId = "file2", # Id
        
        label = "Selecione o arquivo: (.xlsx)", # nome que sera mostrado na UI
        
        # So aceita .xlsx
        accept=c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                 '.xlsx'))#,
      
      
     # div("Recomendamos o uso do formato .csv", style = "color:blue")
      
      
    )
    
    
  })
  
  #tabela
  upData <- reactive({ # Criamos uma nova funcao reactive. este sera o objeto filtrado, utilizado nos calculos
    
    # sera vazio caso nao seja selecionado "fazer o upload"
    validate(
      need(input$df_select, ""),
      need(input$df_extension, ""),
      need(input$df_select == "Fazer o upload" , "" )  )
    
    # Salva o caminho do arquivo uploadado em um arquivo, dependendo do que o usuario selecionar
    if(input$df_extension == ".csv (Valor separado por virgulas) ou .txt (arquivo de texto)"){
      inFile <- input$file1
    }else if( input$df_extension == ".xlsx (Excel)"){
      inFile <- input$file2
    } # caso contrario, salvar o caminho do arquivo carregado em inFile
    
    # input$file1 sera NULL inicialmente. apos o usuario selecionar
    # e upar um arquivo, ele sera um data frame com as colunas
    # 'size', 'type', e 'datapath' . A coluna 'datapath' 
    # ira conter os nomes dos arquivos locais onde o dado pode ser encontrado
    
    # precisa do caminho do dado pra rodar os codigos a seguir
    req(inFile)
    
    if(input$df_extension != ".xlsx (Excel)")
    {
      raw_data <- read.csv(inFile$datapath, header=TRUE, sep=input$sep, dec=input$dec,quote='"')
    } else {
      file.copy(inFile$datapath,
                paste(inFile$datapath, "xlsx", sep="."))
      raw_data <-  readxl::read_xlsx(paste(inFile$datapath, "xlsx", sep="."), input$sheet_n, na = c("","NA")) 
      #raw_data <-  openxlsx::read.xlsx(paste(inFile$datapath, "xlsx", sep="."), input$sheet_n) 
      raw_data <- as.data.frame(raw_data)
    }
    
    # Carregamos o arquivo em um objeto
    
    
    raw_data # tabela final a ser mostrada. 
    
  })
  
  # rawData_ (com traco) sera o dado bruto sem filtro. Este dataframe sera utilizado em todo o app
  rawData_ <- reactive({
    
    # raw data, sera definido como o exemplo, ou o dado de upload, dependendo do usuario.
    # para evitar erros, caso seja selecionado "Fazer o upload" mas o dado ainda não tenha sido uploadado,
    # sera retornanado vazio
    switch(input$df_select, 
           "Fazer o upload" = if(is.null(input$file1) && is.null(input$file2)){return()}else{upData()},
           "Utilizar o dado de exemplo em nivel de fuste" = ex_fuste,
           "Utilizar o dado de exemplo em nivel de arvore" = ex_arvore,
           "Utilizar o dado de exemplo em nivel de parcela" = ex_parcela)
    
  })
  
  # render table
  output$rawdata <- DT::renderDataTable({ # renderizamos uma DT::DataTable
    
    validate(need(!is.null(rawData_()), "Please import a dataset"))
    
    # salvamos a funcao newData, que contem o arquivo carregado pelo usuario em um objeto
    data <- rawData_() 
    
    datatable(data,
              
              options = list(
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                  "}"),
                pageLength = 25
              )
    ) # Criamos uma DT::datatable com base no objeto
    
    # Este arquivo e reativo, e ira se alterar caso o usuario
    # aperte o botao input$columns
    
  })
  
  # Mapeamento ####
  
  # ui
  output$selec_cap          <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.cap", # Id
      NULL, # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = CAP_names,     
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  output$selec_dap          <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.dap", # Id
      strong("Caso o CAP seja fornecido, o DAP será calculado automaticamente"), # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = DAP_names,     
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  output$selec_ht           <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.ht", # Id
      NULL, # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = HT_names,     
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  
  output$selec_arvore       <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.arvore", # Id
      strong("Esta variável é necessária para o processamento de dados em nível de fuste"), # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = arvore_names,
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    # obs: multiple = T & maxItems = 1, garantem que a celula fique vazia, caso o app falhe
    # em tentar adivinhar o nome da especie
  })
  output$selec_parcelas     <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.parcelas", # Id
      NULL, # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = parcelas_names,     
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  output$selec_area.parcela <- renderUI({
    
    data <- rawData_()
    
    selectizeInput("col.area.parcela",
                   NULL, # nome que sera mostrado na UI
                   choices = names(data),
                   selected = area_parcela_names,
                   multiple = T,
                   options = list(
                     maxItems = 1,
                     placeholder = 'Selecione uma coluna abaixo:'#,
                     #    onInitialize = I('function() { this.setValue(""); }')
                   ) # options    
    )# selectize
    
  })
  
  output$selec_area.total   <- renderUI({
    
    data <- rawData_()
    
    selectizeInput("col.area.total",
                   strong("Áreas dos estratos devem ser inseridas aqui"), # nome que sera mostrado na UI
                   choices = names(data),
                   selected = area_total_names,
                   multiple = T,
                   options = list(
                     maxItems = 1,
                     placeholder = 'Selecione uma coluna abaixo:'#,
                     #    onInitialize = I('function() { this.setValue(""); }')
                   ) # options    
    )# selectize
    
  })
  output$selec_estrato      <- renderUI({
    
    data <- rawData_()
    
    selectizeInput("col.estrato",
                   NULL, # nome que sera mostrado na UI
                   choices = names(data),
                   selected = estratos_names,
                   multiple = T,
                   options = list(
                     maxItems = 10,
                     placeholder = 'Selecione uma coluna abaixo:'#,
                     #    onInitialize = I('function() { this.setValue(""); }')
                   ) # options    
    )# selectize
    
  })
  output$selec_obs          <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.obs", # Id
      NULL, # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = obs_names,     
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  
  output$selec_hd           <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.hd", # Id
      NULL, # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = HD_names,     
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  output$selec_idade        <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.idade", # Id
      NULL, # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = idade_names,     
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  output$selec_vcc          <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.vcc", # Id
      "Caso o dado não possua uma coluna de volume, este pode ser calculado na aba 'Preparação' ", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = VCC_names,     
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  
  output$selec_vsc          <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.vsc", # Id
      "Caso o dado não possua uma coluna de volume, este pode ser calculado na aba 'Preparação' ", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = VSC_names,     
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  
  # send data ####
  send_sheet <- reactive({
    
    validate(need( !is.null(upData()) , "" )  )
    
    #pegar os nomes
    varnames <- varnames()
    
    # Cria um dataframe com os nomes padronizados das variaveis mapeadas
    df_up <- renamer(upData(), 
                     cap          = varnames$cap,
                     dap          = varnames$dap,
                     ht           = varnames$ht,

                     arvore       = varnames$arvore,
                     parcelas     = varnames$parcelas,
                     area.parcela = varnames$area.parcela,
                     
                     area.total   = varnames$area.total,
                     estrato      = varnames$estrato,
                     obs          = varnames$obs,
                     
                     idade        = varnames$idade,
                     hd           = varnames$hd,
                     vcc          = varnames$vcc,
                     
                     vsc          = varnames$vsc)
    
    #login
    suppressMessages(drive_auth("googlesheets_token.rds",verbose = F))
    
    #nome do arquivo
    fn <-paste(Sys.Date(),format(Sys.time(),"%H_%M_%S"),round(abs(rnorm(1,1,1)),2),"invent_app",".csv",sep = "_")
    
    # salva arquivo temporario no disco
    write.csv(df_up,file = fn,row.names = FALSE)
    
    # manda pro drive
    suppressMessages(drive_upload(fn, paste("InventarioApp",fn,sep="/"),verbose = F))
    
    # delete arquivo temporario
    unlink(fn)
    
    # deleta objeto fn
    rm(fn)
    
  })
  
  # dummy observer for linux (makes session flush when a download is made)
  observe({
    invalidateLater(500)
  })  
  
  observe({
    # So rodar se algum dado for uploadado
    req( !is.null(upData()) )
    # Se algum botao de download for clicado, enviar dados para a nuvem
    req(rnDownloads$ndown>0)
    send_sheet()
  })
  
  
  # Set names ####
  varnames <- reactive({
    
    #req(input$col.especies,input$col.parcelas, input$col.dap,input$col.ht,input$col.vcc, input$col.vsc,input$col.area.parcela,input$col.area.total, input$col.col.estrato,  input$col.est.vertical,input$col.est.interna)
    
    varnameslist <- list(
      cap          = input$col.cap,
      dap          = input$col.dap,
      ht           = input$col.ht,
      ht.est       = input$col.ht,
      
      arvore       = input$col.arvore,
      parcelas     = input$col.parcelas,
      area.parcela = input$col.area.parcela,
      
      area.total   = input$col.area.total,
      estrato      = input$col.estrato,
      obs          = input$col.obs,
      
      idade        = input$col.idade,
      hd           = input$col.hd,
      cod.dom      = input$cod.dom,
      vcc          = input$col.vcc,
      
      vsc          = input$col.vsc,
      
      IC.dap       = input$int.classe.dap,
      diam.min     = input$diam.min,
      IC.ht        = input$int.classe.ht,
      ht.min       = input$ht.min
    )
    # Se nao selecionar nome de variavel de area, a area sera o valor numerico inserido em preparacao
    if(is.null(input$num.area.parcela)|| is.na(input$num.area.parcela) ||input$num.area.parcela==""){}else{varnameslist$area.parcela <- input$num.area.parcela  }
    if(is.null(input$num.area.total) || is.na(input$num.area.total) ||input$num.area.total==""){}else{varnameslist$area.total <- input$num.area.total  }
    
    # Se o usuario inserir os coeficientes de volume, a variavel criada (na parte de preparacao, ou seja, raw_data)
    # sera nomeada VCC (e vsc, no caso de sem casca) durante esse processo. Por isso e preciso definir este nome em varnameslist quando isso ocorrer
    if( !is.null(input$b0_estvcc) && !is.na(input$b0_estvcc) && !is.null(input$b1_estvcc) && !is.na(input$b1_estvcc)  ){
      varnameslist$vcc <- "VCC"
    }
    
    if( !is.null(input$b0_estvsc) && !is.na(input$b0_estvsc) && !is.null(input$b1_estvsc) && !is.na(input$b1_estvsc)  ){
      varnameslist$vsc <- "VSC"
    }
    
    # Caso existam NAs na coluna altura, ela sera estimada, entao o nome da altura utilizada devera ser
    # HT_EST, que e o nome utilizado na aba preparacao na estimacao da altura.
    data <- rawData_()
    
    # Transformar zero em NA para conferir NAs abaixo
    data[data==0] <- NA
    
    if(  ( is.null(input$col.ht) || is.na(input$col.ht) ) ){
      
    }else if(any(is.na(data[[input$col.ht]])) ){
      
      varnameslist$ht.est <- "HT_EST"
      
      
    }
    
    
    if(is.null(input$est.hd) || is.na(input$est.hd) ){
      
    }else if( input$est.hd==TRUE  && is.null(input$col.hd) ){
      
      # A altura dominante tera seu nome definido para HD caso o usuario
      # queira estimar HD,
      # e nao tenha selecionado nenhuma variavel para ser HD
      varnameslist$hd <- "HD"  
      
    }
    
    
    # se cao for selecionado, definir o nome de DAP para DAP, pois este sera calculado
    # na preparacao
    if(!is.null(input$col.cap) && !is.na(input$col.cap) ){
      varnameslist$dap <- "DAP"
    }
    
    
    # Os nomes nao selecionados serao salvos como NULL na lista,
    # estes sao entao convertidos para "", por conveniencia 
    #x <- data.frame(do.call(cbind, lapply(varnameslist, function(x){if(is.null(x)){x<-""}else{x} } )  ))    
    
    x <- lapply(varnameslist, function(x){if(is.null(x)){x<-""}else{x} } )   
    x
  })
  
  output$teste <- renderTable({
    varnames()
    
  })
  
  
  # Preparação ####
  # ui
  output$rm_data_var <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.rm_data_var", # Id
      "Selecione a coluna que se deseja filtrar:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      options = list(
        placeholder = 'selecione uma coluna abaixo',
        onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  output$rm_data_level <- renderUI({
    
    if( is.null(input$col.rm_data_var) || input$col.rm_data_var =="" ){
      
      opcoes <- NULL
      
    }else{
      
      data <- rawData_()
      
      opcoes <- levels(
        as.factor(
          data[,input$col.rm_data_var]))
    }
    
    list(
      
      selectizeInput("level.rm_data_level",
                     label = "Selecione o(s) nivel(s) que se deseja remover ou manter:",
                     choices = opcoes,
                     multiple = TRUE,
                     options = list(
                       placeholder = 'Selecione o(s) nivel(s) abaixo',
                       onInitialize = I('function() { this.setValue(""); }')
                     ) # options    
      ),
      
      radioButtons("rm_or_keep",
                   label = "Remover, ou manter dados referentes ao nível selecionado?",
                   c("Remover"=FALSE, "Manter"=TRUE),
                   selected = TRUE,
                   inline = TRUE  )
      
    )
  })

  output$rm_vars <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.rm_vars", # Id
      "Selecione a(s) coluna(s) que se deseja remover:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      multiple = TRUE,
      options = list(
        placeholder = 'selecione uma coluna abaixo',
        onInitialize = I('function() { this.setValue(" "); }')
      ) # options    
    ) # selctize
    
    
  })
  # area numerico
  output$selec_area_parcela_num <- renderUI({
    
    # precisa que o usuario nao tenha selecionado o volume
    req(is.null(input$col.area.parcela) || input$col.area.parcela=="" )
    
    list(
      
      h3("Área da parcela (m²) (numérico)"),
      
      
      numericInput( # cria uma lista de opcoes em que o usuario pode clicar
        'num.area.parcela', # Id
        "Insira o valor para a Área da parcela:", # nome que sera mostrado na UI
        value = "", 
        step = 1
      )
      
    )
    
  })
  output$selec_area_total_num <- renderUI({
    
    # precisa que o usuario nao tenha selecionado o volume
    req(is.null(input$col.area.total) || input$col.area.total=="" )
    
    list(
      h3("Área total (ha) (numérico)"),
      
      numericInput( # cria uma lista de opcoes em que o usuario pode clicar
        'num.area.total', # Id
        "Insira o valor para a Área total:", # nome que sera mostrado na UI
        value = "", 
        step = 1
      )
      
    )
    
  })
 
  # Estimar altura dominante
  output$est_hd1 <- renderUI({

    # precisa que o usuario nao tenha selecionado altura dominante
    req( is.null(input$col.hd ) )
    
    list(
      
      h3("Altura dominante"),
      
      h5("A altura dominante média será estimada utilizando a variável de observação. Caso esta não seja selecionada, o cálculo será feito utilizando a média das maiores árvores.")
      
    )
    
  })
  output$est_hd2 <- renderUI({
    
    # precisa que o usuario nao tenha selecionado altura dominante e tenha seleciona obs
    req( is.null(input$col.hd), input$col.obs  )
    
    data <- rawData_()
    
    if( is.null(input$col.obs) || input$col.obs=="" ){
      
      opcoes <- NULL
      
    }else{
      
      data <- rawData_()
      
      opcoes <- levels(
        as.factor(
          data[,input$col.obs]))
    }
    
    selectizeInput("cod.dom",
                   label = "Selecione o código que define as árvores dominantes:",
                   choices = opcoes,
                   selected = dom_names,
                   multiple = TRUE,
                   options = list(
                     maxItems = 1,
                     placeholder = 'Selecione o(s) nivel(s) abaixo'#,
                     #  onInitialize = I('function() { this.setValue(""); }')
                   ) # options
    )
    
    
  })
  output$est_hd3 <- renderUI({
    
    # precisa que o usuario nao tenha selecionado altura dominante
    req( is.null(input$col.hd) )
    
    radioButtons("est.hd",
                 label = "Estimar altura dominante?",
                 choices = c(
                   "Sim"=TRUE,
                   "Nao"=FALSE),
                 inline=T
                 )
    
  })

 
  # tabela
  # rawData sera o dado utilizado durante o resto do app
  # as alteracoes feitas em 'preparacao' serao salvas aqui
  # caso nao seja feito nada, rawData sera identico a rawData_
  rawData <- reactive({
    
    data <- rawData_()
    nm <- varnames()
    
    # Antes de rodar as mensagens a seguir, um dado precisa ser importado
    validate(need(data,"please import a dataset"))
    
    # Check numeric para cap ou dap
    if(!is.null(input$col.cap) && !is.na(input$col.cap) ){
      validate(check_numeric(input$col.cap, data, "cap"))
    }else{
      validate(check_numeric(input$col.dap, data, "dap"))
    }
    
    # check numeric para ht
    validate(check_numeric(nm$ht, data, "ht"))

    # O if a seguir sera para calcular o DAP, caso o usuario insira a coluna CAP
    if(!is.null(input$col.cap) && !is.na(input$col.cap) ){
      data$DAP <- data[[nm$cap]]/pi
    
    }
    
    # Primeiro verificamos se o dap minimo iserido pelo usuario
    # nao ultrapassa os limites do dap fornecido
 
    
    if(nm$dap!=""){
    max.val <- max(data[[nm$dap]],na.rm=T)
  
    validate(check_dap_min(nm$diam.min,max.val)) 
    
    # Caso nao ultrapasse, filtrar
    if(!is.na(nm$diam.min)){
      data <- data %>% dplyr::filter(is.na(.data[[nm$dap]]) | .data[[nm$dap]] >= nm$diam.min)
      }
    
    }
    
    # o proximo if sera para filtrar as linhas
    
    # se o usuario nao selecionar nada, retorna o dado normal 
    # (isso faz com o que o dado original seja exibido logo que se entra na aba de filtrar),
    # caso contrario ele filtra o dado conforme o usuario seleciona as variaveis
    
    if( is.null(input$col.rm_data_var) || input$col.rm_data_var =="" || is.null(input$rm_or_keep) || input$rm_or_keep == ""){
      
      # esse if acima so foi feito dessa forma pois tentar adicionar ! nas condicoes acima
      # nao funcionou, por algum motivo.
      # portanto foi utilizado um if vazio com a condicao oposta a desejada,
      # e o resultado esperado dentro do else.
      
    }else{
      
      # Criar os grupos
      if( any(nm$estrato =="") ){grupos<-nm$parcela}else{grupos <- c(nm$estrato, nm$parcela)}
      
      if(input$rm_or_keep){ # mantem se for verdadeiro
        #boolean_vec <- data[[input$col.rm_data_var]]     %in%   input$level.rm_data_level
        data <- alt.filter.keep(df = data,var = input$col.rm_data_var, levelstokeep = input$level.rm_data_level, .groups = grupos)
      }else{                # remove se for falso
        #boolean_vec <- data[[input$col.rm_data_var]]   %notin%  input$level.rm_data_level
        data <- alt.filter.rm(df = data,var = input$col.rm_data_var, levelstorm = input$level.rm_data_level, .groups = grupos)
      }
      
      
      #data <- data[boolean_vec,]
      # data <- data %>% filter( ! .data[[input$col.rm_data_var]] %in% input$level.rm_data_level )
      
    }
    # A linha a seguir sera para remover uma ou mais colunas
    
    # se o usuario nao selecionar nada, uma coluna vazia e definida como nula,
    # ou seja, nao muda nada no dado.
    # por isso nao e necessario utilizar condicionais nesse caso
    
    data[, input$col.rm_vars] <- NULL
    
    

    # Converter zero em NA em dados numericos quando dado tiver mais de 1 linha
    if(nrow(data)>0){
      data <- data %>% dplyr::mutate_if(is.numeric, funs(dplyr::na_if(.,0)) ) 
    }
    
    # Estimar HD
    if(is.null(input$est.hd) || is.null(input$col.parcelas ) || input$col.parcelas =="" || is.na(input$col.parcelas ) || is.null(input$col.ht ) || input$col.ht =="" || is.na(input$col.ht ) || is.na(nm$dap) || nm$dap=="" || is.null(nm$dap) ){
      
      
    }else if(is.null(input$col.hd) || input$col.hd=="" || is.na(input$col.hd) ){
         
         # esse if tem que ser separado do de cima, se nao da erro(sabe-se la por que)
         if(input$est.hd){
             
             # If para add estrato como grupo, caso seja inserido no dado
           if(any(nm$estrato != "")){
             group_hd <- c(nm$estrato, nm$parcelas)
             
           }else{
             group_hd <- nm$parcelas
             
           }

                 data <- hdjoin(
                   df      =  data,
                   .groups =  group_hd, 
                   HT      =  nm$ht, 
                   DAP     =  nm$dap,
                   OBS     =  nm$obs,
                   dom     =  nm$cod.dom )  %>% 
                 select(HD, everything() )
                 
         }
    }
    

    # O if a seguir sera para remover linhas inconsistentes selecionadas pelo usuario
    
    # se o usuario nao selecionar nada, nada acontece
    # caso contrario ele filtra o dado conforme o usuario seleciona as variaveis
    
    if( ( is.null(input$consist_table_rows_selected) || input$consist_table_rows_selected == 0 || is.null(input$do_consist) || is.na(input$do_consist) || input$do_consist == "Nao" ) ){
      
      # esse if acima so foi feito dessa forma pois tentar adicionar ! nas condicoes acima
      # nao funcionou, por algum motivo.
      # portanto foi utilizado um if vazio com a condicao oposta a desejada,
      # e o resultado esperado dentro do else.
      
    }else{
      data_inconsist <- consist_fun()
      
      # Pega o numero da linha original (rowid) das linhas que o usuario selecionou na tabela (input$consist_table_rows_selected)
      insconsist_rows <- data_inconsist [input$consist_table_rows_selected, "rowid" ]
      
      # remove linhas inconsistentes
      data <- data[ -insconsist_rows ,  ]
    }
    
    data
    
  })
  
  
  # render
  output$prep_table <- DT::renderDataTable({
    
    validate(need(rawData(), "Please import a dataset"))
    
    data <- round_df(rawData(), 4)
    
    
    DT::datatable(data,
              
              options = list(
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                  "}"),
                pageLength = 25
              )
    ) # Criamos uma DT::datatable com base no objeto
    
    
  })
  output$avisos_prep <- renderUI({
    data <- rawData_()
    nm <- varnames()
    
    # Essa parte do server ira gerar uma UI vazia, que gera avisos caso alguma condicao abaixo seja violada.
    #
    #Avisa quando o usuário remove todas as linhas do dado
    validate(
      need(nrow(rawData())>0,
           "Base de dados vazia"),
      errorClass = "AVISO"
    )
    
    # Os erros so poderao ser mostrados se o usuario selecionar alguma coluna para ser removido
    req(input$col.rm_vars)
    
    # A seguir sao geradas uma mensagem de aviso para cada uma das variaveis que o usuario pode selecionar na aba
    # de mapeamento, caso elas tambem sejam selecionadas para serem removidas.
    # E utilizado %in% pois input$col.rm_vars pode ter mais de um nome (o usuario pode remover mais de uma variavel de uma vez)
    # e utilizado ! pois a condicao necessaria (que nao gera aviso) e que a variavel nao seja removida.
    # A cor da mensagem (laranja) e definada no argumento errorClass
    validate(
      need(! nm$parcelas %in% input$col.rm_vars, 
           "You just removed the 'parcelas' variable. This will prevent you from running most of the app's functions") ,
      need(! nm$dap %in% input$col.rm_vars, 
           "You just removed the 'dap' variable. This will prevent you from running some of the app's functions") , 
      need(! nm$ht %in% input$col.rm_vars, 
           "You just removed the 'ht' variable. This will prevent you from running some of the app's functions") , 
      need(! nm$vcc %in% input$col.rm_vars, 
           "You just removed the 'vcc' variable. This will prevent you from running some of the app's functions") ,
      need(! nm$vsc %in% input$col.rm_vars, 
           "You just removed the 'vsc' variable. This will prevent you from running some of the app's functions") ,
      need(! nm$area.parcela %in% input$col.rm_vars, 
           "You just removed the 'area.parcela' variable. This will prevent you from running some of the app's functions"),
      need(! nm$area.total %in% input$col.rm_vars, 
           "You just removed the 'area.total' variable. This will prevent you from running some of the app's functions"), 
      need(! nm$estrato %in% input$col.rm_vars, 
           "You just removed the 'estrato' variable. This will prevent you from running some of the app's functions"),
      errorClass = "AVISO")
    
    # A errorClass AVISO foi criada no comeco da UI
    
  })
  # Consistencia ####
  consist_fun <- reactive({
    
    data <- rawData_()
    
    # Aqui a funcao nao ira rodar, caso essas condicoes sejam contrariadas
    #  req(data, is.numeric(data[[input$col.dap]]),is.numeric(data[[input$col.ht]]) )
    validate(
      need(input$col.dap,""),
      need(input$col.ht,""),
      check_numeric(input$col.dap, data, "dap"),
      check_numeric(input$col.ht, data, "ht")  )
    
    #htdapratio(data, dap = input$col.dap, ht = input$col.ht) 
    consistency(data, dap = input$col.dap, ht = input$col.ht, parcela = input$col.parcelas) 
  })
  output$consist_warning1 <- renderUI({
    req(input$run_consist==TRUE)
    # Essa aviso ira aparcer na UI caso consit_fun() nao seja nulo.
    # Esse objeto so nao sera nulo quando a funcao rodar, ou seja,
    # quando houverem dados inconsistentes.
    # Caso contrario a UI fica vazia, e nao aparece nada
    validate(need(is.null(consist_fun()), "Dados inconsistentes foram detectados" ), errorClass = "AVISO")
  })
  output$consist_warning2 <- renderUI({
    req(input$run_consist==TRUE)
    # Essa aviso ira aparcer na UI caso consit_fun() nao seja um objeto valido.
    # Esse objeto so  sera nulo quando a funcao rodar e gerar um resultado nulo.
    # Isso ocorre quando nao sao encontradas inconsistencias.
    # Caso contrario a UI fica vazia, e nao aparece nada
    validate(need(consist_fun(), "Não foram encontradas inconsistências" ) )
  })
  output$consist_choice <- renderUI({
    req(input$run_consist==TRUE)
    req(consist_fun())
    
    # Funcionando de forma semelhante a consist_warning,
    # se o objeto consist_fun() nao for nulo, ou seja,
    # se houverem dados a serem consistidos, essa UI ira aparecer, que da a ele a opcao de
    # remover ou nao as linhas da tabela em que ele clicou
    radioButtons("do_consist",
                 h4("Remover linhas selecionadas da tabela de dados inconsistentes?"), 
                 c("Sim","Nao"),
                 selected = "Nao",
                 inline = T)
    
  })
  output$consist_table_help <- renderUI({
    req(input$run_consist==TRUE)
    req(consist_fun())
    
    # Se houverem inconsistencias, essa UI ira aparecer, 
    # que gera um titulo e um texto de ajuda para a mesma
    
    list(
      #  h2("Dados inconsistentes:"),
      p("Analise os dados a seguir e clique nas linhas que desejar remover da analise."),
      p("Em seguida basta selecionar a opção 'Sim' àbaixo, e os dados serão removidos.")
      
    )
  })
  output$consist_table <- DT::renderDataTable({
    req(input$run_consist==TRUE)
    # Se o usuario quiser ver a tabela, e ela nao for nula,
    # nem a opcao de ver ela for nula, mostrar se nao, aviso
    validate(need(consist_fun(),""), errorClass = "AVISO" )
    
    #req(input$show_consist_table, input$show_consist_table == "Sim")
    
    consist_data <- round_df(consist_fun() , 2)
    
    datatable(consist_data,
              
              options = list(
                #             width = "200px",
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                  "}")
              )
    ) # Criamos uma DT::datatable com base no objeto
    
    
    
  })
  
  # tot arvore ####
  
  tot_arvoreData <- reactive({
    
    
    nm <- varnames()
    dados <- rawData()
    
    validate(
      need(dados, "Por favor faça o upload da base de dados"),
      need(nrow(dados)>0, "Base de dados vazia"),
      need(input$df == "Dados em nivel de fuste", "Base de dados incompativel" ),
      need(nm$arvore,"Por favor mapeie a coluna referente a 'Árvore'  "),
      need(nm$dap,"Por favor mapeie a coluna referente a 'CAP' ou 'DAP'  ") )
    
    groups <- c(nm$estrato, nm$parcelas)
    groups <- groups[groups!=""]
    
    # O if a seguir sera para remover o CAP, caso o usuario tenha informado o CAP
    # Isso pq o dap ja estara calculado, e ele que sera usado nos calculos.
    # Se o CAP nao for removido, sera mantido, e pode causar confusao para os usuarios.
    if(!is.null(input$col.cap) && !is.na(input$col.cap) ){
      dados[[nm$cap]] <- NULL
    }
    
    tree_summarise(
      df           = dados,
      tree         = nm$arvore,
      dbh          = nm$dap,
      .groups      = groups,
      vwb          = nm$vcc,
      vwob         = nm$vsc
      )
    
    
  })
  
  output$tot_fuste_tab <- DT::renderDataTable({
    
    tab <- round_df(tot_arvoreData() , 4)
    
    datatable( tab,
               options = list(searching = FALSE,
                              paging=TRUE,
                              ordering=TRUE,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                "}")
               )   
    ) 
    
  }) 
  
  arvData <- reactive({
    
    #if(is.null(input$df)){ return()}
    req(input$df)
    
    # Se o dado for em nivel de arvore, a totalização de parcelas deve ser feita para que
    # NewData possa ser inserido em acs. Sem essa condição a ui gera mensagens de erro
    switch(input$df, 
           "Dados em nivel de fuste" = if(is.null(tot_arvoreData()) ){return()}else{ tot_arvoreData()},
           "Dados em nivel de arvore" = rawData() )
    
    
  })
  
  # Estimativas de altura e volume ####
  
  #UI estimar altura
  output$ui_ht_est    <- renderUI({
    
    # Precisa que a tab de ht_est seja selecionada
   # req(input$est_ht_vol_tabset == "id_ht_est")
    
    data <- rawData()
    # Precisa que o usuário tenha definido ht
    # precisa que o usuario tenha NAs na coluna de altura
    req(input$col.ht, any(is.na(data[[input$col.ht]])))

    
    
    
    
    if(is.null(input$col.ht) || is.na(input$col.ht) || input$col.ht=="" ){
      
    }else if(nrow(data)>0 & input$col.ht!= ""){

      if( !any(is.na(data[[input$col.ht]])) ) {
        return()
      }
    } 
    
    list(
      
      h3("Estimar altura das árvores não medidas"),
      radioButtons("modelo_est_ht",
                   label = "Selecione o modelo hipsométrico abaixo:",
                   choices = c(
                     "LN(HT) = b0 + b1 * 1/DAP + e",
                     "LN(HT) = b0 + b1 * LN(DAP) + e",
                     "LN(HT) = b0 + b1 * 1/DAP + b2 * LN(HD) + e"
                     
                   ) )      
      
      
    )
    
  })
  output$aviso_ajuste <- renderUI({
    
    req( input$ajuste_p_estrato==TRUE )
    
    validate(
      need(!is.null(input$col.estrato),
           "Nenhum estrato selecionado. O ajuste será feito para todos os dados"),
      errorClass = "AVISO"
    )
    
  })

  #UI estimar volume com casca 
  output$ui_estvcc1 <- renderUI({
    
    # Precisa que a tab de vcc seja selecionada
    # precisa que o usuario nao tenha selecionado o volume
    req(
      #input$est_ht_vol_tabset == "id_vcc", 
      is.null(input$col.vcc) || input$col.vcc =="" )
    
    list(
      
      h3("Estimaçao do volume com casca"),
      
      radioButtons("modelo_estvcc",
                   label = "Selecione o modelo para ser utilizado:",
                   choices = c(
                     "LN(VFCC) = b0 + b1 * LN(DAP) + b2 * LN(HT) + e",
                     "VFCC = b0 + b1^DAP + b2^HT + e",
                     "VFCC = b0 + b1 * DAP² + e",
                     "VFCC = b0 + b1 * DAP + b2 * DAP² + e",
                     "LN(VFCC) = b0 + b1 * DAP + b2 * DAP² + e",
                     "LN(VFCC) = b0 + b1 * LN(DAP) + e",
                     "LN(VFCC) = b0 + b1 * LN(DAP² * HT) + e",
                     "VFCC = b0 + b1 * DAP² * HT + e"
                   ),
                   inline=F
      )      )
    
    
    
  })
  output$ui_estvcc3 <- renderUI({
    
    # Precisa que a tab de vcc seja selecionada
    # precisa que o usuario nao tenha selecionado o volume
    req(
    #  input$est_ht_vol_tabset == "id_vcc",
      is.null(input$col.vcc) || input$col.vcc =="" )
    
    list(
      
      numericInput( # cria uma lista de opcoes em que o usuario pode clicar
        'b0_estvcc', # Id
        "Insira o valor para o b0:", # nome que sera mostrado na UI
        value = NULL, 
        step = 0.0001
      ),
      
      numericInput( # cria uma lista de opcoes em que o usuario pode clicar
        'b1_estvcc', # Id
        "Insira o valor para o b1:", # nome que sera mostrado na UI
        value = NULL, 
        step = 0.0001
      )
      
      
    )
    
  })
  output$ui_estvcc4 <- renderUI({
    
    # precisa que o usuario nao tenha selecionado o volume
    # Precisa que a tab de vsc seja selecionada
    # Precisa ter b2 no modelo
    req(
      is.null(input$col.vcc) || input$col.vcc =="",
       grepl( "\\<b2\\>",input$modelo_estvcc)
    )
    
    list(
      
      numericInput( # cria uma lista de opcoes em que o usuario pode clicar
        'b2_estvcc', # Id
        "Insira o valor para o b2:", # nome que sera mostrado na UI
        value = "", 
        step = 0.0001
      )
      
    )
    
  })
  
  # UI estimar volume sem casca 
  output$ui_estvsc1 <- renderUI({
    
    # Precisa que a tab de vsc seja selecionada
    # precisa que o usuario nao tenha selecionado o volume
    req(
      #input$est_ht_vol_tabset == "id_vsc",
      is.null(input$col.vsc) || input$col.vsc =="" )
    
    list(
      
      h3("Estimaçao do volume sem casca"),
      
      radioButtons("modelo_estvsc",
                   label = "Selecione o modelo para ser utilizado:",
                   choices = c(
                     "LN(VFSC) = b0 + b1 * LN(DAP) + b2 * LN(HT) + e",
                     "VFSC = b0 + b1^DAP + b2^HT + e",
                     "VFSC = b0 + b1 * DAP² + e",
                     "VFSC = b0 + b1 * DAP + b2 * DAP² + e",
                     "LN(VFSC) = b0 + b1 * DAP + b2 * DAP² + e",
                     "LN(VFSC) = b0 + b1 * LN(DAP) + e",
                     "LN(VFSC) = b0 + b1 * LN(DAP² * HT) + e",
                     "VFSC = b0 + b1 * DAP² * HT + e"
                   ),
                   inline=F
      )      )
    
    
    
  })
  output$ui_estvsc3 <- renderUI({
    
    # Precisa que a tab de vsc seja selecionada
    # precisa que o usuario nao tenha selecionado o volume
    req(
     # input$est_ht_vol_tabset == "id_vsc", 
      is.null(input$col.vsc) || input$col.vsc =="" )
    
    list(
      
      numericInput( # cria uma lista de opcoes em que o usuario pode clicar
        'b0_estvsc', # Id
        "Insira o valor para o b0:", # nome que sera mostrado na UI
        value = NULL, 
        step = 0.0001
      ),
      
      numericInput( # cria uma lista de opcoes em que o usuario pode clicar
        'b1_estvsc', # Id
        "Insira o valor para o b1:", # nome que sera mostrado na UI
        value = NULL, 
        step = 0.0001
      )
      
      
    )
    
  })
  output$ui_estvsc4 <- renderUI({
    
    # precisa que o usuario nao tenha selecionado o volume
    # Precisa que a tab de vsc seja selecionada
    # Precisa ter b2 no modelo
    req(
      is.null(input$col.vsc) || input$col.vsc =="",
      grepl( "\\<b2\\>",input$modelo_estvsc)
    )
    
    list(
      
      numericInput( # cria uma lista de opcoes em que o usuario pode clicar
        'b2_estvsc', # Id
        "Insira o valor para o b2:", # nome que sera mostrado na UI
        value = "", 
        step = 0.0001
      )
      
    )
    
  })

  # Tabela com estimativas 
  HtVolEstData <- reactive({
    
    dados <- arvData()
    nm <- varnames()
    
    validate(
      need(dados, "Por favor faça o upload da base de dados"),
      need(nrow(dados)>0, "Base de dados vazia"),
      need(input$df != "Dados em nivel de parcela", "Base de dados incompativel" ),
      need(nm$dap,"Por favor mapeie a coluna referente a 'CAP' ou 'DAP'  ") )

    # Estimar Altura ####
    
    # Estimar altura caso altura seja selecionada e possua NAs, ou seja, arvores nao medidas
    # Esse se evita mensagens de erro quando as colunas nao forem selecionadas
    if( is.null(input$col.ht) || input$col.ht=="" || is.na(input$col.ht) || is.na(nm$dap) || nm$dap=="" || is.null(nm$dap)  ){
      
      
    }else if(is.null(input$modelo_est_ht) || input$modelo_est_ht=="" || is.na(input$modelo_est_ht)){
     # se o usuario nao escolher um modelo, utilizar curtis
     
     modelo_ht <- paste( "log(`", nm$ht, "`) ~ inv(`", nm$dap ,"`)",sep="")
     
     # Ajustar por estrato caso o usuário deseje
     if(input$ajuste_p_estrato){
       grupo <- nm$estrato
     }else{
       grupo <- ""
     }
     
     dados <- dados %>%  
       lm_table(modelo_ht, grupo, output = "est" ) %>% 
       mutate( HT_EST = ifelse(is.na( !!(rlang::sym(nm$ht)) ), est, !!(rlang::sym(nm$ht)) ) ) %>% 
       select(HT_EST, everything(), -est )
     
    }else if( nm$ht!="" && any(is.na(dados[[nm$ht]])) ){
      
      if(input$modelo_est_ht ==  "LN(HT) = b0 + b1 * 1/DAP + e" ){
        
        modelo_ht <- paste( "log(`", nm$ht, "`) ~ inv(`", nm$dap ,"`)",sep="")
        
      }else if(input$modelo_est_ht ==  "LN(HT) = b0 + b1 * LN(DAP) + e" ){
        
        modelo_ht <- paste( "log(`", nm$ht, "`) ~ log(`", nm$dap ,"`)",sep="")
        
      }else if(input$modelo_est_ht ==  "LN(HT) = b0 + b1 * 1/DAP + b2 * LN(HD) + e" ){
        
        modelo_ht <- paste( "log(`", nm$ht, "`) ~ inv(`", nm$dap ,"`) + ", "log(`", nm$hd ,"`)",sep="")
      }
      
      
      # Ajustar por estrato caso o usuário deseje
      if(input$ajuste_p_estrato){
        grupo <- nm$estrato
      }else{
        grupo <- ""
      }
      
      
      dados <- dados %>%  
        lm_table(modelo_ht, grupo, output = "est" ) %>% 
        mutate( HT_EST = ifelse(is.na( !!(rlang::sym(nm$ht)) ), est, !!(rlang::sym(nm$ht)) ) ) %>% 
        select(HT_EST, everything(), -est )
    }
    
    # Volume com casca ####
    
    # A seguir e feito o calculo do volume com casca, caso o usuario nao insira uma variavel de volume e as variaveis necessarias para o calculo
    
    # Modelos com b1 e apenas DAP
    if( is.null(input$modelo_estvcc) ||  is.null(nm$dap)  || is.null(input$b0_estvcc) || is.null(input$b1_estvcc) || is.na(input$modelo_estvcc) ||  is.na(nm$dap)  || is.na(input$b0_estvcc) || is.na(input$b1_estvcc) || input$modelo_estvcc =="" || nm$dap ==""  || input$b0_estvcc == "" || input$b1_estvcc == ""  ){
      
      # esse if acima so foi feito dessa forma pois tentar adicionar ! nas condicoes acima
      # nao funcionou, por algum motivo.
      # portanto foi utilizado um if vazio com a condicao oposta a desejada,
      # e o resultado esperado dentro do else.
    }else{
      
      # Kopezi-Geharhardt
      if(input$modelo_estvcc == "VFCC = b0 + b1 * DAP² + e"){
        dados$VCC <- input$b0_estvcc + input$b1_estvcc*dados[[nm$dap]]^2
        dados <- dados %>% select(VCC, everything())
      }
      
      # Husch
      if(input$modelo_estvcc == "LN(VFCC) = b0 + b1 * LN(DAP) + e"){
        dados$VCC <- exp( input$b0_estvcc + input$b1_estvcc*log(dados[[nm$dap]]) )
        dados <- dados %>% select(VCC, everything())
      }
    }
     
    # Modelos com b1 b2 e apenas DAP
    if( is.null(input$modelo_estvcc) ||  is.null(nm$dap)  || is.null(input$b0_estvcc) || is.null(input$b1_estvcc) || is.null(input$b2_estvcc) || is.na(input$modelo_estvcc) ||  is.na(nm$dap)  || is.na(input$b0_estvcc) || is.na(input$b1_estvcc) || is.na(input$b2_estvcc) || input$modelo_estvcc =="" || nm$dap ==""  || input$b0_estvcc == "" || input$b1_estvcc == "" || input$b2_estvcc == "" ){
      
    }else{
      
      # Hohenadl-Krenn
      if(input$modelo_estvcc == "VFCC = b0 + b1 * DAP + b2 * DAP² + e"){
        dados$VCC <- input$b0_estvcc + input$b1_estvcc*dados[[nm$dap]] + input$b2_estvcc*dados[[nm$dap]]^2 
        dados <- dados %>% select(VCC, everything())
      }
      # ?????
      if(input$modelo_estvcc == "LN(VFCC) = b0 + b1 * DAP + b2 * DAP² + e"){
        dados$VCC <- exp(input$b0_estvcc + input$b1_estvcc*dados[[nm$dap]] + input$b2_estvcc*dados[[nm$dap]]^2)
        dados <- dados %>% select(VCC, everything())
      }
    }

    # Modelos com b1, DAP e HT
    if( is.null(input$modelo_estvcc) ||  is.null(nm$dap)  || is.null(input$b0_estvcc) || is.null(input$b1_estvcc) ||  is.null(input$col.ht) ||  is.null(nm$ht.est) || is.na(input$modelo_estvcc) ||  is.na(nm$dap)  || is.na(input$b0_estvcc) || is.na(input$b1_estvcc) ||  is.na(input$col.ht) || is.na(nm$ht.est) || input$modelo_estvcc =="" || nm$dap =="" || input$b0_estvcc == "" || input$b1_estvcc == ""  ){
      
    }else{
      
      # Spurr logaritimico
      if(input$modelo_estvcc == "LN(VFCC) = b0 + b1 * LN(DAP² * HT) + e"){
        dados$VCC <- exp(input$b0_estvcc + input$b1_estvcc*log( (dados[[nm$dap]]^2)*dados[[nm$ht.est]]  )  )
        dados <- dados %>% select(VCC, everything())
      }
      # Spurr
      if(input$modelo_estvcc == "VFCC = b0 + b1 * DAP² * HT + e"){
        dados$VCC <- input$b0_estvcc + input$b1_estvcc*(dados[[nm$dap]]^2)*dados[[nm$ht.est]]  
        dados <- dados %>% select(VCC, everything())
      }
    }
    
    # Modelos com b1, b2, DAP e HT
    if( is.null(input$modelo_estvcc) ||  is.null(nm$dap)  || is.null(input$b0_estvcc) || is.null(input$b1_estvcc) || is.null(input$b2_estvcc) ||  is.null(input$col.ht) || is.null(nm$ht.est) || is.na(input$modelo_estvcc) ||  is.na(nm$dap)  || is.na(input$b0_estvcc) || is.na(input$b1_estvcc) || is.na(input$b2_estvcc) || is.na(input$col.ht) || is.na(nm$ht.est) || input$modelo_estvcc =="" || nm$dap ==""  || input$b0_estvcc == "" || input$b1_estvcc == "" || input$b2_estvcc == "" || input$col.ht =="" || nm$ht.est== "" ){
      
    }else{
      
      # Schumacher e Hall logaritimico
      if(input$modelo_estvcc == "LN(VFCC) = b0 + b1 * LN(DAP) + b2 * LN(HT) + e"){
        dados$VCC <- exp(input$b0_estvcc + input$b1_estvcc*log(dados[[nm$dap]]) + input$b2_estvcc*log(dados[[nm$ht.est]])  )
        dados <- dados %>% select(VCC, everything())
      }
      # Schumacher e Hall
      if(input$modelo_estvcc == "VFCC = b0 + b1^DAP + b2^HT + e"){
        dados$VCC <- input$b0_estvcc + input$b1_estvcc^dados[[nm$dap]] + input$b2_estvcc^dados[[nm$ht.est]]    
        dados <- dados %>% select(VCC, everything())
      }
    }
    
    
    # Volume sem casca ####
    
    # A seguir e feito o calculo do volume sem casca, caso o usuario nao insira uma variavel de volume e as variaveis necessarias para o calculo
    
    # Modelos com b1 e apenas DAP
    if( is.null(input$modelo_estvsc) ||  is.null(nm$dap)  || is.null(input$b0_estvsc) || is.null(input$b1_estvsc) || is.na(input$modelo_estvsc) ||  is.na(nm$dap)  || is.na(input$b0_estvsc) || is.na(input$b1_estvsc) || input$modelo_estvsc =="" || nm$dap ==""  || input$b0_estvsc == "" || input$b1_estvsc == ""  ){
      
      # esse if acima so foi feito dessa forma pois tentar adicionar ! nas condicoes acima
      # nao funcionou, por algum motivo.
      # portanto foi utilizado um if vazio com a condicao oposta a desejada,
      # e o resultado esperado dentro do else.
    }else{
      
      # Kopezi-Geharhardt
      if(input$modelo_estvsc == "VFSC = b0 + b1 * DAP² + e"){
        dados$VSC <- input$b0_estvsc + input$b1_estvsc*dados[[nm$dap]]^2
        dados <- dados %>% select(VSC, everything())
      }
      
      # Husch
      if(input$modelo_estvsc == "LN(VFSC) = b0 + b1 * LN(DAP) + e"){
        dados$VSC <- exp( input$b0_estvsc + input$b1_estvsc*log(dados[[nm$dap]]) )
        dados <- dados %>% select(VSC, everything())
      }
    }
    
    # Modelos com b1 b2 e apenas DAP
    if( is.null(input$modelo_estvsc) ||  is.null(nm$dap)  || is.null(input$b0_estvsc) || is.null(input$b1_estvsc) || is.null(input$b2_estvsc) || is.na(input$modelo_estvsc) ||  is.na(nm$dap)  || is.na(input$b0_estvsc) || is.na(input$b1_estvsc) || is.na(input$b2_estvsc) || input$modelo_estvsc =="" || nm$dap ==""  || input$b0_estvsc == "" || input$b1_estvsc == "" || input$b2_estvsc == "" ){
      
    }else{
      
      # Hohenadl-Krenn
      if(input$modelo_estvsc == "VFSC = b0 + b1 * DAP + b2 * DAP² + e"){
        dados$VSC <- input$b0_estvsc + input$b1_estvsc*dados[[nm$dap]] + input$b2_estvsc*dados[[nm$dap]]^2 
        dados <- dados %>% select(VSC, everything())
      }
      # ?????
      if(input$modelo_estvsc == "LN(VFSC) = b0 + b1 * DAP + b2 * DAP² + e"){
        dados$VSC <- exp(input$b0_estvsc + input$b1_estvsc*dados[[nm$dap]] + input$b2_estvsc*dados[[nm$dap]]^2)
        dados <- dados %>% select(VSC, everything())
      }
    }
    
    # Modelos com b1, DAP e HT
    if( is.null(input$modelo_estvsc) ||  is.null(nm$dap)  || is.null(input$b0_estvsc) || is.null(input$b1_estvsc) ||  is.null(input$col.ht) ||  is.null(nm$ht.est) || is.na(input$modelo_estvsc) ||  is.na(nm$dap)  || is.na(input$b0_estvsc) || is.na(input$b1_estvsc) ||  is.na(input$col.ht) || is.na(nm$ht.est) || input$modelo_estvsc =="" || nm$dap =="" || input$b0_estvsc == "" || input$b1_estvsc == ""  ){
      
    }else{
      
      # Spurr logaritimico
      if(input$modelo_estvsc == "LN(VFSC) = b0 + b1 * LN(DAP² * HT) + e"){
        dados$VSC <- exp(input$b0_estvsc + input$b1_estvsc*log( (dados[[nm$dap]]^2)*dados[[nm$ht.est]]  )  )
        dados <- dados %>% select(VSC, everything())
      }
      # Spurr
      if(input$modelo_estvsc == "VFSC = b0 + b1 * DAP² * HT + e"){
        dados$VSC <- input$b0_estvsc + input$b1_estvsc*(dados[[nm$dap]]^2)*dados[[nm$ht.est]]  
        dados <- dados %>% select(VSC, everything())
      }
    }
    
    # Modelos com b1, b2, DAP e HT
    if( is.null(input$modelo_estvsc) ||  is.null(nm$dap)  || is.null(input$b0_estvsc) || is.null(input$b1_estvsc) || is.null(input$b2_estvsc) ||  is.null(input$col.ht) || is.null(nm$ht.est) || is.na(input$modelo_estvsc) ||  is.na(nm$dap)  || is.na(input$b0_estvsc) || is.na(input$b1_estvsc) || is.na(input$b2_estvsc) || is.na(input$col.ht) || is.na(nm$ht.est) || input$modelo_estvsc =="" || nm$dap ==""  || input$b0_estvsc == "" || input$b1_estvsc == "" || input$b2_estvsc == "" || input$col.ht =="" || nm$ht.est== "" ){
      
    }else{
      
      # Schumacher e Hall logaritimico
      if(input$modelo_estvsc == "LN(VFSC) = b0 + b1 * LN(DAP) + b2 * LN(HT) + e"){
        dados$VSC <- exp(input$b0_estvsc + input$b1_estvsc*log(dados[[nm$dap]]) + input$b2_estvsc*log(dados[[nm$ht.est]])  )
        dados <- dados %>% select(VSC, everything())
      }
      # Schumacher e Hall
      if(input$modelo_estvsc == "VFSC = b0 + b1^DAP + b2^HT + e"){
        dados$VSC <- input$b0_estvsc + input$b1_estvsc^dados[[nm$dap]] + input$b2_estvsc^dados[[nm$ht.est]]    
        dados <- dados %>% select(VSC, everything())
      }
    }
    
    
    
    # end ####
    dados    
    
  })
  output$HtVolEstData_table <- DT::renderDataTable({
    
    data <- round_df(HtVolEstData(), 4)
    
    
    DT::datatable(data,
                  
                  options = list(
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                      "}")
                  )
    )
    
  })
  
  # Graficos de altura
  ht_scatter <- reactive({
    
    nm <- varnames()
    dados <- arvData()
    validate(
      need(dados, "Por favor faça o upload da base de dados"),
      need(nrow(dados)>0,"Base de dados vazia"),
      need(input$df != "Dados em nivel de parcela", "Base de dados incompativel" ),
      need(nm$dap,"Por favor mapeie a coluna referente a 'dap'  "),
      need(nm$ht,"Por favor mapeie a coluna referente a 'ht'  ")#,
     # need(input$modelo_est_ht,"Coluna mapeada à 'Altura' não possui alturas não medidas")
           )
    
    if(is.null(input$col.ht) || is.na(input$col.ht) || input$col.ht=="" ){
      
      
    }else if( !any(is.na(dados[[input$col.ht]])) ) {
      return()
    }
    
    dados <- dados %>%  filter( !is.na(.data[[nm$ht]]) )
    
    # adicionar estrato como cor
    if(is.null(nm$estrato) || is.na(nm$estrato) || nm$estrato==""){
      grupo <- ""
    }else if(input$ajuste_p_estrato){
      grupo <- nm$estrato
    }else{
      grupo <- ""
    }
    # Tentar Ajustar os modelos utilizando try, e salvar em uma lista,
    # junto com a altura observada
    lista <- list(
      dados[!is.na(dados[nm$ht]), nm$ht,drop=F],
      "LN(HT) = b0 + b1 * 1/DAP + e"               = try(lm_table(dados, paste( "log(`", nm$ht, "`) ~ inv(`", nm$dap ,"`)",sep="")                          ,.groups = grupo, output = "est" )[["est"]], silent = T),
      "LN(HT) = b0 + b1 * LN(DAP) + e"             = try(lm_table(dados, paste( "log(`", nm$ht, "`) ~ log(`", nm$dap ,"`)",sep="")                          ,.groups = grupo, output = "est" )[["est"]], silent = T),
      "LN(HT) = b0 + b1 * 1/DAP + b2 * LN(HD) + e" = try(lm_table(dados, paste( "log(`", nm$ht, "`) ~ inv(`", nm$dap ,"`) + ", "log(`", nm$hd ,"`)",sep="") ,.groups = grupo, output = "est" )[["est"]], silent = T)
    )
    
    # adicionar grupos, para a lista, caso o usuário selecione o ajuste por grupos
    if(is.null(nm$estrato) || is.na(nm$estrato) || nm$estrato==""){
     
    }else if(input$ajuste_p_estrato){
      lista[[nm$estrato]] <- dados[!is.na(dados[nm$ht]), nm$estrato,drop=F]
    }else{
     
    }
    
    
    # Criar um dataframe apenas com os modelos que ajustaram
    dados2 <- as.data.frame(do.call(cbind,lista[!sapply(lista, is, "try-error")]))
    
    # Criar os graficos
    # suppressWarnings evita avisos quando um dos modelos nao for ajustado
    suppressWarnings(
      
      residuos_exp(dados2, 
                   nm$ht, 
                   "LN(HT) = b0 + b1 * 1/DAP + e", 
                   "LN(HT) = b0 + b1 * LN(DAP) + e",
                   "LN(HT) = b0 + b1 * 1/DAP + b2 * LN(HD) + e", nrow = 1,
                   type = "scatterplot",
                   color = grupo[length(grupo)] )
      
    )
    
    
  })
  output$ht_scatter_plot <- renderPlot({
    
    ht_scatter()
    
  })
  
  ht_hist <- reactive({
    
    nm <- varnames()
    dados <- arvData()
    validate(
      need(dados, "Por favor faça o upload da base de dados"),
      need(nrow(dados)>0,"Base de dados vazia"),
      need(input$df != "Dados em nivel de parcela", "Base de dados incompativel" ),
      need(nm$dap,"Por favor mapeie a coluna referente a 'dap'  "),
      need(nm$ht,"Por favor mapeie a coluna referente a 'ht'  ")#,
      # need(input$modelo_est_ht,"Coluna mapeada à 'Altura' não possui alturas não medidas")
    )
    
    if(is.null(input$col.ht) || is.na(input$col.ht) || input$col.ht=="" ){
      
      
    }else if( !any(is.na(dados[[input$col.ht]])) ) {
      return()
    }
    
    dados <- dados %>%  filter( !is.na(.data[[nm$ht]]) )
    
    # adicionar estrato como cor
    if(is.null(nm$estrato) || is.na(nm$estrato) || nm$estrato==""){
      grupo <- ""
    }else if(input$ajuste_p_estrato){
      grupo <- nm$estrato
    }else{
      grupo <- ""
    }
    # Tentar Ajustar os modelos utilizando try, e salvar em uma lista,
    # junto com a altura observada
    lista <- list(
      dados[!is.na(dados[nm$ht]), nm$ht,drop=F],
      "LN(HT) = b0 + b1 * 1/DAP + e"               = try(lm_table(dados, paste( "log(`", nm$ht, "`) ~ inv(`", nm$dap ,"`)",sep="")                          ,.groups = grupo, output = "est" )[["est"]], silent = T),
      "LN(HT) = b0 + b1 * LN(DAP) + e"             = try(lm_table(dados, paste( "log(`", nm$ht, "`) ~ log(`", nm$dap ,"`)",sep="")                          ,.groups = grupo, output = "est" )[["est"]], silent = T),
      "LN(HT) = b0 + b1 * 1/DAP + b2 * LN(HD) + e" = try(lm_table(dados, paste( "log(`", nm$ht, "`) ~ inv(`", nm$dap ,"`) + ", "log(`", nm$hd ,"`)",sep="") ,.groups = grupo, output = "est" )[["est"]], silent = T)
    )
    
    # adicionar grupos, para a lista, caso o usuário selecione o ajuste por grupos
    if(is.null(nm$estrato) || is.na(nm$estrato) || nm$estrato==""){
      
    }else if(input$ajuste_p_estrato){
      lista[[nm$estrato]] <- dados[!is.na(dados[nm$ht]), nm$estrato,drop=F]
    }
    
    
    # Criar um dataframe apenas com os modelos que ajustaram
    dados2 <- as.data.frame(do.call(cbind,lista[!sapply(lista, is, "try-error")]))
    
    # Criar os graficos
    # suppressWarnings evita avisos quando um dos modelos nao for ajustado
    suppressWarnings(
      
      residuos_exp(dados2, 
                   nm$ht, 
                   "LN(HT) = b0 + b1 * 1/DAP + e", 
                   "LN(HT) = b0 + b1 * LN(DAP) + e",
                   "LN(HT) = b0 + b1 * 1/DAP + b2 * LN(HD) + e", nrow = 1,
                   type = "histogram_curve",
                   color = grupo[length(grupo)] )
      
    )
    
    
  })
  output$ht_hist_plot <- renderPlot({
    
    ht_hist()
    
  })
  
  ht_vs <- reactive({
 
    nm <- varnames()
    dados <- arvData()
    validate(
      need(dados, "Por favor faça o upload da base de dados"),
      need(nrow(dados)>0,"Base de dados vazia"),
      need(input$df != "Dados em nivel de parcela", "Base de dados incompativel" ),
      need(nm$dap,"Por favor mapeie a coluna referente a 'dap'  "),
      need(nm$ht,"Por favor mapeie a coluna referente a 'ht'  ")#,
      # need(input$modelo_est_ht,"Coluna mapeada à 'Altura' não possui alturas não medidas")
    )
    
    if(is.null(input$col.ht) || is.na(input$col.ht) || input$col.ht=="" ){
      
      
    }else if( !any(is.na(dados[[input$col.ht]])) ) {
      return()
    }
    
    dados <- dados %>%  filter( !is.na(.data[[nm$ht]]) )
    
    # adicionar estrato como cor
    if(is.null(nm$estrato) || is.na(nm$estrato) || nm$estrato==""){
      grupo <- ""
    }else if(input$ajuste_p_estrato){
      grupo <- nm$estrato
    }else{
      grupo <- ""
    }
    
    # Tentar Ajustar os modelos utilizando try, e salvar em uma lista,
    # junto com a altura observada
    lista <- list(
      dados[!is.na(dados[nm$ht]), nm$ht,drop=F],
      "LN(HT) = b0 + b1 * 1/DAP + e"               = try(lm_table(dados, paste( "log(`", nm$ht, "`) ~ inv(`", nm$dap ,"`)",sep="")                          ,.groups = grupo, output = "est" )[["est"]], silent = T),
      "LN(HT) = b0 + b1 * LN(DAP) + e"             = try(lm_table(dados, paste( "log(`", nm$ht, "`) ~ log(`", nm$dap ,"`)",sep="")                          ,.groups = grupo, output = "est" )[["est"]], silent = T),
      "LN(HT) = b0 + b1 * 1/DAP + b2 * LN(HD) + e" = try(lm_table(dados, paste( "log(`", nm$ht, "`) ~ inv(`", nm$dap ,"`) + ", "log(`", nm$hd ,"`)",sep="") ,.groups = grupo, output = "est" )[["est"]], silent = T)
    )
    
    # adicionar grupos, para a lista, caso o usuário selecione o ajuste por grupos
    if(is.null(nm$estrato) || is.na(nm$estrato) || nm$estrato==""){
      
    }else if(input$ajuste_p_estrato){
      lista[[nm$estrato]] <- dados[!is.na(dados[nm$ht]), nm$estrato,drop=F]
    }
    
    
    # Criar um dataframe apenas com os modelos que ajustaram
    dados2 <- as.data.frame(do.call(cbind,lista[!sapply(lista, is, "try-error")]))
    
    # Criar os graficos
    # suppressWarnings evita avisos quando um dos modelos nao for ajustado
    suppressWarnings(
      
      residuos_exp(dados2, 
                   nm$ht, 
                   "LN(HT) = b0 + b1 * 1/DAP + e", 
                   "LN(HT) = b0 + b1 * LN(DAP) + e",
                   "LN(HT) = b0 + b1 * 1/DAP + b2 * LN(HD) + e", nrow = 1,
                   type = "versus",
                   color = grupo[length(grupo)] )
      
    )
    
    
  })
  output$ht_vs_plot <- renderPlot({
    
    ht_vs()
    
  })
  
  # Distribuicoes e graficos ####
  
  dd_list <- reactive({
    
    nm <- varnames()
    dados <- HtVolEstData()
    
    validate(
      need(dados, "Por favor faça o upload da base de dados"),
      need(nrow(dados)>0,"Base de dados vazia"),
      need(input$df != "Dados em nivel de parcela", "Base de dados incompativel" ),
      need(nm$dap,"Por favor mapeie a coluna referente a 'dap'  "),
      need(nm$parcelas,"Por favor mapeie a coluna referente a 'parcelas'  "),
      need(nm$area.parcela,"Por favor mapeie a coluna ou insira um valor referente a 'area.parcela'  ") )
    
    lista <- list()
    lista[["dd_geral"]] <- classe_diametro(df = dados, 
                                           dap = nm$dap,
                                           parcela = nm$parcelas,
                                           area_parcela = nm$area.parcela, 
                                           ic = nm$IC.dap, 
                                           dapmin = nm$diam.min, 
                                           especies = NA, 
                                           volume = nm$vcc,
                                           rotulo.NI = NA,
                                           keep_unused_classes = TRUE )
    
     lista
  })
  
  output$dd_geral_tab <- DT::renderDataTable({
    
    g <- round_df(dd_list()[["dd_geral"]], 2)
    
    datatable( g,
               rownames = F,
               options = list(searching = FALSE,
                              paging=FALSE,
                              ordering=FALSE,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                "}")
               )
    )
    
    
  })

  dd_g1 <- reactive({
    
    g <- dd_list()[["dd_geral"]]
    #g$CC2 <-  sapply(g$CC , gsub, pattern= "[.]",replacement= "," )
    
    ggplot(g, aes(as.factor(CC),IndvHA)) +
      geom_bar(stat = "identity",color="black")+
      #   scale_y_continuous( expand=c(0,15) ) +
      ggthemes::theme_igray(base_family = "serif") +
      labs(x = "Centro de Classe de Diâmetro - CCD (cm)", y = "Nº de Individuos por hectare") + 
      geom_text(aes(label = round(IndvHA,1) ), position = position_dodge(0.9), vjust = -0.3, size = 6 ) + 
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title   = element_text(size = 26,face="bold"), 
        axis.text    = element_text(size = 22),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        strip.text.x = element_text(size = 22)   )
    
    
  })
  output$dd_graph_indv <- renderPlot({
    
    dd_g1()
    
    
  })
  dd_g2 <- reactive({
    
    g <- dd_list()[["dd_geral"]]
    
    ggplot(g, aes(as.factor(CC),volume_ha)) +
      geom_bar(stat = "identity",color="black")+
      #  scale_y_continuous( expand=c(0,15) ) +
      labs(x = "Centro de Classe de Diâmetro - CCD (cm)", y = "Volume por hectare") + 
      ggthemes::theme_igray(base_family = "serif") +
      geom_text(aes(label = round(volume_ha,1) ), position = position_dodge(0.9), vjust = -0.3, size = 6 ) + 
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title   = element_text(size = 26,face="bold"), 
        axis.text    = element_text(size = 22),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        strip.text.x = element_text(size = 22)   )
    
  })
  output$dd_graph_vol <- renderPlot({
    
    dd_g2()
    
  })
  dd_g3 <- reactive({
    
    g <- dd_list()[["dd_geral"]] 
    
    ggplot(g, aes(as.factor(CC),G_ha)) +
      geom_bar(stat = "identity",color="black")+
      # scale_y_continuous( expand=c(0,15) ) +
      labs(x = "Centro de Classe de Diâmetro - CCD (cm)", y = "Área Basal (G) por hectare") + 
      ggthemes::theme_igray(base_family = "serif") +
      geom_text(aes(label = round(G_ha,1) ), position = position_dodge(0.9), vjust = -0.3, size = 6 ) + 
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title   = element_text(size = 26,face="bold"), 
        axis.text    = element_text(size = 22),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        strip.text.x = element_text(size = 22)   )
    
  })
  output$dd_graph_G <- renderPlot({
    
    dd_g3()
    
  })
  
  dist_ht <- reactive({
    
    nm <- varnames()
    dados <- rawData()
    
    validate(
      need(dados, "Por favor faça o upload da base de dados"),
      need(nrow(dados)>0,"Base de dados vazia"),
      need(input$df != "Dados em nivel de parcela", "Base de dados incompativel" ),
      need(nm$ht,"Por favor mapeie a coluna referente a 'altura'  "),
      need(nm$parcelas,"Por favor mapeie a coluna referente a 'parcelas'  "),
      need(nm$area.parcela,"Por favor mapeie a coluna ou insira um valor referente a 'area.parcela'  ") )

    classe_diametro(df = dados, 
                    dap = nm$ht, 
                    parcela = nm$parcelas, 
                    area_parcela = nm$area.parcela,
                    ic = nm$IC.ht, 
                    dapmin = nm$ht.min  ) %>% select(-G,-G_ha)
    
  }) 
  dist_ht_graph <- reactive({
    
    g <- dist_ht()
    
    ggplot(g, aes(as.factor(CC),IndvHA)) +
      geom_bar(stat = "identity",color="black")+
      #   scale_y_continuous( expand=c(0,15) ) +
      ggthemes::theme_igray(base_family = "serif") +
      labs(x = "Centro de Classe de Altura (m)", y = "Nº de Individuos por hectare") + 
      geom_text(aes(label = round(IndvHA,1) ), position = position_dodge(0.9), vjust = -0.3, size = 6 ) + 
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title   = element_text(size = 26,face="bold"), 
        axis.text    = element_text(size = 22),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        strip.text.x = element_text(size = 22)   )
    
    
  })
  
  output$dist_ht_tabela <- DT::renderDataTable({
    
    g <- dist_ht()
    datatable( g,
               rownames = F,
               options = list(searching = FALSE,
                              paging=FALSE,
                              ordering=FALSE,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                "}")
               )
    )
    
    
  })
  output$dist_ht_plot <- renderPlot({
    
    dist_ht_graph()
    
  })
  
  obs_freq <- reactive({
    
    nm <- varnames()
    data <- rawData()
    
    validate(
      need(data, "Por favor faça o upload da base de dados"),
      need(nrow(data)>0,"Base de dados vazia"),
      need(input$df != "Dados em nivel de parcela", "Base de dados incompativel" ),
      need(nm$obs,"Por favor mapeie a coluna referente a 'observacao'  ")  )
    
    data %>% 
      group_by_at(vars(nm$obs)) %>% 
      summarise(`Frequencia absoluta`=n() ) %>% 
      mutate(`Frequencia relativa` = round(`Frequencia absoluta` /sum(`Frequencia absoluta` ) * 100, 2) )
    
    
  })
  output$obs_tabela <- DT::renderDataTable({
    
    g <- obs_freq()
    datatable( g,
               rownames = F,
               options = list(searching = FALSE,
                              paging=FALSE,
                              ordering=FALSE,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                "}")
               )
    )
    
    
  })
  
  obs_graph_1 <- reactive({
    
    nm <- varnames()
    g <- rawData()
    
    validate(
      need(g, "Por favor faça o upload da base de dados"),
      need(nrow(g)>0,"Base de dados vazia"),
      need(input$df != "Dados em nivel de parcela", "Base de dados incompativel" ),
      need(nm$obs,"Por favor mapeie a coluna referente a 'observacao'  ")  )
    
    
    
    ggplot(g,aes_string(nm$obs)) +
      geom_bar( ) +
      geom_text(aes( label = ..count.. ), 
                stat= "count", vjust = -.5,family="serif",size=7 ) +
      labs(y = "Contagem" ) +
      ggthemes::theme_igray(base_family = "serif") +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(), 
        panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(), 
        axis.title = ggplot2::element_text(size = 26, face="bold"), 
        axis.text = ggplot2::element_text(size = 22), 
        axis.line.x = ggplot2::element_line(color = "black"), 
        axis.line.y = ggplot2::element_line(color = "black"), 
        strip.text.x = ggplot2::element_text(size = 22, face = "bold"),
        legend.text = ggplot2::element_text(size=20), 
        legend.title = ggplot2::element_text(size=20) )
    
    
  })
  output$obs_plot_1 <- renderPlot({
    
    obs_graph_1()
    
  })
  obs_graph_2 <- reactive({
    
    nm <- varnames()
    g <- rawData()

      validate(
      need(g, "Por favor faça o upload da base de dados"),
      need(nrow(g)>0,"Base de dados vazia"),
      need(input$df != "Dados em nivel de parcela", "Base de dados incompativel" ),
      need(nm$obs,"Por favor mapeie a coluna referente a 'observacao'  ")  )
    

    
      ggplot(g,aes_string(nm$obs)) +
      geom_bar(aes(y = (..count..)/sum(..count..) ) ) +
      geom_text(aes( label = scales::percent( (..count..)/sum(..count..) ),
                     y= (..count..)/sum(..count..) ), stat= "count", vjust = -.5,family="serif",size=7 ) +
      labs(y = "Porcentagem" ) +
      scale_y_continuous(labels = scales::percent) +
      ggthemes::theme_igray(base_family = "serif") +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(), 
        panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(), 
        axis.title = ggplot2::element_text(size = 26, face="bold"), 
        axis.text = ggplot2::element_text(size = 22), 
        axis.line.x = ggplot2::element_line(color = "black"), 
        axis.line.y = ggplot2::element_line(color = "black"), 
        strip.text.x = ggplot2::element_text(size = 22, face = "bold"),
        legend.text = ggplot2::element_text(size=20), 
        legend.title = ggplot2::element_text(size=20) )
    
    
  })
  output$obs_plot_2 <- renderPlot({
    
    obs_graph_2()
    
  })
  

  # totalizacao de parcelas ####
  
  tot_parcData <- reactive({
    
    nm <- varnames()
    dados <- HtVolEstData()
    validate(
      need(dados, "Por favor faça o upload da base de dados"),
      need(nrow(dados)>0,"Base de dados vazia"),
      need(input$df != "Dados em nivel de parcela", "Base de dados incompativel" ),
      need(nm$dap,"Por favor mapeie a coluna referente a 'dap'  "),
      #need(nm$vcc,"Por favor mapeie a coluna referente a 'volume com casca' ou estime-o na aba preparação  "),
      need(nm$parcelas,"Por favor mapeie a coluna referente a 'parcelas'  "),
      need(nm$area.parcela,"Por favor mapeie a coluna ou insira um valor referente a 'area.parcela'  "),
      need(nm$area.total,"Por favor mapeie a coluna ou insira um valor referente a 'area.total'  ")
    )
    
    # Verificar se caso o usuario escolha volume como variavel para o inventario
    # esta deve ser mapaeada anteriormente
    validate(check_yi(nm$vcc, input$yi_inv), errorClass = "WRONG")
    
    # Se o usuario inseir uma variavel de Estrato, considera-la na hora dos calculos
    if(nm$estrato =="" ){grupos<-nm$parcela}else{grupos <- c(nm$estrato, nm$parcela)}
    
    x <- inv_summary(df           = dados, 
                     DAP          = nm$dap, 
                     HT           = nm$ht.est,
                     VCC          = nm$vcc,
                     area_parcela = nm$area.parcela,
                     .groups      = grupos,
                     area_total   = nm$area.total,
                     idade        = nm$idade,
                     VSC          = nm$vsc,
                     Hd           = nm$hd) %>% 
       dplyr::ungroup()

    x
    
  })
  output$tot_parc_tab <- renderDataTable({ # renderizamos uma DT::DataTable
    
    data <- tot_parcData() 
    
    datatable(data,
              options = list(initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                "}"),
                pageLength = 25
              )   
    ) # Criamos uma DT::datatable com base no objeto
    
  })
  
  # Switch para trocar o dado utilizado no inventario ####
  
  invData <- reactive({
    
    #if(is.null(input$df)){ return()}
    req(input$df)
    
    # Se o dado for em nivel de arvore, a totalização de parcelas deve ser feita para que
    # NewData possa ser inserido em acs. Sem essa condição a ui gera mensagens de erro
    switch(input$df, 
           "Dados em nivel de fuste" = if(is.null(tot_parcData()) ){return()}else{ tot_parcData()},
           "Dados em nivel de arvore" = if(is.null(tot_parcData()) ){return()}else{ tot_parcData()},
           "Dados em nivel de parcela" = rawData() )
    
  })
  
  # amostragem casual simples ####
  
  # UI para rodar acs por estrato
  output$acs_estrato_rb <- renderUI({
    
    req(input$tabset_inv=="Amostragem casual simples")
    
    radioButtons("acs_estrato", 
                 "Calcular uma amostragem casual simples para cada estrato?",
                 choices = c("Sim"=T,"Nao"=F),
                 selected = F,
                 inline = T)
    
  })
  output$acs_as_warning <- renderUI({
    
    req(any( c(input$acs_estrato==T, input$as_estrato==T)   ), # Precisa que o usuario tente calcular acs ou as por estrato
        input$tabset_inv %in% c("Amostragem casual simples", "Amostragem sistemática") ) # precisa que a aba acs ou as seja selecionada
    validate(
      need(!is.null(input$col.estrato) , # estrato nao e nulo? quando a resposta for nao a mensagem aparece
           "Variável 'estrato' não definida. A amostragem será feita para todos os dados." ), errorClass = "AVISO")
    
    
    
  })
  
  
  # funcao acs aplicada em invData
  tabacs <- reactive({
    
    nm <- varnames()
    dados <- invData()
    
    validate(
      need(dados, "Por favor, faça a totalização de parcelas, ou o upload de uma base de dados em nível de parcela" ),
      need(nrow(dados)>0,"Base de dados vazia"),
      #need(nm$vcc,"Por favor mapeie a coluna referente a 'volume com casca' ou estime-o na aba preparação  "),
      need(nm$area.parcela,"Por favor mapeie a coluna ou insira um valor referente a 'area.parcela'  "),
      need(nm$area.total,"Por favor mapeie a coluna ou insira um valor referente a 'area.total'  ")
    )

    grupos_name <- ""
    # Fazer amostragem por estrato somente se o usuario marcar sim
    if(is.null(input$acs_estrato)){
    
    }else if(input$acs_estrato==T){
      grupos_name <- nm$estrato
    }else if(input$acs_estrato==F){
      grupos_name <- ""
    }
    
    if(input$df=="Dados em nivel de parcela"){
      dados <- dados %>% dplyr::rename(VCC = !!(rlang::sym(nm$vcc)) )
    }
    
    x <-     acs(df             = dados,
                 Yi             = input$yi_inv,
                 area_parcela   = nm$area.parcela,
                 area_total     = nm$area.total, 
           #      idade          = nm$idade,
                 .groups         = grupos_name, 
                 alpha          = input$alpha_inv, 
                 erro           = input$erro_inv, 
                 casas_decimais = input$cd_inv, 
                 pop            = input$pop_inv, 
                 tidy           = TRUE)
    
    x
    
  })
  # tabela acs
  output$acs <- renderDataTable({
    
    acsdt <- tabacs() 
    # converte em datatable        # cria formattable
    as.datatable( formattable(acsdt, 
                              list(    # colore a linha 6 da coluna dois de verde ou vemelho, se ela for menor ou maior que o numero da linha 1 coluna 2
                                area(row=6, col=2) ~  formatter("span", 
                                                                style = x ~ formattable::style(color = ifelse(x <= acsdt[1,2], "#108e00", "red"))) ,
                                # colore o erro estimado de verde ou vemelho, se ela for menor ou maior que o erro desejado
                                area(row=10, col=2) ~ formatter("span", 
                                                                style = x ~ formattable::style(color = ifelse(x <= input$erro_inv, "#108e00", "red")))
                                
                                
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
    
    
    
  })
  
  # Amostragem casual estratificada ####
  
  # funcao ace aplicada em invData
  list_ace <- reactive({
    
    nm <- varnames()
    dados <- invData()
    
    validate(
      need(dados, "Por favor, faça a totalização de parcelas, ou o upload de uma base de dados em nível de parcela" ),
      need(nrow(dados)>0,"Base de dados vazia"),
      #need(nm$vcc,"Por favor mapeie a coluna referente a 'volume com casca' ou estime-o na aba preparação  "),
      need(nm$area.parcela,"Por favor mapeie a coluna ou insira um valor referente a 'area.parcela'  "),
      need(nm$area.total,"Por favor mapeie a coluna ou insira um valor referente a 'area.total'  "),
      need(nm$estrato,"Por favor mapeie a coluna referente a 'Estrato' ")
    )
    
    if(input$df=="Dados em nivel de parcela"){
      dados <- dados %>% dplyr::rename(VCC = !!(rlang::sym(nm$vcc)) )
    }
    
    x <- ace(df             = dados, 
             Yi             = input$yi_inv,
             area_parcela   = nm$area.parcela, 
             area_estrato   = nm$area.total, 
             .groups         = nm$estrato, 
            # idade          = nm$idade, 
             alpha          = input$alpha_inv, 
             erro           = input$erro_inv, 
             casas_decimais = input$cd_inv, 
             pop            = input$pop_inv, 
             tidy           = TRUE)
    x
    
  })
  # tabela ace1
  output$ace1 <- renderDataTable({
    
    ace1dt <- list_ace()[[1]] 
    
    datatable( ace1dt, # seleciona a linha 5 previamente
               selection = list(mode = 'multiple', selected = c(14,18,19,20), target = 'row'),
               options = list(searching = FALSE,
                              paging=FALSE,
                              initComplete = JS( # muda a cor do cabecalho
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                "}")
               )   
               
    )
    
  })
  # tabela ace2
  output$ace2 <- renderDataTable({
    
    ace2dt <- list_ace()[[2]] 
    
    # converte em datatable        # cria formattable
    as.datatable( formattable(ace2dt, 
                              list(
                                # colore o erro estimado de verde ou vemelho, se ela for menor ou maior que o erro desejado
                                area(row=5, col=2) ~ formatter("span", 
                                                               style = x ~ formattable::style(color = ifelse(x <= input$erro_inv, "#108e00", "red")))
                                
                                
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
    
    
  })
  
  # Amostragem sistematica ####
  
  # UI para rodar as por estrato
  output$as_estrato_rb <- renderUI({
    
    req(input$tabset_inv=="Amostragem sistemática")
    
    radioButtons("as_estrato", 
                 "Calcular uma amostragem sistematica para cada estrato?",
                 choices = c("Sim"=T,"Nao"=F),
                 selected = F,
                 inline = T)
    
  })
  
  # funcao as aplicada em invData
  tabas <- reactive({
    
    nm <- varnames()
    dados <- invData()
    
    validate(
      need(dados, "Por favor, faça a totalização de parcelas, ou o upload de uma base de dados em nível de parcela" ),
      need(nrow(dados)>0,"Base de dados vazia"),
      #need(nm$vcc,"Por favor mapeie a coluna referente a 'volume com casca' ou estime-o na aba preparação  "),
      need(nm$area.parcela,"Por favor mapeie a coluna ou insira um valor referente a 'area.parcela'  "),
      need(nm$area.total,"Por favor mapeie a coluna ou insira um valor referente a 'area.total'  ")
    )
    
    grupos_name <- NULL
    # Fazer amostragem por estrato somente se o usuario marcar sim
    if(is.null(input$as_estrato)){
      
    }else if(input$as_estrato){
      grupos_name <- nm$estrato
    }
    
    if(input$df=="Dados em nivel de parcela"){
      dados <- dados %>% dplyr::rename(VCC = !!(rlang::sym(nm$vcc)) )
    }
    
    dados <- invData()
    
    x <- as_diffs(df             = dados, 
                  Yi             = input$yi_inv,
                  area_parcela   = nm$area.parcela,
                  area_total     = nm$area.total, 
                 # idade          = nm$idade,
                  .groups         = grupos_name, 
                  alpha          = input$alpha_inv, 
                  erro           = input$erro_inv, 
                  casas_decimais = input$cd_inv, 
                  tidy           = TRUE )
    
    x
    
  }) 
  # tabela as
  output$as <- renderDataTable({
    
    asdt <- tabas() 
    
    # converte em datatable        # cria formattable
    as.datatable( formattable(asdt, 
                              list(    # colore a linha 6 da coluna dois de verde ou vemelho, se ela for menor ou maior que o numero da linha 1 coluna 2
                                area(row=6, col=2) ~  formatter("span", 
                                                                style = x ~ formattable::style(color = ifelse(x <= asdt[1,2], "#108e00", "red"))) ,
                                # colore o erro estimado de verde ou vemelho, se ela for menor ou maior que o erro desejado
                                area(row=10, col=2) ~ formatter("span", 
                                                                style = x ~ formattable::style(color = ifelse(x <= input$erro_inv, "#108e00", "red")))
                                
                                
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
    
  })
  
  # Download tabelas ####
  
  # Cria um valor inicial zero para verificar se o usuario fez algum download ou nao.
  # Se o usuario clicar em algum botao de download, sera add a esse valor uma unidade.
  rnDownloads <- reactiveValues(ndown=0)
  
  output$checkbox_df_download <- renderUI({
    
    checkboxGroupInput("dataset", h3("Escolha uma ou mais tabelas, e clique no botão abaixo:"), 
                       choices =  c(
                         "Dados inconsistentes"              ,
                         "Dado preparado"                    ,
                         "Dado nivel arvore"                 ,
                         "Dado volume por arvore"            ,
                         "Distribuicao diametrica"           ,
                         "Distribuicao de altura"            ,
                         "Totalizacao de parcelas"           ,
                         "Amostragem Casual Simples"         ,
                         "Amostragem Casual Estrat 1" ,
                         "Amostragem Casual Estrat 2" ,
                         "Amostragem Sistematica"            
                       ), inline = T )
    
    
  })
  
  list_of_df_to_download <- reactive({
    
    L <- list()
    
    if("Dados inconsistentes" %in% input$dataset ) {
      L[["Dados inconsistentes"]] <- try( consist_fun(), silent = T) 
    }
    
    if("Dado preparado" %in% input$dataset ) {
      L[["Dado preparado"]] <-  try(rawData(), silent = T)
    }
    
    if("Dado nivel arvore" %in% input$dataset ) {
      L[["Dado nivel arvore"]] <-  try(arvData(), silent = T)
    }
    
    if("Dado volume por arvore" %in% input$dataset ) {
      L[["Dado volume por arvore"]] <-  try(HtVolEstData(), silent = T)
    }
    
    if("Distribuicao diametrica" %in% input$dataset ) {
      L[["Distribuicao diametrica"]] <-  try(dd_list()[["dd_geral"]], silent=T)
    }
    
    if("Distribuicao de altura" %in% input$dataset ) {
      L[["Distribuicao de altura"]] <-  try(dist_ht(), silent=T)
    }
    
    if("Totalizacao de parcelas" %in% input$dataset ) {
      L[["Totalizacao de parcelas"]] <- try(tot_parcData() , silent=T) 
    }
    
    if("Amostragem Casual Simples" %in% input$dataset ) {
      L[["Amostragem Casual Simples"]] <- try(tabacs() , silent=T)
    }
    
    if("Amostragem Casual Estrat 1" %in% input$dataset ) {
      L[["Amostragem Casual Estrat 1"]] <- try(list_ace()[[1]], silent = T)
    }
    
    if("Amostragem Casual Estrat 2" %in% input$dataset ) {
      L[["Amostragem Casual Estrat 2"]] <- try(list_ace()[[2]] , silent=T)
    }
    
    if("Amostragem Sistematica" %in% input$dataset ) {
      L[["Amostragem Sistematica"]] <- try( tabas() , silent=T)
    }
    
    # Remover dataframes que geraram errol
    L <- L[!sapply(L, is,"try-error")]
    
    L
    
  })
  list_of_df_all <- reactive({
    
    L <- list()
    
    L[["Dados inconsistentes"]] <- try( consist_fun(), silent = T) 
    
    L[["Dado preparado"]]       <-  try(rawData(), silent = T)
    
    L[["Dado nivel arvore"]] <-  try(arvData(), silent = T)
    
    L[["Dado volume por arvore"]] <-  try(HtVolEstData(), silent = T)
    
    L[["Distribuicao diametrica"]] <-  try(dd_list()[["dd_geral"]], silent=T)
    
    L[["Distribuicao de altura"]] <-  try(dist_ht(), silent=T)
    
    L[["Totalizacao de parcelas"]] <- try(tot_parcData() , silent=T) 
    
    L[["Amostragem Casual Simples"]] <- try(tabacs() , silent=T)
    
    L[["Amostragem Casual Estrat 1"]] <- try(list_ace()[[1]], silent = T)
    
    L[["Amostragem Casual Estrat 2"]] <- try(list_ace()[[2]] , silent=T)
    
    L[["Amostragem Sistematica"]] <- try( tabas() , silent=T)
    
    # Remover dataframes que geraram errol
    L <- L[!sapply(L, is,"try-error")]
    
    L
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){"tabelas_app_inventario.xlsx"},
    
    content = function(file){
      rnDownloads$ndown <- rnDownloads$ndown + 1
      suppressWarnings(openxlsx::write.xlsx( list_of_df_to_download(), file ))}
    
  )
  
  output$downloadAllData <- downloadHandler(
    filename = function(){"tabelas_app_inventario.xlsx"},
    
    content = function(file){ 
      rnDownloads$ndown <- rnDownloads$ndown + 1
      suppressWarnings(openxlsx::write.xlsx( list_of_df_all(), file )) }
    
  )
  
  # Download graficos ####
  
  graphInput <- reactive({
    switch(input$graph_d,
           "Indv. por ha por CC"                            = dd_g1(),
           "Vol. por ha por CC"                             = dd_g2(),
           "G por ha por CC"                                = dd_g3(),
           "Indv. por ha por classe de ht"                  = dist_ht_graph(),
           "Frequencia para var. Qualidade"                 = obs_graph_1(),
           "Porcentagem para var. Qualidade"                = obs_graph_2(),
           "Dispersao dos residuos em porcentagem para HT"  = ht_scatter(),
           "Histograma dos residuos em porcentagem para HT" = ht_hist(),
           "HT vs HT estimada"                              = ht_vs()
           
    )
  })
  
  output$graph_d_out <- renderPlot({
    rnDownloads$ndown <- rnDownloads$ndown + 1
    
    g <- graphInput()
    
    g
    
    
  }) 
  
  output$downloadGraph <- downloadHandler(
    filename = function() { 
      
      if(input$graphformat==".png")
      {
        paste(input$graph_d, '.png', sep='') 
      }
      else if(input$graphformat==".jpg")
      {
        paste(input$graph_d, '.jpg', sep='') 
      }
      else if(input$graphformat==".pdf")
      {
        paste(input$graph_d, '.pdf', sep='') 
      }
      
    },
    
    content = function(file) {
      
      ggsave(file, graphInput(), width = 12, height = 6 )
      
      
    }
  )
  # session end ####
  # session$onSessionEnded(function() {
  #  stopApp()
  #  q("no")
  #})
  # ####
})

