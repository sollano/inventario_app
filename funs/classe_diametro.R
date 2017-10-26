#' @export

classe_diametro <- function(df, dap, parcela, area_parcela, ic = 5, dapmin = 5, especies=NA, volume=NA, rotulo.NI="NI", cc_to_column=F, G_to_cc=F, cctc_ha=T){
  
  # se df nao for fornecido, for igual "", nulo, ou  nao for dataframe, parar
  if(  missing(df) || df == "" || is.null(df) || !is.data.frame(df) ){  
    stop("df not set", call. = F) 
  }
  
  # se dap nao for fornecido ou for igual "", parar
  if(  missing(dap) || gsub('"', "", deparse(substitute(dap)))== "" ){ 
    stop("dap not set", call. = F) 
  }
  
  # ic precisa ser numerico e de tamanho 1
  if(!is.numeric(ic) || length(ic)>1){
    stop("ic must be a single number", call. = F)
  }
  
  # dapmin precisa ser numerico e de tamanho 1
  if(!is.numeric(dapmin) || length(dapmin)>1){
    stop("dapmin must be a single number", call. = F)
  }
  
  
  # se parcela nao for fornecida, for NULL, NA ou "", transformar em 1
  if(missing(parcela) || is.null(parcela) || is.na(parcela) ||  parcela==""){
    parcela <- 1
  }
  
  # se area_parcela nao for fornecida, for NULL, NA ou "", transformar em 10000
  if(missing(area_parcela) || is.null(area_parcela) || is.na(area_parcela) ||  area_parcela==""){
    area_parcela <- 10000
  }
  
  # app: Se especies for "" ou NULL, transformar em NA
  if(is.null(especies) || is.na(especies) ||  especies==""){
    especies <- NA
  }
  
  # app: Se volume for "" ou NULL, transformar em NA
  if(is.null(volume)|| is.na(volume) || volume==""){
    volume <- NA
  }
  
  # Parar se o usuario pedir o resultado por ha sem fornecer as variaveis necessarias
  if(cctc_ha==T & cc_to_column==T &  parcela == 1 & area_parcela == 10000 ){stop("Parcela and area_parcela must be provided if cctc_ha=TRUE",call. = F)}
  
  # Permitir se e nse como entrada de nomes de variaveis
  DAPAA <- dap
  PARCELAAA <- parcela
  AREA_PARCELAAA <- area_parcela
  ESPECIESAA <- especies
  VOLUMEAA <- volume
  
  # Testar se PARCELA e um nome de variavel
  test <- try(df %>% pull(PARCELAAA), silent = T)
  
  if(is(test, "try-error")){
    # Se nao for, salvar em npar (provavelmente e o numero de parcelas)
    npar <- parcela
  }else{
    # Se PARCELAAA for um nome de variavel, obter o numero de parcelas e salvar em uma var    
    npar <- df %>% pull(PARCELAAA) %>% as.factor %>% nlevels
  }
  
  
  # Testar se AREA_PARCELAAA e um nome de variavel
  testap <- try(df %>% pull(AREA_PARCELAAA), silent = T)
  
  if(is(testap, "try-error")){
    # Se nao for, salvar em npar (provavelmente e o numero de parcelas)
    AREA_PARCELAAA <- area_parcela
  }else{
    # Se PARCELA for um nome de variavel, obter o numero de parcelas e salvar em uma var    
    AREA_PARCELAAA <- df %>% group_by_at( vars(PARCELAAA) ) %>% summarise(AREA = mean(.data[[AREA_PARCELAAA]]), na.rm=T) %>% summarise(A = mean(AREA, na.rm=T)) %>% pull(A) 
  }
  
  # Testar se ESPECIESAA e um nome de variavel
  testesp <- try(df %>% pull(ESPECIESAA), silent = T)
  
  # Se especie for fornecida, ela deve ser um nome de variavel
  if(is(testesp, "try-error") && !is.na(especies) ){
    
    stop("especies must be a variable name",call. = F)
    
    # Se nao for, sera criada uma variavel descartavel no dataframe, para que o group_by possa rodar
  }else if(is.na(especies) || is.null(especies) || especies==""){
    
    df$ESPECIESAA <- "yay"
    
    # Se a especie for fornecida e for um nome de variavel,
    # remover as especies nao identificadas do dado
  }else if( !is(testesp, "try-error") && !is.na(especies) ){
    
    # converter rotulos NI (aplicativo)
    if(is.null(rotulo.NI)||rotulo.NI==""){rotulo.NI <- "NI"}
    
    # Remover especies nao identificadas
    df <- df[! df[[ESPECIESAA]] %in% rotulo.NI, ] 
    
  }
  
  testvol<- try(df %>% pull(VOLUMEAA), silent = T)
  
  # Se volume for fornecida, ela deve ser um nome de variavel
  if(is(testvol, "try-error") && !is.na(volume) ){
    
    stop("volume must be a variable name",call. = F)
    
    # Se nao for, sera criada uma variavel descartavel no dataframe, para que o summarise possa rodar
  }else if(is.na(volume) || is.null(volume) || volume==""){
    
    df$vol <- 23
    
    VOLUMEAA <- "vol"
    
  }
  
  # VOLUMEAA <- rlang::sym(VOLUMEAA)
  
  df_final <- df %>% 
    filter(!is.na( .data[[DAPAA]] ) ) %>% # remover NA
    mutate(
      CC = ceiling(( .data[[DAPAA]] )/ic) * ic - ic/2, # Calcular Centro de classe
      g = pi * .data[[DAPAA]]^2 / 40000   ) %>%  # Calcular area seccional
    group_by_at(vars(ESPECIESAA, CC )) %>% # Agrupar e calcular o numero de individuos, e n de individuos por ha
    summarise(
      NumIndv=n(),
      IndvHA = round( NumIndv / (AREA_PARCELAAA/10000 * npar ),  1 ),
      G = sum(g),
      G_ha = sum(g) / (AREA_PARCELAAA/10000 * npar ),
      volume = sum( .data[[VOLUMEAA]], na.rm = T  ),
      volume_ha = sum( .data[[VOLUMEAA]], na.rm = T) / (AREA_PARCELAAA/10000 * npar )     ) %>% 
    mutate(DR =  round(NumIndv/sum(NumIndv) * 100, 4) ) %>% # Calcular densidade relativa
    arrange( CC ) %>% 
    filter(CC >= dapmin) %>% # Remover classes menores que o dap minimo
    ungroup %>% 
    as.data.frame
  
  # Remover a variavel descartavel, caso tenha sido criada
  df_final$ESPECIESAA <- NULL
  
  # Remover volume caso nao tenha fornecido pelo usuario
  if(is.na(volume) || is.null(volume) || volume==""){
    
    df_final$volume <- NULL
    df_final$volume_ha <- NULL
  }
  
  # se parcela ou area_parcela nao for fornecida, for NULL, NA ou "", remover variaveis extrapoladas ha
  #if(missing(parcela) || is.null(parcela) || is.na(parcela) ||  parcela=="" || missing(area_parcela) || is.null(area_parcela) || is.na(area_parcela) ||  area_parcela=="" ){
  if(parcela == 1 & area_parcela == 10000){
    df_final$IndvHA <- NULL
    df_final$G_ha <- NULL
    df_final$volume_ha <- NULL
  }
  
  # Se o usuario quiser o centro de classe na coluna e nao tiver fornecido volume,
  # popular o centro de classe com o numero de individuos
  if(cc_to_column==T &&  G_to_cc==T ){
    
    if(cctc_ha==T){df_final$G_f <- df_final$G_ha}else if(cctc_ha==F){df_final$G_f <- df_final$G}else(stop("cctc_ha must be TRUE or FALSE",call. = F))
    
    df_final <- df_final %>% 
      select(ESPECIESAA,CC,G_f) %>% 
      tidyr::spread(CC,G_f, fill = 0) %>% 
      mutate(Total=rowSums(.[, sapply(., is.numeric)]) ) %>% 
      as.data.frame %>% 
      round_df(4)
    df_final[df_final==0] <- ""
    
  }else if(cc_to_column==T && !is(testesp, "try-error") && is(testvol, "try-error") ){
    
    if(cctc_ha==T){df_final$NI <- df_final$IndvHA}else if(cctc_ha==F){df_final$NI <- df_final$NumIndv}else(stop("cctc_ha must be TRUE or FALSE",call. = F))
    
    df_final <- df_final %>% 
      select(ESPECIESAA,CC,NI) %>% 
      tidyr::spread(CC,NI, fill = 0 ) %>% 
      mutate(Total = rowSums(.[  ,  sapply(., is.numeric)  ]  ) ) %>% 
      as.data.frame %>% 
      round_df(4)
    
    df_final[df_final==0] <- ""
    # Se o usuario quiser o centro de classe na coluna e tiver fornecido volume,
    # popular o centro de classe com o volume
  }else if(cc_to_column==T && !is(testesp, "try-error") && !is(testvol, "try-error") ){
    
    if(cctc_ha==T){df_final$VOL<- df_final$volume_ha}else if(cctc_ha==F){df_final$VOL<- df_final$volume}else(stop("cctc_ha must be TRUE or FALSE",call. = F))
    
    df_final <- df_final %>% 
      select(ESPECIESAA,CC,VOL) %>% 
      tidyr::spread(CC,VOL, fill = 0) %>% 
      mutate(Total=rowSums(.[, sapply(., is.numeric)]) ) %>% 
      as.data.frame %>% 
      round_df(4)
    df_final[df_final==0] <- ""
    
  }else if(cc_to_column==T && is(testesp, "try-error") ){
    
    stop("Especies column must be provided if cc_to_column is true ", call. = F)
    
  }
  
  return(df_final)
  
}
