#' @export

arv_summary <- function(df, arvore, dap, .groups){
  # Checagem de variaveis ####
  
  # Definir pipe para facilitar
  `%>%` <- dplyr::`%>%`
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se arvore nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(arvore) ){  
    stop("arvore not set", call. = F) 
  }else if( !is.character(arvore) ){
    stop("'arvore' must be a character containing a variable name", call.=F)
  }else if(length(arvore)!=1){
    stop("Length of 'arvore' must be 1", call.=F)
  }else if(check_names(df, arvore)==F){
    stop(check_names(df, arvore, boolean=F), call.=F)
  }
  
  # se dap nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(dap) ){  
    stop("dap not set", call. = F) 
  }else if( !is.character(dap) ){
    stop("'dap' must be a character containing a variable name", call.=F)
  }else if(length(dap)!=1){
    stop("Length of 'dap' must be 1", call.=F)
  }else if(check_names(df, dap)==F){
    stop(check_names(df, dap, boolean=F), call.=F)
  }
  
  # Se .groups nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if(missing(.groups)||is.null(.groups)||is.na(.groups)||.groups==F||.groups==""){
    .groups_syms <- character()
    # Se groups for fornecido verificar se todos os nomes de variaveis fornecidos existem no dado  
  }else if(!is.character(.groups)){
    stop(".groups must be a character", call. = F)
  }else if(! length(.groups)%in% 1:10){
    stop("Length of '.groups' must be between 1 and 10", call.=F)
  }else if(check_names(df,.groups)==F){
    # Parar se algum nome nao existir, e avisar qual nome nao existe
    stop(check_names(df,.groups, boolean=F), call.=F)
    # se os grupos forem fornecidos e forem nomes dos dados
    # Transformar o objeto em simbolo, para que dplyr entenda
    # e procure o nome das variaveis dentro dos objetos
  }else{
    .groups_syms <- rlang::syms(.groups)
  }
  
  dap_name <- dap
  arvore_name <- arvore

  dap_sym          <- rlang::sym(dap)
  arvore_sym       <- rlang::sym(arvore)

  # ####
  
  x <- df %>% 
    dplyr::group_by(!!!.groups_syms, !!arvore_sym) %>% 
    dplyr::summarise(
    !!dap_name := sqrt( sum( (!!dap_sym)^2, na.rm=T) ) ) %>% 
    dplyr::na_if(0) %>% 
    as.data.frame() %>% 
    dplyr::ungroup()
  #return(x)
 
  # Remove data from other boles, keep only the first one
 df <- df %>% 
   dplyr::group_by(!!!.groups_syms, !!arvore_sym) %>% 
   dplyr::mutate(n=1:dplyr::n()) %>% 
   dplyr::filter(n==1) %>% 
   dplyr::select(-n, - (!!dap_sym) ) 
  
  
  # Unir aos dados originais remanescentes
  if(missing(.groups)||is.null(.groups)||is.na(.groups)||.groups==F||.groups==""){
    return(as.data.frame(left_join(df,x, by=carvore)) ) 
  }else{
    return(as.data.frame(left_join(df,x, by=c(.groups,arvore) )) )
  }
  
  
  
  
}
