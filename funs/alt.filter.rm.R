alt.filter.rm <- function(df, var,levelstorm, .groups, varstokeep){
  # ####
  `%>%` <- dplyr::`%>%`
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se var nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(var) ){  
    stop("var not set", call. = F) 
  }else if( !is.character(var) ){
    stop("'var' must be a character containing a variable name", call.=F)
  }else if(length(var)!=1){
    stop("Length of 'var' must be 1", call.=F)
  }else if(forestr::check_names(df, var)==F){
    stop(forestr::check_names(df, var, boolean=F), call.=F)
  }
  
  # Se .groups nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if(missing(.groups) && is.null(dplyr::groups(df))){
    stop(".groups must be set if data doesn't have any groups", call. = F)
  }else if(missing(.groups) && !is.null(dplyr::groups(df))){
    .groups_syms <- rlang::syms(dplyr::groups(df))
  }else if(!is.character(.groups)){
    stop(".groups must be a character", call. = F)
  }else if(! length(.groups)%in% 1:10){
    stop("Length of '.groups' must be between 1 and 10", call.=F)
  }else if(forestr::check_names(df,.groups)==F){
    # Parar se algum nome nao existir, e avisar qual nome nao existe
    stop(forestr::check_names(df,.groups, boolean=F), call.=F)
    # se os grupos forem fornecidos e forem nomes dos dados
    # Transformar o objeto em simbolo, para que dplyr entenda
    # e procure o nome das variaveis dentro dos objetos
  }else{
    .groups_syms <- rlang::syms(.groups)
  }
  
  # se varstokeep nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(varstokeep) ){  
    varstokeep <- NA
  }else if( !is.character(varstokeep) ){
    stop("'varstokeep' must be a character containing a variable name", call.=F)
  }else if(length(varstokeep) > ncol(df)){
    stop("Length of 'varstokeep' must be 1", call.=F)
  }else if(forestr::check_names(df, varstokeep)==F){
    stop(forestr::check_names(df, varstokeep, boolean=F), call.=F)
  }
  
  lvls <- levels(as.factor(df[[var]]))
  nlvls <- length(lvls)
  
  # Se levelstorm nao for character,ou nao for de tamanho 1, parar
  
  if(is.null( levelstorm ) || is.na(levelstorm) || levelstorm==""  ){
   return(df)
  }else if(!is.character( levelstorm )){
    stop( "'levelstorm' must be character", call.=F)
  }else if(length(levelstorm) > nlvls ){
    stop("Length of 'levelstorm' must be 1", call.=F)
  }else if(! levelstorm %in% lvls ){ 
    stop("'levelstorm' must be a valid entry", call. = F) 
  }
  # ####
  var_sym <- rlang::sym(var)
  
  if(any(is.na(varstokeep))){
    d1 <- df %>% 
      dplyr::filter((!!var_sym) %in% levelstorm) %>% 
      dplyr::group_by(!!!.groups_syms) %>% 
      dplyr::summarise(n=n()) %>% 
      dplyr::select(-n)
    
    d2 <- df %>% dplyr::filter( ! (!!var_sym) %in% levelstorm)
    
    gg <- .groups
    
    
  }else{
    
    d1 <- df %>% 
      dplyr::filter((!!var_sym) %in% levelstorm) %>% 
      dplyr::group_by(!!!.groups_syms) %>% 
      dplyr::summarise_at(vars(varstokeep), mean, na.rm=T)
    
    d2 <- df %>% dplyr::filter( ! (!!var_sym) %in% levelstorm)
    
    gg <- c(.groups,varstokeep)
    
  }
  
  if(var %in% .groups){ # Se var for igual a um dos grupos, utilizar right join
    dplyr::right_join(d1,d2, by=gg)
  }else{
    dplyr::left_join(d1,d2, by=gg)
  }
  
}
