#' Amostragem Sistematica
#'
#'Funcao para o processamento de inventario florestal utilizando a amostragem sistematica.
#'@details 
#' Essa funcao permite o processamento de um inventario florestal utilizando a amostragem casual estratificada,
#' para n talhoes, considerando a populcao finita ou infinita.
#' E possivel executar varios inventarios de uma vez utilizando uma variavel categorica indicada no parametro \code{.groups}().
#'
#' @param df Data frame a ser utilizado.
#' @param Yi Nome entre aspas da variavel volume, ou variavel a ser amostrada.
#' @param area_parcela  Nome entre aspas da variavel area da unidade amostral utilizada, em metros quadrados. Pode tambem ser um valor numerico.
#' @param area_total Nome entre aspas da variavel area total, em ha. Pode tambem ser um vetor contendo os valores das areas dos estratos.
#' @param .groups Parametro opcional. Utilizado quando se deseja fazer mais de um inventario por vez.
#' deve-se indicar o nome entre aspas da(s) variavel(is) fatoriais a serem consideradas. 
#' Caso seja mais de uma, deve-se inserir um vetor contendo os nomes das variaveis.
#' Por exemplo: para um inventario sistematica para cada codigo genetico, assumindo que o nome
#' da variavel que designa os codigos geneticos seja CODGEN, utiliza-se: \code{"CODGEN"}.
#' @param idade Nome entre aspas da variavel idade. Parametro opcional. Calcula a media da idade fornecida.
#' @param alpha Valor da significancia a ser utilizada no calculo de t-student. Padrao: \code{0.05}.
#' @param erro Valor do erro minimo admitido no inventario, em porcentagem. Padrao: \code{10}.
#' @param casas_decimais Numero de casas decimais que seram utilizdas no resultado final. Padrao: \code{4}.
#' @param tidy A tabela final deve ser tidy (organizada) ou nao? Padrao: \code{TRUE}.
#' @return Dataframe contendo as informacoes sobre a amostragem.
#' 
#' @keywords Amostragem sistematica
#' @references 
#' CAMPOS, J. C. C.; LEITE, H. G. Mensuracao florestal: perguntas e respostas. 3a. ed. Vicosa: Editora UFV, 2013. 605 p.
#' 
#' SOARES, C. P. B.; NETO, F. D. P.; SOUZA, A. L. D. Dendrometria e Inventario Florestal. 2a. ed. Vicosa: UFV, 2012. 272 p.
#' 
#' @seealso outras funcoes de amostragem: 
#'   \code{\link{ace}} para amostragem casual estratificada, e
#'   \code{\link{as_diffs}} para amostragem sistematica.
#' @export
#' @examples
#' library(forestr)
#' data("ex2_mfr")
#' data("ex5_mfr")
#'
#' # Inventaria-se uma area de forma sistematica;
#' # primeiro tenta-se processar o inventario utilizando a amostragem casual simples:
#' acs(ex5_mfr,  "VCC", "AREA_PARCELA", "AREA_TOTAL")
#'
#' # Gera-se um erro de 22%. Agora, processamos este inventario
#' # utilizando o metodo das diferencas sucessivas:
#' as_diffs(ex5_mfr,  "VCC", "AREA_PARCELA", "AREA_TOTAL")
#'
#' # Percebe-se uma diminuicao significativa no erro.
#' 
#' # Valores de area podem ser numericos
#' as_diffs(ex5_mfr,  "VCC", 200, 18)
#' 
#' # um inventario sistematico para cada talhao:
#' as_diffs(ex2_mfr,  "VCC", "AREA_PARCELA", "AREA_TALHAO",.groups = "TALHAO")
#'
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}

as_diffs <- function(df, Yi, area_parcela, area_total,  idade, .groups, alpha = 0.05, erro = 10, casas_decimais=4, tidy=T ) {
  
  # checagem de variaveis ####
  
  # Definir pipe do dplyr, para facilitar
  `%>%` <- dplyr::`%>%`
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, parar
  if(  missing(df) || all(is.null(df)) || all(is.na(df)) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop( "length and number of rows 'df' must be greater than 1", call.=F)
  }
  
  # se Yi nao for fornecido, não for character, ou nao for um nome de variavel, parar
  if(  missing(Yi) || Yi == "" ){  
    stop("Yi not set", call. = F) 
  }else if( !is.character(Yi) ){
    stop("'Yi' must be a character containing a variable name", call.=F)
  }else if(length(Yi)!=1){
    stop("length of 'Yi' must be 1", call.=F)
  }else if(check_names(df, Yi)==F){
    stop(check_names(df, Yi, boolean = F), call.=F)
  }
  
  # se area_parcela nao for fornecido, nao for numerico nem character, ou nao for um nome de variavel, parar
  if(  missing(area_parcela) ||area_parcela == "" ){  
    stop("area_parcela not set", call. = F) 
  }else if(is.numeric(area_parcela) & length(area_parcela)==1){
    df$area_parcela <- area_parcela
    area_parcela <- "area_parcela"
  }else if(!is.character(area_parcela)){
    stop("'area_parcela' must be a character containing a variable name or a numeric value", call.=F)
  }else if(length(area_parcela)!=1){
    stop("length of 'area_parcela' must be 1", call.=F)
  }else if(check_names(df, area_parcela)==F){
    stop(check_names(df, area_parcela, boolean = F), call.=F)
  }
  
  # se area_total nao for fornecido, nao for numerico nem character,  ou nao for um nome de variavel, parar
  if(  missing(area_total) || area_total == "" ){  
    stop("area_total not set", call. = F) 
  }else if(is.numeric(area_total) & length(area_total)==1){
    df$area_total <- area_total
    area_total <- "area_total"
  }else if(!is.character(area_total)){
    stop("'area_total' must be a character containing a variable name or a numeric value", call.=F)
  }else if(length(area_total)!=1){
    stop("length of 'area_total' must be 1", call.=F)
  }else if(check_names(df, area_total)==F){
    stop(check_names(df, area_total, boolean = F), call.=F)
  }
  
  # se idade nao for fornecido, for igual "", nulo, nao existir no dataframe, criar
  # variavel vazia
  # se existir e nao for character,  parar
  if(missing(idade)||is.null(idade)||is.na(idade)||idade==""){
    df$idade <- NA
    idade <- "idade"
  }else if(!is.character(idade)){
    stop("'idade' must be a character containing a variable name", call.=F)
  }else if(length(idade)!=1){
    stop("length of 'idade' must be 1", call.=F)
  }else if(check_names(df, idade)==F){
    stop(check_names(df, idade, boolean = F), call.=F)
  }
  
  # Se .groups nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if(missing(.groups)||is.null(.groups)||is.na(.groups)||.groups==F||.groups==""){
    .groups_syms <- character()
    # Se groups for fornecido verificar se todos os nomes de variaveis fornecidos existem no dado  
  }else if(!is.character(.groups)){
    stop(".groups must be a character", call.=F)
  }else if(!length(.groups) %in% 1:10){
    stop("length of '.groups' must be between 1 and 10", call.=F)
  }else if(check_names(df,.groups)==F ){
    stop(check_names(df,.groups, boolean=F), call.=F)
  }else{
    .groups_syms <- rlang::syms(.groups)
  }
  
  # Se alpha nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( alpha )){
    stop( "'alpha' must be numeric",call.=F)
  }else if(length(alpha)!=1){
    stop("length of 'alpha' must be 1",call.=F)
  }else if(! alpha > 0 | ! alpha <= 0.30){
    stop("'alpha' must be a number between 0 and 0.30", call.=F)
  }
  
  # Se erro nao for numerico, parar
  if(!is.numeric( erro )){
    stop( "'erro' must be numeric", call.=F )
  }else if(length(erro)!=1){
    stop("length of 'erro' must be 1",call.=F)
  }else if(!erro > 0 | !erro <= 20){
    stop("'erro' must be a number between 0 and 20", call.=F)
  }
  
  # Se casas_decimais nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( casas_decimais )){
    stop( "'casas_decimais' must be numeric", call.=F)
  }else if(length(casas_decimais)!=1){
    stop("length of 'casas_decimais' must be 1",call.=F)
  }else if(! casas_decimais %in% seq(from=0,to=9,by=1) ){
    stop("'casas_decimais' must be a integer between 0 and 9", call.=F)
  }
  
  # se tidy nao for igual a TRUE ou FALSE, parar
  if( is.null(tidy) || ! tidy %in% c(TRUE, FALSE) ){ 
    stop("tidy must be equal to TRUE or FALSE", call. = F) 
  }else if(length(tidy)!=1){
    stop( "length of 'tidy' must be 1", call.=F)
  }
  
  # Transformar os objetos em simbolos, para que o dplyr entenda
  # e procure o nome das variaveis dentro dos objetos
  Yi_sym <- rlang::sym(Yi)
  area_parcela_sym <- rlang::sym(area_parcela)
  area_total_sym <- rlang::sym(area_total)
  idade_sym <- rlang::sym(idade)
  
  # ####
  
  x_ <-df %>%
    dplyr::na_if(0) %>%
    dplyr::group_by(!!!.groups_syms, add=T) %>%
    dplyr::summarise(
      idade        = mean(!!idade_sym,na.rm=T), # usa-se média pois os valores estão repetidos
      n            = n() , # número de amostras
      N            = mean(!!area_total_sym,na.rm=T) / ( mean(!!area_parcela_sym,na.rm=T)/10000 ), 
      CV           = stats::sd(!!Yi_sym,na.rm=T) / mean(!!Yi_sym,na.rm=T) * 100, # Cálculo do coeficiente de variação
      t            = qt(alpha/2, df = n-1, lower.tail = FALSE) ,
      t_rec        = qt(alpha/2, df = ceiling( t^2 * CV^2 / erro^2) - 1, lower.tail = FALSE),
      n_recalc     = ceiling( t_rec ^2 * CV^2 / erro^2 ) ,
      S2           = stats::var(!!Yi_sym,na.rm=T), #Variancia
      sd           = sd(!!Yi_sym,na.rm=T), # desvio padrao
      Y            = mean(!!Yi_sym, na.rm=T), # Média do volume
      Sy           = sqrt( (sum(diff(!!Yi_sym)^2,na.rm=T) / (2 * n * (n-1) ) ) * ((N-n)/N) ),
      Erroabs      = Sy * t , # Erro Absoluto
      Erroperc     = Erroabs / Y * 100 , # Erro Percentual
      Yhat         = Y * N, # Média estimada para Área total
      Erro_Total   = Erroabs * N, # Erro EStimado Para Área Total
      IC_Inf       = Y - Erroabs, # Intervalo de confiança inferior
      IC_Sup       = Y + Erroabs, # Intervalo de confiança superior
      IC_ha_Inf    = (Y - Erroabs)*10000/mean(!!area_parcela_sym,na.rm=T), # Intervalo de confiança por ha inferior
      IC_ha_Sup    = (Y + Erroabs)*10000/mean(!!area_parcela_sym,na.rm=T), # Intervalo de confiança por ha superior
      IC_Total_inf = Yhat - Erro_Total, # Intervalo de confiança total inferior
      IC_Total_Sup = Yhat + Erro_Total) %>% # Intervalo de confiança total superior
    dplyr::na_if(0) %>% # substitui 0 por NA
    dplyr::select_if(Negate(anyNA) ) %>%  # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
    round_df(casas_decimais)
  
  
  x <- x_ %>% 
    plyr::rename(c( "idade"        = "Idade (meses)"                  , 
                    "n"            = "Numero de Parcelas (n)"         ,
                    "N"            = "Numero de Parcelas cabiveis (N)", 
                    "t"            = "t-student"                      ,
                    "t_rec"        = "t-student recalculado"                  ,
                    "n_recalc"     = "Numero de amostras referente ao erro admitido",
                    "S2"           = "Variancia (S²)"                 ,
                    "sd"           = "Desvio Padrao (s)"             ,
                    "CV"           = "Coeficiente de Variancia (CV)"  ,
                    "Y"            = "Media geral (Y)"                ,
                    "Sy"           = "Erro-Padrao da Media (Sy)"      ,
                    "Erroabs"      = "Erro Absoluto"                  ,
                    "Erroperc"     = "Erro Relativo (%)"              ,
                    "Yhat"         = "Volume total estimado (Yhat)"   , 
                    "Erro_Total"   = "Erro Total"                     ,
                    "IC_Inf"       = "IC (m3) Inferior"               ,
                    "IC_Sup"       = "IC (m3) Superior"               ,
                    "IC_ha_Inf"    = "IC (m3/ha) Inferior"            ,
                    "IC_ha_Sup"    = "IC (m3/ha) Superior"            ,
                    "IC_Total_inf" = "IC Total (m3) inferior"         ,
                    "IC_Total_Sup" = "IC Total (m3) Superior")        , 
                 warn_missing = F) # nao gera erro mesmo quando se renomeia variaveis inexistentes
  
  
  if(tidy==F)
  {
    return(x_)
  } 
  else if(tidy==T & length(.groups_syms)==0 )
  {
    #x <- data.frame(Variaveis = names(x), Valores = t(x) )
    #rownames(x) <- NULL
    # ou
    x <- tibble::rownames_to_column(data.frame("Valores"=t(x)) , "Variaveis" ) 
    
    return(x)
  }
  else
  {
    # Primeiro cria-se um vetor que contem os nomes de todas as variaveis criadas anteriormente
    # exceto as variaveis de grupo
    all_but_group_vars <- rlang::syms(names(x)[! names(x) %in% .groups ])
    
    # Aqui identifica-se a ultima variavel de grupo colocada pelo usuario.
    # Esta sera usada para espalhar os dados por coluna. Ou seja,
    # Cada nivel deste fator vai virar uma coluna dos dados
    last_group_var <- rlang::sym(.groups[length(.groups)])
    
    y <- x %>%
      tidyr::gather("Variaveis","value", !!!all_but_group_vars, factor_key=T ) %>% #juntar todo mundo menos as variaveis de grupo
      dplyr::arrange(!!! .groups_syms ) %>%  # organiza os dados (meio desnecessario, mas ok)
      tidyr::spread(!!last_group_var,"value",sep="") %>%  # Colocar cada talhao(por exemplo) em um coluna, espalhando-o pela tabela de forma horizontal
      dplyr::ungroup() # 'desgrupificar' o dado
    
    return(y)
    
  }
  
}
