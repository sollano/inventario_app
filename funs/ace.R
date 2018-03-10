#' Amostragem Casual Estratificada
#'
#' Funcao para o processamento de inventario florestal utilizando a amostragem casual estratificada.
#' @details 
#' Essa funcao permite o processamento de um inventario florestal utilizando a amostragem casual estratificada,
#' para n talhoes, considerando a populcao finita ou infinita.
#' E possivel executar varios inventarios de uma vez utilizando uma variavel categorica indicada no parametro \code{.groups}().

#' @param df Data frame a ser utilizado.
#' @param Yi Nome entre aspas da variavel volume, ou variavel a ser amostrada.
#' @param area_parcela  Nome entre aspas da variavel area da unidade amostral utilizada, em metros quadrados. Pode tambem ser um valor numerico.
#' @param area_estrato Nome entre aspas da variavel area dos estratos, em ha. Pode tambem ser um vetor contendo os valores das areas dos estratos.
#' @param .groups Nome entre aspas da(s) variavel(is) a serem utilizadas na estratificacao. 
#' Caso seja mais de uma, deve-se inserir um vetor contendo os nomes das variaveis.
#' Por exemplo: para um inventario estratificado para cada fazenda, contendo n talhoes cada,
#' utiliza-se: \code{c("FAZENDA", "TALHAO")}.
#' @param idade Nome entre aspas da variavel idade. Parametro opcional. Calcula a media da idade fornecida.
#' @param alpha Valor da significancia a ser utilizada no calculo de t-student. Padrao: \code{0.05}.
#' @param erro Valor do erro minimo admitido no inventario, em porcentagem. Padrao: \code{10}.
#' @param casas_decimais Numero de casas decimais que seram utilizdas no resultado final. Padrao: \code{4}.
#' @param pop Populacao considerada nos calculos. Pode ser infinita(\code{"inf"}) ou finita (\code{"fin"}). Padrao: \code{"inf"}.
#' @param tidy A tabela final deve ser tidy (organizada) ou nao? Padrao: \code{TRUE}.
#' @return Dataframe contendo as informacoes sobre a amostragem.
#' 
#' @keywords Amostragem casual estratificada
#' @references 
#' CAMPOS, J. C. C.; LEITE, H. G. Mensuracao florestal: perguntas e respostas. 3a. ed. Vicosa: Editora UFV, 2013. 605 p.
#' 
#' SOARES, C. P. B.; NETO, F. D. P.; SOUZA, A. L. D. Dendrometria e Inventario Florestal. 2a. ed. Vicosa: UFV, 2012. 272 p.
#' 
#' @seealso outras funcoes de amostragem: 
#'   \code{\link{acs}} para amostragem casual simples, e
#'   \code{\link{as_diffs}} para amostragem sistematica.
#' @export
#' @examples
#' library(forestr)
#' data("ex1_mfr")
#' data("ex2_mfr")
#' data("ex6_mfr")
#' 
#' # Deseja-se inventariar uma area, atingindo um erro de 5%.
#' # Primeiro e realizdo um inventario piloto, considerando um erro de 5% e populacao finita:
#' ace(ex1_mfr, "VCC", "AREA_PARCELA", "AREA_TALHAO",  .groups = "TALHAO", erro = 5, pop = "fin")
#'
#' # Percebe-se que para se atingir o erro desejado, precisa-se de 59 parcelas ao total,
#' # sendo 14 no talhao 1, 21 no talhao 2 e 24 no talhao 3.
#' # Apos lancar as novas parcelas, realiza-se um inventario definitivo,
#' # considerando um erro 5% e populacao finita:
#' ace(ex2_mfr, "VCC", "AREA_PARCELA", "AREA_TALHAO",  .groups = "TALHAO", erro = 5, pop = "fin")
#'
#' # O erro desejado foi atingido.
#' 
#' # Valores de area podem ser numericas:
#' ace(ex2_mfr, "VCC", 1000, c(14.4, 16.4,14.2),  .groups = "TALHAO", erro = 5, pop = "fin")
#'
#' # variavel opcional idade, e um inventario estratificado para cada fazenda:
#' ace(ex6_mfr, "VCC", "AREA_PARCELA", "AREA_TALHAO",  .groups = c("FAZENDA","TALHAO"), idade = "IDADE")
#'
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}

ace <- function(df, Yi, area_parcela, area_estrato, .groups, idade, alpha = 0.05, erro = 10, casas_decimais = 4, pop="inf", tidy=T ){
  # Checagem de variaveis ####
  
  # Definir pipe para facilitar
  `%>%` <- dplyr::`%>%`
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) || is.null(df) || is.na(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se Yi nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(Yi) || Yi == ""  ){  
    stop("Yi not set", call. = F) 
  }else if( !is.character(Yi) ){
    stop("'Yi' must be a character containing a variable name", call.=F)
  }else if(length(Yi)!=1){
    stop("Length of 'Yi' must be 1", call.=F)
  }else if(check_names(df, Yi)==F){
    stop(check_names(df, Yi, boolean=F), call.=F)
  }
  
  # se area_parcela nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, parar
  if(  missing(area_parcela) || area_parcela == "" ){  
    stop("area_parcela not set", call. = F) 
  }else if( is.null(area_parcela) || is.na(area_parcela) || area_parcela == "" ){
    stop("'area_parcela' must be a character containing a variable name or a numeric value", call.=F)
  }else if(is.numeric(area_parcela) & length(area_parcela)==1){
    df $ area_parcela <- area_parcela
    area_parcela <- "area_parcela"
  }else if(!is.character(area_parcela)){
    stop("'area_parcela' must be a character containing a variable name or a numeric value", call.=F)
  }else if(length(area_parcela)!=1){
    stop("Length of 'area_parcela' must be 1", call.=F)
  }else if(check_names(df, area_parcela)==F){
    stop(check_names(df, area_parcela, boolean = F), call.=F)
  }
  
  # se area_estrato nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, parar
  if(  missing(area_estrato) || area_estrato == "" ){  
    stop("area_estrato not set", call. = F) 
  }else if( is.null(area_estrato) || is.na(area_estrato) || area_estrato == "" ){
    stop("'area_estrato' must be a character containing a variable name or a numeric value", call.=F)
  }else if(is.numeric(area_estrato) & length(area_estrato)==1){
    df $ area_estrato <- area_estrato
    area_estrato <- "area_estrato"
  }else if(is.numeric(area_estrato) & length(area_estrato)>1){
    
    # Se area do estrato for fornecida numericamente, e for maior que uma area,
    # temos que criar uma tabela com os nomes dos talhoes e suas areas, e juntar com os dados originais.
    estrato_name <- .groups[length(.groups)]
    estratos <- levels(factor(df[[estrato_name]]))
    
    if(!all.equal(length(estratos), length(area_estrato))){stop("numero de estratos e número de areas de estrato nao coincidem")}
    
    
    tab_estratos <- data.frame( estratos, area_estrato)
    
    area_estrato <- "area_estrato"
    
    names(tab_estratos) <- c(estrato_name, "area_estrato")
    
    df[[estrato_name]] <- as.factor(df[[estrato_name]] )
    df <- dplyr::left_join(df, tab_estratos, by = estrato_name)
    
  }else if(!is.character(area_estrato)){
    stop("'area_estrato' must be a character containing a variable name or a numeric value", call.=F)
  }else if(length(area_estrato)!=1){
    stop("Length of 'area_estrato' must be 1", call.=F)
  }else if(check_names(df, area_estrato)==F){
    stop(check_names(df, area_estrato, boolean = F), call.=F)
  }
  
  # se idade nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(idade) || is.null(idade) || is.na(idade) || idade == "" ){
    df$idade <- NA
    idade <- "idade"
  }else if(!is.character(idade)){
    stop("'idade' must be a character containing a variable name", call.=F)
  }else if(length(idade)!=1){
    stop("Length of 'idade' must be 1", call.=F)
  }else if(check_names(df, idade)==F){
    stop(check_names(df, idade, boolean=F), call.=F)
  }
  
  # Se .groups nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if( missing(.groups) ){
    stop(".groups not set", call. = F) 
    # Se groups for fornecido verificar se todos os nomes de variaveis fornecidos existem no dado  
  }else if(!is.character(.groups)){
    stop(".groups must be a character", call.=F)
  }else if(! length(.groups)%in% 1:10){
    stop("Length of '.groups' must be between 1 and 10", call.=F)
  }else if(check_names(df,.groups)==F ){
    # Parar se algum nome nao existir, e avisar qual nome nao existe
    stop(check_names(df,.groups, boolean=F), call.=F)
    # se os grupos forem fornecidos e forem nomes dos dados
    # Transformar o objeto em simbolo, para que dplyr entenda
    # e procure o nome das variaveis dentro dos objetos
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
  
  # Se pop nao for character ou for maior que 1, parar
  if(!is.character( pop )){
    stop( "'pop' must be character", call.=F)
  }else if(length(pop)!=1){
    stop( "length of 'pop' must be 1", call.=F)
  }else if( ! pop %in% c("fin", "inf" ) ){ 
    stop("'pop' must be equal to 'fin' or 'inf' ", call. = F) 
  }
  
  # se tidy nao for igual a TRUE ou FALSE, parar
  if( is.null(tidy) || ! tidy %in% c(TRUE, FALSE) ){ 
    stop("tidy must be equal to TRUE or FALSE", call. = F) 
  }else if(length(tidy)!=1){
    stop( "length of 'tidy' must be 1", call.=F)
  }
  
  # ####
  
  Yi_sym <- rlang::sym(Yi)
  area_parcela_sym <- rlang::sym(area_parcela)
  area_estrato_sym <- rlang::sym(area_estrato)
  idade_sym <- rlang::sym(idade)
  
  # Calcula-se o N separado, para caso se tenha diferentes tamanhos de area por talhao
  aux <- df %>%
    dplyr::na_if(0) %>%
    dplyr::group_by( !!!.groups_syms ) %>%
    dplyr::summarise( Nj = mean(!!area_estrato_sym) / (mean(!!area_parcela_sym)/10000) ) %>%
    dplyr::summarise(N  = sum(Nj) )
  
  # Se tiver apenas linha, ou seja, se tiver apenas um talhao, cbind
  if(nrow(aux) == 1) {
    
    x_ <- cbind(data.frame(df),N = aux$N)
    
  }else{
    # se tiver mais de uma linha, ou seja, varios talhoes,
    # unir as areas aos dados originais utilizando join
    x_ <- dplyr::left_join(data.frame(df),aux, by = .groups[-length(.groups)])
    
  }
  
  x_ <- x_ %>% 
    dplyr::mutate(Nj = (!!area_estrato_sym ) / ( (!!area_parcela_sym)/10000 ) ) %>%
    dplyr::group_by( !!!.groups_syms) %>%
    dplyr::summarise(
      IDADE  = mean(!!idade_sym),
      nj     = n() ,
      Nj     = mean(Nj),
      N      = mean(N),
      Pj     = Nj/N,
      Eyj    = sum(!!Yi_sym),
      Eyj2   = sum((!!Yi_sym)^2),
      Yj     = mean(!!Yi_sym, na.rm=T),
      Pj_Sj2 = Pj * stats::var(!!Yi_sym, na.rm=T),
      Pj_Sj  = Pj * stats::sd(!!Yi_sym, na.rm=T),
      Pj_Yj  = Pj * Yj ) %>%
    # ungroup %>%
    dplyr::mutate( EPj_Sj2  =   sum(Pj_Sj2), 
                   EPj_Sj   =   sum(Pj_Sj), 
                   Y        =   sum(Pj_Yj), # media estratificada (ponderada)     
                   CV       = EPj_Sj / Y * 100, # Coeficiente de variancia
                   t        = qt(alpha/2, df = sum(nj)-1, lower.tail = FALSE),     # a seguir, o t sera calculado utilizando o n calculado, de forma direta
                   t_rec    = ifelse(pop=="inf",
                                     qt(alpha/2, df = ceiling( t^2 * EPj_Sj^2 / (erro*Y/100)^2 )-1, lower.tail = FALSE),
                                     qt(alpha/2, df = ceiling( t^2 * EPj_Sj^2 / ( (erro*Y/100)^2 + (t^2 * EPj_Sj2 / N )  ) )-1, lower.tail = FALSE)
                   ),
                   n_recalc = ifelse(pop=="inf",
                                     ceiling( t_rec^2 * EPj_Sj^2 / (erro*Y/100)^2 ),
                                     ceiling( t_rec^2 * EPj_Sj^2 / ( (erro*Y/100)^2 + (t_rec^2 * EPj_Sj2 / N )  ) ) 
                   ), # agora fazemos o recalculo do n, utilizando o t correto
                   nj_otimo = ceiling(n_recalc*Pj_Sj/EPj_Sj), # por estrato utilizando o metodo de Neyman
                   n_otimo  = sum(nj_otimo), # n calculado total
                   Yhatj    = Nj * Yj )  %>% # producao total por estrato
    dplyr::na_if(0) %>% # substitui 0 por NA
    dplyr::select_if(Negate(anyNA) ) # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
  
  
  x <- x_ %>% 
    plyr::rename(
      c("IDADE" = "Idade (meses)",
        "nj" = "numero de amostras / estrato (nj)" ,
        "Nj" = "Numero de amostras cabiveis / estrato (Nj)",
        "N" = "Numero de amostras cabiveis (N)", 
        "Pj" = "Proporcao Nj/N (Pj)",
        "Eyj" = "Somatorio por estrato (Eyj)", 
        "Eyj2" = "Soma quadratica do por estrato (Eyj2)", 
        "Yj" = "Media do volume por estrato (Yj)", 
        "Pj_Sj2" = "PjSj2", 
        "Pj_Sj" = "PjSj", 
        "Pj_Yj" = "PjYj",
        "EPj_Sj2" = "EPjSj2",
        "EPj_Sj" = "EPjSj", 
        "Y" = "Media Estratificada (Y)",
        "CV" = "Coeficiente de Variancia (CV)", 
        "t" = "t-student", 
        "t_rec" = "t-student recalculado", 
        "n_recalc" = "Numero de amostras referente ao erro admitido",
        "nj_otimo" = "Numero otimo de amostras por estrato (nj otimo)", 
        "n_otimo" = "numero otimo de amostras (n otimo)", 
        "Yhatj" = "Valor total por estrato (Yhatj)"  ),
      warn_missing = F) %>% 
    round_df(casas_decimais)  
  
  
  y_ <- x_ %>%
    dplyr::group_by(!!! (.groups_syms[-length(.groups)]) ) %>%
    #dplyr::group_by_(.dots=.groups[-length(.groups)] ) %>%
    dplyr::summarise(t     = mean(t),
                     Sy           = ifelse(pop=="inf",
                                           sqrt(sum(Pj_Sj)^2 / sum(nj) ),
                                           sqrt(sum(Pj_Sj) ^2 / sum(nj) - (mean(EPj_Sj2) / mean(N) )  )
                     ), # Erro-padrao da media
                     Y            = sum(Pj_Yj), # media de Yi estratificada (ponderada) 
                     Erroabs      = Sy * t, # Erro Absoluto
                     Erroperc     = Erroabs / Y * 100, # Erro percentual
                     Yhat         = sum(Nj) * Y, # Volume Total
                     Erro_Total   = Erroabs * sum(Nj), # Erro Total
                     IC_ha_Inf    = Y - Erroabs, # Intervalo de confianca por ha inferior
                     IC_ha_Sup    = Y + Erroabs, # Intervalo de confianca por ha superior
                     IC_Total_inf = Yhat - Erro_Total, # Intervalo de confianca total inferior
                     IC_Total_Sup = Yhat + Erro_Total    ) %>% # Intervalo de confianca total superior)
    round_df(casas_decimais)  
  
  
  y <- y_ %>% 
    plyr::rename(
      c("t" = "t-student",
        "Sy" = "Erro-Padrao da Media (Sy)",
        "Y" = "Media estratificada (Y)",
        "Erroabs" = "Erro Absoluto" ,
        "Erroperc" = "Erro Relativo (%)",
        "Yhat" = "Valor total estimado (Yhat)", 
        "Erro_Total" = "Erro Total",
        "IC_ha_Inf" = "IC (m3/ha) Inferior" ,
        "IC_ha_Sup" = "IC (m3/ha) Superior",
        "IC_Total_inf" = "IC Total (m3) inferior",
        "IC_Total_Sup" = "IC Total (m3) Superior"),
      warn_missing = F)
  
  x_ <- round_df(x_, casas_decimais)  
  
  
  if(tidy==F){
    
    z <- list(Tabela1 = as.data.frame(x_), Tabela2 = as.data.frame(y_))
    
    return(z)
    
  }else{
    
    vec1 <- rlang::syms(names(x)[! names(x) %in% .groups ])
    vec2 <- rlang::sym(.groups[length(.groups)])
    vec3 <- rlang::syms(.groups[.groups!=vec2])
    vec4 <- rlang::syms(names(y)[! names(y) %in% vec3 ]) 
    vec5 <- as.character(vec3[length(vec3)])
    vec6 <- vec3[as.character(vec3)!=as.character(vec5)]
    
    x <-  x %>%
      tidyr::gather("Variaveis","value", !!!vec1, factor_key=T) %>% 
      dplyr::arrange(!!!.groups_syms) %>% 
      tidyr::spread(!!vec2,"value",sep=" ") %>%
      dplyr::group_by(!!!vec3)
    
    if(length(.groups)!=1 ){
      
      y <- y %>%
        tidyr::gather("Variaveis","value", !!!vec4, factor_key=T) %>% 
        dplyr::arrange(!!!vec3) %>% 
        tidyr::spread(vec5,"value") %>%
        dplyr::group_by(!!!vec6)
      
    } else{
      
      y <- y %>%
        tidyr::gather("Variaveis","value", !!!vec4, factor_key=T)
      
      
    }
    
    z <- list(Tabela1 = as.data.frame(x), Tabela2 = as.data.frame(y))
    
    return(z) 
    
  }
  
  
  
}
