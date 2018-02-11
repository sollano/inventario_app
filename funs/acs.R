#' Amostragem Casual Simples
#'
#'Funcao para o processamento de inventario florestal utilizando a amostragem casual simples.
#'@details 
#' Essa funcao permite o processamento de um inventario florestal utilizando a amostragem casual estratificada,
#' para n talhoes, considerando a populcao finita ou infinita.
#' E possivel executar varios inventarios de uma vez utilizando uma variavel categorica indicada no parametro \code{grupos}().
#'
#' @param df Data frame a ser utilizado.
#' @param Yi Nome entre aspas da variavel volume, ou variavel a ser amostrada.
#' @param area_parcela  Nome entre aspas da variavel area da unidade amostral utilizada, em metros quadrados. Pode tambem ser um valor numerico.
#' @param area_total Nome entre aspas da variavel area total, em ha. Pode tambem ser um vetor contendo os valores das areas dos estratos.
#' @param grupos Parametro opcional. Utilizado quando se deseja fazer mais de um inventario por vez.
#' deve-se indicar o nome entre aspas da(s) variavel(is) fatoriais a serem consideradas. 
#' Caso seja mais de uma, deve-se inserir um vetor contendo os nomes das variaveis.
#' Por exemplo: para um inventario casual simples para cada codigo genetico, assumindo que o nome
#' da variavel que designa os codigos geneticos seja CODGEN, utiliza-se: \code{"CODGEN"}.
#' @param idade Nome entre aspas da variavel idade. Parametro opcional. Calcula a media da idade fornecida.
#' @param alpha Valor da significancia a ser utilizada no calculo de t-student. Padrao: \code{0.05}.
#' @param Erro Valor do Erro minimo admitido no inventario, em porcentagem. Padrao: \code{10}.
#' @param casas_decimais Numero de casas decimais que seram utilizdas no resultado final. Padrao: \code{4}.
#' @param pop Populacao considerada nos calculos. Pode ser infinita(\code{"inf"}) ou finita (\code{"fin"}). Padrao: \code{"inf"}.
#' @param tidy A tabela final deve ser tidy (organizada) ou nao? Padrao: \code{TRUE}.
#' @return Dataframe contendo as informacoes sobre a amostragem.
#' 
#' @keywords Amostragem casual simples
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
#' data("ex3_mfr")
#' data("ex4_mfr")
#'
#' # Deseja-se inventariar uma area, atingindo um erro de 20%.
#' # Primeiro e realizdo um inventario piloto:
#' # inventario piloto, considerando um erro de 20%, pop finita:
#' acs(ex3_mfr, "VCC", "AREA_PARCELA", "AREA_TOTAL", Erro = 20, pop = "fin")
#'
#' # Percebe-se que temos 10 amostras, mas precisamos de 25 para obter o erro desejado.
#' # Apos amostrar novas parcelas, realiza-se o inventario definitivo:
#' # inventario definitivo, considerando um erro de 20%, pop finita:
#' acs(ex4_mfr, "VCC", "AREA_PARCELA", "AREA_TOTAL", Erro = 20, pop = "fin")
#'
#' # O Erro desejado foi alcancado.
#' 
#' # Valores de area podem ser numericos
#' acs(ex4_mfr, "VCC", 3000, 46.8, Erro = 20, pop = "fin")
#' 
#' # um inventario casual simples para cada talhao:
#' acs(ex2_mfr, "VCC", "AREA_PARCELA", "AREA_TALHAO",grupos = "TALHAO" ,Erro = 20, pop = "fin")
#'
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}

acs <- function(df,Yi, area_parcela, area_total, idade, grupos, alpha = 0.05, Erro = 10, casas_decimais=4, pop="inf",tidy=T){
  
  if(missing(grupos)||is.null(grupos)||grupos==F||grupos==""){grupos<-NULL}
  
  if(missing(df)||is.null(df)||df==F||df=="")
  {stop("Escolha o data frame") }
  
  if(missing(area_total)||is.null(area_total)||area_total==F||area_total=="")
  {stop("Escolha a variavel Area total (ha) ") }
  
  if(missing(area_parcela)||is.null(area_parcela)||area_parcela==F||area_parcela=="")
  {stop("Escolha a variavel Area da parcela (m2) ") }
  
  if(missing(Yi)||is.null(Yi)||Yi==F||Yi=="")
  {stop("Escolha a variavel Volume (m3)") }
  
  # argumentos opcionais
  if(missing(idade)||is.null(idade)||idade==F||idade==""){df$idade<-NA; idade <- "idade"}
  
  # argumentos de area podem ser numericos
  if(is.numeric(area_parcela)){df$area_parcela <- area_parcela; area_parcela <- "area_parcela"}
  if(is.numeric(area_total  )){df$area_total   <- area_total; area_total     <- "area_total"}

  x_ <-df %>%
    na_if(0) %>%
    group_by_(.dots = grupos) %>%
    summarise_(
      .dots = 
        setNames( 
          list( 
            lazyeval::interp(~ mean(idade), idade = as.name(idade) ),
            lazyeval::interp(~ n() ),
            lazyeval::interp(~ mean(area_total) / ( mean(area_parcela)/10000 ), area_total = as.name(area_total), area_parcela = as.name(area_parcela)  ),
            lazyeval::interp(~ sd(Yi) / mean(Yi) * 100, Yi = as.name(Yi) ),
            ~ qt(alpha/2, df = n-1, lower.tail = FALSE),
            ~ ifelse(pop=="inf", 
                     qt(alpha/2, df = ceiling( t^2 * CV^2 / Erro^2) - 1, lower.tail = FALSE)  ,
                     qt(alpha/2, df = ceiling( t^2 * CV^2 / ( Erro^2 +(t^2 * CV^2 / N) ) ) - 1, lower.tail = FALSE) ) ,
            ~ ifelse(pop=="inf",
                     ceiling( t_rec ^2 * CV^2 / Erro^2 ) ,
                     ceiling( t_rec ^2 * CV^2 / ( Erro^2 +(t_rec^2 * CV^2 / N) ) ) ),
            lazyeval::interp(~ mean(Yi, na.rm=T), Yi = as.name(Yi) ),
            lazyeval::interp(~ ifelse(pop=="inf", 
                            sqrt( var(Yi)/n ), 
                            sqrt( var(Yi)/n  * (1 - (n/N)) ) ) , 
                   Yi = as.name(Yi), n = as.name("n"), N = as.name("N") ),
            ~ Sy * t ,
            ~ Erroabs / Y * 100,
            ~ Y * N,
            ~ Erroabs * N,
            ~ Y - Erroabs,
            ~ Y + Erroabs,
            ~ Yhat - Erro_Total,
            ~ Yhat + Erro_Total
          ), 
          nm=c("idade", "n","N", "CV","t","t_rec","n_recalc", "Y","Sy","Erroabs" ,"Erroperc","Yhat", "Erro_Total","IC_ha_Inf" ,"IC_ha_Sup","IC_Total_inf","IC_Total_Sup")
        ) 
    ) %>%
    na_if(0) %>% # substitui 0 por NA
    select_if(Negate(anyNA) ) %>%  # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
    round_df(casas_decimais)
  
  x <- x_ %>% 
    plyr::rename(c( "idade"        = "Idade (meses)"                  , 
                    "n"            = "Numero de amostras (n)"         ,
                    "N"            = "Numero de amostras cabiveis (N)", 
                    "CV"           = "Coeficiente de Variancia (CV)"  ,
                    "t"            = "t-student"                      ,
                    "t_rec"        = "t recalculado"                  ,
                    "n_recalc"     = "Numero de amostras referente ao erro admitido",
                    "Y"            = "Media geral (Y)"                ,
                    "Sy"           = "Erro-Padrao da Media (Sy)"      ,
                    "Erroabs"      = "Erro Absoluto"                  ,
                    "Erroperc"     = "Erro Relativo (%)"              ,
                    "Yhat"         = "Valor total estimado (Yhat)"   , 
                    "Erro_Total"   = "Erro Total"                     ,
                    "IC_ha_Inf"    = "IC (m3/ha) Inferior"            ,
                    "IC_ha_Sup"    = "IC (m3/ha) Superior"            ,
                    "IC_Total_inf" = "IC Total (m3) inferior"         ,
                    "IC_Total_Sup" = "IC Total (m3) Superior")        , 
                 warn_missing = F) # nao gera erro mesmo quando se renomeia variaveis inexistentes
  
  if(tidy==F)
  {
    return(x_)
  } 
  else if(tidy==T & is.null(grupos) )
  {
    x <- data.frame(Variaveis = names(x), Valores = t(x) )
    rownames(x) <- NULL
    # ou
    # x <- tibble::rownames_to_column(data.frame("Valores"=t(x)) , "Variaveis" ) 
    
    return(x)
  }
  else
  {
    vec1 <- names(x)[! names(x) %in% grupos ]
    vec2 <- grupos[length(grupos)]
    vec3 <- grupos[grupos!=vec2]
    
    y <- x %>%
      tidyr::gather_("Variaveis","value", vec1, factor_key=T ) %>% 
      arrange_( grupos ) %>% 
      tidyr::spread_(vec2,"value",sep="")%>%
      group_by_(.dots=vec3)
    return(y)
    
  }
  
}
