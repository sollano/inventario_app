as_diffs <- function(df, VCC, area_parcela, area_total,  idade, grupos, alpha = 0.05, Erro = 10, casas_decimais=4, tidy=T ) {
  
  suppressPackageStartupMessages(require(dplyr))
  require(tidyr)
  require(lazyeval)
  
  if(missing(grupos)||is.null(grupos)|| is.na(grupos) ||grupos==F||grupos==""){grupos<-NULL}
  
  if(missing(df)||is.null(df) || is.na(df) ||df==F||df=="")
  {stop("Escolha o data frame") }
  
  if(missing(area_total)|| is.na(area_total) ||is.null(area_total)||area_total==F||area_total=="")
  {stop("Escolha a variavel Area total (ha) ") }
  
  if(missing(area_parcela) || is.na(area_parcela) ||is.null(area_parcela)||area_parcela==F||area_parcela=="")
  {stop("Escolha a variavel Area da parcela (m2) ") }
  
  if(missing(VCC) || is.na(VCC) ||is.null(VCC)||VCC==F||VCC=="")
  {stop("Escolha a variavel Volume (m3)") }
  
  # argumentos opcionais
  if(missing(idade)|| is.na(idade)||is.null(idade)||idade==F||idade==""){df$idade<-NA; idade <- "idade"}
  
  # argumentos de area podem ser numericos
  if(is.numeric(area_parcela)){df$area_parcela <- area_parcela; area_parcela <- "area_parcela"}
  if(is.numeric(area_total  )){df$area_total   <- area_total; area_total     <- "area_total"}
  
  
  x_ <- df %>%
    group_by_(.dots = grupos) %>%
    summarise_(
      .dots = 
        setNames( 
          list( 
            interp(~ mean(idade,na.rm=T), idade = as.name(idade) ),
            interp(~ n() ),
            interp(~ mean(area_total,na.rm=T) / ( mean(area_parcela,na.rm=T)/10000 ), area_total = as.name(area_total), area_parcela = as.name(area_parcela)  ),
            interp(~ sd(VCC,na.rm=T) / mean(VCC,na.rm=T) * 100, VCC = as.name(VCC) ),
            ~ qt(alpha/2, df = n-1, lower.tail = FALSE),
            ~ qt(alpha/2, df = ceiling( t^2 * CV^2 / Erro^2) - 1, lower.tail = FALSE)  ,
            ~ ceiling( t_rec ^2 * CV^2 / Erro^2 ) ,
            interp(~ mean(VCC, na.rm=T), VCC = as.name(VCC) ),
            interp(~ sqrt( (sum(diff(VCC)^2,na.rm=T) / (2 * n * (n-1) ) ) * ((N-n)/N) ) , VCC = as.name(VCC), n = as.name("n"), N = as.name("N") ),
            ~ Sy * t ,
            ~ Erroabs / Y * 100,
            ~ Y * N,
            ~ Erroabs * N,
            ~ Y - Erroabs,
            ~ Y + Erroabs,
            ~ Yhat - Erro_Total,
            ~ Yhat + Erro_Total
          ), 
          nm=c("idade", "n","N", "CV","t","t_rec","n_recalc", "Y", "Sy", "Erroabs","Erroperc","Yhat", "Erro_Total","IC_ha_Inf" ,"IC_ha_Sup","IC_Total_inf","IC_Total_Sup")
        ) 
    ) %>%
    na_if(0) %>% # substitui 0 por NA
    select_if(Negate(anyNA) ) %>%  # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
    round_df(casas_decimais)
  
  
  x <- x_ %>% 
    plyr::rename(c( "idade"        = "Idade (meses)"                  , 
                    "n"            = "Número de Parcelas (n)"         ,
                    "N"            = "Número de Parcelas cabíveis (N)", 
                    "CV"           = "Coeficiente de Variância (CV)"  ,
                    "t"            = "t-student"                      ,
                    "t_rec"        = "t-student recalculado"                  ,
                    "n_recalc"     = "Número de amostras referente ao erro admitido",
                    "Y"            = "Média geral (Y)"                ,
                    "Sy"           = "Erro-Padrao da Média (Sy)"      ,
                    "Erroabs"      = "Erro Absoluto"                  ,
                    "Erroperc"     = "Erro Relativo (%)"              ,
                    "Yhat"         = "Volume total estimado (Yhat)"   , 
                    "Erro_Total"   = "Erro Total"                     ,
                    "IC_ha_Inf"    = "IC (m³/ha) Inferior"            ,
                    "IC_ha_Sup"    = "IC (m³/ha) Superior"            ,
                    "IC_Total_inf" = "IC Total (m³) inferior"         ,
                    "IC_Total_Sup" = "IC Total (m³) Superior")        , 
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
      gather_("Variaveis","value", vec1, factor_key=T ) %>% 
      arrange_( grupos ) %>% 
      spread_(vec2,"value",sep="")%>%
      group_by_(.dots=vec3)
    
    return(y)
  }
  
  
  
}
