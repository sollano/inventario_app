
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


round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

inv_summary <- function(df,DAP, HT, VCC, area_parcela, groups, area_total,idade,VSC,Hd) {
  
  suppressPackageStartupMessages(require(dplyr))
  require(lazyeval)
  
  if( missing(df)||is.null(df)||df==F)
  {stop("please insert data frame")}
  
  if(missing(DAP)||is.null(DAP)||DAP==F||DAP=="")
  {stop("please insert diameter variable")}
  
  if(missing(HT)||is.null(HT)||HT==F||HT=="")
  {stop("please insert height variable")}
  
  if(missing(VCC)||is.null(VCC)||VCC==F||VCC=="")
  {stop("please insert volume variable")}
  
  if(missing(area_parcela)||is.null(area_parcela)||area_parcela==F||area_parcela=="")
  {stop("please insert sample area variable")}
  
  if(missing(groups)||is.null(groups)||groups==F||groups==""){groups<-NULL}else{df <-df[  apply(!is.na(df[groups ]) , MARGIN = 1, function(x) all(x) ), ] }
  
  
  # argumentos opcionais
  if(missing(area_total) || is.null(area_total) || area_total==F || area_total==""   ){df$area_total<-NA; area_total <- "area_total"}
  if(missing(idade)      || is.null(idade)      || idade==F      || idade==""        ){df$idade<-NA;      idade <- "idade"}
  if(missing(VSC)        || is.null(VSC)        || VSC==F        || VSC==""          ){df$VSC<-NA;        VSC <- "VSC"}
  
  # argumentos de area podem ser numericos
  if(is.numeric(area_parcela)){df$area_parcela <- area_parcela; area_parcela <- "area_parcela"}
  if(is.numeric(area_total  )){df$area_total   <- area_total; area_total     <- "area_total"}
  
  
  if(missing(Hd)||is.null(Hd)||Hd==F||Hd=="") { # calculo da altura dominante
    
    if(  "HD" %in% names(df) ){ df$HD <- NULL }
    
    if(is.null(groups)){
      x <-  suppressMessages(   # remove mensagens do dplyr
        df %>% 
          select_(ht = HT) %>% 
          top_n(2) %>% # seleciona as duas maiores arvores
          summarise(HD = mean(ht) ) %>% 
          cbind(df) # como nao ha grupos, usamos cbind
      )    }else{
        x <-  suppressMessages(
          
          df %>% 
            group_by_(.dots = groups) %>% 
            select_(ht = HT) %>% 
            top_n(2) %>% 
            summarise(HD = mean(ht) ) %>% 
            full_join(df) # como ha grupos, usamos join
          
        )
      }
    
  } else{ x <- df %>% rename_(HD = Hd) }
  
  
  x %>% 
    group_by_(.dots = groups) %>% 
    mutate_(.dots = setNames(list( interp(~ pi * DAP^2 / 40000, DAP = as.name(DAP) ) ), nm = "AS" ) ) %>% 
    summarise_(
      .dots =  
        setNames(  list(  
          interp(~ round(mean(as.numeric(idade), na.rm=T) ), idade = as.name(idade)) ,
          interp(~ mean(area_total, na.rm=T), area_total = as.name(area_total)) ,
          interp(~ mean(area_parcela, na.rm=T), area_parcela = as.name(area_parcela)) ,
          interp(~ round(mean(DAP, na.rm=T), 2), DAP = as.name(DAP) ) ,
          ~ round(sqrt(mean(AS, na.rm=T) * 40000 / pi), 2)  ,
          interp(~round(mean(HT, na.rm=T), 2), HT = as.name(HT) ),
          ~ round(mean(HD), 2),
          ~ round(sum(AS, na.rm=T) * 10000/AREA_PARCELA, 4),
          interp(~round(sum(VCC, na.rm=T) * 10000/ AREA_PARCELA, 4 ), VCC = as.name(VCC), AREA_PARCELA = as.name("AREA_PARCELA") ),
          interp(~round(sum(VSC, na.rm=T) * 10000/ AREA_PARCELA, 4 ), VSC = as.name(VSC), AREA_PARCELA = as.name("AREA_PARCELA") )
          
        ), #list 
        nm = c("IDADE","AREA_TOTAL","AREA_PARCELA", "DAP", "q", "HT", "HD","G", "VCC","VSC" ) 
        )#setnames 
    ) %>% #sumarise 
    na_if(0) %>% # substitui 0 por NA
    select_if(Negate(anyNA)) %>%  # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
    as.data.frame
}

acs <- function(df,VCC, area_parcela, area_total, idade, grupos, alpha = 0.05, Erro = 10, casas_decimais=4, pop="inf",tidy=T){
  
  suppressPackageStartupMessages(require(dplyr))
  require(tidyr)
  require(lazyeval)
  
  if(missing(grupos)||is.null(grupos)||grupos==F||grupos==""){grupos<-NULL}
  
  if(missing(df)||is.null(df)||df==F||df=="")
  {stop("Escolha o data frame") }
  
  if(missing(area_total)||is.null(area_total)||area_total==F||area_total=="")
  {stop("Escolha a variavel Area total (ha) ") }
  
  if(missing(area_parcela)||is.null(area_parcela)||area_parcela==F||area_parcela=="")
  {stop("Escolha a variavel Area da parcela (m2) ") }
  
  if(missing(VCC)||is.null(VCC)||VCC==F||VCC=="")
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
            interp(~ mean(idade), idade = as.name(idade) ),
            interp(~ n() ),
            interp(~ mean(area_total) / ( mean(area_parcela)/10000 ), area_total = as.name(area_total), area_parcela = as.name(area_parcela)  ),
            interp(~ sd(VCC) / mean(VCC) * 100, VCC = as.name(VCC) ),
            ~ qt(alpha/2, df = n-1, lower.tail = FALSE),
            ~ ifelse(pop=="inf", 
                     qt(alpha/2, df = ceiling( t^2 * CV^2 / Erro^2) - 1, lower.tail = FALSE)  ,
                     qt(alpha/2, df = ceiling( t^2 * CV^2 / ( Erro^2 +(t^2 * CV^2 / N) ) ) - 1, lower.tail = FALSE) ) ,
            ~ ifelse(pop=="inf",
                     ceiling( t_rec ^2 * CV^2 / Erro^2 ) ,
                     ceiling( t_rec ^2 * CV^2 / ( Erro^2 +(t_rec^2 * CV^2 / N) ) ) ),
            interp(~ mean(VCC, na.rm=T), VCC = as.name(VCC) ),
            interp(~ ifelse(pop=="inf", 
                            sqrt( var(VCC)/n ), 
                            sqrt( var(VCC)/n  * (1 - (n/N)) ) ) , 
                   VCC = as.name(VCC), n = as.name("n"), N = as.name("N") ),
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
                    "Yhat"         = "Volume total estimado (Yhat)"   , 
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
      gather_("Variaveis","value", vec1, factor_key=T ) %>% 
      arrange_( grupos ) %>% 
      spread_(vec2,"value",sep="")%>%
      group_by_(.dots=vec3)
    return(y)
    
  }
  
}

ace <- function(df,VCC, area_parcela, area_estrato, grupos, idade, alpha = 0.05, Erro = 10, casas_decimais = 4, pop="inf", tidy=T ){
  
  require(tidyr)
  suppressPackageStartupMessages(require(dplyr))
  require(lazyeval)
  
  if(missing(df)||is.null(df)||df==F||df=="")
  {stop("Escolha o data frame") }
  
  if(missing(area_estrato)||is.null(area_estrato)||area_estrato==F||area_estrato=="")
  {stop("Escolha a variavel Area do Estrato (ha)") }
  
  if(missing(area_parcela)||is.null(area_parcela)||area_parcela==F||area_parcela=="")
  {stop("Escolha a variavel Area da parcela (m2)") }
  
  if(missing(VCC)||is.null(VCC)||VCC==F||VCC=="")
  {stop("Escolha a variavel Volume (m3)") }
  
  if(missing(grupos)||is.null(grupos)||is.na(grupos)||grupos==F||grupos=="")
  {stop("Escolha a(s) variavel(is) de estratificacao") }
  
  # argumentos opcionais
  if(missing(idade)||is.null(idade)||idade==F||idade=="") {df$idade<-NA; idade<-"idade"}
  
  # argumentos de area podem ser numericos
  if(is.numeric(area_parcela)){df$area_parcela <- area_parcela; area_parcela <- "area_parcela"}
  
  if(is.numeric(area_estrato) && length(area_estrato)==1){
    df$area_estrato <- area_estrato; area_estrato <- "area_estrato"
    
    
  }else if(is.numeric(area_estrato) && length(area_estrato)>1){
    
    
    
    estrato_name <- grupos[length(grupos)]
    estratos <- levels(factor(df[[estrato_name]]))
    
    if(!all.equal(length(estratos), length(area_estrato))){stop("numero de estratos e número de areas de estrato nao coincidem")}
    
    
    tab_estratos <- data.frame( estratos, area_estrato)
    
    area_estrato <- "area_estrato"
    
    names(tab_estratos) <- c(estrato_name, "area_estrato")
    
    df[[estrato_name]] <- as.factor(df[[estrato_name]] )
    df <- left_join(df, tab_estratos, by = estrato_name)
    
  }
  
  
  #if(!all.equal(length(levels(factor(df[[grupos[length(grupos)]]]))) , length(levels(factor(df[[area_estrato]])))  ) ){stop("numero de estratos e número de areas de estrato nao coincidem")}
  
  
  aux <- df %>%
    na_if(0) %>%
    group_by_(.dots = grupos) %>%
    summarise_(.dots = setNames( list( interp( ~ mean(area_estrato) / (mean(area_parcela)/10000), area_estrato = as.name(area_estrato), area_parcela = as.name(area_parcela) ) ), nm="Nj" ) ) %>%
    summarise(N  = sum(Nj) )
  
  if(nrow(aux) == 1) {
    
    x_ <- cbind(data.frame(df),N = aux$N)
    
  }else{
    
    x_ <- left_join(data.frame(df),aux)
    
  }
  
  
  x_ <- x_ %>% 
    mutate_(.dots = setNames( list( interp( ~ area_estrato / ( area_parcela/10000 ), area_estrato = as.name(area_estrato), area_parcela = as.name(area_parcela) ) ), nm="Nj" ) ) %>%
    group_by_(.dots = grupos) %>%
    summarise_(.dots = 
                 setNames( 
                   list(
                     interp(~ mean(idade), idade = as.name(idade) ),
                     interp(~ n() ) ,
                     ~ mean(Nj),
                     ~ mean(N),
                     ~ Nj/N,
                     interp(~ sum(VCC), VCC= as.name(VCC) ),
                     interp(~ sum(VCC^2), VCC= as.name(VCC) ),
                     interp(~ mean(VCC, na.rm =T), VCC= as.name(VCC) ),
                     interp(~ Pj * var(VCC, na.rm=T), Pj = as.name("Pj"), VCC = as.name(VCC) ),
                     interp(~ Pj * sd(VCC, na.rm=T), Pj = as.name("Pj"), VCC = as.name(VCC) ),
                     ~ Pj * Yj
                   ), 
                   nm=c("IDADE", "nj", "Nj", "N", "Pj", "Eyj", "Eyj2", "Yj", "Pj_Sj2", "Pj_Sj", "Pj_Yj")
                 ) 
    ) %>%
    # ungroup %>%
    mutate( EPj_Sj2  =   sum(Pj_Sj2), 
            EPj_Sj   =   sum(Pj_Sj), 
            Y        =   sum(Pj_Yj), # media estratificada (ponderada)     
            CV       = EPj_Sj / Y * 100, # Coeficiente de variancia
            t        = qt(alpha/2, df = sum(nj)-1, lower.tail = FALSE),     # a seguir, o t sera calculado utilizando o n calculado, de forma direta
            t_rec    = ifelse(pop=="inf",
                              qt(alpha/2, df = ceiling( t^2 * EPj_Sj^2 / (Erro*Y/100)^2 )-1, lower.tail = FALSE),
                              qt(alpha/2, df = ceiling( t^2 * EPj_Sj^2 / ( (Erro*Y/100)^2 + (t^2 * EPj_Sj2 / N )  ) )-1, lower.tail = FALSE)
            ),
            n_recalc = ifelse(pop=="inf",
                              ceiling( t_rec^2 * EPj_Sj^2 / (Erro*Y/100)^2 ),
                              ceiling( t_rec^2 * EPj_Sj^2 / ( (Erro*Y/100)^2 + (t_rec^2 * EPj_Sj2 / N )  ) ) 
            ), # agora fazemos o recalculo do n, utilizando o t correto
            nj_otimo = ceiling(n_recalc*Pj_Sj/EPj_Sj), # por estrato utilizando o metodo de Neyman
            n_otimo  = sum(nj_otimo), # n calculado total
            Yhatj    = Nj * Yj )  %>% # producao total por estrato
    na_if(0) %>% # substitui 0 por NA
    select_if(Negate(anyNA) ) %>%  # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
    round_df(casas_decimais)  
  
  x <- x_ %>% 
    plyr::rename(
      c("IDADE" = "Idade (meses)",
        "nj" = "numero de amostras / estrato (nj)" ,
        "Nj" = "Numero de amostras cabiveis / estrato (Nj)",
        "N" = "Numero de amostras cabiveis (N)", 
        "Pj" = "Proporcao Nj/N (Pj)",
        "Eyj" = "Somatorio do volume por estrato (Eyj)", 
        "Eyj2" = "Soma quadratica do volume por estrato (Eyj2)", 
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
        "n_recalc" = "Numero total de amostras referente ao erro admitido",
        "nj_otimo" = "Numero otimo de amostras por estrato (nj otimo)", 
        "n_otimo" = "numero total otimo de amostras (n otimo)", 
        "Yhatj" = "Producao total por estrato (Yhatj)"  ),
      warn_missing = F)
  
  
  
  
  y_ <- x_ %>%
    group_by_(.dots=grupos[-length(grupos)] ) %>%
    summarise(t            = mean(t),
              Sy           = ifelse(pop=="inf",
                                    sqrt(sum(Pj_Sj)^2 / sum(nj) ),
                                    sqrt(sum(Pj_Sj) ^2 / sum(nj) - (mean(EPj_Sj2) / mean(N) )  )
              ), # Erro-padrao da media
              Y            = sum(Pj_Yj), # media de vcc estratificada (ponderada) 
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
        "Yhat" = "Volume total estimado (Yhat)", 
        "Erro_Total" = "Erro Total",
        "IC_ha_Inf" = "IC (m3/ha) Inferior" ,
        "IC_ha_Sup" = "IC (m3/ha) Superior",
        "IC_Total_inf" = "IC Total (m3) inferior",
        "IC_Total_Sup" = "IC Total (m3) Superior"),
      warn_missing = F)
  
  
  if(tidy==F){
    
    z <- list(Tabela1 = as.data.frame(x_), Tabela2 = as.data.frame(y_))
    
    return(z)
    
  }else{
    
    vec1 <- names(x)[! names(x) %in% grupos ]
    vec2 <- grupos[length(grupos)]
    vec3 <- grupos[grupos!=vec2]
    vec4 <- names(y)[! names(y) %in% vec3 ] 
    vec5 <- vec3[length(vec3)]
    vec6 <- vec3[vec3!=vec5]
    
    x <-  x %>%
      gather_("Variaveis","value", vec1, factor_key=T) %>% 
      arrange_(grupos) %>% 
      spread_(vec2,"value",sep=" ") %>%
      group_by_(.dots=vec3)
    
    if(length(grupos)!=1 ){
      
      y <- y %>%
        gather_("Variaveis","value", vec4, factor_key=T) %>% 
        arrange_(vec3) %>% 
        spread_(vec5,"value") %>%
        group_by_(.dots=vec6)
      
    } else{
      
      y <- y %>%
        gather_("Variaveis","value", vec4, factor_key=T)
      
      
    }
    
    z <- list(Tabela1 = as.data.frame(x), Tabela2 = as.data.frame(y))
    
    return(z) 
    
  }
  
  
  
}

as_diffs <- function(df, VCC, area_parcela, area_total,  idade, grupos, alpha = 0.05, Erro = 10, casas_decimais=4, tidy=T ) {
  
  suppressPackageStartupMessages(require(dplyr))
  require(tidyr)
  require(lazyeval)
  
  if(missing(grupos) || is.null(grupos) || grupos==F || grupos==""){grupos=NULL}
  
  if(missing(df)||is.null(df)||df==F||df=="")
  {stop("Escolha o data frame") }
  
  if(missing(area_total)||is.null(area_total)||area_total==F||area_total=="")
  {stop("Escolha a variavel Area total (ha) ") }
  
  if(missing(area_parcela)||is.null(area_parcela)||area_parcela==F||area_parcela=="")
  {stop("Escolha a variavel Area da parcela (m2) ") }
  
  if(missing(VCC)||is.null(VCC)||VCC==F||VCC=="")
  {stop("Escolha a variavel Volume (m3)") }
  
  # argumentos opcionais
  if(missing(idade)||is.null(idade)||idade==F||idade==""){df$idade<-NA; idade <- "idade"}
  
  # argumentos de area podem ser numericos
  if(is.numeric(area_parcela)){df$area_parcela <- area_parcela; area_parcela <- "area_parcela"}
  if(is.numeric(area_total  )){df$area_total   <- area_total; area_total     <- "area_total"}
  
  
  x_ <- df %>%
    group_by_(.dots = grupos) %>%
    summarise_(
      .dots = 
        setNames( 
          list( 
            interp(~ mean(idade), idade = as.name(idade) ),
            interp(~ n() ),
            interp(~ mean(area_total) / ( mean(area_parcela)/10000 ), area_total = as.name(area_total), area_parcela = as.name(area_parcela)  ),
            interp(~ sd(VCC) / mean(VCC) * 100, VCC = as.name(VCC) ),
            ~ qt(alpha/2, df = n-1, lower.tail = FALSE),
            ~ qt(alpha/2, df = ceiling( t^2 * CV^2 / Erro^2) - 1, lower.tail = FALSE)  ,
            ~ ceiling( t_rec ^2 * CV^2 / Erro^2 ) ,
            interp(~ mean(VCC, na.rm=T), VCC = as.name(VCC) ),
            interp(~ sqrt( (sum(diff(VCC)^2) / (2 * n * (n-1) ) ) * ((N-n)/N) ) , VCC = as.name(VCC), n = as.name("n"), N = as.name("N") ),
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
                    "n"            = "Numero de Parcelas (n)"         ,
                    "N"            = "Numero de Parcelas cabiveis (N)", 
                    "CV"           = "Coeficiente de Variancia (CV)"  ,
                    "t"            = "t-student"                      ,
                    "t_rec"        = "t-student recalculado"                  ,
                    "n_recalc"     = "Numero de amostras referente ao erro admitido",
                    "Y"            = "Media geral (Y)"                ,
                    "Sy"           = "Erro-Padrao da Media (Sy)"      ,
                    "Erroabs"      = "Erro Absoluto"                  ,
                    "Erroperc"     = "Erro Relativo (%)"              ,
                    "Yhat"         = "Volume total estimado (Yhat)"   , 
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
      gather_("Variaveis","value", vec1, factor_key=T ) %>% 
      arrange_( grupos ) %>% 
      spread_(vec2,"value",sep="")%>%
      group_by_(.dots=vec3)
    
    return(y)
  }
  
  
  
}

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
                                                                  style = x ~ style(color = ifelse(x <= acsdt[1,2], "#108e00", "red"))) ,
                                  # colore o erro estimado de verde ou vemelho, se ela for menor ou maior que o erro desejado
                                  area(row=10, col=2) ~ formatter("span", 
                                                                  style = x ~ style(color = ifelse(x <= input$erroacs, "#108e00", "red")))
                                  
                                  
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
                                                                  style = x ~ style(color = ifelse(x <= input$erroace, "#108e00", "red")))
                                  
                                  
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
                                                                  style = x ~ style(color = ifelse(x <= asdt[1,2], "#108e00", "red"))) ,
                                  # colore o erro estimado de verde ou vemelho, se ela for menor ou maior que o erro desejado
                                  area(row=10, col=2) ~ formatter("span", 
                                                                  style = x ~ style(color = ifelse(x <= input$erroas, "#108e00", "red")))
                                  
                                  
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

