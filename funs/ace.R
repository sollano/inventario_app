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
        "nj" = "número de amostras / estrato (nj)" ,
        "Nj" = "Número de amostras cabíveis / estrato (Nj)",
        "N" = "Número de amostras cabíveis (N)", 
        "Pj" = "Proporção Nj/N (Pj)",
        "Eyj" = "Somatório do volume por estrato (Eyj)", 
        "Eyj2" = "Soma quadrática do volume por estrato (Eyj2)", 
        "Yj" = "Média do volume por estrato (Yj)", 
        "Pj_Sj2" = "PjSj2", 
        "Pj_Sj" = "PjSj", 
        "Pj_Yj" = "PjYj",
        "EPj_Sj2" = "EPjSj2",
        "EPj_Sj" = "EPjSj", 
        "Y" = "Média Estratificada (Y)",
        "CV" = "Coeficiente de Variância (CV)", 
        "t" = "t-student", 
        "t_rec" = "t-student recalculado", 
        "n_recalc" = "Número total de amostras referente ao erro admitido",
        "nj_otimo" = "Número ótimo de amostras por estrato (nj otimo)", 
        "n_otimo" = "número total ótimo de amostras (n otimo)", 
        "Yhatj" = "Produção total por estrato (Yhatj)"  ),
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
        "Sy" = "Erro-Padrao da Média (Sy)",
        "Y" = "Média estratificada (Y)",
        "Erroabs" = "Erro Absoluto" ,
        "Erroperc" = "Erro Relativo (%)",
        "Yhat" = "Volume total estimado (Yhat)", 
        "Erro_Total" = "Erro Total",
        "IC_ha_Inf" = "IC (m³/ha) Inferior" ,
        "IC_ha_Sup" = "IC (m³/ha) Superior",
        "IC_Total_inf" = "IC Total (m³) inferior",
        "IC_Total_Sup" = "IC Total (m³) Superior"),
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
