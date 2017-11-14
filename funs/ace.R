#' Amostragem Casual Estratificada
#'
#' Funcao para o processamento de inventario florestal utilizando a amostragem casual estratificada.
#' @details 
#' Essa funcao permite o processamento de um inventario florestal utilizando a amostragem casual estratificada,
#' para n talhoes, considerando a populcao finita ou infinita.
#' E possivel executar varios inventarios de uma vez utilizando uma variavel categorica indicada no parametro \code{grupos}().

#' @param df Data frame a ser utilizado.
#' @param Yi Nome entre aspas da variavel volume, ou variavel a ser amostrada.
#' @param area_parcela  Nome entre aspas da variavel area da unidade amostral utilizada, em metros quadrados. Pode tambem ser um valor numerico.
#' @param area_estrato Nome entre aspas da variavel area dos estratos, em ha. Pode tambem ser um vetor contendo os valores das areas dos estratos.
#' @param grupos Nome entre aspas da(s) variavel(is) a serem utilizadas na estratificacao. 
#' Caso seja mais de uma, deve-se inserir um vetor contendo os nomes das variaveis.
#' Por exemplo: para um inventario estratificado para cada fazenda, contendo n talhoes cada,
#' utiliza-se: \code{c("FAZENDA", "TALHAO")}.
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
#' ace(ex1_mfr, "VCC", "AREA_PARCELA", "AREA_TALHAO",  grupos = "TALHAO", Erro = 5, pop = "fin")
#'
#' # Percebe-se que para se atingir o erro desejado, precisa-se de 59 parcelas ao total,
#' # sendo 14 no talhao 1, 21 no talhao 2 e 24 no talhao 3.
#' # Apos lancar as novas parcelas, realiza-se um inventario definitivo,
#' # considerando um erro 5% e populacao finita:
#' ace(ex2_mfr, "VCC", "AREA_PARCELA", "AREA_TALHAO",  grupos = "TALHAO", Erro = 5, pop = "fin")
#'
#' # O erro desejado foi atingido.
#' 
#' # Valores de area podem ser numericas:
#' ace(ex2_mfr, "VCC", 1000, c(14.4, 16.4,14.2),  grupos = "TALHAO", Erro = 5, pop = "fin")
#'
#' # variavel opcional idade, e um inventario estratificado para cada fazenda:
#' ace(ex6_mfr, "VCC", "AREA_PARCELA", "AREA_TALHAO",  grupos = c("FAZENDA","TALHAO"), idade = "IDADE")
#'
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}

ace <- function(df,Yi, area_parcela, area_estrato, grupos, idade, alpha = 0.05, Erro = 10, casas_decimais = 4, pop="inf", tidy=T ){
 

  if(missing(df)||is.null(df)||df==F||df=="")
  {stop("Escolha o data frame") }
  
  if(missing(area_estrato)||is.null(area_estrato)||area_estrato==F||area_estrato=="")
  {stop("Escolha a variavel Area do Estrato (ha)") }
  
  if(missing(area_parcela)||is.null(area_parcela)||area_parcela==F||area_parcela=="")
  {stop("Escolha a variavel Area da parcela (m2)") }
  
  if(missing(Yi)||is.null(Yi)||Yi==F||Yi=="")
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
    summarise_(.dots = setNames( list( lazyeval::interp( ~ mean(area_estrato) / (mean(area_parcela)/10000), area_estrato = as.name(area_estrato), area_parcela = as.name(area_parcela) ) ), nm="Nj" ) ) %>%
    summarise(N  = sum(Nj) )
  
  # Se tiver apenas linha, ou seja, se tiver apenas um talhao, cbind
  if(nrow(aux) == 1) {
    
    x_ <- cbind(data.frame(df),N = aux$N)
    
  }else{
    # se tiver mais de uma linha, ou seja, varios talhoes,
    # unir as areas aos dados originais utilizando join
    x_ <- left_join(data.frame(df),aux, by = grupos[-length(grupos)])
    
  }
  
  
   x_ <- x_ %>% 
    mutate_(.dots = setNames( list( lazyeval::interp( ~ area_estrato / ( area_parcela/10000 ), area_estrato = as.name(area_estrato), area_parcela = as.name(area_parcela) ) ), nm="Nj" ) ) %>%
    group_by_(.dots = grupos) %>%
    summarise_(.dots = 
                 setNames( 
                   list(
                     lazyeval::interp(~ mean(idade), idade = as.name(idade) ),
                     lazyeval::interp(~ n() ) ,
                     ~ mean(Nj),
                     ~ mean(N),
                     ~ Nj/N,
                     lazyeval::interp(~ sum(Yi), Yi= as.name(Yi) ),
                     lazyeval::interp(~ sum(Yi^2), Yi= as.name(Yi) ),
                     lazyeval::interp(~ mean(Yi, na.rm =T), Yi= as.name(Yi) ),
                     lazyeval::interp(~ Pj * var(Yi, na.rm=T), Pj = as.name("Pj"), Yi = as.name(Yi) ),
                     lazyeval::interp(~ Pj * sd(Yi, na.rm=T), Pj = as.name("Pj"), Yi = as.name(Yi) ),
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
    select_if(Negate(anyNA) ) # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
 
  
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
    group_by_(.dots=grupos[-length(grupos)] ) %>%
    summarise(t            = mean(t),
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
    
    vec1 <- names(x)[! names(x) %in% grupos ]
    vec2 <- grupos[length(grupos)]
    vec3 <- grupos[grupos!=vec2]
    vec4 <- names(y)[! names(y) %in% vec3 ] 
    vec5 <- vec3[length(vec3)]
    vec6 <- vec3[vec3!=vec5]
    
    x <-  x %>%
      tidyr::gather_("Variaveis","value", vec1, factor_key=T) %>% 
      arrange_(grupos) %>% 
      tidyr::spread_(vec2,"value",sep=" ") %>%
      group_by_(.dots=vec3)
    
    if(length(grupos)!=1 ){
      
      y <- y %>%
        tidyr::gather_("Variaveis","value", vec4, factor_key=T) %>% 
        arrange_(vec3) %>% 
        tidyr::spread_(vec5,"value") %>%
        group_by_(.dots=vec6)
      
    } else{
      
      y <- y %>%
        tidyr::gather_("Variaveis","value", vec4, factor_key=T)
      
      
    }
    
    z <- list(Tabela1 = as.data.frame(x), Tabela2 = as.data.frame(y))
    
    return(z) 
    
  }
  
  
  
}
