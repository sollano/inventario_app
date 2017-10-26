inv_summary <- function(df,DAP, HT, VCC, area_parcela, groups, area_total,idade,VSC,Hd) {
  
  suppressPackageStartupMessages(require(dplyr))
  require(lazyeval)
  
  if( is.na(df) || missing(df)||is.null(df)||df==F)
  {stop("please insert data frame")}
  
  if( is.na(DAP) || missing(DAP)||is.null(DAP)||DAP==F||DAP=="")
  {stop("please insert diameter variable")}
  
  if( is.na(HT) || missing(HT)||is.null(HT)||HT==F||HT=="")
  {stop("please insert height variable")}
  
  if( is.na(VCC) || missing(VCC)||is.null(VCC)||VCC==F||VCC=="")
  {stop("please insert volume variable")}
  
  if( is.na(area_parcela) || missing(area_parcela)||is.null(area_parcela)||area_parcela==F||area_parcela=="")
  {stop("please insert sample area variable")}
  
  if( is.na(groups) || missing(groups)||is.null(groups)||groups==F||groups==""){groups<-NULL}else{df <-df[  apply(!is.na(df[groups ]) , MARGIN = 1, function(x) all(x) ), ] }
  
  
  # argumentos opcionais
  if( is.na(area_total) || missing(area_total) || is.null(area_total) || area_total==F || area_total==""   ){df$area_total<-NA; area_total <- "area_total"}
  if( is.na(idade)      || missing(idade)      || is.null(idade)      || idade==F      || idade==""        ){df$idade<-NA;      idade <- "idade"}
  if( is.na(VSC)        || missing(VSC)        || is.null(VSC)        || VSC==F        || VSC==""          ){df$VSC<-NA;        VSC <- "VSC"}
  
  # argumentos de area podem ser numericos
  if( !is.na(area_parcela) || is.numeric(area_parcela)){df$area_parcela <- area_parcela; area_parcela <- "area_parcela"}
  if( !is.na(area_total) || is.numeric(area_total  )){df$area_total   <- area_total; area_total     <- "area_total"}
  
  
  if( is.na(Hd) || missing(Hd)||is.null(Hd)||Hd==F||Hd=="") { # calculo da altura dominante
    
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
