inv_summary <- function(df,DAP, HT, VCC, area_parcela, groups, area_total,idade,VSC,Hd) {
  suppressPackageStartupMessages(require(dplyr))
  require(lazyeval)
  
  if( missing(df)||is.null(df)||df==F)
  {stop("please insert data frame", call.=F)}
  
  if(missing(DAP)||is.null(DAP)||DAP==F||DAP=="" || !is.numeric(df[[DAP]]))
  {stop("please insert diameter variable", call.=F)}
  
  # if(missing(HT)||is.null(HT)||HT==F||HT=="" || !is.numeric(df[[HT]]))
  # {stop("please insert height variable", call.=F)}
  
  if(missing(VCC)||is.null(VCC)||VCC==F||VCC=="" || !is.numeric(df[[VCC]]))
  {stop("please insert volume variable", call.=F)}
  
  if(missing(area_parcela)||is.null(area_parcela)||area_parcela==F||area_parcela=="")
  {stop("please insert sample area variable", call.=F)}
  
  if(missing(groups)||is.null(groups)||groups==F||groups==""){groups<-NULL}else{df <-df[  apply(!is.na(df[groups ]) , MARGIN = 1, function(x) all(x) ), ] }
  
  
  # argumentos opcionais
  if(missing(area_total) || is.null(area_total) || is.na(area_total) || area_total==F || area_total==""   ){df$area_total<-NA; area_total <- "area_total"}
  if(missing(idade)      || is.null(idade)      || is.na(idade)      || idade==F      || idade==""        ){df$idade<-NA;      idade <- "idade"}
  if(missing(VSC)        || is.null(VSC)        || is.na(VSC)        || VSC==F        || VSC==""          ){df$VSC<-NA;        VSC <- "VSC"}
  if(missing(HT)         || is.null(HT)         || is.na(HT)         || HT==F         || HT==""           ){df$HT<-NA; df$Hd<-NA; HT <- "HT";   Hd <- "Hd"}
  
  # argumentos de area podem ser numericos
  if(is.numeric(area_parcela)){df$area_parcela <- area_parcela; area_parcela <- "area_parcela"}
  if(is.numeric(area_total  )){df$area_total   <- area_total; area_total     <- "area_total"}
  
  
  if((missing(Hd)||is.null(Hd)|| is.na(Hd)||Hd==F||Hd=="")  &&  !all(is.na(df[[HT]]) )) { # calculo da altura dominante
    
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
    
  } else{ 
    x <- df %>% mutate_(HD = Hd) 
  }
  
  
  x %>% 
    group_by_(.dots = groups) %>% 
    mutate_(.dots = setNames(list( lazyeval::interp(~ pi * DAP^2 / 40000, DAP = as.name(DAP) ) ), nm = "AS" ) ) %>% 
    summarise_(
      .dots =  
        setNames(  list(  
          lazyeval::interp(~ round(mean(as.numeric(idade), na.rm=T) ), idade = as.name(idade)) ,
          lazyeval::interp(~ mean(area_total, na.rm=T), area_total = as.name(area_total)) ,
          lazyeval::interp(~ mean(area_parcela, na.rm=T), area_parcela = as.name(area_parcela)) ,
          lazyeval::interp(~ round(mean(DAP, na.rm=T), 2), DAP = as.name(DAP) ) ,
          ~ round(sqrt(mean(AS, na.rm=T) * 40000 / pi), 2)  ,
          lazyeval::interp(~round(mean(HT, na.rm=T), 2), HT = as.name(HT) ),
          ~ round(mean(HD), 2),
          ~ round(n()* 10000/AREA_PARCELA, 2),
          ~ round(sum(AS, na.rm=T) * 10000/AREA_PARCELA, 4),
          lazyeval::interp(~round(sum(VCC, na.rm=T) * 10000/ AREA_PARCELA, 4 ), VCC = as.name(VCC), AREA_PARCELA = as.name("AREA_PARCELA") ),
          lazyeval::interp(~round(sum(VSC, na.rm=T) * 10000/ AREA_PARCELA, 4 ), VSC = as.name(VSC), AREA_PARCELA = as.name("AREA_PARCELA") )
          
        ), #list 
        nm = c("IDADE","AREA_TOTAL","AREA_PARCELA", "DAP", "q", "HT","HD","IndvHA","G_HA", "VCC_HA","VSC_HA" ) 
        )#setnames 
    ) %>% #sumarise 
    na_if(0) %>% # substitui 0 por NA
    select_if(Negate(anyNA)) # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
  
}
