#' @export
inv_summary <- function(df,DAP, HT, VCC, area_parcela, groups, area_total,idade,VSC,Hd) {
  
  if(missing(df)){stop("please insert data frame")}
  if(missing(DAP)){stop("please insert diameter variable")}
  if(missing(HT)){stop("please insert height variable")}
  if(missing(VCC)){stop("please insert volume variable")}
  if(missing(area_parcela)){stop("please insert sample area variable")}
  
  if(missing(groups)||is.null(groups)||groups==F){groups<-NULL}else{df <-df[  apply(!is.na(df[groups ]) , MARGIN = 1, function(x) all(x) ), ] }
  
  # argumentos opcionais
  if(missing(area_total) || area_total==FALSE || is.null(area_total) ){df$area_total<-NA; area_total <- "area_total"}
  if(missing(idade)      || idade==FALSE      || is.null(idade)      ){df$idade<-NA;      idade <- "idade"}
  if(missing(VSC)        || VSC==FALSE        || is.null(VSC)        ){df$VSC<-NA;        VSC <- "VSC"}
  
  # argumentos de area podem ser numericos
  if(is.numeric(area_parcela)){df$area_parcela <- area_parcela; area_parcela <- "area_parcela"}
  if(is.numeric(area_total  )){df$area_total   <- area_total; area_total     <- "area_total"}
  
  
  
  if(missing(Hd) || Hd=="" || is.null(Hd) ){ # se a altura dominante nao for fornecida
    
    # se ja existir uma variavel chamada "HD", deletar
    if(  "HD" %in% names(df) ){ df$HD <- NULL }
    
    # estimar altura dominante
    x <- hdjoin(df,groups, HT)
    
    # caso contrario, renomear "Hd" para "HD"
  } else{ x <- df %>% rename_(HD = Hd) }
  # novo nome = nome antigo
  
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
