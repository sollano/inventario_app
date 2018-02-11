#' @export
hdjoin <- function(df, grupos, HT, DAP, OBS, dom, names="HD"){

  df[["HD"]] <- NULL
  
 if(missing(OBS) || is.null(OBS) || is.na(OBS) || OBS=="" ){
   
   if(is.null(grupos) || missing(grupos) || is.na(grupos) || grupos==""  ){
     suppressMessages(   # remove mensagens do dplyr
       df %>% 
         select_(ht = HT) %>% 
         top_n(2) %>% # seleciona as duas maiores arvores
         summarise(HD = mean(ht) ) %>% 
         cbind(df) # como nao ha grupos, usamos cbind
     )    }else{
       suppressMessages(
         
         df %>% 
           group_by_(.dots = grupos) %>% 
           select_(ht = HT) %>% 
           top_n(2) %>% 
           summarise(HD = mean(ht) ) %>% 
           full_join(df) # como ha grupos, usamos join
         
       )
     }
   
   
 }else{
   
   x <- df %>%
     group_by_(.dots = grupos) %>%
     filter_( 
       .dots =
         lazyeval::interp(~ !is.na(HT), HT = as.name(HT), .values =  list( HT = as.name(HT) ) ) ,# filtra alturas nao medidas
       lazyeval::interp(~ !is.na(DAP), DAP = as.name(DAP), .values = list( DAP = as.name(DAP) )  ), # filtra arvores nao medidas
       lazyeval::interp(~ OBS == dom, OBS = as.name(OBS), .values = list(OBS = as.name(OBS) ) ) # filtra arvores dominantes
     ) %>%
     summarise_(
       .dots = 
         setNames(
           list(
             lazyeval::interp( ~mean(HT), HT = as.name(HT) )
           ),
           nm=names
         )
     ) %>%
     ungroup
   
   
   df %>%
     filter_( 
       .dots =
         lazyeval::interp(~ !is.na(DAP), DAP = as.name(DAP), .values = list( DAP = as.name(DAP) )  )
     ) %>%
     left_join(x, by = grupos)
   
   
 }
  
}
