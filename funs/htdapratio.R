htdapratio <- function(df, dap, ht, lower=0.2, upper=10){
  
  DF <- as.data.frame(df)
  
  # se DF nao for fornecido, nulo, ou  nao for dataframe, parar
  if(  missing(DF) || is.null(DF) || is.na(DF) || !is.data.frame(DF) ){  
    stop("DF not set", call. = F) 
  }
  
  # se dap nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(dap) || is.null(dap) || is.na(dap) || dap == "" || is.null(DF[[dap]] ) ){  
    stop("dap not set", call. = F) 
  }
  
  # se ht nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(ht) || is.null(ht) || is.na(ht) || ht == "" || is.null(DF[[ht]] ) ){  
    stop("ht not set", call. = F) 
  }
  
  # add nomes das linhas como coluna numerica (numerica e importante caso se queira usar esta coluna como filtro futuramente)
  DF$rowid <- as.numeric(rownames(DF))
  
  # remove NAs
  DF <- DF[ !is.na(DF[[dap]]) | !is.na(DF[[ht]]) , ]
  
  # cria razao
  DF$ratio <- DF[[dap]]/DF[[ht]] 
  
  # checha se razao e maior ou menor que os parametros 
  DF$ratio_boolean <-DF$ratio <= lower |  DF$ratio >= upper
  
  # cria uma variavel classificatoria de acordo com a razao
  DF$ratio_dapht <- ifelse(DF$ratio, "bad dap/ht ratio", "ok")
  
  #y <- DF[  DF$ratio_boolean , ]
  
  # remove a coluna TRUE/FALSE e traz a coluna classificatoria para frente, junto com DAP e ALTURA
  y <- DF %>% filter(ratio_boolean) %>% select(ratio_dapht, rowid, everything(), -ratio_boolean )
  
  #y$ratio_boolean <- NULL
  
  if(nrow(y) == 0){
    z <- NULL
    # warning("No inconsistencies were found. yay!",call. = F)
  }else{
      z <- y
      }
  
  # retorna um datafram com as linhas que se destacaram na razao
  return( z )
  
}
