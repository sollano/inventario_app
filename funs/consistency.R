consistency <- function(df, cap, dap, ht,parcela,especie,arvore,fuste, lower=0.2, upper=10){
  
  DF <- as.data.frame(df)
  
  # se DF nao for fornecido, nulo, ou  nao for dataframe, parar
  if(  missing(DF) || all(is.null(DF)) || all(is.na(DF)) || !is.data.frame(DF) ){  
    stop("DF not set", call. = F) 
  }
  
  # se cap for fornecido, calcular DAP
  if(  missing(cap) || is.null(cap) || is.na(cap) || cap == "" || is.null(DF[[cap]]) ){
    
  }else{  
    DF$DAP <- DF[[cap]]/pi
    dap <- "DAP"
  }
  
  # se dap nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(dap) || is.null(dap) || is.na(dap) || dap == "" || is.null(DF[[dap]] ) ){  
    stop("dap not set", call. = F) 
  }
  
  # se dap nao for numerico, parar
  if(  !is.numeric(DF[[dap]] ) ){  
    stop("dap column must be numeric", call. = F) 
  }
  
  
  # se ht nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(ht) || is.null(ht) || is.na(ht) || ht == "" || is.null(DF[[ht]] ) ){  
    DF$HT <- DF[[dap]]
    ht_ <- "HT"
  }else{
    ht_ <- ht
  }
  
  # se ht nao for numerico, parar
  if(  !is.numeric(DF[[ht_]] ) ){  
    stop("ht column must be numeric", call. = F) 
  }
  
  # se especie nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(especie) || is.null(especie) || is.na(especie) || especie == "" || is.null(df[[especie]]) ){  
    DF$especie_rm <- "none"
    ESPCC <- "especie_rm"
  }else{
    ESPCC <- especie
  }
  
  # se arvore nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(arvore) || is.null(arvore) || is.na(arvore) || arvore == "" || is.null(df[[arvore]]) ){  
    DF$arvore_rm <- "none"
    ARVV <- "arvore_rm"
  }else{
    ARVV <- arvore
  }
  
  # se fuste nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(fuste) || is.null(fuste) || is.na(fuste) || fuste == "" || is.null(df[[fuste]]) ){  
    DF$fuste_rm <- "none"
    FUSTT <- "fuste_rm"
  }else{
    FUSTT <- fuste
  }
  
  # add nomes das linhas como coluna numerica (numerica e importante caso se queira usar esta coluna como filtro futuramente)
  DF$rowid <- as.numeric(rownames(DF))
  
  
  if(missing(parcela) || is.null(parcela) || is.na(parcela) || parcela == ""){
    
    DF$parcela <- "grupoinuteel"
    PARCC <- "parcela"
  }else{
    
    PARCC <- parcela
    
  }
  
  sym <- rlang::sym
  
  DAP <- sym(dap)
  HT <- sym(ht_)
  ESPECIE <- sym(ESPCC)
  ARVORE <- sym(ARVV)
  FUSTE <- sym(FUSTT)
  DF$rowid <- as.numeric(rownames(DF))
  
  y <-  DF %>% 
    group_by_at(vars(PARCC)) %>% 
    rename(dap=!!dap, ht=!!ht_) %>% 
    summarise_at(vars(dap,ht), 
                 funs(
                   mean(.,na.rm=T),
                   sd(.,na.rm=T),
                   mean_minus_3_sd = mean(.,na.rm=T) - sd(.,na.rm=T)*3,
                   mean_plus_3_sd = mean(.,na.rm=T) + sd(.,na.rm=T)*3 ) ) %>% 
    full_join(DF,by=PARCC) %>% 
    rename(dap=!!dap, ht=!!ht_) %>% 
    mutate(
      DAP_test = case_when(
        dap <= dap_mean_minus_3_sd ~ "DAP menor que media + 3 sd",
        dap >  dap_mean_plus_3_sd  ~ "DAP maior que media + 3 sd",
        TRUE                       ~ "ok"
      ),
      
      HT_test = case_when(
        is.na( dap ) & is.numeric( ht ) ~ "Arvore com altura e sem dap",
        ht <= ht_mean_minus_3_sd        ~ "Altura menor que media - 3 sd",
        ht >  ht_mean_plus_3_sd         ~ "Altura maior que media + 3 sd",
        TRUE                            ~ "ok"
      ),
      
      ratio_test = case_when(
        (dap)/(ht) <= lower | (dap)/(ht) >= upper ~ "Razao dap/ht ruim",
        TRUE                                      ~ "ok"  
        
      ),
      especie_test = case_when(
        
        (!!ESPECIE) %in% c("", " ", "  ") | is.na(!!ESPECIE) ~ "Especie vazia",
        stringr::str_sub(!!ESPECIE, 1)==" "                  ~ "Espaco vazio no inicio de especie", 
        stringr::str_sub(!!ESPECIE,-1)==" "                  ~ "Espaco vazio no final de especie",
        TRUE                                                 ~ "ok"
        
      ),
      
      arvore_test = case_when(
        
        (!!ARVORE) %in% c("", " ", "  ") | is.na(!!ARVORE) ~ "arvore vazia",
        stringr::str_sub(!!ARVORE, 1)==" "                  ~ "Espaco vazio no inicio de arvore", 
        stringr::str_sub(!!ARVORE,-1)==" "                  ~ "Espaco vazio no final de arvore",
        TRUE                                                 ~ "ok"
        
      ),
      
      fuste_test = case_when(
        
        (!!FUSTE) %in% c("", " ", "  ") | is.na(!!FUSTE) ~ "fuste vazia",
        stringr::str_sub(!!FUSTE, 1)==" "                ~ "Espaco vazio no inicio de fuste", 
        stringr::str_sub(!!FUSTE,-1)==" "                ~ "Espaco vazio no final de fuste",
        TRUE                                             ~ "ok"
        
      ),
      
      parcela_test = case_when(
        
        (!!sym(PARCC)) %in% c("", " ", "  ") | is.na(!!sym(PARCC)) ~ "parcela vazia",
        stringr::str_sub(!!sym(PARCC), 1)==" "                     ~ "Espaco vazio no inicio de parcela", 
        stringr::str_sub(!!sym(PARCC),-1)==" "                     ~ "Espaco vazio no final de parcela",
        TRUE                                                       ~ "ok"
        
      )
      
    ) %>% 
    filter( !is.na(dap) | !is.na(ht)  ) %>% 
    filter(DAP_test != "ok" | HT_test != "ok" | ratio_test != "ok" | especie_test != "ok") %>% 
    # filter_at(vars( "DAP_test", "HT_test", "ratio_test" ), any_vars(. != "ok") ) %>% 
    select(rowid, DAP_test, HT_test, ratio_test, especie_test, parcela_test,arvore_test,fuste_test,  everything(), -ht_mean_minus_3_sd,-ht_mean_plus_3_sd,-dap_mean_minus_3_sd,-dap_mean_plus_3_sd ) %>% 
    arrange(rowid) %>% 
    as.data.frame
  
  # remover ht e testes relacionados, caso ele nao seja fornecido
  if(  missing(ht) || is.null(ht) || is.na(ht) || ht == "" || is.null(DF[[ht]] ) ){  
    y[c("ht", "HT_test", "ht_mean", "ht_sd", "ratio_test")] <- NULL
    
  }
  
  
  if(missing(parcela) || is.null(parcela) || is.na(parcela) || parcela == ""){
    
    y[c("parcela", "parcela_test")] <- NULL
  }
  
  # se especie nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(especie) || is.null(especie) || is.na(especie) || especie == "" ){  
    y[c("especie_rm", "especie_test")] <- NULL
  }
  
  # se arvore nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(arvore) || is.null(arvore) || is.na(arvore) || arvore == "" ){  
    y[c("arvore_rm", "arvore_test")] <- NULL
  }
  
  # se fuste nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(fuste) || is.null(fuste) || is.na(fuste) || fuste == "" ){  
    y[c("fuste_rm", "fuste_test")] <- NULL
  }
  
  
  if(nrow(y) == 0){
    z <- NULL
    warning("No inconsistencies were found. yay!",call. = F)
  }else{
    z <- y
  }
  
  # retorna um datafram com as linhas que se destacaram na razao
  return( z )
  
  
}

