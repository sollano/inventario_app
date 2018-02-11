xlsx.write.list <- function (file, df_list){
  
  # Remover objs nulos
  df_list_filter <- df_list[!sapply(df_list, is.null)]
  #df_list_filter <- df_list
  objnames <- names(df_list_filter)
  nobjects <- length(df_list_filter)
  
  # return(objnames[[2]])
  
  
  wb <- createWorkbook()
  for(i in 1:nobjects){
    

    try(    addDataFrame(as.data.frame(df_list_filter[[i]]), sheet = createSheet(wb, objnames[i]), row.names=FALSE) , silent = T)    
  }
  
  saveWorkbook(wb, file)
}
