check_yi <- function( vol, yi  ){
  if(is.null(yi)){
    
  }else if(is.na(yi)){
    
  }else  if(is.null(vol)){
    
  }else if(is.na(vol)){
    
  }else if(vol=="" & yi=="VCC_HA"){
    paste("Volume variable must be defined if 'VCC_HA' is chosen as Yi")
  }
  
}
