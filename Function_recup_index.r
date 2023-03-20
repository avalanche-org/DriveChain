
library(stringr)

recup_index<- function(data = d, C = NULL){
  result <- NULL
  allpos <- str_locate_all(data, C)
  condd <- (summary(allpos)[, 1] == 0)
  if(!all(condd)){
    for(i in 1:length(allpos)){
      pos <- allpos[[i]]   
      pos <- pos[, 1]
      if(length(pos) >= 1){
        last  <- pos[(length(pos))]
        index <- str_sub(data[i], 1, last-1)
        result <- rbind(result, index)
      }else{
        result = rbind(result, NA)
      }
    }
    
  }else{
    data <- rep(NA, length(allpos))
    result <- data
  }
  rownames(result) <- NULL
  result <- data.frame(result)
  return(result)
}
