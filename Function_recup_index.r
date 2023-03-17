
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

# x1 <- "AAPpp290A"
# x2 <- "AAPpp290"
# x3 <- "AAPpp290"
# x4 <- "AAPpp290"
# #
# C="C"
# b <-  c(x1,x2,x3,x4)
# b = id_missing_index
# recup_index(data = id_missing_index, "-C")
# aa = recup_index(data = b, C)

# recup_index(data = b, "C")

