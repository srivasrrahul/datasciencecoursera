filename <- function(directory,id) {
  fileName <- ""
  if (id < 10) {
    fileName <- paste("00",id,".csv",sep="")
  }else if (id < 100) {
    fileName <-  paste("0",id,".csv",sep="")
  }else {
    fileName <- paste(id,".csv",sep="")
  }
  
  file.path(directory,fileName)
}

pollutantfile <- function(fileName,pollutant,id) {
  #print(fileName)
  data <- read.csv(fileName)
  
  removedNaData <- na.omit(data)
  ##print(removedNaData)
  removedNaData
}


complete <- function(directory,id = 1:332) {
  data <- data.frame(matrix(ncol=2))
  #data <- data.frame(ncol=2)
  j <- 1
  for (i in id) {
    fileName <- filename(directory,i)
    removedNaData <- pollutantfile(fileName,pollutant)
    data[j,1] <- i
    data[j,2] <- nrow(removedNaData)
    j <- j + 1
  }
  
  names(data) <- c("id","nobs")
  data
}