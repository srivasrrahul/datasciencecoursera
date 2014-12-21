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
  
  x <- data[[pollutant]]
  naVec <- is.na(x)
  removedNaData <- x[!naVec]
  removedNaData
}

pollutantmean <- function(directory,pollutant,id=1:332) {
    meanVec <- vector()
    for (i in id) {
      fileName <- filename(directory,i)
      meanPerFile <- pollutantfile(fileName,pollutant)
      meanVec <- c(meanVec,meanPerFile)
      
    }
    
    mean(meanVec)
}