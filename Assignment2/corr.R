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

pollutantfile <- function(fileName) {
  #print(fileName)
  data <- read.csv(fileName)
  
  removedNaData <- na.omit(data)
  #removedNaData <- subset(DF, !is.na("sulfate"))
  
  removedNaData
}

corr <- function(directory,threshold=0) {
  
  corrvec1 <- vector()
  
  for (i in 1:332) {
    fileName <- filename(directory,i)
    removedNaData <- pollutantfile(fileName)
   
    if (nrow(removedNaData) > threshold) {
      
       sulphatecorr <- removedNaData["sulfate"]
       nitratecorr <- removedNaData["nitrate"]
       corrvec1 <- c(corrvec1,cor(sulphatecorr,nitratecorr))
#       
    }
  
  }

  
  
  #print(nitratedata)
  corrvec1
  #corrvec
  
}