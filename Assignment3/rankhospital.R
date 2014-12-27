rankIndex <- function(data,rank) {
    if (is.numeric(rank)) {
        rank
    }else if (rank == "best") {
        rank <- 1
    }else {
        rank <- nrow(data)
    }
}

rankhospital <- function(state,outcome,num) {
    
    outcomeData <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv")
    #print(names(outcomeData))
    columnIndex <- 11
    
    if (outcome == "heart attack") {
        columnIndex <- 11
    }else if (outcome == "heart failure") {
        #print("Its heart failure")
        columnIndex <- 17
    }else if (outcome == "pneumonia") {
        columnIndex <- 23
    }else {
        stop("invalid outcome")
    }
    
    #print(columnIndex)
    stateVec <- c('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID','IL','IN',
                  'IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH',
                  'NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX',
                  'UT','VT','VA','WA','WV','WI','WY','DC','GU','MP','PR','VI')
    
    if (!(state %in% stateVec)) {
        stop("invalid state")
    }
    
    
    hospital <- data.frame()
    local <- data.frame()
    #lowestRate <- c()
    checkStateAndOutCome <- function(outcomeRow) {
        #local<-hospital
        #print("Data is")
        #print(hospital)
        
        if (outcomeRow[columnIndex] != "Not Available") {
            currentRate <- as.numeric(outcomeRow[columnIndex])
            
            if (outcomeRow[7] == state && is.numeric(currentRate) && !is.na(currentRate)) {
                  #print(as.vector(outcomeRow["Hospital.Name"]))
                  hName <- as.character(outcomeRow["Hospital.Name"])
                  #print(hName)
                  #print(class(hName))
                  #StringAsFactors is a nasty bug :-)
                  newData <- data.frame(hName,currentRate,stringsAsFactors=FALSE)
                  hospital <<- rbind(hospital,newData) 
                 
            }
            
        }
        
        #newData
    }
    
    
    apply(outcomeData,1,checkStateAndOutCome)
    #print(names(hospital))
    hospital <- hospital[order(hospital$currentRate, hospital[["hName"]]),]
    #hospital <- hospital[with(hospital,order(hospital$currentRate, hospital$hName)),]
    #hospital <- hospital[with(hospital,order(hospital$hName)),]
    #hospital <- hospital[order(hospital$hName),]
    
    #print(class(hospital$hName))
    
    #rank <- rank-1
    #hospital[["hName"]]
    
    hospital[rankIndex(hospital,num),1]
    
    
}