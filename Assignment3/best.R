
best <- function(state,outcome) {
   
    outcomeData <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv")
    #print(names(outcomeData))
    columnIndex <- 13
   
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
    
    
    bestHospital <- ""
    lowestRate <- 0
    checkStateAndOutCome <- function(outcomeRow) {
        localBestHospital <- bestHospital
        localLowestRate <- lowestRate
        
        if (outcomeRow[columnIndex] != "Not Available") {
            currentRate <- as.numeric(outcomeRow[columnIndex])
            #print(currentRate)
            if (outcomeRow[7] == state && is.numeric(currentRate) && !is.na(currentRate)) {
                #print(outcomeRow[columnIndex])
                ##print(outcomeRow[7])
                #class(outcomeRow)
                #print(outcomeRow["Hospital.Name"])
                
                if (localBestHospital == "") {
                    #print("Inside first case")
                    bestHospital <<- outcomeRow["Hospital.Name"]
                    #print(bestHospital)
                    lowestRate <<- currentRate
                }else if (currentRate < localLowestRate) {
                        bestHospital <<- outcomeRow["Hospital.Name"]
                        #print(bestHospital)
                        lowestRate <<- currentRate
                }else if (currentRate  == localLowestRate) {
                        if (localBestHospital > outcomeRow["Hospital.Name"]) {
                            bestHospital <<- outcomeRow["Hospital.Name"]
                        } 
                }
            }
            
            #bestHospital
        }
    }
    
     
    apply(outcomeData,1,checkStateAndOutCome)
    #bestHospital<-apply(outcomeData,1,bestInState(state,columnIndex))
    
    as.vector(bestHospital)
    #bestHospital
    
}