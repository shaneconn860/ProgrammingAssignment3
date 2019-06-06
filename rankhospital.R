rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data
        setwd("C:/Users/Shane/Documents/RProjects/ProgAssignment3")
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = c("NA", "Not Available"))
        
        ## If an invalid state value is passed to rankhospital, the
        #function should throw an error via the stop function with the exact message “invalid state”. If an invalid
        #outcome value is passed to rankhospital, the function should throw an error via the stop function with the exact
        #message “invalid outcome”.
        if (!any(state == data$State)) {
                stop('invalid state')
        }
        
        
        if(outcome == 'heart attack') {
                i <- 11
        }
        else if(outcome == 'heart failure') {
                i <- 17
        }
        else if(outcome == 'pneumonia') {
                i <- 23
        }
        else {
                stop('invalid outcome')
        }
        
        #handle the ties
        data.state <- data[data$State == state, ]
        data.state[, i] <- as.numeric(x=data.state[, i])
        data.state <- data.state[complete.cases(data.state), ]
        
        #If the number given by num is larger than the number of hospitals 
        #in that state, then the function should return NA
        
        if (num == "best"){
                num = 1
        }
        
        else if(num == "worst"){
                num = nrow(data.state)
        }
        
        else if(is.numeric(x=num)){
                if(num < 1 || num > nrow(data.state)){
                        return(NA)
                }
        }
        
        else {
                stop('invalid num')
        }
        
        
        
        data.state <- data.state[order(data.state[,i], data.state$Hospital.Name), ]
        
        return.names <- data.state[num, ]$Hospital.Name
        
        sort(return.names)[1]
        
        
}
