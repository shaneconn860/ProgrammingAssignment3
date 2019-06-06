best <- function(state, outcome) {
        ## Read outcome data
        setwd("C:/Users/Shane/Documents/RProjects/ProgAssignment3")
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = c("NA", "Not Available"))
        
        ## If an invalid state value is passed to best, the
        #function should throw an error via the stop function with the exact message “invalid state”. If an invalid
        #outcome value is passed to best, the function should throw an error via the stop function with the exact
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
                
        # todo: handle the ties
        data.state <- data[data$State == state, ]
        data.state[, i] <- as.numeric(x=data.state[, i])
        
        data.state <- data.state[complete.cases(data.state), ]
        
        return.names <- data.state[(data.state[, i] == min(data.state[, i])), ]$Hospital.Name
        
        sort(return.names)[1]
        
        
}
