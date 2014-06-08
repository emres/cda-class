best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    fileName <- "outcome-of-care-measures.csv"
    myData <- read.csv(fileName,
                       na.strings=c("Not Available"))

    isStateValid <- any(levels(myData$State) == state)
    if(!isStateValid) {
        stop("invalid state")
    }

    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    isOutcomeValid <- any(validOutcomes == outcome)
    if(!isOutcomeValid) {
        stop("invalid outcome")
    }

    myData <- myData[(myData$State == state) ,]

    if(outcome == "heart attack") {
        myData <- myData[!is.na(myData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) ,]
        myData <- myData[with(myData,
                              order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)) ,]
    }

    if(outcome == "heart failure") {
        myData <- myData[!is.na(myData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) ,]
        myData <- myData[with(myData,
                              order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)) ,]
    }

    if(outcome == "pneumonia") {
        myData <- myData[!is.na(myData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) ,]
        myData <- myData[with(myData,
                              order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)) ,]
    }

    toString(myData$Hospital.Name[1])
}
