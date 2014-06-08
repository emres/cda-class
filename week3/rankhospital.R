rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    fileName <- "outcome-of-care-measures.csv"
    myData <- read.csv(fileName,
                       na.strings=c("Not Available"))

    ## Check that state and outcome are valid
    isStateValid <- any(levels(myData$State) == state)
    if(!isStateValid) {
        stop("invalid state")
    }

    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    isOutcomeValid <- any(validOutcomes == outcome)
    if(!isOutcomeValid) {
        stop("invalid outcome")
    }

    myData <- myData[myData$State == state ,]

    ## Return hospital name in that state with the given rank
    ## 30-day death rate

    if(outcome == "heart attack") {
        myData <- myData[!is.na(myData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) ,]
        myData <- myData[with(myData,
                              order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name)) ,]
    }

    if(outcome == "heart failure") {
        myData <- myData[!is.na(myData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) ,]
        myData <- myData[with(myData,
                              order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name)) ,]
    }

    if(outcome == "pneumonia") {
        myData <- myData[!is.na(myData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) ,]
        myData <- myData[with(myData,
                              order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name)) ,]
    }

    if(num == "best") {
        num = 1
        result <- toString(myData$Hospital.Name[num])
    } else if(num == "worst") {
        num = nrow(myData)
        result <- toString(myData$Hospital.Name[num])
    } else if(num > nrow(myData)) {
        result <- "NA"
    } else {
        result <- toString(myData$Hospital.Name[num])
    }

    result
}
