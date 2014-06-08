rankall <- function(outcome, num = "best") {
    ## Read outcome data
    fileName <- "outcome-of-care-measures.csv"
    myData <- read.csv(fileName,
                       na.strings=c("Not Available"))

    ## Check that state is valid
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    isOutcomeValid <- any(validOutcomes == outcome)
    if(!isOutcomeValid) {
        stop("invalid outcome")
    }

    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name

    hospital = c()
    state = c()

    for (s in levels(myData$State)) {
        myStateData <- myData[myData$State == s ,]

        if(outcome == "heart attack") {
            myStateData <- myStateData[!is.na(myStateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) ,]
            myStateData <- myStateData[with(myStateData,
                                  order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name)) ,]
        }

        if(outcome == "heart failure") {
            myStateData <- myStateData[!is.na(myStateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) ,]
            myStateData <- myStateData[with(myStateData,
                                  order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name)) ,]
        }

        if(outcome == "pneumonia") {
            myStateData <- myStateData[!is.na(myStateData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) ,]
            myStateData <- myStateData[with(myStateData,
                                  order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name)) ,]
        }

        if(num == "best") {
            num = 1
            result <- toString(myStateData$Hospital.Name[num])
        } else if(num == "worst") {
            index = nrow(myStateData)
            result <- toString(myStateData$Hospital.Name[index])
        } else if(num > nrow(myStateData)) {
            result <- "NA"
        } else {
            result <- toString(myStateData$Hospital.Name[num])
        }

        state <- c(state, s)
        hospital <- c(hospital, result)
    }

    data.frame(hospital, state)

}
