getmonitor <- function(id, directory, summarize = FALSE) {
    ## 'id' is a vector of length 1 indicating the monitor ID
    ## number. The user can specify 'id' as either an integer, a
    ## character, or a numeric.

    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'summarize' is a logical indicating whether a summary of
    ## the data should be printed to the console; the default is
    ## FALSE

    ## Your code here

    strId <- toString(id)
    fileId <- strId

    if(nchar(strId) == 1) {
        fileId <- paste("00", strId, sep = '')
    }

    if(nchar(strId) == 2) {
        fileId <- paste("0", strId, sep = '')
    }

    fileName <- paste(directory, fileId, sep = '/')
    fileName <- paste(fileName, ".csv", sep = '')

    myData <- read.csv(fileName)

    if(summarize) {
        print(summary(myData))
    }

    myData
}
