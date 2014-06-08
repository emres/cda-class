corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations

    result = numeric()

    id <- 1:332
    for(file in id) {
        completeLevel = complete(directory, file)
        if(completeLevel$nobs > threshold) {
            myData <- getmonitor(file, directory)
            myCor <- cor(myData$sulfate, myData$nitrate, use="complete.obs")
            result <- c(result, myCor)
        }
    }

    result
}
