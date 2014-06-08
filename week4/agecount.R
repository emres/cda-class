agecount <- function(age = NULL) {
    ## Check that "age" is non-NULL; else throw error
    if (is.null(age)) {
        stop("age argument cannot be NULL")
    }
    ## Read "homicides.txt" data file
    homicides <- readLines("homicides.txt")

    ## Extract ages of victims; ignore records where no age is
    ## given
    ## Return integer containing count of homicides for that age
    length(grep(paste("Age: +", age, " ", sep = "")
                ,homicides
                ,ignore.case = TRUE))
}

