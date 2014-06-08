count <- function(cause = NULL) {
    ## Check that "cause" is non-NULL; else throw error
    if (is.null(cause)) {
        stop("cause argument cannot be NULL")
    }

    ## Check that specific "cause" is allowed; else throw error
    causes = c("asphyxiation", "force", "other", "shooting"
               ,"stabbing", "unknown")

    if (!any(causes == cause)) {
        stop(paste("cause argument should be one of the following:"
                   ,paste(causes, collapse = ", ")))
    }

    ## Read "homicides.txt" data file
    homicides <- readLines("homicides.txt")
    ## Extract causes of death
    ## Return integer containing count of homicides for that cause
    length(grep(paste("Cause: ", cause, sep = ""), homicides, ignore.case = TRUE))
}
