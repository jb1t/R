rankall <- function(outcome, num = "best") {
    colIndexLowestMortalityValue <- 1
    colIndexHosptialName <- 2
    colIndexState <- 7
    
    # Check that outcome is valid
    validConditions <- c("heart attack", "heart failure", "pneumonia")
    if(!(outcome %in% validConditions))
    {
        stop("invalid outcome")
    }
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    columns <- c(11,17,23)
    matchIndex <- match(outcome, validConditions)
    colIndex <- columns[matchIndex]
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate

    filteredDF <- data[complete.cases(suppressWarnings(as.numeric(data[,colIndex]))), c(colIndex, colIndexHosptialName, colIndexState)]
    sortOrder <- order(filteredDF[,3], suppressWarnings(as.numeric(filteredDF[,colIndexLowestMortalityValue])), filteredDF[,colIndexHosptialName])
    sortedDF <- filteredDF[sortOrder,]
    
    if(num == "best")
    {
        num <- 1
    }
    
    s <- split(sortedDF, sortedDF$State)
    
    df <- lapply(s, function(x) {
        rank <- num
        if(num == "worst")
        {
            rank <- nrow(x)
        }
        #list(x[rank,2], unique(x$State))
        outputDF <- x[rank, c(2,3)]
        if(is.na(outputDF[1,2]))
        {
            outputDF[1,2] <- unique(x$State)
        }
        outputDF
    })
    
    resultDF <- do.call(rbind, df)
    colnames(resultDF) <- c("hospital", "state")
    resultDF
}

