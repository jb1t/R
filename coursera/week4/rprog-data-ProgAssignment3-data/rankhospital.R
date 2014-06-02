rankhospital <- function(state, outcome, rank = "best")
{
    colIndexLowestMortalityValue <- 1
    colIndexHosptialName <- 2
    # Check that outcome is valid
    validConditions <- c("heart attack", "heart failure", "pneumonia")
    if(!(outcome %in% validConditions))
    {
        stop("invalid outcome")
    }
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    validStates <- unique(data$State)
    
    ## Check that state is valid
    if(!(state %in% validStates))
    {
        stop("invalid state")
    }
    
    columns <- c(11,17,23)
    matchIndex <- match(outcome, validConditions)
    colIndex <- columns[matchIndex]
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    filteredVector <- data[data$State==state & complete.cases(suppressWarnings(as.numeric(data[,colIndex]))), c(colIndex, colIndexHosptialName)]
    sortOrder <- order(suppressWarnings(as.numeric(filteredVector[,colIndexLowestMortalityValue])), filteredVector[,colIndexHosptialName])
    sortedVector <- filteredVector[sortOrder,]
    numberOfRows <- nrow(sortedVector)
    
    if(rank == "best")
    {
        rank <- 1
    }
    if(rank == "worst")
    {
        rank <- numberOfRows
    }

    sortedVector[rank,colIndexHosptialName]
}