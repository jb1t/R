corr <- function(directory, threshold = 0, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    output <- vector()
    
    for(i in id)
    {
        filename <- paste(directory, "/", sprintf("%03.0f", i), ".csv", sep="")
        pollutantDF <- read.csv(filename)
        ccc <- nrow(pollutantDF[complete.cases(pollutantDF),])
        
        if(ccc > threshold)
        {
            tempDF <- pollutantDF[complete.cases(pollutantDF),]
            output <- c(output, cor(tempDF[,2], tempDF[,3]))
        }
    }
    output
}