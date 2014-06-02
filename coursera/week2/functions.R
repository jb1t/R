columnmean <- function(torm, removeNA = TRUE)
{
    numberColumns <- ncol(torm)
    means <- numeric(numberColumns)
    for(i in 1:numberColumns)
    {
        means[i] <- mean(torm[,i], na.rm=removeNA)
    }
    means
}