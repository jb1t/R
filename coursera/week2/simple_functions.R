add2 <- function(x, y)
{
    x + y
}

above10 <- function(v){
    aboveNumber(v, 10)
}

aboveNumber <- function(v, n = 10)
{
    use <- v > n
    v[use]
}

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

make.power <- function(n){
    pow <- function(x){
        x^n
    }
    pow
}

cube <- make.power(3)
square <- make.power(2)


