add2 <- function(x,y){
        x+y
}

above10 <- function(x){
        use <- x > 10
        x[use]
}

above <- function(x, n = 10){
        use <- x > n
        x[use]
}

columnmean <- function(y, removeNA = TRUE){
        nc <- ncol(y)
        means <- numeric(nc)
        for(i in 1:nc) {
                means[i] <- mean(y[, i], na.rm = removeNA)
        }
        means
}

f <- function(x) {
        g <- function(y) {
                y + z
        }
        z <- 4
        x + g(x)
}

z <- 10
f(3)

x <- 5
y <- if(x < 3) {
        NA
} else {
        10
}

h <- function(x, y = NULL, d = 3L) {
        z <- cbind(x, d)
        if(!is.null(y))
                z <- z + y
        else
                z <- z + f
        g <- x + y / z
        if(d == 3L)
                return(g)
        g <- g + 10
        g
}

#
#  Scoping
#

a <- "Global"
f <- function() {
        
        g <- function(x) cat( x,a,"\n")
        g("called in f before assn of a:")
        a <- "in f"
        g("called in f after assn of a:")
        h <- function(x){
                a <- "defined in h"
                g(x)
        }
        h( "h calling g from in f:")
        a <- "redefined in f"
        h("h calling g in f after redefining a")
        rm(a)
        h("h calling g after removing a")
}

f()

