corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        
        files_list <- list.files(directory, full.names = TRUE)
        dat <- data.frame()
        correl <- vector(mode="numeric")
        
        subset_complete <- complete(directory) ## take all complete cases
        subset_complete <- subset_complete[which(subset_complete$nobs>threshold),"id"] ## use only >threshold
        
        for(i in subset_complete) {
                dat <- read.csv(files_list[i])
                dat <- na.omit(dat)
                correl <- rbind(correl, cor(dat$nitrate,dat$sulfate))
        }
        
        correl
}