
rm(list = ls())


##------ 1st assignment (first version) 
pollutantmean <- function(directory, pollutant, id = 1:332) {
        directory <- "/Users/giorgiocavallo/Desktop/GIORGIO/COURSERA/specdata/"
        fileslist <- list.files(directory)
        df <- data.frame()
        for(i in id) {
                df <- rbind(df, read.csv(paste(directory, fileslist[i], sep = "")))
        }
        mean(df[,pollutant], na.rm = T) 
}

pollutantmean("specdata", "nitrate", 23)



##------- 2nd assignment with dplyr 
library(dplyr)

complete <- function(directory, id = 1:332) {
        directory <- "/Users/giorgiocavallo/Desktop/GIORGIO/COURSERA/specdata/"
        fileslist <- list.files(directory)
        df <- data.frame()
        for(i in id) {
                df <- rbind(df, read.csv(paste(directory, fileslist[i], sep = "")))
        }
        df %>%
                mutate(nobs = complete.cases(df)) %>%
                group_by(ID)                      %>%
                summarise(nobs = sum(nobs))
        
}

complete("specdata", 30:25)


##------- 2nd assignment (second version)
complete <- function(directory, id=1:332){
        directory <- "/Users/giorgiocavallo/Desktop/GIORGIO/COURSERA/specdata/"
        df <-  data.frame()
        fileslist <- list.files(directory)
        for (i in id){
                temp <- read.csv(paste(directory, fileslist[i], sep = ""))   
                u <- complete.cases(temp)
                df2 <- data.frame(cbind(i,nrow(temp[u,])))
                df <- rbind(df, df2)
        }
        names(df) <- c("id","nobs")
        df
        
}

complete("specdata", 30:25)


##------- 3rd assignment (first version)
corr <- function(directory, threshold = 0) {
        directory <- "/Users/giorgiocavallo/Desktop/GIORGIO/COURSERA/specdata/"
        fileslist <- list.files(path = directory)
        cor_vector <- numeric()
        for(i in 1:length(fileslist)) {
                df <- read.csv(paste(directory, fileslist[i], sep = ""))
                cc <- sum(complete.cases(df))
                if(cc > threshold) {
                        cor_vector <- c(cor_vector,cor(df$nitrate, df$sulfate, use = "pairwise.complete.obs"))
                        
                }} 
        
        cor_vector}


cor <- corr("specdata")

##------- 3rd assignment (second version)

corr <- function(directory, threshold=0){
        
        #List of all csv files
        filelist <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
        
        #Vector for values to be input into
        cor_vector <- numeric()
        
        #Loop for each file in list
        for (i in 1:length(filelist)) {
                data <- read.csv(filelist[i])
                
                cc <- sum(complete.cases(data))
                
                if (cc > threshold){
                        
                        dataN <- data[complete.cases(data[c('sulfate', 'nitrate')]),]
                        
                        cor_vector <- c(cor_vector, cor(dataN$sulfate, dataN$nitrate))
                        
                }
                
        }
        
        cor_vector
        
}
cr <- corr("specdata")

