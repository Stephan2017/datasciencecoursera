pollutantmean <- function(directory, pollutant="sulfate", id = 1:332) {
        tm <- 0
        tc <- 0
        for (i in id) {
                idtest <- as.character(i)
                if(length(idtest)=1) {
                        idtest = paste0("  ",idtest) 
                }
                else if(length(idtest)=2) {
                        idtest = paste0(" ",idtest)
                }
                x<-read.csv(paste(directory,idtest,".csv"))
                if(pollutant="sulfate") {
                        tm <- tm + mean(x.$sulfate,na.rm=TRUE)
                        tc <- tc + 1 
                }
                if(pollutant="nitrate") {
                        tm <- tm + mean(x.$nitrate,na.rm=TRUE)
                        tc <- tc + 1 
                }              
        }
        tm / tc
}