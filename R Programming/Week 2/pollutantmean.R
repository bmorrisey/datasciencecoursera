pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        idchars=as.character(id)
        idchars[nchar(idchars)<3] <- paste("0",idchars[nchar(idchars)<3],sep="")
        idchars[nchar(idchars)<3] <- paste("0",idchars[nchar(idchars)<3],sep="")
        
        fnames=paste(directory,"/",idchars,".csv",sep="")
        
        data=frame()
        
        for(i in seq_along(fnames)){
                
                data=rbind(data,read.csv(fnames[i]))
        }
        
#         data=read.csv("specdata/001.csv")
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        return(mean(data[[pollutant]],na.rm=TRUE))
}

# pollutantmean("specdata", "sulfate", 1:10)