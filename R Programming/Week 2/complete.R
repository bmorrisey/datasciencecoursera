complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        idchars=as.character(id)
        idchars[nchar(idchars)<3] <- paste("0",idchars[nchar(idchars)<3],sep="")
        idchars[nchar(idchars)<3] <- paste("0",idchars[nchar(idchars)<3],sep="")
        
        fnames=paste(directory,"/",idchars,".csv",sep="")
        
        nobs <- NULL
        
        for(i in seq_along(fnames)){
                
                data=read.csv(fnames[i])
                nobs[i]=sum(complete.cases(data))
        }
        
        return(data.frame(id,nobs))
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
}