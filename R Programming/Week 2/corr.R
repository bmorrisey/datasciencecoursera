source("complete.R")

corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        nComplete=complete(directory)
        
        ids=nComplete$id[nComplete$nobs>=threshold]
        if(length(ids)==0){return(numeric(0))}
        
        
        idchars=as.character(ids)
        idchars[nchar(idchars)<3] <- paste("0",idchars[nchar(idchars)<3],sep="")
        idchars[nchar(idchars)<3] <- paste("0",idchars[nchar(idchars)<3],sep="")
        
        fnames=paste(directory,"/",idchars,".csv",sep="")
        
        cors =numeric(0)
        
        for(i in seq_along(fnames)){
                
                data=read.csv(fnames[i])
                cors=c(cors,cor(data$sulfate,data$nitrate,use="pairwise.complete.obs"))
        }
        
#         return(round(cors,digits=4))
        return(cors)
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
}