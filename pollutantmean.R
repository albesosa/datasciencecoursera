

pollutantmean<-function(directory,pollutant,id=1:332) {
  
  
  setwd(file.path("C:/Coursera 2017/",directory)) ## setting the location where CSV files are
  filenames<-list.files() ## list the names of all files in the location previously set
  
  obs=0
  tot=0
  
  ## loop over the files specified by vector id
  for (k in id) {  
   
    data <- read.csv(filenames[k], header=TRUE, sep=",")
    
    ## selecting the pollutant
    if (pollutant=="sulfate") { data1=data$sulfate}
     else if (pollutant=="nitrate") {data1=data$nitrate}
    
    data2<-data1[!is.na(data1)]
    obs=obs+length(data2)
    tot=tot+sum(data2) 
    
    rm(data,data1,data2) 
    }
    
    
    return(tot/obs)
  
}


  
  
