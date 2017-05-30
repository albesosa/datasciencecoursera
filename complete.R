

complete<-function(directory,id=1:332) {
 
  setwd(file.path("C:/Coursera 2017/",directory)) # setting the location where CSV files are located
  
  filenames<-list.files() ## list the names of all files in the location previously set
  
  nobs=0
  s=1
  
  ## loop over the files specified by vector id
  for (k in id){
    data <- read.csv(filenames[k], header=TRUE, sep=",")
    
    # Extact data from columns named sulfate and nitrate
    datas=data$sulfate
    datan=data$nitrate
    
    #Eliminating na values from sulfate and nitrate data sets
    datas1=datas[!is.na(datas)]
    datan1=datan[!is.na(datan)]
    
    nobs[s]=min(length(datas1),length(datan1))
    s=s+1
    
    rm(data,datas,datas1,datan,datan1) ## clear variables
  }

  data.frame(id=id,nobs=nobs)
  
  
}