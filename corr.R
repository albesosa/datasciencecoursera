

corr<- function(directory, threshold=0){
  
  setwd(file.path("C:/Coursera 2017/",directory)) # setting the location where CSV files are located
  
  filenames<-list.files() # list the names of all files in the location previously set
  
  comp<-complete(directory,1:length(filenames)) # completely observed cases in each data file
  
  compth<-comp[,2]>threshold
  
  ind<-comp[compth,1] # these are the files satisfying that the number of complete measurements are
                     # above the threshold
  correlation=c()
  
  if (length(ind)==0) {correlation=0}
      
     s=1
     for(k in ind){
         data <- read.csv(filenames[k], header=TRUE, sep=",")
         datasna<-na.omit(data)
         correlation[s]=c(datasna$sulfate, datasna$nitrate)
         s=s+1  
       
       rm(data,datasna)
      }
    
  correlation
  
}