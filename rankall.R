rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name

  ##Read outcome data
  data<-read.csv("C:/Coursera 2017/rprog%2Fdata%2FProgAssignment3-data/outcome-of-care-measures.csv",header=TRUE,sep=",")
  
  #Order the data by the Hospital names
  data<-data[order(data$Hospital.Name),]
  
  
  #Define a vector with the name of the three possible outcomes
  outnames<-c("heart attack", "heart failure", "pneumonia")
  
  #Function that returns all the hospitals with a given rank defined by num
  allrank<-function(data,outcome,num){
    
    if (outcome==outnames[1]){cn=11}
    if (outcome==outnames[2]){cn=17}
    if (outcome==outnames[3]){cn=23}
  
   # Remove columns except 2,7 and outcome
    data = data[,c(2,7,cn)]
    data[,3]<-suppressWarnings(as.numeric(as.character(data[,3])))
    data <- data[!is.na(data[,3]),] # Remove NA
    
    splited <- split(data, data$State)
    ans <- lapply(splited, function(x, num){
    x<-x[order(x[,3]),]
    if (num=="best"){num=as.numeric(1)}
    if (num=="worst"){num=as.numeric(nrow(x))} 
    
     x$Hospital.Name[num]
    },num)
      
    return ( data.frame(hospital=unlist(ans), state=names(ans)) )
   
    
    
    }
    
    
    
    
    
    
    
    
  
  
  
  
  
  
 
  
  
  
  if( !(outcome %in% outnames) ) {
    stop("invalid outcome.The only valid outcomes are:hart attack, heart failure and pneumonia")
    
  } else {
    
    hosplist <- allrank(data,outcome,num)
  }
  
  
  return(hosplist)
  
  
  
  
  }