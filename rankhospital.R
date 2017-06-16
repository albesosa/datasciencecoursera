rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  
  ##Read outcome data
  data<-read.csv("C:/Coursera 2017/rprog%2Fdata%2FProgAssignment3-data/outcome-of-care-measures.csv",header=TRUE,sep=",")
  
  #Order the data by the Hospital names
  data<-data[order(data$Hospital.Name),]
  
  
  #Define a vector with the name of the three possible outcomes
  outnames<-c("heart attack", "heart failure", "pneumonia")
  
  #returns a character vector with the name of the hospital that has the ranking specifed by the num argument.
    bestrank<-function(data,state,outcome,num) {
    
    if (outcome==outnames[1]){cn=11}
    if (outcome==outnames[2]){cn=17}
    if (outcome==outnames[3]){cn=23}
    
    ST<-subset(data,State==state)
    STnum<-suppressWarnings(as.numeric(as.character(ST[,cn])))
    ST<-ST[order(STnum, na.last=NA),2]
    ST<-na.omit(ST)
    
    if (num=="best"){num=as.numeric(1)}
    if (num=="worst"){num=as.numeric(length(ST))}
    
    as.character(ST[num])
  }
  
  
  
  
  
  #Extract abbreviated names of States
  abbrev<-levels(data$State)
  
  
  
  
  
  
  if ( !(state %in% abbrev) ) {
    stop("invalid state")
  } else if( !(outcome %in% outnames) ) {
    stop("invalid outcome.The only valid outcomes are:hart attack, heart failure and pneumonia")
    
  } else {
 
       hname <- bestrank(data,state,outcome,num)
  }
  
  
  return(hname)
  
  
  
  
  }