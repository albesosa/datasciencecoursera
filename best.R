##This function:
## Reads outcome data
## Checks that state and outcome are valid
## Returns hospital name in that state with lowest 30-day death rate

##Just check that there is not na values in the columns of interest
#sum(as.numeric(is.na(outcomes[,11])))
#sum(as.numeric(is.na(outcomes[,17])))
#sum(as.numeric(is.na(outcomes[,23])))

best<-function(state,outcome) {

 
  
 ##Read outcome data
 data<-read.csv("C:/Coursera 2017/rprog%2Fdata%2FProgAssignment3-data/outcome-of-care-measures.csv",header=TRUE,sep=",")
 #Order the data by the Hospital names
 data<-data[order(data$Hospital.Name),]
 
  
 #Define a vector with the name of the three possible outcomes
 outnames<-c("heart attack", "heart failure", "pneumonia")
 
 ## Function that determines the hospital name (in a particular state) with lowest 30-day death rate
 
 besthosp<-function(data,state,outcome) {
   
   if (outcome==outnames[1]){cn=11}
   if (outcome==outnames[2]){cn=17}
   if (outcome==outnames[3]){cn=23}
   
   subset_ST<-data[data[,7]==state,]
   subset_out<-as.numeric(as.character(subset_ST[,cn]))
   ind<-which(subset_out==min(subset_out,na.rm=TRUE))
   hname<-as.character(subset_ST[ind[1],2])
   return(hname)
 }
 
 
 
 
 
 #Extract abbreviated names of States
 abbrev<-levels(data$State)
 
 
 

 
 
   if ( !(state %in% abbrev) ) {
     stop("invalid state")
   } else if( !(outcome %in% outnames) ) {
     stop("invalid outcome.The only valid outcomes are:hart attack, heart failure and pneumonia")
  
   } else {
  hname <- besthosp(data,state,outcome)
     }
   
   
   return(hname)
  
  
    
}