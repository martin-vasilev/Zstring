
assign_task<- function(data){
  
  new_dat<- NULL
  data$task<- NA
  nsubs<- unique(data$sub)
  
  for(i in 1:length(nsubs)){
    n<- subset(data, sub== nsubs[i])
    
    d<- read.csv(paste("Experiment/design/P", toString(nsubs[i]), ".txt", sep=""), sep="")
    d<- subset(d, item<181) # remove practice
    d$task<- as.character(d$task)
    
    for (j in 1:nrow(n)){
      a<- which(d$item== n$item[j])
      n$task[j]<- d$task[a]
    }
    new_dat<- rbind(new_dat, n)
  }
  
  return(new_dat)
}