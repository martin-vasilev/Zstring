
rm(list= ls())

# colorblind palletes: # https://venngage.com/blog/color-blind-friendly-palette/
pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F") # "Classic & trustworthy"

# load/ install required packages:
packages= c("reshape", "brms", "grid", "emmeans", "parallel", "boot", "simr", "dplyr") # list of used packages:

for(i in 1:length(packages)){
  
  if(packages[i] %in% rownames(installed.packages())==FALSE){
    install.packages(packages[i])
    library(packages[i], character.only=TRUE)
  }else{
    library(packages[i], character.only=TRUE)
  }
}

source("power/gen_data.R")

#### Bayesian model parameters:
NwarmUp<- 500#1000
Niter<- 1000#6000
Nchains<- 4 #10


dat<- gen_data(nSub = 24)



