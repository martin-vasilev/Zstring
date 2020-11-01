
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


#### Bayesian model parameters:
NwarmUp<- 500#1000
Niter<- 1000#6000
Nchains<- 4 #10


# Load previous data by Vasilev et al. (submitted): 0 ms vs 120 ms delay manipulation:
dat <- read.csv2("D:/R/Zstring/power/dat.csv")

# In this experiment, we have 120ms delay of sounds. So, we subset only trials
# with 120ms delay in Vasilev et al.

dat<- subset(dat, del==120)

# code sound as factor:
dat$sound_type<- as.factor(dat$sound_type)
contrasts(dat$sound_type)<- c(1, -1)
contrasts(dat$sound_type) # effect size is positive


# Next, we create a fake "task" factor since there is no existing data
dat$task<- "reading"

# We can randomly split observations by task, and then manually change
# the slope values in the power analysis models:
a= sample_n(dat, nrow(dat)/2)
dat$task[is.element(dat$X, a$X)]<- "scanning"

table(dat$task)






