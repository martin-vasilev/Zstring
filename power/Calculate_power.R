
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

options(scipen = 999)
#### Bayesian model parameters:
NwarmUp<- 100#1000
Niter<- 500#6000
Nchains<- 2 #10


### Simulation settings:
nSub= 24



dat<- gen_data(nSub = 24)

contrasts(dat$task)
contrasts(dat$sound)

# slope priors:

log(260)- log(240) # 0.08
(log(260)- log(240))*2 # 2x SD
# N~ (0, 0.08)

BM<- brm(formula = fix_dur ~ sound*task + (sound|sub)+ (1|item), data = dat, warmup = NwarmUp, iter = Niter, chains = Nchains,
          sample_prior = TRUE, cores = detectCores(), seed= 1234, control = list(adapt_delta = 0.9),
          prior =  c(set_prior('normal(0, 0.08)', class = 'b', coef= 'sound1'),
                     set_prior('normal(0, 0.08)', class = 'b', coef= 'task1'),
                     set_prior('normal(0, 0.08)', class = 'b', coef= 'sound1:task1'),
                    set_prior('normal(0, 5)', class = 'Intercept')))

A= print(BM, digits=5)
prior_summary(BM)

#save(BM, file= "power/BM.Rda")

## Bayes factors:

# Note: the Bayes Factor is BH_10, so values >1 indicate evidence for the alternative, and values <1 indicate 
# evidence in support of the null. Brms reports them the other way around, but I reverse them here because I 
# Think BF_10 reporting is somewhat more common

# sound effect:
BF_sound = hypothesis(BM, hypothesis = 'sound1 = 0', seed= 1234)  # H0: No sound effect
1/BF_sound$hypothesis$Evid.Ratio

# task effect:
BF_task = hypothesis(BM, hypothesis = 'task1 = 0', seed= 1234)  # H0: No delay effect
1/BF_task$hypothesis$Evid.Ratio

# interaction effect:
BF_int = hypothesis(BM, hypothesis = 'sound1:task1 = 0', seed= 1234)  # H0: No sound x delay interaction
1/BF_int$hypothesis$Evid.Ratio


t<- data.frame("N"= nSub, "Run"= i, "BF1"= 1/BF_sound$hypothesis$Evid.Ratio, 
               "BF2"= 1/BF_task$hypothesis$Evid.Ratio, "BF3"= 1/BF_int$hypothesis$Evid.Ratio,
               "b1"= A$fixed[2,1], "b2"= A$fixed[3,1], "b3"= A$fixed[4,1],
               "L1"= A$fixed[2,3], "U1"= A$fixed[2,4],
               "L2"= A$fixed[3,3], "U2"= A$fixed[3,4],
               "L3"= A$fixed[4,3], "U3"= A$fixed[4,4],
                min_Rhat= min(rhat(BM)), max_Rhat= max(rhat(BM)))
