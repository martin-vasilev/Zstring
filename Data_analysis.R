
rm(list= ls())

# colorblind palletes: # https://venngage.com/blog/color-blind-friendly-palette/
pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F") # "Classic & trustworthy"

# load/ install required packages:
packages= c("reshape", "lme4", 'car', "ggplot2", "ggpubr", "grid", "emmeans", 'BayesFactor') # list of used packages:

for(i in 1:length(packages)){
  
  if(packages[i] %in% rownames(installed.packages())==FALSE){
    install.packages(packages[i])
    library(packages[i], character.only=TRUE)
  }else{
    library(packages[i], character.only=TRUE)
  }
}

source('https://raw.githubusercontent.com/martin-vasilev/R_scripts/master/CohensD_raw.R')

## Load data:
accuracy <- read_csv("data/task_accuracy.csv")



#################
# Task accuracy #
#################

DesQuest<- melt(accuracy, id=c('sub', 'item', 'cond', 'task'), 
                measure=c("accuracy"), na.rm=TRUE)
mQuest<- cast(DesQuest, task ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))

# calculate accuracy per subject:
mQuest_sub<- cast(DesQuest, sub ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))

mean(accuracy$accuracy)*100
sd(accuracy$accuracy)*100
min(mQuest_sub$accuracy_M)
max(mQuest_sub$accuracy_M)

accuracy$sound<- as.factor(accuracy$sound)
accuracy$sound<- factor(accuracy$sound, levels= c('standard', 'silence', 'novel'))
contrasts(accuracy$sound)
accuracy$task<- as.factor(accuracy$task)
contrasts(accuracy$task)<- c(1, -1)
contrasts(accuracy$task)

if(!file.exists("Models/GM1.Rda")){
  summary(GM1<- glmer(accuracy~ sound*task+ (1|sub)+ (1|item), family = binomial, data= accuracy))
  save(GM1, file = "Models/GM1.Rda")
}else{
  load("Models/GM1.Rda")
  summary(GM1)
}

# calculate Cohen's d for the task main effect in comprehension
mQuest<- cast(DesQuest, task+sub ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))

CohensD_raw(data = mQuest, measure = 'accuracy_M', group_var = 'task', baseline = 'scanning', avg_var = 'sub')



##############
# Trial time #
##############

t <- read.csv("D:/R/Zstring/data/Trial_time.csv")


DesTime<- melt(t, id=c('sub', 'item', 'sound', 'task'), 
               measure=c("duration_ms"), na.rm=TRUE)
mTime<- cast(DesTime, task+sound ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))

t$sound<- as.factor(t$sound)
t$sound<- factor(t$sound, levels= c('standard', 'silence', 'novel'))
contrasts(t$sound)

t$task<- as.factor(t$task)
contrasts(t$task)<- c(1, -1)
contrasts(t$task)

if(!file.exists('Models/LM1.Rda')){
  
  summary(LM1<- lmer(log(duration_ms)~ sound*task +(task+sound|sub)+(task|item), data= t))
  save(LM1, file= 'Models/LM1.Rda')
  
}else{
  summary(LM1)
}



#####################
# Fixation duration #
#####################


raw_fix <- read.csv("D:/R/Zstring/data/raw_fixations.csv")

DesFix<- melt(raw_fix, id=c('sub', 'item', 'sound', 'task'), 
              measure=c("fix_dur"), na.rm=TRUE)
mFix<- cast(DesFix, task+sound ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))


#####################
# Saccade length    #
#####################

DesSL<- melt(raw_fix, id=c('sub', 'item', 'sound', 'task'), 
              measure=c("sacc_len"), na.rm=TRUE)
mSL<- cast(DesSL, task+sound ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

#######################
# Number of fixations #
#######################

nFix <- read.csv("data/number_fixations.csv")


DesFixNum<- melt(nFix, id=c('sub', 'item', 'sound', 'task'), 
                 measure=c("Nfix_1st", "Nfix_2nd", "Nfix_all"), na.rm=TRUE)
mFixNum<- cast(DesFixNum, task+sound ~ variable
               ,function(x) c(M=signif(mean(x),3)
                              , SD= sd(x) ))

