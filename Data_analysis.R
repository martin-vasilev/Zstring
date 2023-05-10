
rm(list= ls())

# colorblind palletes: # https://venngage.com/blog/color-blind-friendly-palette/
pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F") # "Classic & trustworthy"

# load/ install required packages:
packages= c("reshape", "lme4", 'car', "ggplot2", "ggpubr", "grid", "emmeans", 
            'effects', 'readr', 'ggeffects') # list of used packages:

for(i in 1:length(packages)){
  
  if(packages[i] %in% rownames(installed.packages())==FALSE){
    install.packages(packages[i])
    library(packages[i], character.only=TRUE)
  }else{
    library(packages[i], character.only=TRUE)
  }
}

source('functions/CohensD_raw.R')

## Load data:
accuracy <- read_csv("data/task_accuracy.csv")

options(scipen=999)



#### Task accuracy ####


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

# get range for each task:
mQuest_task<- cast(DesQuest, task+sub ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))
# reading:
min(mQuest_task$accuracy_M[mQuest_task$task=="reading"])
max(mQuest_task$accuracy_M[mQuest_task$task=="reading"])

# scanning:
min(mQuest_task$accuracy_M[mQuest_task$task=="scanning"])
max(mQuest_task$accuracy_M[mQuest_task$task=="scanning"])

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


#### Trial time ####


t <- read.csv("data/Trial_time.csv")


DesTime<- melt(t, id=c('sub', 'item', 'sound', 'task'), 
               measure=c("duration_ms"), na.rm=TRUE)
mTime<- cast(DesTime, task ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))

t$sound<- as.factor(t$sound)
t$sound<- factor(t$sound, levels= c('standard', 'silence', 'novel'))
contrasts(t$sound)

t$task<- as.factor(t$task)
contrasts(t$task)<- c(1, -1)
contrasts(t$task)

if(!file.exists('Models/LM1.Rda')){
  
  summary(LM1<- lmer(log(duration_ms)~ sound*task +(task|sub)+(task|item), data= t))
  save(LM1, file= 'Models/LM1.Rda')
  summary(LM1)
  
}else{
  load("Models/LM1.Rda")
  summary(LM1)
}

effect('task', LM1)


#### Fixation duration ####


raw_fix <- read.csv("D:/R/Zstring/data/raw_fixations.csv")

DesFix<- melt(raw_fix, id=c('sub', 'item', 'sound', 'task'), 
              measure=c("fix_dur"), na.rm=TRUE)
mFix<- cast(DesFix, task+sound ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))


raw_fix$sound<- as.factor(raw_fix$sound)
raw_fix$sound<- factor(raw_fix$sound, levels= c('standard', 'silence', 'novel'))
contrasts(raw_fix$sound)

raw_fix$task<- as.factor(raw_fix$task)
contrasts(raw_fix$task)<- c(1, -1)
contrasts(raw_fix$task)

if(!file.exists('Models/LM2.Rda')){
  
  summary(LM2<- lmer(log(fix_dur)~ sound*task +(sound|sub)+(1|item), data= raw_fix))
  save(LM2, file= 'Models/LM2.Rda')
  
}else{
  load("Models/LM2.Rda")
  summary(LM2)
}

effect('task', LM2)
CohensD_raw(data = raw_fix, measure = 'fix_dur', group_var = 'task',
            baseline = 'scanning', avg_var = 'sub')



#### Number of fixations ####

library(reshape2)
#nFix <- read.csv("data/number_fixations.csv")
word_measures <- read_csv("data/word_measures.csv")

DesFixNum<- melt(word_measures, id=c('sub', 'item', 'sound', 'task'), 
                 measure=c("nfix1", "nfix2", "nfixAll"), na.rm=TRUE)
mFixNum<- cast(DesFixNum, task+sound ~ variable
               ,function(x) c(M=signif(mean(x),3)
                              , SD= sd(x) ))

word_measures$sound<- as.factor(word_measures$sound)
word_measures$sound<- factor(word_measures$sound, levels= c('standard', 'silence', 'novel'))
contrasts(word_measures$sound)

word_measures$task<- as.factor(word_measures$task)
contrasts(word_measures$task)<- c(1, -1)
contrasts(word_measures$task)


if(!file.exists('Models/GM2.Rda')){
  
  summary(GM2<- glmer(nfixAll~ sound*task +(task|sub)+(task|item), data= word_measures, family= poisson))
  save(GM2, file= 'Models/GM2.Rda')
  
}else{
  load("Models/GM2.Rda")
  summary(GM2)
}

plot(effect('sound', GM2))
effect('sound', GM2)
effect('task', GM2)


CohensD_raw(data = word_measures, measure = 'nfixAll', group_var = 'task',
            baseline = 'scanning', avg_var = 'sub')

CohensD_raw(data = subset(word_measures, sound!= 'novel'), measure = 'nfixAll', group_var = 'sound',
            baseline = 'silence', avg_var = 'sub')


#### Saccade length ####


DesSL<- melt(raw_fix, id=c('sub', 'item', 'sound', 'task'), 
              measure=c("sacc_len"), na.rm=TRUE)
mSL<- cast(DesSL, task+sound ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

if(!file.exists('Models/LM3.Rda')){
  
  summary(LM3<- lmer(sacc_len~ sound*task +(1|sub)+(1|item), data= raw_fix))
  save(LM3, file= 'Models/LM3.Rda')
  
}else{
  load("Models/LM3.Rda")
  summary(LM3)
}

effect('task', LM3)
effect('sound', LM3)
effect('sound:task', LM3)


CohensD_raw(data = raw_fix, measure = 'sacc_len', group_var = 'task',
            baseline = 'scanning', avg_var = 'sub')

CohensD_raw(data = subset(raw_fix, sound!= 'novel'), measure = 'sacc_len', group_var = 'sound',
            baseline = 'silence', avg_var = 'sub')


# interaction:
CohensD_raw(data = subset(raw_fix, sound!= 'novel' & task== 'reading'), 
            measure = 'sacc_len', group_var = 'sound', baseline = 'silence', avg_var = 'sub')

# interaction:
CohensD_raw(data = subset(raw_fix, sound!= 'novel' & task== 'scanning'), 
            measure = 'sacc_len', group_var = 'sound', baseline = 'silence', avg_var = 'sub')



#### Regression probability ####

DesR<- melt(raw_fix, id=c('sub', 'item', 'sound', 'task'), 
            measure=c("regress"), na.rm=TRUE)
mR<- cast(DesR, task+sound ~ variable
          ,function(x) c(M=signif(mean(x),3)
                         , SD= sd(x) ))

if(!file.exists('Models/GM3.Rda')){
  
  # regressions:
  summary(GM3<- glmer(regress~ sound*task+ (task+sound|sub)+ (task|item), family = binomial, data= raw_fix))
  save(GM3, file= 'Models/GM3.Rda')
  
}else{
  load("Models/GM3.Rda")
  summary(GM3)
}

CohensD_raw(data = raw_fix, measure = 'regress', group_var = 'task',
            baseline = 'scanning', avg_var = 'sub')

effect('sound:task', GM3)



####  Skipping probability  ####

DesW<- melt(word_measures, id=c('sub', 'item', 'sound', 'task'), 
            measure=c("skip_1st"), na.rm=TRUE)
mW<- cast(DesW, task+sound ~ variable
          ,function(x) c(M=signif(mean(x),3)
                         , SD= sd(x) ))

if(!file.exists('Models/GM4.Rda')){
  
  summary(GM4<- glmer(skip_1st~ sound*task+ (task|sub)+ (task|item), family = binomial, data= word_measures))
  save(GM4, file= 'Models/GM4.Rda')
  
}else{
  load("Models/GM4.Rda")
  summary(GM4)
}

CohensD_raw(data = word_measures, measure = 'skip_1st', group_var = 'task',
            baseline = 'scanning', avg_var = 'sub')

effect('sound', GM4)
effect('sound:task', GM4)

CohensD_raw(data = subset(word_measures, sound!= "novel"), measure = 'skip_1st', group_var = 'sound',
            baseline = 'silence', avg_var = 'sub')

CohensD_raw(data = subset(word_measures, sound!= "novel" & task== 'reading'), measure = 'skip_1st',
            group_var = 'sound', baseline = 'silence', avg_var = 'sub')

CohensD_raw(data = subset(word_measures, sound!= "novel" & task== 'scanning'), measure = 'skip_1st',
            group_var = 'sound', baseline = 'silence', avg_var = 'sub')


####   Initial landing position ####
DesILP<- melt(word_measures, id=c('sub', 'item', 'sound', 'task'), 
              measure=c("ILP"), na.rm=TRUE)
mILP<- cast(DesILP, task+sound ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

if(!file.exists('Models/GM5.Rda')){
  
  summary(GM5<- glmer(ILP~ sound*task +(task+sound|sub)+(task+sound|item), data= word_measures, family= poisson))
  save(GM5, file= 'Models/GM5.Rda')
  
}else{
  load("Models/GM5.Rda")
  summary(GM5)
}

CohensD_raw(data = word_measures, measure = 'ILP', group_var = 'task',
            baseline = 'scanning', avg_var = 'sub')



#### First fixation duration ####
ffd <- read.csv2("data/first_fix_data.csv")

DesFFD<- melt(ffd, id=c('sub', 'item', 'sound', 'task'), 
                 measure=c("first_fix_dur", "next_fix_dur"), na.rm=TRUE)
mFFD<- cast(DesFFD, task+sound ~ variable
               ,function(x) c(M=signif(mean(x),3)
                              , SD= sd(x) ))

## Trial numbers post exclusion:
tpe= data.frame(table(ffd$sub, ffd$cond, ffd$task))

#aggregate(tpe$Freq, by= list(tpe$Var1, tpe$Var3, tpe$Var2), FUN= function(x) c(mean = mean(x, na.rm= T), 
     
# reading silence:                                                                                                                                        sd = sd(x, na.rm=T) ))
mean(tpe$Freq[which(tpe$Var2=="1" & tpe$Var3== "reading")])
sd(tpe$Freq[which(tpe$Var2=="1" & tpe$Var3== "reading")])
min(tpe$Freq[which(tpe$Var2=="1" & tpe$Var3== "reading")])
max(tpe$Freq[which(tpe$Var2=="1" & tpe$Var3== "reading")])

# reading standard:                                                                                                                                        sd = sd(x, na.rm=T) ))
mean(tpe$Freq[which(tpe$Var2=="2" & tpe$Var3== "reading")])
sd(tpe$Freq[which(tpe$Var2=="2" & tpe$Var3== "reading")])
min(tpe$Freq[which(tpe$Var2=="2" & tpe$Var3== "reading")])
max(tpe$Freq[which(tpe$Var2=="2" & tpe$Var3== "reading")])

# reading novel:                                                                                                                                        sd = sd(x, na.rm=T) ))
mean(tpe$Freq[which(tpe$Var2=="3" & tpe$Var3== "reading")])
sd(tpe$Freq[which(tpe$Var2=="3" & tpe$Var3== "reading")])
min(tpe$Freq[which(tpe$Var2=="3" & tpe$Var3== "reading")])
max(tpe$Freq[which(tpe$Var2=="3" & tpe$Var3== "reading")])

# scanning silence:                                                                                                                                        sd = sd(x, na.rm=T) ))
mean(tpe$Freq[which(tpe$Var2=="1" & tpe$Var3== "scanning")])
sd(tpe$Freq[which(tpe$Var2=="1" & tpe$Var3== "scanning")])
min(tpe$Freq[which(tpe$Var2=="1" & tpe$Var3== "scanning")])
max(tpe$Freq[which(tpe$Var2=="1" & tpe$Var3== "scanning")])

# scanning standard:                                                                                                                                        sd = sd(x, na.rm=T) ))
mean(tpe$Freq[which(tpe$Var2=="2" & tpe$Var3== "scanning")])
sd(tpe$Freq[which(tpe$Var2=="2" & tpe$Var3== "scanning")])
min(tpe$Freq[which(tpe$Var2=="2" & tpe$Var3== "scanning")])
max(tpe$Freq[which(tpe$Var2=="2" & tpe$Var3== "scanning")])

# scanning novel:                                                                                                                                        sd = sd(x, na.rm=T) ))
mean(tpe$Freq[which(tpe$Var2=="3" & tpe$Var3== "scanning")])
sd(tpe$Freq[which(tpe$Var2=="3" & tpe$Var3== "scanning")])
min(tpe$Freq[which(tpe$Var2=="3" & tpe$Var3== "scanning")])
max(tpe$Freq[which(tpe$Var2=="3" & tpe$Var3== "scanning")])

chisq.test(table(ffd$cond, ffd$task))



ffd$sound<- as.factor(ffd$sound)
ffd$sound<- factor(ffd$sound, levels= c("standard", "silence", "novel"))
contrasts(ffd$sound)


ffd$task<- as.factor(ffd$task)
contrasts(ffd$task)<- c(1, -1)
contrasts(ffd$task)

library(lme4)

if(!file.exists('Models/LM_FFD.Rda')){
  LM<- lmer(log(first_fix_dur)~ sound*task + (task+sound|sub) + (1|item), data = ffd, REML= T)
  save(LM, file= 'Models/LM_FFD.Rda')
  summary(LM)
}else{
  load('Models/LM_FFD.Rda')
  summary(LM)
}



effect('sound:task', LM)

CohensD_raw(data = subset(ffd, sound!='silence'), measure = 'first_fix_dur', group_var = 'sound',
            baseline = 'standard', avg_var = 'sub')

CohensD_raw(data = subset(ffd, sound!='silence'& task== 'reading'), measure = 'first_fix_dur',
            group_var = 'sound', baseline = 'standard', avg_var = 'sub')

CohensD_raw(data = subset(ffd, sound!='silence'& task== 'zString'), measure = 'first_fix_dur',
            group_var = 'sound', baseline = 'standard', avg_var = 'sub')


# Bayesian model of first fixation duration to test for null interaction:


#### Model parameters:
library(brms)
NwarmUp<- 1000
Niter<- 6000
Nchains<- 10

if(!file.exists("Models/BLM1.Rda")){
  BLM1<- brm(formula = log(first_fix_dur) ~ sound*task + (task+sound|sub) + (1|item), data = ffd,
            warmup = NwarmUp, iter = Niter, chains = Nchains,
            sample_prior = TRUE, seed= 1234, cores = 6, control = list(adapt_delta = 0.9),
            prior =  c(set_prior('normal(0, 0.08)', class = 'b', coef= 'soundsilence'),
                       set_prior('normal(0, 0.08)', class = 'b', coef= 'soundnovel'),
                       set_prior('normal(0, 0.08)', class = 'b', coef= 'task1'),
                       set_prior('normal(0, 0.08)', class = 'b', coef= 'soundsilence:task1'),
                       set_prior('normal(0, 0.08)', class = 'b', coef= 'soundnovel:task1'),
                       set_prior('normal(0, 5)', class = 'Intercept')))
  
  save(BLM1, file= "Models/BLM1.Rda")
}else{
  load('Models/BLM1.Rda')
}


print(BLM1, digits = 3)
prior_summary(BLM1)
VarCorr(BLM1)


## Bayes factors:

# Note: the Bayes Factor is BH_10, so values >1 indicate evidence for the alternative, and values <1 indicate 
# evidence in support of the null. Brms reports them the other way around, but I reverse them here because I 
# Think BF_10 reporting is somewhat more common

# Silence vs Silence sound effect:
BF1 = hypothesis(BLM1, hypothesis = 'soundsilence = 0', seed= 1234)
1/BF1$hypothesis$Evid.Ratio

# Novel vs Standard sound effect:
BF2 = hypothesis(BLM1, hypothesis = 'soundnovel = 0', seed= 1234)
1/BF2$hypothesis$Evid.Ratio

formatC(1/BF2$hypothesis$Evid.Ratio, format = "e", digits = 3) 

# Task effect:
BF3 = hypothesis(BLM1, hypothesis = 'task1 = 0', seed= 1234)  # H0: No sound x delay interaction
1/BF3$hypothesis$Evid.Ratio

# Silence vs Silence x Task effect:
BF4 = hypothesis(BLM1, hypothesis = 'soundsilence:task1 = 0', seed= 1234)  # H0: No sound x delay interaction
1/BF4$hypothesis$Evid.Ratio

# Novel vs Standard x Task effect:
BF5 = hypothesis(BLM1, hypothesis = 'soundnovel:task1 = 0', seed= 1234)  # H0: No sound x delay interaction
1/BF5$hypothesis$Evid.Ratio




# Sentistivity analysis excluding skips
ffd2<- subset(ffd, onTarget=="Yes")

LMs<- lmer(log(first_fix_dur)~ sound*task + (task+sound|sub) + (1|item), data = ffd2, REML= T)
summary(LMs)

DesFFD2<- melt(ffd2, id=c('sub', 'item', 'sound', 'task'), 
              measure=c("first_fix_dur", "next_fix_dur"), na.rm=TRUE)
mFFD2<- cast(DesFFD2, task+sound ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))



# Additional analysis of whether the presence of a search target ("a") affected distraction:

ffd3<- subset(ffd, task== "scanning")
raw_fix <- read.csv("data/raw_fixations.csv")

# find the fixations in the "raw_fix" dataframe and add the letter string that was fixated
ffd3$letter_string<- NA
  
for(i in 1:nrow(ffd3)){
  
  a<- which(raw_fix$SFIX== ffd3$tSFIX[i] & raw_fix$sub== ffd3$sub[i]) # find row number of fixations
  ffd3$letter_string[i]<- raw_fix$wordID[a]
  
}

ffd3$has_letter_target<- NA
for(i in 1:nrow(ffd3)){
  
  if(grepl(x = ffd3$letter_string[i], pattern = "o", fixed = TRUE)== TRUE){
    ffd3$has_letter_target[i]<- "Yes"
  }else{
    ffd3$has_letter_target[i]<- "No"
  }
  
}

table(ffd3$has_letter_target)

ffd3$has_letter_target<- as.factor(ffd3$has_letter_target)
contrasts(ffd3$has_letter_target)

LMs2<- lmer(log(first_fix_dur)~ sound*has_letter_target + (sound|sub) + (1|item), data = ffd3, REML= T)
summary(LMs2)

library(ggeffects)

mydf<- ggpredict(LMs2, terms = c("sound", "has_letter_target"), back.transform = T)
colnames(mydf)<- c("sound", "predicted", "std.error", "conf.low", "conf.high", "Letter 'o' present in string?")

mydf$sound<- as.factor(mydf$sound)
levels(mydf$sound)
mydf$sound<- factor(mydf$sound, levels= c("silence", "standard", "novel" ))


Eff_plot <-ggplot(mydf, aes(sound, predicted, group = `Letter 'o' present in string?`, 
                            colour= `Letter 'o' present in string?`,
                            fill= `Letter 'o' present in string?`,
                            shape= `Letter 'o' present in string?`,
                            ymin= conf.low, ymax= conf.high)) + 
  geom_line(size= 1.3) + geom_point(size= 4) +
  geom_ribbon(alpha=0.10, 
              colour=NA)+
  scale_color_manual(values=pallete1[1:2])+
  scale_fill_manual(values=pallete1[1:2])+
  xlab("Sound")+
  ylab("First fixation duration (model prediction)")+
  theme_classic(26)+ theme(legend.position = "top");Eff_plot

ggsave(filename = "Plots/scanning_target_analysis.pdf", plot = Eff_plot, width = 9, height = 9)


# Analysis of the NEXT fixation after playing the sound:

summary(LNF<- lmer(log(next_fix_dur)~ sound*task + (task|sub) + (1|item), data = ffd, REML= T))

DesNF<- melt(ffd, id=c('sub', 'item', 'sound', 'task'), 
            measure=c("next_fix_dur"), na.rm=TRUE)
mNF<- cast(DesNF, task+sound ~ variable
          ,function(x) c(M=signif(mean(x),3)
                         , SD= sd(x) ))


library(ggeffects)

mydf2<- ggpredict(LNF, terms = c("sound", "task"), back.transform = T)
colnames(mydf2)<- c("sound", "predicted", "std.error", "conf.low", "conf.high", "Task")

mydf2$sound<- as.factor(mydf2$sound)
levels(mydf2$sound)
mydf2$sound<- factor(mydf2$sound, levels= c("silence", "standard", "novel" ))


Eff_plot <-ggplot(mydf2, aes(sound, predicted, group = Task, 
                            colour= Task,
                            fill= Task,
                            shape= Task,
                            ymin= conf.low, ymax= conf.high)) + 
  geom_line(size= 1.3) + geom_point(size= 4) +
  geom_ribbon(alpha=0.10, 
              colour=NA)+
  scale_color_manual(values=pallete1[1:2])+
  scale_fill_manual(values=pallete1[1:2])+
  xlab("Sound")+
  ylab("Second fixation duration (model prediction)")+
  theme_classic(26)+ theme(legend.position = "top");Eff_plot

ggsave(filename = "Plots/second_fix_dur.pdf", plot = Eff_plot, width = 9, height = 9)




#### Regression probability after playing the sound:

if(!file.exists('data/regressions_after_sound.csv')){
  
  # load datasets again:
  ffd <- read.csv2("data/first_fix_data.csv")
  raw_fix <- read.csv("D:/R/Zstring/data/raw_fixations.csv")
  
  # Mark in the raw fixation file the fixations that occurred AFTER playing the sound:
  
  rf2<- NULL
  nsubs<- unique(raw_fix$sub)
  
  for(i in 1:length(nsubs)){
    n<- subset(raw_fix, sub== nsubs[i])
    nitems<- unique(n$item)
    
    for(j in 1:length(nitems)){
      m<- subset(n, item== nitems[j])
      a<- which(ffd$sub== m$sub[1] & ffd$item== m$item[1]) 
      
      if(length(a)==1){
        m$after_sound<- ifelse(m$SFIX> ffd$tPlaySound[a], 1, 0)
        m$fixation_number<- NA
        m$fixation_number[which(m$after_sound==1)]<- 1:length(which(m$after_sound==1))
      }else{
        next;
        #m$after_sound<- NA
        #m$fixation_number<- NA
      }
      
      
      rf2<- rbind(rf2, m)
    }
    
    cat(sprintf("%g ", i)); cat(" ")
    
  }
  
  write.csv(x = rf2, file = 'data/regressions_after_sound.csv')
  
}else{
  rg <- read.csv("D:/R/Zstring/data/regressions_after_sound.csv")
}

rg<- subset(rg, after_sound==1)

rg$sound<- as.factor(rg$sound)
rg$sound<- factor(rg$sound, levels= c('standard', 'silence', 'novel'))
contrasts(rg$sound)

rg$task<- as.factor(rg$task)
contrasts(rg$task)<- c(1, -1)
contrasts(rg$task)

rg$fixation_number_c<- scale(rg$fixation_number, scale = F)

if(!file.exists("Models/RGM.Rda")){
  summary(RGM<- glmer(regress ~ sound*task+ fixation_number_c + (sound|sub) + (1|item),
                      data = rg, family = binomial))
  save(RGM, file="Models/RGM.Rda")
}else{
  load("Models/RGM.Rda")
  summary(RGM)
}

library(ggeffects)

plot(ggpredict(RGM, c("sound", "task")))

library(reshape)
library(ggplot2)

rg$fixation_number_bin<- as.character(rg$fixation_number)
rg$fixation_number_bin[which(rg$fixation_number>=10)]<- "10+"

DesRG<- melt(rg, id=c('sub', 'item', 'sound', 'task', 'fixation_number_bin'), 
               measure=c("regress"), na.rm=TRUE)
mRGs<- cast(DesRG, task+sound+fixation_number_bin ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))

mRGs$fixation_number_bin<- as.factor(mRGs$fixation_number_bin)
mRGs$fixation_number_bin<- factor(mRGs$fixation_number_bin,
    levels= c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10+"))

mRGs$sound<- as.factor(mRGs$sound)
mRGs$sound<- factor(mRGs$sound, levels = c("silence","standard", "novel"))
mRGs$SE<- mRGs$regress_SD/sqrt(72)

Reg_plot<- ggplot(data=mRGs, aes(x=fixation_number_bin, y=regress_M, group= sound, color= sound,
                      fill= sound, ymin= regress_M- SE, ymax= regress_M+ SE)) +
  geom_bar(stat="identity", position=position_dodge())+ ylim(0, 1)+
  geom_errorbar(width=.4,
                position=position_dodge(.9))+
  scale_color_manual(values=pallete1[c(3,1,2)])+
  scale_fill_manual(values=pallete1[c(3,1,2)])+
  #  geom_line()+
#  geom_point()+ 
  facet_wrap(~task, nrow = 2)+
  theme_classic(26) +ylab("Regression probability")+
  xlab("Fixation number (after playing the sound)")+
  theme(legend.position = 'top',  strip.background = element_rect(colour=NA, fill=NA))

ggsave(plot = Reg_plot, filename = "Plots/Reg_plot.pdf", height = 8, width =12)


# create a plot with just condition means:
mRGs2<- cast(DesRG, task+sound ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))
mRGs2$sound<- as.factor(mRGs2$sound)
mRGs2$sound<- factor(mRGs2$sound, levels = c("silence","standard", "novel"))
mRGs2$SE<- mRGs2$regress_SD/sqrt(72)

Reg_plot2<- ggplot(data=mRGs, aes(x=task, y=regress_M, group= sound, color= sound,
                                 fill= sound, ymin= regress_M- SE, ymax= regress_M+ SE)) +
  geom_bar(stat="identity", position=position_dodge())+ ylim(0, 1)+
  geom_errorbar(width=.4,
                position=position_dodge(.9))+
  scale_color_manual(values=pallete1[c(3,1,2)])+
  scale_fill_manual(values=pallete1[c(3,1,2)])+
  #  geom_line()+
  #  geom_point()+ 
  theme_classic(26) +ylab("Regression probability")+
  xlab("Task")+
  theme(legend.position = 'top',  strip.background = element_rect(colour=NA, fill=NA))

ggsave(plot = Reg_plot2, filename = "Plots/Reg_plot_mean.pdf", height = 8, width = 12)





#### Saccade scanpaths:
library(scanpath)
library(tidyverse)
library(magrittr)

set.seed(1234)


fix<- read.csv("D:/R/Zstring/data/raw_fixations.csv")
fix<- subset(fix, !is.na(SFIX) & !is.na(word))
fix<- fix[, c("sub", "item", "word", "xPos", "yPos","fix_dur", 'task')]
colnames(fix)<- c('subject', 'trial', 'word', 'x', 'y', 'duration', 'task')


# plot sample scanpaths for 1st item:

s<- subset(fix, trial==1)
#s$subject<- paste(s$subject, sep= '')
s$subject<- as.factor(s$subject)
#s$subject<- paste('subject ', s$subject, sep= '')

levels(s$subject)

P1= plot_scanpaths(s, duration ~ word | subject, task) +xlab('Word number in sentence')+ ylab('Trial time (in ms)')+ 
  ggtitle('Scan paths for all participants reading/ scanning Item #1',
          subtitle = "Subplot for each subject (72 in total). Note that subjects saw the item in only 1 condition.")+
  theme_minimal(20)+ theme(legend.position="top")+ scale_color_manual(values=pallete1[1:2])+
  labs(colour= "Task")

ggsave(plot = P1, filename = 'Plots/scan_path_example.pdf', width = 12, height = 18)



# plot scanpaths for all items:

for(i in 1:180){ # for each item...
  s<- subset(fix, trial==i)
  #s$subject<- paste(s$subject, sep= '')
  s$subject<- as.factor(s$subject)
  
  
  # generate plot:
  P1= plot_scanpaths(s, duration ~ word | subject, task) +xlab('Word number in sentence')+ ylab('Trial time (in ms)')+ 
    ggtitle(paste('Scan paths for all participants reading/ scanning Item #', i, sep= ''),
            subtitle = "Subplot for each subject (72 in total). Note that subjects saw the item in only 1 condition.")+
    theme_minimal(20)+ theme(legend.position="top")+ scale_color_manual(values=pallete1[1:2])+
    labs(colour= "Task")
  
  ggsave(plot = P1, filename = paste('Plots/scanpaths/Item', i, '.pdf', sep= ''), width = 12, height = 18)
  
}






# plot_scanpaths(s, x ~ duration | subject, task)
# 
# d<- subset(fix, subject==1)
# 
# d2 <- scasim(data = subset(fix, task== 'reading'), data2 =  subset(fix, task== 'scanning'), formula = 
#              duration ~ x+x  | trial, center_x =  1920/2, center_y =  1080/2,
#              viewing_distance =  62, unit_size =  54/1920)
# 
# map <- cmdscale(d2)
# d2.dash <- as.matrix(dist(map))
# plot(d2, d2.dash)
# abline(0, 1)
# 
# 
# set.seed(4)
# clusters <- kmeans(map, 2, iter.max=100)
# plot(map, cex=4, col=clusters$cluster, pch=19)
# text(map, labels=rownames(map), col="white")
# points(clusters$centers, col="blue", pch=3, cex=4)

