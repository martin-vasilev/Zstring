
rm(list= ls())

library(EMreading)
library(reshape)
source('functions/soundCheck.R')
source('functions/assign_task.R')


### Trial time:
t<- trialTime(data_list = 'D:/Data/zString', maxtrial = 180)
t<- assign_task(t)

save(t, file= 'data/Trial_time.Rda')
write.csv(t, 'data/Trial_time.csv')

DesTime<- melt(t, id=c('sub', 'item', 'cond', 'task'), 
                measure=c("duration_ms"), na.rm=TRUE)
mTime<- cast(DesTime, task ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))


### Task accuracy:
q<- Question(data_list = 'D:/Data/zString', maxtrial = 180)
q<- assign_task(q)

save(q, file= 'data/Accuracy.Rda')
write.csv(q, 'data/Accuracy.csv')

DesQuest<- melt(q, id=c('sub', 'item', 'cond', 'task'), 
                measure=c("accuracy"), na.rm=TRUE)
mQuest<- cast(DesQuest, task ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))


### Extract fixation data from whole sentence:

raw_fix<- SingleLine(data_list = 'D:/Data/zString', maxtrial = 180, tBlink = 100)
raw_fix<- assign_task(raw_fix)

save(raw_fix, file= 'preproc/raw_fix_temp.Rda')
write.csv(raw_fix, 'preproc/raw_fix_temp.csv')

DesFix<- melt(raw_fix, id=c('sub', 'item', 'cond', 'task'), 
                measure=c("fix_dur"), na.rm=TRUE)
mFix<- cast(DesFix, task ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))


##########################################

sound<- soundCheck(maxtrial = 180, nsounds = 5, ppl = 14, ResX = 1920, soundLatency = 12)
sound<- assign_task(sound)
sound<- subset(sound, sound!=1)

source("functions/re_map.R")
sound<- re_map(sound)

sound$word<- NA
for(i in 1:nrow(sound)){
  if(sound$sound[i]==2){
    sound$word[i]<- 5
  }
  if(sound$sound[i]==3){
    sound$word[i]<- 7
  }
  if(sound$sound[i]==4){
    sound$word[i]<- 9
  }
  if(sound$sound[i]==5){
    sound$word[i]<- 11
  }
}


sound$Trialt<- sound$trialEnd- sound$trialStart

save(sound, file= "preproc/sound_check_t.Rda")
write.csv(sound, "preproc/sound_check_t.csv")
#rm(sound)




DesS<- melt(sound, id=c('sub', 'item', 'cond', 'task', 'sound_type'), 
              measure=c("N1"), na.rm=TRUE)
mS<- cast(DesS, task+sound_type ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

mS2<- cast(DesS, task+sound_type+sub ~ variable
                ,function(x) c(M=signif(mean(x),3)
                               , SD= sd(x) ))
mS2<- subset(mS2, sound_type!= 'SLC')

r<- subset(mS2, task== 'reading')
s<- subset(mS2, task== 'zString')


# reading:
(MDr<- r$N1_M[r$sound_type=="DEV"]- r$N1_M[r$sound_type=="STD"]) 

(MDs<- s$N1_M[s$sound_type=="DEV"]- s$N1_M[r$sound_type=="STD"])


library(lme4)



plot(MDr, MDs, xlab= 'reading ES (in ms)', ylab= 'scanning ES (in ms)', col= 'steelblue', pch= 16, family='serif',
     cex.axis= 1.3, cex.lab= 1.5)

cor(MDr, MDs)

sound$sound_type<- as.factor(sound$sound_type)
sound$sound_type<- factor(sound$sound_type, levels= c("STD", "SLC", "DEV"))
contrasts(sound$sound_type)

sound$task<- as.factor(sound$task)
contrasts(sound$task)<- c(1, -1)
contrasts(sound$task)

library(lme4)

summary(LM<- lmer(log(N1)~ sound_type*task + (1|sub), data = sound))

