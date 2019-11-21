
rm(list= ls())

library(EMreading)
library(reshape)
source('functions/soundCheck.R')


t<- trialTime(data_list = 'D:/Data/zString', maxtrial = 180)
t$task<- NA
t$task[which(t$sub%%2==1 & t$item<91 | t$sub%%2==0 & t$item>90)]<- 'reading'
t$task[which(t$sub%%2==1 & t$item>90 | t$sub%%2==0 & t$item<91)]<- 'scanning'

DesTime<- melt(t, id=c('sub', 'item', 'cond', 'task'), 
                measure=c("duration_ms"), na.rm=TRUE)
mTime<- cast(DesTime, task ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))


# accuracy:
q<- Question(data_list = 'D:/Data/zString', maxtrial = 180)
q$task<- NA
q$task[which(q$sub%%2==1 & q$item<91 | q$sub%%2==0 & q$item>90)]<- 'reading'
q$task[which(q$sub%%2==1 & q$item>90 | q$sub%%2==0 & q$item<91)]<- 'scanning'

DesQuest<- melt(q, id=c('sub', 'item', 'cond', 'task'), 
                measure=c("accuracy"), na.rm=TRUE)
mQuest<- cast(DesQuest, task ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))



raw_fix<- SingleLine(data_list = 'D:/Data/zString', maxtrial = 180, tBlink = 100)
raw_fix$task<- NA
raw_fix$task[which(raw_fix$sub%%2==1 & raw_fix$item<91 | raw_fix$sub%%2==0 & raw_fix$item>90)]<- 'reading'
raw_fix$task[which(raw_fix$sub%%2==1 & raw_fix$item>90 | raw_fix$sub%%2==0 & raw_fix$item<91)]<- 'scanning'

DesFix<- melt(raw_fix, id=c('sub', 'item', 'cond', 'task'), 
                measure=c("fix_dur"), na.rm=TRUE)
mFix<- cast(DesFix, task+sub ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))

##########################################

sound<- soundCheck(maxtrial = 180, nsounds = 5, ppl = 14, ResX = 1920, soundLatency = 12)
sound$task<- NA
sound$task[which(sound$sub%%2==1 & sound$item<91 | sound$sub%%2==0 & sound$item>90)]<- 'reading'
sound$task[which(sound$sub%%2==1 & sound$item>90 | sound$sub%%2==0 & sound$item<91)]<- 'scanning'

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
s<- subset(mS2, task== 'scanning')


# reading:
r$N1_M[r$sound_type=="DEV"]- r$N1_M[r$sound_type=="STD"] 

s$N1_M[s$sound_type=="DEV"]- s$N1_M[r$sound_type=="STD"] 
