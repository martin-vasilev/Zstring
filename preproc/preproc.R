
# Martin R. Vasilev, 2019-2021


rm(list= ls())

if('devtools' %in% rownames(installed.packages())==FALSE){
  install.packages('devtools')
  library(devtools)
  install_github('martin-vasilev/EMreading')
}else{
  library(devtools)
  library(EMreading)
}



library(reshape)
source('functions/soundCheck.R')
source('functions/assign_task.R')


### Trial time:
t<- trialTime(data_list = 'D:/Data/zString', maxtrial = 180)
t<- assign_task(t)
t$sound<- ifelse(t$cond==1, "silence", ifelse(t$cond==2, "standard", "novel"))
t$task[which(t$task=="zString")]<- "scanning"
write.csv(t, 'data/Trial_time.csv')


### Task accuracy:
q<- Question(data_list = 'D:/Data/zString', maxtrial = 180)
q<- assign_task(q)
q$dependnum<- NULL
q$sound<- ifelse(q$cond==1, "silence", ifelse(q$cond==2, "standard", "novel"))
q$task[which(q$task=="zString")]<- "scanning"
write.csv(q, 'data/task_accuracy.csv')


### Extract fixation data from whole sentence:

if(!file.exists("preproc/raw_fix_temp.csv")){
  raw_fix<- SingleLine(data_list = 'D:/Data/zString', maxtrial = 180, tBlink = 100)
  raw_fix<- assign_task(raw_fix)
  raw_fix$sound<- ifelse(raw_fix$cond==1, "silence", ifelse(raw_fix$cond==2, "standard", "novel"))
  raw_fix$task[which(raw_fix$task=="zString")]<- "scanning"
  write.csv(raw_fix, 'preproc/raw_fix_temp.csv', row.names = F)

} else{
  raw_fix <- read.csv("D:/R/Zstring/preproc/raw_fix_temp.csv")
}

# merge fixations less than 80 ms within 1 character away from each other:
raw_fix<- cleanData(raw_fix = raw_fix, removeOutsideText = F, removeBlinks = F, combineNearbySmallFix =T,
                    combineMethod = 'char', combineDist = 1, removeSmallFix = F, smallFixCutoff = 80, 
                    removeOutliers = F)

ntotal<- nrow(raw_fix)

# remove other fixations < 80 and >1000 as outliers
outliers<- which(raw_fix$fix_dur<80 | raw_fix$fix_dur>1000)

(length(outliers)/ntotal)*100

raw_fix<- raw_fix[-outliers,]

# remove blinks:
blinks<- which(raw_fix$blink==1 | raw_fix$after_blink==1 | raw_fix$prev_blink==1)
raw_fix<- raw_fix[-blinks,]

(length(blinks)/ntotal)*100

### save processed raw fixation data:
write.csv(raw_fix, "data/raw_fixations.csv", row.names = F)

words<- wordMeasures(raw_fix)
words<- assign_task(words)
words$sound<- ifelse(words$cond==1, "silence", ifelse(words$cond==2, "standard", "novel"))
words$task[which(words$task=="zString")]<- "scanning"
write.csv(words, 'data/word_measures.csv', row.names = F)


# # code initial landing position:
# 
# subs<- unique(words$sub)
# new<- NULL
# 
# for(i in 1:length(subs)){ # for each subject...
#   
#   n<- subset(words, sub== subs[i])
#   
#   items<- sort(unique(n$item))
#   
#   for (j in 1:length(items)){ # for each item
#     m<- subset(n, item== items[j])
#     
#     o<- unique()
#   
#     
#   }
#   
# }
# 





### Number of fixations per trial:
nFix<- num_fix(raw_fix)

#save(nFix, file= 'data/number_fixations.Rda')
write.csv(nFix, 'data/number_fixations.csv')




##########################################

sound<- soundCheck(data_list = "D:/Data/zString", maxtrial = 180, nsounds = 5, ppl = 14, ResX = 1920, soundLatency = 12)
sound<- assign_task(sound)
sound<- subset(sound, sound_pos!=1)

source("functions/re_map.R")
sound<- re_map(sound)

sound$word<- NA
for(i in 1:nrow(sound)){
  if(sound$sound_pos[i]==2){
    sound$word[i]<- 5
  }
  if(sound$sound_pos[i]==3){
    sound$word[i]<- 7
  }
  if(sound$sound_pos[i]==4){
    sound$word[i]<- 9
  }
  if(sound$sound_pos[i]==5){
    sound$word[i]<- 11
  }
}


sound$Trialt<- sound$trialEnd- sound$trialStart

save(sound, file= "preproc/sound_check_t.Rda")
write.csv(sound, "preproc/sound_check_t.csv")
#rm(sound)


##########################
#      filter data:
##########################

load("preproc/sound_check_t.Rda")
nobs<- nrow(sound)

# remove blinks on critical words:
#blinks<- which(sound_check$blink=='Yes')
blinks2<- which(sound$blink2=='Yes')

#nblinks<- length(blinks)
nblinks<- length(blinks2)
#sound_check<- sound_check[-blinks,]
sound<- sound[-blinks2,]

# remove sounds played after fixation has started:
infix<- which(sound$delFix>10 | sound$delFix< -10 &  sound$nextFlag== "EFIX") 
test<- sound[infix,]
infixn<- length(infix)
sound<- sound[-infix,]

nhook<- nrow(sound)
sound<- subset(sound, hook=="No")
nhook<- ((nhook- nrow(sound))/nobs)*100

outliers<- which(sound$first_fix_dur<80 | sound$first_fix_dur>1000)
outTab<- sound[outliers,]
if(nrow(outTab)>0){
  sound<- sound[-outliers,]
}

noutliers<- nrow(outTab)

cat(sprintf("%f percent of data excluded due to blinks", (nblinks/nobs)*100))
cat(sprintf("%f percent of data excluded due to in-fixations", (infixn/nobs)*100))
cat(sprintf("%f percent of data excluded due to hooks", abs(nhook)))
cat(sprintf("%f percent of data excluded as outliers (<80; > 1000ms)",  (noutliers/nobs)*100))
cat(sprintf("%f percent of data remains for analysis", (nrow(sound)/nobs)*100))




#sound_check<- subset(sound_check, delFix<80)

sound$next_sacc<- abs(sound$N1x- sound$N2x)/14

dat<- sound

# remove some columns we don't need during analysis:
dat$blink<- NULL
dat$blink2<- NULL
dat$prevGood<- NULL
dat$inRegion<- NULL
dat$hook<- NULL
dat$keep<- NULL
dat$N1len<- NULL
dat$N2len<- NULL

dat$sound[which(dat$sound=="SLC")]<- 'silence'
dat$sound[which(dat$sound=="STD")]<- 'standard'
dat$sound[which(dat$sound=="DEV")]<- 'novel'

dat$task[which(dat$task=="zString")]<- "scanning"

#save(dat, file= "data/dat.Rda")
write.csv2(dat, file= "data/first_fix_data.csv")

mean(sound$FixSoundOnset)
sd(sound$FixSoundOnset)
