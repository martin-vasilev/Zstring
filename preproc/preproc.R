
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

# remove other fixations < 80 and >1000 as outliers
outliers<- which(raw_fix$fix_dur<80 | raw_fix$fix_dur>1000)

raw_fix<- raw_fix[-outliers,]

# remove blinks:
blinks<- which(raw_fix$blink==1 | raw_fix$after_blink==1 | raw_fix$prev_blink==1)
raw_fix<- raw_fix[-blinks,]


### save processed raw fixation data:
write.csv(raw_fix, "data/raw_fixations.csv", row.names = F)

words<- wordMeasures(raw_fix)
write.csv(words, 'data/word_measures.csv')


DesFix<- melt(raw_fix, id=c('sub', 'item', 'cond', 'task'), 
                measure=c("fix_dur"), na.rm=TRUE)
mFix<- cast(DesFix, task ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))

### Number of fixations per trial:
nFix<- num_fix(raw_fix)

#save(nFix, file= 'data/number_fixations.Rda')
write.csv(nFix, 'data/number_fixations.csv')

DesFixNum<- melt(nFix, id=c('sub', 'item', 'cond', 'task'), 
              measure=c("Nfix_1st", "Nfix_2nd", "Nfix_all"), na.rm=TRUE)
mFixNum<- cast(DesFixNum, task ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

nFix$cond<- as.factor(nFix$cond)
nFix$cond<- factor(nFix$cond, levels= c("2", "1", "3"))
contrasts(nFix$cond)

nFix$task<- as.factor(nFix$task)
contrasts(nFix$task)<- c(1, -1)
contrasts(nFix$task)

summary(LM3<- glmer(Nfix_all ~cond*task +(1|sub)+(1|item), data= nFix, family = poisson))

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


#save(dat, file= "data/dat.Rda")
write.csv2(dat, file= "data/first_fix_data.csv")



# DesS<- melt(sound, id=c('sub', 'item', 'cond', 'task', 'sound_type'), 
#               measure=c("N1"), na.rm=TRUE)
# mS<- cast(DesS, task+sound_type ~ variable
#             ,function(x) c(M=signif(mean(x),3)
#                            , SD= sd(x) ))
# 
# mS2<- cast(DesS, task+sound_type+sub ~ variable
#                 ,function(x) c(M=signif(mean(x),3)
#                                , SD= sd(x) ))
# mS2<- subset(mS2, sound_type!= 'SLC')
# 
# r<- subset(mS2, task== 'reading')
# s<- subset(mS2, task== 'zString')
# 
# 
# # reading:
# (MDr<- r$N1_M[r$sound_type=="DEV"]- r$N1_M[r$sound_type=="STD"]) 
# 
# (MDs<- s$N1_M[s$sound_type=="DEV"]- s$N1_M[r$sound_type=="STD"])
# 
# mean(MDr)
# mean(MDs)
# 
# library(lme4)
# 
# source('https://raw.githubusercontent.com/martin-vasilev/R_scripts/master/CohensD_raw.R')
# 
# l<- subset(sound, sound_type!="SLC")
# l$sound_type<- droplevels(l$sound_type)
# 
# CohensD_raw(data = l, measure = "N1", group_var = "sound_type", baseline = "STD")
# 
# 
# plot(MDr, MDs, xlab= 'reading ES (in ms)', ylab= 'scanning ES (in ms)', col= 'steelblue', pch= 16, family='serif',
#      cex.axis= 1.3, cex.lab= 1.5)
# 
# cor(MDr, MDs)
# 
# sound$sound_type<- as.factor(sound$sound_type)
# sound$sound_type<- factor(sound$sound_type, levels= c("STD", "SLC", "DEV"))
# contrasts(sound$sound_type)
# 
# sound$task<- as.factor(sound$task)
# contrasts(sound$task)<- c(1, -1)
# contrasts(sound$task)
# 
# library(lme4)
# 
# summary(LM<- lmer(N1~ sound_type*task + (task+sound|sub) + (task+sound|item), data = sound))
# 
# library(effects)
# effect('sound_type:task', LM)
# 
# #### Plot
# 
# colnames(mS)<- c("Task", "Sound", "Mean", "SD")
# mS$SE<- mS$SD/sqrt(length(unique(sound$sub)))
# mS$Task<- as.factor(mS$Task)
# levels(mS$Task)<- c('reading', 'letter scanning')
# mS$Sound<- as.factor(mS$Sound)
# mS$Sound<- factor(mS$Sound, levels= c('SLC', 'STD', 'DEV'))
# levels(mS$Sound)<- c("Silence", "Standard", 'Novel')
# 
# library(ggplot2)
# 
# Plot <-ggplot(mS, aes(x= Sound, y= Mean, group= Task, fill=Task, colour= Task, shape= Task,
#                         ymin= Mean- SE, ymax= Mean+SE)) +
#   theme_classic(18) +geom_point(size=4.5)+ geom_line(size=2)+ 
#   scale_colour_brewer(palette="Accent")+ 
#   #scale_color_manual(values=pallete1[1:2])+
#   #scale_fill_manual(values=pallete1[1:2])+
#   coord_cartesian(clip = 'off')+
#   xlab("Sound")+ ylim(210, 280)+
#   scale_shape_manual(values=c(16, 17))+
#   scale_x_discrete(expand = c(0.1,0.1))+
#   ylab("First fixation duration (ms)")+ geom_errorbar(width=0.1)+
#   #annotate("text", x = -2, y = 290, label = "a)             ")+
#   theme(legend.position= c(0.285,0.87), legend.key.width=unit(1.5,"cm"),
#         panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
#         panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"));Plot#+
#   #facet_grid(. ~ Task); Plot
# ggsave(filename = 'Plots/FFD.png', plot = Plot, width = 7, height = 7)
# 
# 
# 
# 
# ### Descriptives for table:
# 
# tTime<- cast(DesTime, task+cond ~ variable
#              ,function(x) c(M=signif(mean(x),3)
#                             , SD= sd(x) ))
# tFixDur<- cast(DesFix, task+cond ~ variable
#                       ,function(x) c(M=signif(mean(x),3)
#                                      , SD= sd(x) ))
# 
# tNfix<- cast(DesFixNum, task+cond ~ variable
#                        ,function(x) c(M=signif(mean(x),3)
#                                       , SD= sd(x) ))
# 
# DesSacc_len<- melt(raw_fix, id=c('sub', 'item', 'cond', 'task'), 
#                    measure=c("sacc_len"), na.rm=TRUE)
# 
# tSaccLen<- cast(DesSacc_len, task+cond ~ variable
#                 ,function(x) c(M=signif(mean(x),3)
#                                , SD= sd(x) ))
# 
# means<- merge(tTime, tFixDur)
# means<- merge(means, tNfix)
# means<- merge(means, tSaccLen)
# 
# means[,3:14]<- round(means[, 3:14], 2)
# 
# means$cond<- as.factor(means$cond)
# levels(means$cond)<- c("Silence", "Standard", "Novel")
# 
# means$task<- as.factor(means$task)
# levels(means$task)<- c("reading", "scanning")
# 
# means$duration_ms_SD<- round(means$duration_ms_SD)
# means$fix_dur_SD<- round(means$fix_dur_SD)
# 
# time<- paste(means$duration_ms_M, " (", means$duration_ms_SD, ")", sep= '')
# fixDur<- paste(means$fix_dur_M, " (", means$fix_dur_SD, ")", sep= '')
# Nfix1<- paste(means$Nfix_1st_M, " (", means$Nfix_1st_SD, ")", sep= '')
# Nfix2<- paste(means$Nfix_2nd_M, " (", means$Nfix_2nd_SD, ")", sep= '')
# NfixAll<- paste(means$Nfix_all_M, " (", means$Nfix_all_SD, ")", sep= '')
# SaccLen<- paste(means$sacc_len_M, " (", means$sacc_len_SD, ")", sep= '')
# 
# descr<- means[,1:2]
# descr$time<- time
# descr$fixDur<-fixDur 
# descr$Nfix1<-Nfix1 
# descr$Nfix2<- Nfix2
# descr$NfixAll<- NfixAll
# descr$SaccLen<- SaccLen
# 
# write.csv(descr, "descriptives.csv")
# 



# ###### Project data files:
# 
# library(reshape)
# 
# # Question accuracy:
# 
# mQuest1<- cast(DesQuest, task+cond+sub ~ variable
#               ,function(x) c(M=signif(mean(x),3)
#                              , SD= sd(x) ))
# write.csv(mQuest1,  "projects/accuracy_sub.csv")
# 
# 
# mQuest2<- cast(DesQuest, task+cond+item ~ variable
#                ,function(x) c(M=signif(mean(x),3)
#                               , SD= sd(x) ))
# write.csv(mQuest2,  "projects/accuracy_item.csv")
# 
# 
# # Trial time:
# mTime1<- cast(DesTime, task+cond+sub ~ variable
#              ,function(x) c(M=signif(mean(x),3)
#                             , SD= sd(x) ))
# write.csv(mTime1,  "projects/trialTime_sub.csv")
# 
# mTime1<- cast(DesTime, task+cond+item ~ variable
#               ,function(x) c(M=signif(mean(x),3)
#                              , SD= sd(x) ))
# write.csv(mTime1,  "projects/trialTime_item.csv")
# 
# 
# # Number of fixations:
# 
# mFixNum1<- cast(DesFixNum, task+cond+sub ~ variable
#                ,function(x) c(M=signif(mean(x),3)
#                               , SD= sd(x) ))
# write.csv(mFixNum1,  "projects/numberFixations_sub.csv")
# 
# 
# mFixNum2<- cast(DesFixNum, task+cond+item ~ variable
#                 ,function(x) c(M=signif(mean(x),3)
#                                , SD= sd(x) ))
# write.csv(mFixNum2,  "projects/numberFixations_item.csv")
# 
# 
# ### Fixation durations:
# 
# mFix1<- cast(DesFix, task+cond+sub ~ variable
#             ,function(x) c(M=signif(mean(x),3)
#                            , SD= sd(x) ))
# 
# write.csv(mFix1,  "projects/allFixationDurations_sub.csv")
# 
# mFix2<- cast(DesFix, task+cond+item ~ variable
#             ,function(x) c(M=signif(mean(x),3)
#                            , SD= sd(x) ))
# 
# write.csv(mFix2,  "projects/allFixationDurations_item.csv")
# 
# 
# 
# # saccade length:
# 
# DesLen<- melt(raw_fix, id=c('sub', 'item', 'cond', 'task'), 
#                measure=c("sacc_len"), na.rm=TRUE)
# mLen1<- cast(DesLen, task+cond+sub ~ variable
#              ,function(x) c(M=signif(mean(x),3)
#                             , SD= sd(x) ))
# 
# write.csv(mLen1,  "projects/saccadeLength_sub.csv")
# 
# 
# 
# mLen1<- cast(DesLen, task+cond+item ~ variable
#              ,function(x) c(M=signif(mean(x),3)
#                             , SD= sd(x) ))
# 
# write.csv(mLen1,  "projects/saccadeLength_item.csv")
# 
# 
# 
# #### word frequencies:
# 
# dat$wordID<- NA
# 
# raw_fix$wordID<- as.character(raw_fix$wordID)
# 
# for(i in 1:nrow(dat)){
#   
#   word<- which(raw_fix$word== dat$word[i]& raw_fix$item== dat$item[i]& raw_fix$task=='reading')
#   word<- word[1]
#   
#   dat$wordID[i]<- raw_fix$wordID[word]
#   
#   
# }
# 
# 
# library(EMreading)
# 
# dat2<- Frequency(dat)
# 
# 
# 
# DesS<- melt(dat2, id=c('sub', 'item', 'cond', 'task', 'sound_type'), 
#             measure=c("N1"), na.rm=TRUE)
# mS1<- cast(DesS, task+cond+sub ~ variable
#           ,function(x) c(M=signif(mean(x),3)
#                          , SD= sd(x) ))
# write.csv(mS1,  "projects/firstFixationDuratios_sub.csv")
# 
# 
# mS2<- cast(DesS, task+cond+item ~ variable
#            ,function(x) c(M=signif(mean(x),3)
#                           , SD= sd(x) ))
# write.csv(mS2,  "projects/firstFixationDuratios_item.csv")
# 
# 
# ### Frequencies
# 
# dat3<- subset(dat2, task=='reading')
# 
# DesS<- melt(dat3, id=c('sub', 'item', 'cond', 'task', 'sound_type'), 
#             measure=c("N1", "freq", 'zipf'), na.rm=TRUE)
# 
# mS3<- cast(DesS, task+cond+sub ~ variable
#            ,function(x) c(M=signif(mean(x),3)
#                           , SD= sd(x) ))
# colnames(mS3)<- c( "task" ,   "cond",    "sub",     "FFD_M",    "FFD_SD",   "freq_M",  "freq_SD", "zipf_M",
#                    "zipf_SD")
# write.csv(mS3,  "projects/firstFixationDuratios_sub_FREQUENCY.csv")
# 
# 
# mS4<- cast(DesS, task+cond+item ~ variable
#            ,function(x) c(M=signif(mean(x),3)
#                           , SD= sd(x) ))
# colnames(mS4)<- c( "task" ,   "cond",    "sub",     "FFD_M",    "FFD_SD",   "freq_M",  "freq_SD", "zipf_M",
#                    "zipf_SD")
# write.csv(mS4,  "projects/firstFixationDuratios_item_FREQUENCY.csv")







