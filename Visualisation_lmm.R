
rm(list= ls())


dat <- read.csv2("data/first_fix_data.csv")


# colorblind palletes: # https://venngage.com/blog/color-blind-friendly-palette/
pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F") # "Classic & trustworthy"


dat$sound<- as.factor(dat$sound)
dat$sound<- factor(dat$sound, levels= c("standard", "silence", "novel"))
contrasts(dat$sound)

dat$task<- as.factor(dat$task)
contrasts(dat$task)<- c(1, -1)
contrasts(dat$task)

library(lme4)
library(ggpubr)

summary(LM<- lmer(first_fix_dur~ sound*task + (task+sound|sub) + (1|item), data = dat))

library(effects)
effect('sound:task', LM)


dat$predicted<- predict(LM)

library(reshape)
DesS<- melt(dat, id=c('sub', 'item', 'cond', 'task', 'sound'), 
#            measure=c("first_fix_dur"), na.rm=TRUE)
            measure=c("predicted"), na.rm=TRUE)
mS<- cast(DesS, task+sound+sub ~ variable
          ,function(x) c(M=signif(mean(x),3)
                         , SD= sd(x) ))

library(ggplot2)

fun_mean <- function(x){
  return(data.frame(y=mean(x),label= paste("M= ", round(mean(x,na.rm=T)), sep= '')))}


mS$sound<- as.factor(mS$sound)
mS$sound<- factor(mS$sound, levels= c("silence", "standard", "novel"))


###########################
# Plot just the effect sizes:


reading<- subset(mS, task== "reading")
scanning<- subset(mS, task== "scanning")

ctrlR<- reading$predicted_M[which(reading$sound== "standard")] - reading$predicted_M[which(reading$sound== "silence")]
ctrlS<- scanning$predicted_M[which(scanning$sound== "standard")] - scanning$predicted_M[which(scanning$sound== "silence")]

#ctrlR<- reading$first_fix_dur_M[which(reading$sound== "standard")] - reading$first_fix_dur_M[which(reading$sound== "silence")]
#ctrlS<- scanning$first_fix_dur_M[which(scanning$sound== "standard")] - scanning$first_fix_dur_M[which(scanning$sound== "silence")]



s1<- data.frame("ES"= ctrlR, "task"= "Reading", "contrast"= "Standard - Silence\n (methodological control)")
s2<- data.frame("ES"= ctrlS, "task"= "Scanning", "contrast"= "Standard - Silence\n (methodological control)")

s<- rbind(s1, s2)


novR<- reading$predicted_M[which(reading$sound== "novel")] - reading$predicted_M[which(reading$sound== "standard")]
novS<- scanning$predicted_M[which(scanning$sound== "novel")] - scanning$predicted_M[which(scanning$sound== "standard")]

#novR<- reading$first_fix_dur_M[which(reading$sound== "novel")] - reading$first_fix_dur_M[which(reading$sound== "standard")]
#novS<- scanning$first_fix_dur_M[which(scanning$sound== "novel")] - scanning$first_fix_dur_M[which(scanning$sound== "standard")]


n1<- data.frame("ES"= novR, "task"= "Reading",  "contrast"= "Novel- Standard\n (novelty distraction effect)")
n2<- data.frame("ES"= novS, "task"= "Scanning", "contrast"= "Novel- Standard\n (novelty distraction effect)")

s<- rbind(s, n1, n2)


fun_mean <- function(x){
  return(data.frame(y=mean(x),label= paste("M= ", round(mean(x,na.rm=T),1), sep= '')))}

MPlot <-ggplot(s, aes(x = task, y = ES, color= task, fill= task)) + 
  # ggdist::stat_halfeye(
  #   adjust = .5, 
  #   width = .6, 
  #   .width = 0, 
  #   justification = -.3, 
  #   point_colour = NA) + 
  geom_boxplot(
    width = .7,
    outlier.shape = NA, fill= NA, 
  ) +
  geom_point(
    size = 3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .25
    )
  ) + facet_grid(. ~ contrast)+
 # coord_cartesian(xlim = c(1.2, NA), clip = "off")+
  scale_color_manual(values=pallete1[1:3])+
  scale_fill_manual(values=pallete1[1:3])+
  ylim(-100, 100)+ 
  theme_classic(26) +ylab("First fixation duration effect size (in ms)")+ xlab("Task")+
  theme(legend.position = 'none',  strip.background = element_rect(colour=NA, fill=NA))+
  stat_summary(fun = mean, geom="point",colour=pallete1[5], size=6) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1, colour=pallete1[5], size= 7)+
  geom_hline(yintercept=0, linetype="dashed", color = pallete1[5], size=1.2)+
  ggtitle("b)")

MPlot


ggsave(plot = MPlot, filename = "Plots/Effect_size_lmm.pdf", height = 8, width =8)


fun_mean <- function(x){return(data.frame(y=mean(x),label= paste("M= ", round(mean(x,na.rm=T),0), sep= '')))}


mS$task<- as.factor(mS$task)
levels(mS$task)<- c('Reading', 'Scanning')

p2 <- ggplot(data = mS, aes(x = sound, y = predicted_M, fill = sound, color= sound)) +
  geom_boxplot(
    width = .7,
    outlier.shape = NA, fill= NA, 
  ) +
  geom_point(size = 3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .25
    ))+
  stat_summary(fun = mean, geom="point",colour=pallete1[5], size=6) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1, colour=pallete1[5], size= 7)+
#  geom_dotplot(binaxis = 'y', stackdir = 'center', binwidth = 5, dotsize = .65, alpha = .3) +
#  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
#                geom="pointrange", color=pallete1[5], size= 1.4) +
#  stat_summary(fun.data = fun_mean, geom="text", vjust=-8, colour=pallete1[5], position = position_dodge(width = 1), size= 6)+
  ylim(c(150, 370))+
  scale_color_manual(values=pallete1[c(3,2,1)])+
  scale_fill_manual(values=pallete1[c(3,2,1)])+
  guides(fill= "none") +
  theme_classic(26) +
  theme(legend.position = 'none',  strip.background = element_rect(colour=NA, fill=NA))+
  ylab("First fixation duration (in ms)")+
  xlab("Sound")+
  ggtitle("a)")+
  facet_grid(. ~ task); p2
  #theme_cowplot()+
  #;p2


figure2 <- ggarrange(p2, MPlot, ncol = 1, nrow = 2)

ggsave(filename = 'Plots/local_lmm.pdf', plot = figure2, width = 10, height = 16)


###################################################################################################
#                                         GLOBAL MEASURES
###################################################################################################

library(lme4)
library(reshape)
library(ggplot2)
library(ggdist)
library(ggpubr)
library(readr)

fun_mean <- function(x, rounding= 0){
  return(data.frame(y=mean(x),label= paste("M= ", round(mean(x,na.rm=T)), sep= '')))}

################
#  Trial time:

t <- read.csv("D:/R/Zstring/data/Trial_time.csv")
t$sound<- as.factor(t$sound)
t$sound<- factor(t$sound, levels= c('standard', 'silence', 'novel'))
contrasts(t$sound)

# t$task<- as.factor(t$task)
# contrasts(t$task)<- c(1, -1)
# contrasts(t$task)
# 
# summary(LM1<- lmer(duration_ms~ sound*task +(task|sub)+(task|item), data= t))
# t$fitted<- fitted(LM1)


DesTime<- melt(t, id=c('sub', 'item', 'sound', 'task'), 
               measure=c("duration_ms"), na.rm=TRUE)
mTime<- cast(DesTime, task+sub ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))
df1<- data.frame('Mean'= mTime$duration_ms_M, 'task'= mTime$task,
                 'sub'= mTime$sub) 

TrialTime <-ggplot(df1, aes(x = task, y = Mean, color= task, fill= task)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA, fill= NA
  ) +
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+
  scale_color_manual(values=pallete1[1:3])+
  scale_fill_manual(values=pallete1[1:3])+
  theme_classic(22) +ylab("Trial time (in ms)")+
  theme(legend.position = 'none')+
  stat_summary(fun = mean, geom="point",colour="black", size=3, ) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7, hjust= 0.8, colour="black", size= 5)

TrialTime


############################
#  All fixation durations: #
############################

raw_fix <- read.csv("data/raw_fixations.csv")

DesFix<- melt(raw_fix, id=c('sub', 'item', 'sound', 'task'), 
              measure=c("fix_dur"), na.rm=TRUE)
mFix<- cast(DesFix, task+sub ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

df2<- data.frame('Mean'= mFix$fix_dur_M, 'task'= mFix$task,
                 'sub'= mFix$sub) 

FixDur <-ggplot(df2, aes(x = task, y = Mean, color= task, fill= task)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA, fill= NA
  ) +
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+
  scale_color_manual(values=pallete1[1:3])+
  scale_fill_manual(values=pallete1[1:3])+
  theme_classic(22) +ylab("Duration of all fixations (in ms)")+
  theme(legend.position = 'none',
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank())+
  xlab('')+
  stat_summary(fun = mean, geom="point",colour="black", size=3, ) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7, hjust= 0.8, colour="black", size= 5)

FixDur


#######################
# Number of fixations #
#######################
library(reshape2)
#nFix <- read.csv("data/number_fixations.csv")
word_measures <- read_csv("data/word_measures.csv")

DesFixNum<- melt(word_measures, id=c('sub', 'item', 'sound', 'task'), 
                 measure=c("nfix1", "nfix2", "nfixAll"), na.rm=TRUE)
mFixNum<- cast(DesFixNum, task+sub ~ variable
               ,function(x) c(M=signif(mean(x),3)
                              , SD= sd(x) ))


df3<- data.frame('Mean'= mFixNum$nfixAll_M, 'task'= mFixNum$task,
                 'sub'= mFixNum$sub) 

fun_mean2 <- function(x){
  return(data.frame(y=mean(x),label= paste("M= ", round(mean(x,na.rm=T), 2), sep= '')))}

NumFix <-ggplot(df3, aes(x = task, y = Mean, color= task, fill= task)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA, fill= NA
  ) +
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+
  scale_color_manual(values=pallete1[1:3])+
  scale_fill_manual(values=pallete1[1:3])+
  theme_classic(22) +ylab("Fixations per word")+
  theme(legend.position = 'none',
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank())+
  xlab('')+
  stat_summary(fun = mean, geom="point",colour="black", size=3, ) +
  stat_summary(fun.data = fun_mean2, geom="text", vjust=-0.7, hjust= 0.8, colour="black", size= 5)

NumFix


#######################
#   Saccade length    #
#######################

DesSL<- melt(raw_fix, id=c('sub', 'item', 'sound', 'task'), 
             measure=c("sacc_len"), na.rm=TRUE)
mSL<- cast(DesSL, task+sub ~ variable
           ,function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))



df4<- data.frame('Mean'= mSL$sacc_len_M, 'task'= mSL$task,
                 'sub'= mSL$sub) 

fun_mean2 <- function(x){
  return(data.frame(y=mean(x),label= paste("M= ", round(mean(x,na.rm=T), 1), sep= '')))}

SaccLen <-ggplot(df4, aes(x = task, y = Mean, color= task, fill= task)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA, fill= NA
  ) +
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+
  scale_color_manual(values=pallete1[1:3])+
  scale_fill_manual(values=pallete1[1:3])+
  theme_classic(22) +ylab("Saccade length (in letters)")+
  theme(legend.position = 'none',
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank())+
  xlab('')+
  stat_summary(fun = mean, geom="point",colour="black", size=3, ) +
  stat_summary(fun.data = fun_mean2, geom="text", vjust=-0.7, hjust= 0.8, colour="black", size= 5)

SaccLen


##########################
#   Skipping probability #
##########################

DesW<- melt(word_measures, id=c('sub', 'item', 'sound', 'task'), 
             measure=c("skip_1st"), na.rm=TRUE)
mW<- cast(DesW, task+sub ~ variable
           ,function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))


df5<- data.frame('Mean'= mW$skip_1st_M, 'task'= mW$task,
                 'sub'= mW$sub) 

fun_mean3 <- function(x){
  return(data.frame(y=mean(x),label= paste("M= ", round(mean(x,na.rm=T), 2), sep= '')))}

Skip <-ggplot(df5, aes(x = task, y = Mean, color= task, fill= task)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA, fill= NA
  ) +
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+
  scale_color_manual(values=pallete1[1:3])+
  scale_fill_manual(values=pallete1[1:3])+
  theme_classic(22) +ylab("Skipping probability (1st-pass)")+
  theme(legend.position = 'none')+
  stat_summary(fun = mean, geom="point",colour="black", size=3, ) +
  stat_summary(fun.data = fun_mean3, geom="text", vjust=-0.7, hjust= 0.8, colour="black", size= 5)

Skip




##########################
#   Regression probability #
##########################

DesR<- melt(raw_fix, id=c('sub', 'item', 'sound', 'task'), 
            measure=c("regress"), na.rm=TRUE)
mR<- cast(DesR, task+sub ~ variable
          ,function(x) c(M=signif(mean(x),3)
                         , SD= sd(x) ))


df6<- data.frame('Mean'= mR$regress_M, 'task'= mR$task,
                 'sub'= mR$sub) 

fun_mean3 <- function(x){
  return(data.frame(y=mean(x),label= paste("M= ", round(mean(x,na.rm=T), 2), sep= '')))}

Regress <-ggplot(df6, aes(x = task, y = Mean, color= task, fill= task)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA, fill= NA
  ) +
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+
  scale_color_manual(values=pallete1[1:3])+
  scale_fill_manual(values=pallete1[1:3])+
  theme_classic(22) +ylab("Regression probability")+
  theme(legend.position = 'none')+
  stat_summary(fun = mean, geom="point",colour="black", size=3) +
  stat_summary(fun.data = fun_mean3, geom="text", vjust=-0.7, hjust= 0.8, colour="black", size= 5)

Regress


##############################
#   Initial landing position #
##############################

DesILP<- melt(word_measures, id=c('sub', 'item', 'sound', 'task'), 
            measure=c("ILP"), na.rm=TRUE)
mILP<- cast(DesILP, task+sub ~ variable
          ,function(x) c(M=signif(mean(x),3)
                         , SD= sd(x) ))


df7<- data.frame('Mean'= mILP$ILP_M, 'task'= mILP$task,
                 'sub'= mILP$sub) 

fun_mean3 <- function(x){
  return(data.frame(y=mean(x),label= paste("M= ", round(mean(x,na.rm=T), 2), sep= '')))}

ILP <-ggplot(df7, aes(x = task, y = Mean, color= task, fill= task)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA, fill= NA
  ) +
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+
  scale_color_manual(values=pallete1[1:3])+
  scale_fill_manual(values=pallete1[1:3])+
  theme_classic(22) +ylab("Initial landing position (in letters)")+
  theme(legend.position = 'none')+
  stat_summary(fun = mean, geom="point",colour="black", size=3, ) +
  stat_summary(fun.data = fun_mean3, geom="text", vjust=-0.7, hjust= 0.8, colour="black", size= 5)

ILP


########################
# Merge plots together #
########################

figure1 <- ggarrange(FixDur, NumFix, SaccLen, ILP, Skip, Regress, ncol = 3, nrow = 2)

ggsave(filename = 'Plots/Task_global.pdf', plot = figure1, width = 15, height = 10)





######################################################################################
#                               Word length curves                                   #
######################################################################################

raw_fix <- read.csv("data/raw_fixations.csv")
raw_fix$word_length <- nchar(raw_fix$wordID)


##### Fixation durations:

DesFix<- melt(subset(raw_fix, !is.na(word_length)), id=c('sub', 'item', 'sound', 'task', 'word_length'), 
              measure=c("fix_dur"), na.rm=TRUE)
mFix<- cast(DesFix, task+word_length ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

df2<- data.frame('Mean'= mFix$fix_dur_M, 'Task'= mFix$task,
                 'word_length'= mFix$word_length, "SD"= mFix$fix_dur_SD) 
df2$SE<- df2$SD/sqrt(length(unique(raw_fix$sub)))




C1<- ggplot(df2, aes(x=word_length, y= Mean, ymin= Mean- SE, ymax= Mean+SE,
                     group=Task, fill= Task, shape= Task, color= Task)) + 
  geom_line(aes(linetype=Task), size= 1.25)+
#  xlim(0, 15)+ # geom_errorbar() + 
  geom_ribbon(alpha= 0.05, colour= NA)+
  theme_classic(22)+
  theme(legend.position = c(0.15, 0.9),
        legend.key.width = unit(2,"cm"))+
  scale_x_continuous(breaks = seq(1, 14, by = 1))+
  scale_color_manual(values=pallete1[1:2])+
  scale_fill_manual(values=pallete1[1:2])+
  ylab("Duration of all fixations (in ms)")+
  xlab("Word length (in letters)")
C1


##### Fixations per word:
word_measures <- read_csv("data/word_measures.csv")
word_measures$word_length <- nchar(word_measures$wordID)


DesFixNum<- melt(subset(word_measures, !is.na(word_length)), id=c('sub', 'item', 'sound', 'task', 'word_length'), 
              measure=c("nfixAll"), na.rm=TRUE)
mFixNum<- cast(DesFixNum, task+word_length ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

df3<- data.frame('Mean'= mFixNum$nfixAll_M, 'Task'= mFixNum$task,
                 'word_length'= mFixNum$word_length, "SD"= mFixNum$nfixAll_SD) 
df3$SE<- df3$SD/sqrt(length(unique(raw_fix$sub)))




C2<- ggplot(df3, aes(x=word_length, y= Mean, ymin= Mean- SE, ymax= Mean+SE,
                     group=Task, fill= Task, shape= Task, color= Task)) + 
  geom_line(aes(linetype=Task), size= 1.25)+
  #  xlim(0, 15)+ # geom_errorbar() + 
  geom_ribbon(alpha= 0.05, colour= NA)+
  theme_classic(22)+
  theme(legend.position = c(0.15, 0.9),
        legend.key.width = unit(2,"cm"))+
  scale_x_continuous(breaks = seq(1, 14, by = 1))+
  scale_color_manual(values=pallete1[1:2])+
  scale_fill_manual(values=pallete1[1:2])+
  ylab("Number of fixations (per word)")+
  xlab("Word length (in letters)")
C2



##### Saccade length:

# DesSaccLen<- melt(subset(raw_fix, !is.na(word_length)), id=c('sub', 'item', 'sound', 'task', 'word_length'),
#                  measure=c("sacc_len"), na.rm=TRUE)
# mSaccLen<- cast(DesSaccLen, task+word_length ~ variable
#                ,function(x) c(M=signif(mean(x),3)
#                               , SD= sd(x) ))
# 
# df4<- data.frame('Mean'= mSaccLen$sacc_len_M, 'Task'= mSaccLen$task,
#                  'word_length'= mSaccLen$word_length, "SD"= mSaccLen$sacc_len_SD)
# df4$SE<- df4$SD/sqrt(length(unique(raw_fix$sub)))
# 
# 
# 
# 
# C3<- ggplot(df4, aes(x=word_length, y= Mean, ymin= Mean- SE, ymax= Mean+SE,
#                      group=Task, fill= Task, shape= Task, color= Task)) +
#   geom_line(aes(linetype=Task), size= 1.25)+
#   #  xlim(0, 15)+ # geom_errorbar() +
#   geom_ribbon(alpha= 0.05, colour= NA)+
#   theme_classic(22)+
#   theme(legend.position = c(0.15, 0.9),
#         legend.key.width = unit(2,"cm"))+
#   scale_x_continuous(breaks = seq(1, 14, by = 1))+
#   scale_color_manual(values=pallete1[1:2])+
#   scale_fill_manual(values=pallete1[1:2])+
#   ylab("Saccade length (in letters)")+
#   xlab("Word length (in letters)")
# C3


##### Initial landing position:
DesILP<- melt(subset(word_measures, !is.na(word_length)), id=c('sub', 'item', 'sound', 'task', 'word_length'),
                  measure=c("ILP"), na.rm=TRUE)
mILP<- cast(DesILP, task+word_length ~ variable
                ,function(x) c(M=signif(mean(x),3)
                               , SD= sd(x) ))

df4<- data.frame('Mean'= mILP$ILP_M, 'Task'= mILP$task,
                 'word_length'= mILP$word_length, "SD"= mILP$ILP_SD)
df4$SE<- df4$SD/sqrt(length(unique(raw_fix$sub)))




C3<- ggplot(df4, aes(x=word_length, y= Mean, ymin= Mean- SE, ymax= Mean+SE,
                     group=Task, fill= Task, shape= Task, color= Task)) +
  geom_line(aes(linetype=Task), size= 1.25)+
  #  xlim(0, 15)+ # geom_errorbar() +
  geom_ribbon(alpha= 0.05, colour= NA)+
  theme_classic(22)+
  theme(legend.position = c(0.15, 0.9),
        legend.key.width = unit(2,"cm"))+
  scale_x_continuous(breaks = seq(1, 14, by = 1))+
  scale_color_manual(values=pallete1[1:2])+
  scale_fill_manual(values=pallete1[1:2])+
  ylab("Initial landing position (in letters)")+
  xlab("Word length (in letters)")
C3



##### Skipping probability:
DesSkip<- melt(subset(word_measures, !is.na(word_length)), id=c('sub', 'item', 'sound', 'task', 'word_length'),
              measure=c("skip_1st"), na.rm=TRUE)
mSkip<- cast(DesSkip, task+word_length ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

df5<- data.frame('Mean'= mSkip$skip_1st_M, 'Task'= mSkip$task,
                 'word_length'= mSkip$word_length, "SD"= mSkip$skip_1st_SD)
df5$SE<- df5$SD/sqrt(length(unique(raw_fix$sub)))


C4<- ggplot(df5, aes(x=word_length, y= Mean, ymin= Mean- SE, ymax= Mean+SE,
                     group=Task, fill= Task, shape= Task, color= Task)) +
  geom_line(aes(linetype=Task), size= 1.25)+
  #  xlim(0, 15)+ # geom_errorbar() +
  geom_ribbon(alpha= 0.05, colour= NA)+
  theme_classic(22)+
  theme(legend.position = c(0.15, 0.9),
        legend.key.width = unit(2,"cm"))+
  scale_x_continuous(breaks = seq(1, 14, by = 1))+
  scale_color_manual(values=pallete1[1:2])+
  scale_fill_manual(values=pallete1[1:2])+
  ylab("Skipping probability (fist-pass)")+
  xlab("Word length (in letters)")
C4







########################
# Merge plots together #
########################

figure2 <- ggarrange(C1, C2, C3, C4, ncol = 2, nrow = 2, common.legend = T)

ggsave(filename = 'Plots/Task_global_curve.pdf', plot = figure2, width = 18, height = 12)


