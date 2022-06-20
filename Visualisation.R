
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

summary(LM<- lmer(first_fix_dur~ sound*task + (task+sound|sub) + (task|item), data = dat))

library(effects)
effect('sound:task', LM)


dat$predicted<- predict(LM)

library(reshape)
DesS<- melt(dat, id=c('sub', 'item', 'cond', 'task', 'sound'), 
            measure=c("predicted"), na.rm=TRUE)
mS<- cast(DesS, task+sound+sub ~ variable
          ,function(x) c(M=signif(mean(x),3)
                         , SD= sd(x) ))

library(ggplot2)

fun_mean <- function(x){
  return(data.frame(y=mean(x),label= paste("M= ", round(mean(x,na.rm=T)), sep= '')))}


mS$sound<- as.factor(mS$sound)
mS$sound<- factor(mS$sound, levels= c("SLC", "STD", "DEV"))

a <- ggplot(mS, aes(x = sound, y= predicted_M, fill= task, color= task))+
  geom_boxplot(
    width = .25, 
    outlier.shape = NA, fill= NA, position = position_dodge(width = 0.7)
  ) +
  geom_point(aes(fill=task),
             size = 1.3,
             alpha = .6,
             position =  position_jitterdodge(jitter.width = 0.1, dodge.width = 0.7)
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+
  scale_color_manual(values=pallete1[1:2])+
  scale_fill_manual(values=pallete1[1:2])+
  theme_classic(20) +ylab("First fixation duration (in ms)")+
  xlab("Sound condition")+
  theme(legend.position = 'top')+
  stat_summary(fun = mean, geom="point",colour="black", size=3, position = position_dodge(0.7), show.legend = F) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7, colour="black", position = position_dodge(0.7))

ggsave(plot = a, filename = "Plots/FFD.pdf", height = 7, width = 10)



###########################
# Plot just the effect sizes:


reading<- subset(mS, task== "reading")
scanning<- subset(mS, task== "zString")

ctrlR<- reading$predicted_M[which(reading$sound== "STD")] - reading$predicted_M[which(reading$sound== "SLC")]
ctrlS<- scanning$predicted_M[which(scanning$sound== "STD")] - scanning$predicted_M[which(scanning$sound== "SLC")]


s1<- data.frame("ES"= ctrlR, "task"= "Reading", "contrast"= "Standard - Silence\n (methodological control)")
s2<- data.frame("ES"= ctrlS, "task"= "Scanning", "contrast"= "Standard - Silence\n (methodological control)")

s<- rbind(s1, s2)


novR<- reading$predicted_M[which(reading$sound== "DEV")] - reading$predicted_M[which(reading$sound== "STD")]

novS<- scanning$predicted_M[which(scanning$sound== "DEV")] - scanning$predicted_M[which(scanning$sound== "STD")]


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
    width = .75,
    outlier.shape = NA, fill= NA, 
  ) +
  geom_point(
    size = 1.5,
    alpha = .4,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + facet_grid(. ~ contrast)+
 # coord_cartesian(xlim = c(1.2, NA), clip = "off")+
  scale_color_manual(values=pallete1[1:3])+
  scale_fill_manual(values=pallete1[1:3])+
  theme_classic(22) +ylab("Effect size in first fixatation duration (in ms)")+ xlab("Task")+
  theme(legend.position = 'none',  strip.background = element_rect(colour=NA, fill=NA))+
  stat_summary(fun = mean, geom="point",colour="black", size=3) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7, colour="black", size= 5)+
  geom_hline(yintercept=0, linetype="dashed", color = pallete1[5], size=1.2)

MPlot


ggsave(plot = MPlot, filename = "Plots/Effect_size.pdf", height = 10, width =10)






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

raw_fix <- read.csv("D:/R/Zstring/data/raw_fixations.csv")

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
  stat_summary(fun = mean, geom="point",colour="black", size=3, ) +
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

