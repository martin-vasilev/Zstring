
rm(list= ls())


dat <- read.csv2("data/first_fix_data.csv")


# colorblind palletes: # https://venngage.com/blog/color-blind-friendly-palette/
pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F") # "Classic & trustworthy"


dat$sound<- as.factor(dat$sound)
dat$sound<- factor(dat$sound, levels= c("standard", "silence", "deviant"))
contrasts(dat$sound)

dat$task<- as.factor(dat$task)
contrasts(dat$task)<- c(1, -1)
contrasts(dat$task)

library(lme4)

summary(LM<- lmer(first_fix_dur~ sound*task + (task+sound|sub) + (1|item), data = dat))

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

