
rm(list= ls())


dat <- read.csv2("D:/R/Zstring/data/dat.csv")


# colorblind palletes: # https://venngage.com/blog/color-blind-friendly-palette/
pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F") # "Classic & trustworthy"


dat$sound_type<- as.factor(dat$sound_type)
dat$sound_type<- factor(dat$sound_type, levels= c("STD", "SLC", "DEV"))
contrasts(dat$sound_type)

dat$task<- as.factor(dat$task)
contrasts(dat$task)<- c(1, -1)
contrasts(dat$task)

library(lme4)

summary(LM<- lmer(N1~ sound_type*task + (task|sub) + (task|item), data = dat))

library(effects)
effect('sound_type:task', LM)


dat$predicted<- predict(LM)

library(reshape)
DesS<- melt(dat, id=c('sub', 'item', 'cond', 'task', 'sound_type'), 
            measure=c("predicted"), na.rm=TRUE)
mS<- cast(DesS, task+sound_type+sub ~ variable
          ,function(x) c(M=signif(mean(x),3)
                         , SD= sd(x) ))

library(ggplot2)

fun_mean <- function(x){
  return(data.frame(y=mean(x),label= paste("M= ", round(mean(x,na.rm=T)), sep= '')))}

a <- ggplot(mS, aes(x = sound_type, y= predicted_M, fill= task, color= task))+
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
  theme(legend.position = 'top')+
  stat_summary(fun = mean, geom="point",colour="black", size=3, position = position_dodge(0.7), show.legend = F) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7, colour="black", position = position_dodge(0.7))

ggsave(plot = a, filename = "Plots/FFD.pdf", height = 7, width = 10)



