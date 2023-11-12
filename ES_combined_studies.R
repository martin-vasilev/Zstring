
rm(list= ls())

library(tidyverse)


# Vasilev et al. (2023)

dat <- read.csv2("data/first_fix_data.csv")
#dat<- subset(dat, onTarget== "Yes" & sub!=21)

# colorblind palletes: # https://venngage.com/blog/color-blind-friendly-palette/
pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F") # "Classic & trustworthy"


dat<- subset(dat, task== "reading" & sound!= "silence")

e3<- dat %>% group_by(sub, sound) %>% summarise(Mean= mean(first_fix_dur))
e3$Experiment= "Vasilev et al. (2023)"


e3$sound[which(e3$sound== "standard")] <- "expected"
e3$sound[which(e3$sound== "novel")] <- "unexpected"


## Vasilev et al. (2021)

dat2 <- read_delim("power/dat.csv", delim = ";", 
                        escape_double = FALSE, trim_ws = TRUE)

e2<- dat2 %>% group_by(sub, sound_type) %>% summarise(Mean= mean(N1))
e2$Experiment= "Vasilev et al. (2021)"

e2$sound_type[which(e2$sound_type== "STD")] <- "expected"
e2$sound_type[which(e2$sound_type== "DEV")] <- "unexpected"

colnames(e2)<- c("sub", "sound", "Mean", "Experiment")



## Vasilev et al. (2019)

load("D:/R/DEVS/data/FD.Rda")

FD<- subset(FD, sound!= "SLC")

e1<- FD %>% group_by(sub, sound) %>% summarise(Mean= mean(FFD, na.rm=T))
e1$Experiment= "Vasilev et al. (2019)"

e1$sound[which(e1$sound== "STD")] <- "expected"
e1$sound[which(e1$sound== "DEV")] <- "unexpected"

combined<- rbind(e1, e2, e3)

# combined<- combined %>% pivot_longer(cols= sound, names_to = "Sound type", 
#                                       values_to =  "Fix_dur" )

fun_mean <- function(x){
  return(data.frame(y=mean(x),label= paste("M= ", round(mean(x,na.rm=T),0), sep= '')))}

MPlot <-ggplot(combined, aes(x = sound, y = Mean, color= sound, fill= sound)) + 
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
  ) + facet_grid(. ~ Experiment)+
  # coord_cartesian(xlim = c(1.2, NA), clip = "off")+
  scale_color_manual(values=pallete1[1:3])+
  scale_fill_manual(values=pallete1[1:3])+
  theme_classic(26) +ylab("First fixation duration (in ms)")+ xlab("Sound type")+
  theme(legend.position = 'none',  strip.background = element_rect(colour=NA, fill=NA))+
  stat_summary(fun = mean, geom="point",colour=pallete1[5], size=6) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1, colour=pallete1[5], size= 7)

  
  
  
  
  
  ylim(-100, 100)+ 
  theme_classic(26) +ylab("First fixation duration effect size (in ms)")+ xlab("Task")+
  theme(legend.position = 'none',  strip.background = element_rect(colour=NA, fill=NA))+
  stat_summary(fun = mean, geom="point",colour=pallete1[5], size=6) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1, colour=pallete1[5], size= 7)+
  geom_hline(yintercept=0, linetype="dashed", color = pallete1[5], size=1.2)+
  ggtitle("b)")



