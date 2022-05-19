
rm(list= ls())

# colorblind palletes: # https://venngage.com/blog/color-blind-friendly-palette/
pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F") # "Classic & trustworthy"

# load/ install required packages:
packages= c("reshape", "ggplot2", "grid", "emmeans", "parallel", "boot", "simr", "dplyr", "lme4", "job") # list of used packages:

for(i in 1:length(packages)){
  
  if(packages[i] %in% rownames(installed.packages())==FALSE){
    install.packages(packages[i])
    library(packages[i], character.only=TRUE)
  }else{
    library(packages[i], character.only=TRUE)
  }
}

source("power/gen_data.R")
source('functions/CohensD_raw.R')

options(scipen = 999)

# Calculate Cohen's d
library(readr)
v21 <- read_delim("power/dat.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

CohensD_raw(data = v21, measure = "N1", baseline = 'STD', group_var = "sound_type", avg_var = "sub")



# load data for sumulation:
dat<- gen_data(nSub = 100)

contrasts(dat$task)
contrasts(dat$sound)


summary(LM1<- lmer(fix_dur ~ sound*task + (sound|sub)+ (1|item), data = dat, REML=T))

# reduce effect size to 50% to similate interaction effect that is half as strong
LM1@beta[names(fixef(LM1)) == "sound1"] <- LM1@beta[names(fixef(LM1)) == "sound1"]/2
coef(summary(LM1))


pc1<- powerCurve(LM1, nsim=1000, test = fixed(xname = 'sound1', method = "z"),
                 along = 'sub', breaks= seq(5, 100, 5), seed = 12345)
save(pc1, file= 'power/power_analysis.Rda')
plot(pc1, xlab= "Number of subjects")


result= summary(pc1)


PPlot<- ggplot(result, aes(x=nlevels, y= mean, ymin= lower, ymax= upper))+ 
  ylim(0, 1)+
  scale_y_continuous(breaks= seq(0.2, 1, 0.2))+
  geom_errorbar(color= '#E69F00') +
  geom_line(size=1, color= '#E69F00') +
  geom_point(color= '#E69F00')+
  geom_hline(yintercept=0.8, linetype="dashed", 
             color = "black", size=0.75)+
  labs(title="",x="Number of participants", y = "Power")+
  theme_classic(18)+ theme(legend.position = 'none')

ggsave(filename = 'power/power_plot.pdf', plot = PPlot, width= 7, height= 5)
