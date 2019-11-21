
rm(list= ls())

library(EMreading)

t<- trialTime(data_list = '', maxtrial = 180)

q<- Question(data_list = '', maxtrial = 180)

raw_fix<- SingleLine(data_list = '', maxtrial = 180, tBlink = 100)
