
# Martin R. Vasilev, 2017

library(readr)

design<- NULL

for(i in 1:72){
  t <- read_delim(paste("design/P", toString(i), ".txt", sep=''), 
                       " ", escape_double = FALSE, trim_ws = TRUE)
  
  t$sub<- i
  design<- rbind(design, t)
}

design<- subset(design, item<181) # remove practice items

table(design$item, design$sound)
table(design$item, design$pos)
table(design$sound, design$pos)
table(design$item, design$task)
table(design$sound, design$task, design$pos)

b<- data.frame(table(design$item, design$pos, design$sound))

a<- data.frame(table(design$item, design$sound, design$task))

c<- data.frame(table(design$item, design$sound, design$pos, design$task))

range(a$Freq)
range(b$Freq)
range(c$Freq)
