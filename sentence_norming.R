
rm(list= ls())

norm <- read.csv("D:/R/Zstring/preproc/norming_data_new_sentences.csv")
norm<- norm[-1,-c(1:19)]

norm2<- norm[, -c(1:2)]
norm2<- norm2[, -121]


cols<- colnames(norm2)

ratings<- NULL
get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}

for(i in 1:length(cols)){
  
  if(grepl("Q", cols[i], fixed = TRUE)){
    Measure= "Difficulty"
  }else{
    Measure= "Naturalness"
  }
  
  item<- as.numeric(get_num(unlist(strsplit(cols[i], "_"))[1]))
  
  t<- data.frame("sub"= 1:nrow(norm2), "item"= item,
                 "measure"= rep(Measure, nrow(norm2)),
                 "rating"= norm2[,i])
  
  ratings<- rbind(ratings, t)
  
}


# descriptives:
library(reshape)
Des<- melt(ratings, id=c('sub', 'item', 'measure'), 
              measure=c("rating"), na.rm=TRUE)
cast(Des, measure ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

