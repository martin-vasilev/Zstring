
gen_data<- function(nSub= 24, b_sound= NULL, b_task = NULL, b_inter= NULL){
  
  # load/ install required packages:
  packages= c("simr", "lme4") # list of used packages:
  
  for(i in 1:length(packages)){
    
    if(packages[i] %in% rownames(installed.packages())==FALSE){
      install.packages(packages[i])
      library(packages[i], character.only=TRUE)
    }else{
      library(packages[i], character.only=TRUE)
    }
  }

  
  # Load previous data by Vasilev et al. (2020): 0 ms vs 120 ms delay manipulation:
  dat <- read.csv2("power/dat.csv")
  
  # In this experiment, we have 120ms delay of sounds. So, we subset only trials
  # with 120ms delay in Vasilev et al.2020
  
  dat<- subset(dat, del==120)
  
  # code sound as factor:
  dat$sound_type<- as.factor(dat$sound_type)
  contrasts(dat$sound_type)<- c(1, -1)
  contrasts(dat$sound_type) # effect size is positive
  
  
  # Next, we create a fake "task" factor since there is no existing data
  dat$task<- "reading"
  
  # We can randomly split observations by task, and then manually change
  # the slope values in the power analysis models:
  a= sample_n(dat, nrow(dat)/2)
  dat$task[is.element(dat$X, a$X)]<- "scanning"
  
  table(dat$task)
  
  dat$task<- as.factor(dat$task)
  contrasts(dat$task)<- c(1, -1)
  contrasts(dat$task)
  
  # take only columns we need:
  
  dat<- dat[, c("sub", "item", "sound_type", "task", "N1")] 
  colnames(dat)<- c("sub", "item", "sound", "task", "fix_dur") 
  
  # Let's simulate a dataset:
  
  # We can use simr package to generate a fake dataset and fit model that we later
  # modify
  
  summary(dummyM<- lmer(log(fix_dur) ~ sound*task +(sound|sub)+ (1|item), data= dat))
  
  
  
  # Next we load up some dummy design matrix to create fake experimental data
  design_pwr <- read.delim("power/design_pwr.dat", header=FALSE)
  design_pwr$V1<- NULL
  
  nsub<- nSub # we can take any sub number (up to 120 in thi matrix)
  df<- NULL
  
  
  # before we reshape data frame, so it's in usable form and gives us the number 
  # of desired subjects:
  if(nsub> ncol(design_pwr)-1){
    stop("Not enough subjects in design matrix!")
  }else{
    
    for(i in 1:nsub){
      t<- data.frame("sub"= rep(i, nrow(design_pwr)), "item"= design_pwr[,1], "cond"= design_pwr[,i+1])
      
      df<- rbind(df, t)
    }
    
    df$sound<- ifelse(is.element(df$cond, c(1,4)), "SLC", ifelse(is.element(df$cond, c(2,5)), "STD", "NOV"))
    df$task<- ifelse(df$cond<4, "reading", "scanning")
    table(df$task, df$sound)
    
    # we get rid of silence for now, as it is not important for the power analysis:
    df<- subset(df, sound!= "SLC")
    
    # add same contrast coding 
    df$sound<- as.factor(df$sound)
    contrasts(df$sound)<- c(1, -1)
    contrasts(df$sound)
    
    df$task<- as.factor(df$task)
    contrasts(df$task)<- c(1, -1)
    contrasts(df$task)
    
  }
  
  b <- coef(summary(dummyM))[,1] # fixed intercept and slope
  RE <- VarCorr(dummyM) # random effects
  s <-  0.29481    # residual sd
  
  # make fake LMM model based on slope and var covar structure:
  model1 <- makeLmer(fix_dur ~ sound*task + (sound|sub) + (1|item), fixef=b, VarCorr= RE, sigma=s, data=df)
  summary(model1)
  
  # Extract sumulated data from the simr model using desired var-covar structure
  sim_dat<- getData(model1)
  
  return(sim_dat)
    
}