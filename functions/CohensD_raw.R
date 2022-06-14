#' Calculates Cohen's d from raw data by taking in data frame and grouping variable.
#' Currently works only for within-subject designs
#'
#' @author Martin R. Vasilev
#'
#' @param data A data frame containing the variables for calculating Cohen's d
#' 
#' @param measure Dependent measure to be used in the calculation
#' 
#' @param group_var A grouping variable used on in the calculation. It should have only 2 levels (e.g., male vs female,
#' control vs experimental condition)
#' 
#' @param baseline Which level of the grouping variable is to be used for calculating ES. 
#' The formula is always "level 2 - baseline". So, if baseline condition has the smaller mean, Cohen's d will be positive.
#' Otherwise, Cohen's d will be negative.
#' 
#' @param avg_var Averaging variable. This is typically subject number and is used for averaging responses for each subject
#' in order to calculate the correlation between the two conditions. This is necessary as there may be unequal number of 
#' oservation in each condiiton.
#'
#' @return A list containing Cohen's d, it's variance, and 95% confidence interval
#'
#' @example d<- CohensD_raw(data_list= myDF, measure= "RT", group_var= "Condition", avg_var= "sub")


CohensD_raw<- function(data, measure, group_var, baseline, avg_var= "sub"){
  
  #source("https://raw.githubusercontent.com/martin-vasilev/reading_sounds/master/Functions/effect_sizes.R")
  
  Cohens_d<- function(M_C, M_E, S_C, S_E, N_C=NULL, N_E=NULL, N=NULL, r=NULL, design="between", type= "C-E"){
    
    if(type=="C-E"){
      MD<- M_C-M_E
    }else{
      MD<- M_E-M_C
    }
    
    if(design=="between"){
      
      SDp<- sqrt(((N_C-1)*(S_C^2) + (N_E-1)*(S_E^2))/(N_C+N_E-2))
      
    }else{
      Sdiff<- sqrt(S_C^2 + S_E^2 - 2*r*S_C*S_E)
      SDp<- Sdiff/sqrt(2*(1-r))
    }
    
    return(MD/SDp)
  }
  
  
  Cohens_d_var<- function(d, N_C=NULL, N_E=NULL, N=NULL, r=NULL, design="between"){
    
    if(design=="between"){
      d_var<- (N_C + N_E)/(N_C*N_E) + (d^2)/(2*(N_C+N_E))
    } else{
      d_var<- (1/N + (d^2)/(2*N))* (2*(1-r)) 
    }
    
    return(d_var)
  }
  
  warning("This function currently works only with within-subject data")
  
  dat<- data[, c(measure, group_var, avg_var)]
  
  colnames(dat)<- c("measure", "group_var", "avg_var")
  
  dat$group_var<- as.factor(dat$group_var)
  
  dat$group_var<- droplevels(dat$group_var)
  
  groups<- levels(dat$group_var)
  
  if(length(groups)!=2){
    stop("Cohen's d can be calculated only with 2 groups!")
  }
  
  
  # calculate correlation between two means:
  cond1<- NULL; cond2<- NULL
  subs<- unique(dat$avg_var)
  N<- length(subs) # number of subjects
  
  for (i in 1:length(subs)){
    # cond 1:
    a<- subset(dat, avg_var== subs[i] & group_var== groups[1]) 
    cond1[i]<- mean(a$measure, na.rm= T)
    
    # cond 2:
    b<- subset(dat, avg_var== subs[i] & group_var== groups[2]) 
    cond2[i]<- mean(b$measure, na.rm= T)
    
  }
  
  mean_corr<- cor(cond1, cond2, use = 'pairwise.complete.obs') 
  
  # calculate group means and SDs:
  M1<- mean(dat$measure [which(dat$group_var== groups[1])], na.rm=T)
  M2<- mean(dat$measure [which(dat$group_var== groups[2])], na.rm=T)
  
  SD1<- sd(dat$measure [which(dat$group_var== groups[1])], na.rm=T)
  SD2<- sd(dat$measure [which(dat$group_var== groups[2])], na.rm=T)
  
  # Calculate Cohen's d
  if(baseline== groups[1]){
    type= "E-C"
  }else{
    type= "C-E"
  }
  
  
  d<- Cohens_d(M_C = M1, M_E = M2, S_C = SD1, S_E = SD2, N = N, r = mean_corr, design = 'within', type = type)
  d_var<- Cohens_d_var(d = d, N = N, r = mean_corr, design = 'within')
  SE<- sqrt(d_var)
  L95CI<- d- 1.96*SE
  U95CI<- d+1.96*SE
  
  result= list(d= d, var= d_var, lower95= L95CI, upper95= U95CI)
  
  return(result)
  
}