# randomForestSRCHurdleNB
randomForestSRCHurdleNB is a method for handling zero-inflated and zero-deflated count responses using a set of covariates. In this approach, a Random Forest tree is constructed with a novel splitting rule so that the best split maximizes the likelihood of child nodes among all allowable splits. randomForestSRCHurdleNB utilizes the "randomForestSRC" package (Ishwaran and Kogalur, 2022) available at <https://cran.r-project.org/package=randomForestSRC> with version 3.1.0. The custom splitting rule feature is employed to implement the proposed splitting rule. This package introduces two types of split rules for handling count data: ***Split custom 1***, tailored for count responses following a Poisson distribution, and ***Split custom 2***, designed for count responses following a Negative Binomial distribution. The methodology is elaborated in Mathlouthi et al. (2020) and Masaebi et al. (2024).

# Authors
This package is written and maintained by *Fatemeh Masaebi*, *Morteza Mohammadzadeh*, *Denis Larocque (<denis.larocque@hec.ca>)* , *Farid Zayeri (<fzayeri@gmail.com>)*

# Installation
To install this package, follow these steps:
1. install.packages("devtools")
2. devtools::install_github("FatemehMasaebi/randomForestSRCHurdleNB")


# Example usage:
Below is an example illustrating the utilization of the **rfsrc** function with two custom split rules and extracting parameter estimates.

```
# Loading libraries and preparing the data.
library(randomForestSRCHurdleNB)
library(countreg)
library(tidyverse)
data("NB_test", package = "randomForestSRCHurdleNB")
data("NB_train", package = "randomForestSRCHurdleNB")
dat_train = NB_train
dat_test = NB_test
```

Before fitting Random Forests and extracting estimations, two functions as outlined below need to be executed. **Function 1**: Defining the BOP set for each subject in the test data. **Function 2**: Implementing a zero-truncated model on the BOP data, fitting a zero-truncated Poisson model for the BOP data from the Poisson forest, and fitting a zero-truncated Negative Binomial model for the BOP data from the NB forest. Estimation parameters of mu and phi have been obtained based on the zero-truncation model.

```
########################## **Function 1**#################################
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "tree", "terminalnode", "trainid", "testid",
                           "bop", "ib_count"))
}

## build test-BOP with oob neighbors of observations in terminal nodes
buildtestbop <- function (mem.train, mem.test, inbag) {
  ## Inputs
  # mem.train: the terminal node membership of training observations
  # mem.test: the terminal node membership of test observations
  # inbag: inbag counts of training observations
  
  ## Output
  # Test-BOP: a matrix of neighbour observations with ntest rows and ntrain columns
  
  ## convert mem.test to data.table and set key
  mem.test.dt <- data.table::melt(
    data.table::as.data.table(mem.test)[, `:=`(testid = .I)],
    id.vars = c("testid"),
    measure.vars = 1:ncol(mem.test),
    variable.name = "tree",
    value.name = "terminalnode",
    variable.factor = FALSE)
  data.table::setkey(mem.test.dt, tree, terminalnode)
  
  ## get the terminal node membership of the training observations
  ## in trees where they are OOB
  mem.train[inbag != 0] <- NA
  
  ## convert mem.train to data.table
  mem.train.dt <- data.table::melt(
    data.table::as.data.table(mem.train)[, `:=`(trainid = .I)],
    id.vars = c("trainid"),
    measure.vars = 1:ncol(mem.train),
    variable.name = "tree",
    value.name = "terminalnode",
    variable.factor = FALSE,
    na.rm = TRUE)
  
  mem.train.dt <- mem.train.dt[,
                               .(bop = list(trainid)),
                               keyby = c("tree", "terminalnode")]
  
  ## build test BOP
  BOPtest <- mem.train.dt[mem.test.dt,
                          .(tree, terminalnode, testid, bop)][,
                                                              .(bop = list(sort(unlist(bop)))),
                                                              keyby = c("testid")]
  ## convert BOP list to weight matrix
  # BOPtest2 <- t(sapply(BOPtest$bop, function(x) tabulate(x, nbins = nrow(mem.train))))
  
  # return(BOPtest2)
  return(  BOPtest$bop)
  
}

## build OOB-BOP with OOB neighbors of observations in terminal nodes
buildoobbop <- function (mem.train, inbag) {
  ## Inputs
  # mem.train: the terminal node membership of training observations
  # inbag: inbag counts of training observations
  
  ## Output
  # OOB-BOP: a matrix of neighbour observations with ntrain rows and ntrain columns
  
  ## mem.test will be mem.train with only OOB memberships
  mem.test <- mem.train
  mem.test[inbag != 0] <- NA
  
  ## convert mem.test to data.table and set key
  mem.test.dt <- data.table::melt(
    data.table::as.data.table(mem.test)[, `:=`(testid = .I)],
    id.vars = c("testid"),
    measure.vars = 1:ncol(mem.test),
    variable.name = "tree",
    value.name = "terminalnode",
    variable.factor = FALSE,
    na.rm = TRUE)
  data.table::setkey(mem.test.dt, tree, terminalnode)
  
  ## get the terminal node membership of the training observations
  ## in trees where they are OOB
  mem.train[inbag != 0] <- NA
  
  ## convert mem.train to data.table
  mem.train.dt <- data.table::melt(
    data.table::as.data.table(mem.train)[, `:=`(trainid = .I)],
    id.vars = c("trainid"),
    measure.vars = 1:ncol(mem.train),
    variable.name = "tree",
    value.name = "terminalnode",
    variable.factor = FALSE,
    na.rm = TRUE)
  
  mem.train.dt <- mem.train.dt[,
                               .(bop = list(trainid)),
                               keyby = c("tree", "terminalnode")]
  
  ## build OOB-BOP with observation IDs
  BOPoob <- mem.train.dt[mem.test.dt,
                         .(tree, terminalnode, testid, bop)][,
                                                             .(bop = list(sort(unlist(bop)))),
                                                             keyby = c("testid")]
  
  ## convert BOP list to weight matrix
  BOPoob2 <- t(sapply(BOPoob$bop, function(x) tabulate(x, nbins = nrow(mem.train))))
  
  ## remove the observation itself from its OOB-BOP
  diag(BOPoob2) <- 0
  
  return(BOPoob2)
}



bop_index_func=function(rfmodel){
  pred.model=predict(rfmodel,membership =T
                     ,predict.all=TRUE,newdata=dat_test)
  
  mem.train <- rfmodel$membership
  inbag <- rfmodel$inbag
  mem.test<-pred.model$membership
  
  # BOP_ind_ztNBforest<-buildoobbop(mem.train,inbag)
  BOP_ind<-buildtestbop(mem.train = mem.train,mem.test = mem.test,inbag = inbag)
  return(BOP_ind)}

########################## **Function 2**#################################
BOP_estimate<-function(irow,rfmodel,dist,prefix=substitute(rfmodel)){
    ind=bop_index[[irow]]
    df=data.frame(Ybop=dat_train$y[ind])
    if(dist=="negbin.gamlss"){
      model=try({
        gamlss(Ybop ~ 1
               ,data = df
               , family=Ttruc.Zero.NB)
      },
      silent = T)
      if(class(model)[1]=="try-error"){
        ret=c("muhat_nbbop"=NA,
              "thetahat_nbbop"=NA,
              "phihat_nbbop"=NA)
        
        cat("(zero trunc error) Failed; BOP=",nrow(df) ,";Test id= ",irow,"\r")
      }else{
        muhat_ztnb=exp( model$mu.coefficients[[1]])
        thetahat_ztnb=1
        phihat_ztnb=1
        
        
        ret=c("muhat_nbbop"=muhat_ztnb,
              "thetahat_nbbop"=thetahat_ztnb,
              "phihat_nbbop"=phihat_ztnb
        )
        cat("Success; BOP=",nrow(df) ,";Test id= ",irow,"\r")
        
      }
    }## end if negbin
    if(dist=="negbin"){
      model=try({
        mu.start=dat_test$mu[irow]
        theta.start=dat_test$theta[irow]
        zerotrunc(Ybop ~ 1
                  ,data = df
                  ,dist ="negbin"
                  # ,start=c(lmu=log(mu.start),theta=theta.start)
                  #,control = zerotrunc.control(method = "L-BFGS-B"
                  # ,lower=c(log(mu.start)-1,0)
                  #,upper=c(log(mu.start)+1,theta.start+.5)
                  #)
        )
      },silent = T)
      if(class(model)=="try-error"){
        ret=c("muhat_nbbop"=NA,
              "thetahat_nbbop"=NA,
              "phihat_nbbop"=NA
        )
        cat("(zero trunc error) Failed; BOP=",nrow(df) ,";Test id= ",irow,"\r")
      }else{
        muhat_ztnb=exp(model$coefficients[[1]])
        thetahat_ztnb=model$theta
        phihat_ztnb=1/model$theta
        
        
        ret=c("muhat_nbbop"=muhat_ztnb,
              "thetahat_nbbop"=thetahat_ztnb,
              "phihat_nbbop"=phihat_ztnb
              
        )
        cat("Success; BOP=",nrow(df) ,";Test id= ",irow,"\r")
        
      }
    }## end if negbin
    if(dist=="poisson"){
      model=try(
        zerotrunc(Ybop ~ 1
                  ,data = df
                  ,dist ="poisson")
                  
       
        ,silent = T)
      if(class(model)=="try-error"){
        ret=c("muhat_poisbop"=NA,
              "thetahat_poisbop"=NA,
              "phihat_poisbop"=NA
        )
        cat("(zero trunc error) Failed; BOP=",nrow(df) ,";Test id= ",irow,"\r")
      }else{
        muhat_ztpois=exp(model$coefficients[[1]])
        thetahat_ztpois=Inf
        phihat_ztpois=0
        
        
        ret=c("muhat_poisbop"=muhat_ztpois,
              "thetahat_poisbop"=thetahat_ztpois,
              "phihat_poisbop"=phihat_ztpois
              
        )
        cat("Success; BOP=",nrow(df) ,";Test id= ",irow,"\r")
        
      }
    }## end if poisson
    names(ret)=paste0(names(ret),".",prefix)
    return(ret)
  }
```
After running the required functions, we can now construct NB and Poisson Random Forests.

```
# Build a Random Forest with the truncated Poisson split rule with the training data
rfztpois = rfsrc(y~x1+x2+x3+x4+x5+x6+x7+x8+x9 ,membership = T, splitrule="custom1", data=dat_train)

# Implementing the BOP index function (Function 1) to construct the BOP set based on the Poisson forest.
bop_index=bop_index_func(rfztpois)

# extracting the estimates of mu and phi (phi = 0) based on the BOP_estimate function (Function 2) derived from the Poisson forest.
poisbop=sapply(1:50,
                 BOP_estimate,
                 rfmodel = rfztpois, dist = "poisson"
                 ,simplify = T )%>%t

# Estimates of mu and phi for each subject in the test data have been included in the dataset.
dat_test=bind_cols(dat_test,poisbop)


# Build a Random Forest with the truncated NB split rule with the training data
rfztNB= rfsrc(y~x1+x2+x3+x4+x5+x6+x7+x8+x9, splitrule="custom2", membership = T, data=dat_train)

# Implementing the BOP index function (Function 1) to construct the BOP set based on the NB forest.
bop_index=bop_index_func(rfztNB)

# extracting the estimates of mu and phi based on the BOP_estimate function (Function 2) derived from the NB forest.
 nbbop=sapply(1:50,
               BOP_estimate,
               rfmodel = rfztNB,dist = "negbin")%>%t

# Estimates of mu and phi for each subject in the test data have been included in the dataset.
dat_test=bind_cols(dat_test,nbbop)
```
# References
Mathlouthi W, Larocque D, Fredette M. Random forests for homogeneous and non-homogeneous Poisson processes with excess zeros. Statistical Methods in Medical Research. 2020 Aug;29(8):2217-37.





























