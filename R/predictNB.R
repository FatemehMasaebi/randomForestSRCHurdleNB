predictNB = function(rfmodel,dat_test,dist){
  
  
  
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
  bop_index=bop_index_func(rfmodel)
  
  
    estimation = sapply(1:nrow(dat_test),
                 BOP_estimate,
                 rfmodel = rfmodel,dist = dist)%>%t
  
    
  return(estimation)
}


































