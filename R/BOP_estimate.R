BOP_estimate<-function(irow,rfmodel,dist
                       ,bop_index
                       ,dat_train
                       ,prefix=substitute(rfmodel)){
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
      countreg::zerotrunc(Ybop ~ 1
                ,data = df
                ,dist ="negbin"
                # ,start=c(lmu=log(mu.start),theta=theta.start)
                #,control = countreg::zerotrunc.control(method = "L-BFGS-B"
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
      countreg::zerotrunc(Ybop ~ 1
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
