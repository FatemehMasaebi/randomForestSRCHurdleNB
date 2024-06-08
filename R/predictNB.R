predictNB = function(rfmodel,dat_test,dist){
  
  bop_index=bop_index_func(rfmodel,dat_test)
  dat_train=cbind(y=rfmodel$yvar,rfmodel$xvar)
  estimation = t(sapply(1:nrow(dat_test),
                      BOP_estimate
                      ,rfmodel = rfmodel
                      ,dist = dist
                      ,bop_index=bop_index
                      ,dat_train)
  )
  
  
  return(estimation)
}
