bop_index_func=function(rfmodel,dat_test){
  pred.model=predict.rfsrc(rfmodel,membership =T
                     ,predict.all=TRUE,newdata=dat_test)
  
  mem.train <- rfmodel$membership
  inbag <- rfmodel$inbag
  mem.test<-pred.model$membership
  
  # BOP_ind_ztNBforest<-buildoobbop_new(mem.train,inbag)
  BOP_ind<-buildtestbop_new(mem.train = mem.train,mem.test = mem.test,inbag = inbag)
  return(BOP_ind)
}
