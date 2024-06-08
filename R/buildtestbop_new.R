buildtestbop_new <- function (mem.train, mem.test, inbag) {
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
