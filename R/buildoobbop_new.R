## build OOB-BOP with OOB neighbors of observations in terminal nodes
buildoobbop_new <- function (mem.train, inbag) {
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
    data.table::as.data.table(mem.test)[, data.table::`:=`(testid = .I)],
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
    data.table::as.data.table(mem.train)[, data.table::`:=`(trainid = .I)],
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