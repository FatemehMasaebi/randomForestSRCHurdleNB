\name{subsample.rfsrc}
\alias{subsample.rfsrc}
\alias{subsample}
\alias{extract.subsample}
\alias{extract.bootsample}
\title{Subsample Forests for VIMP Confidence Intervals}
\description{
  Use subsampling to calculate confidence intervals and standard
  errors for VIMP (variable importance). Applies to all families.
}
\usage{\method{subsample}{rfsrc}(obj,
  B = 100,
  block.size = 1,
  subratio = NULL,
  stratify = TRUE,
  performance = FALSE,
  joint = FALSE,
  xvar.names = NULL,
  bootstrap = FALSE,
  verbose = TRUE)
}
\arguments{

  \item{obj}{A forest grow object.}

  \item{B}{Number of subsamples (or number of bootstraps).}

  \item{block.size}{Specifies number of trees in a block when calculating
    VIMP.  This is over-ridden if VIMP is present in the original grow
    call in which case the grow value is used.} 

  \item{subratio}{Ratio of subsample size to original sample size.  The
    default is the inverse square root of the sample size.}

  \item{stratify}{Use stratified subsampling?  See details below.}

  \item{performance}{Error rate of forest?  Used to obtain
    standard error and confidence region for the ensemble out-of-sample performance.}

  \item{joint}{Joint VIMP for all variables? Users can also request joint
    VIMP for specific variables using \code{xvar.names}.}

  \item{xvar.names}{Specifies variables for calculating joint VIMP.  By
    default all variables are used.}

  \item{bootstrap}{Use double bootstrap approach in place of
    subsampling?  Much slower, but potentially more accurate.}

  \item{verbose}{Provide verbose output?}

}
\details{

  Using a previously trained forest, subsamples the data and constructs
  subsampled forests to estimate standard errors and confidence
  intervals for VIMP (Ishwaran and Lu, 2019).  If bootstrapping is
  requested, a double bootstrap is applied in place of subsampling.
  Additionally, \code{performance="TRUE"} constructs standard errors and
  confidence regions for the error rate (OOB performance) of the
  trained forest.

  If the trained forest does not have VIMP values, the algorithm will
  first need to calculate VIMP.  Therefore, if the user plans to make
  repeated calls to \code{subsample}, it is advisable to include VIMP in
  the original grow call.  Subsampled forests are calculated using the
  same tuning parameters as the original forest.  While a sophisticated
  algorithm is utilized to acquire as many of these parameters as
  possible, keep in mind there are some conditions where this will fail:
  for example there are certain settings where the user has specified
  non-standard sampling in the grow forest.
  
  Delete-d jackknife estimators of the variance (Shao and Wu, 1989) are
  returned alongside subsampled variance estimators (Politis and Romano,
  1994).  While these methods are closely related, estimated standard
  error for VIMP from delete-d estimators are generally larger, which is
  a form of bias correction, and which occurs primarily for variables
  with true signal.  Confidence interval coverage is generally better
  under delete-d estimators, but undercoverage for strong variables and
  overcoverage for noise variables is exhibited by both estimators.
  This can be considered beneficial if the goal is variable selection
  (Ishwaran and Lu, 2019).

  By default, stratified subsampling is used for classification,
  survival, and competing risk families.  For classification,
  stratification is on the class label, while for survival and competing
  risk, stratification is on the event type and censoring.  Users are
  discouraged from over-riding this option, especially in small sample
  settings, as this could lead to error due to subsampled data not
  having full representation of class labels in classification settings,
  and in survival settings, subsampled data may be devoid of deaths
  and/or have reduced number of competing risks.  Note also that
  stratified sampling is not available for multivariate families --
  users should especially exercise caution when selecting subsampling
  rates here.

  The function \command{extract.subsample} can be used to extract
  information from the subsampled object.  It returns summary
  information (used for plotting confidence intervals) as well as VIMP
  from the original forest and VIMP from the subsampled forests.  Keep in
  mind this subsampled VIMP is "raw" in the sense it equals VIMP
  from a forest constructed with a much smaller sample size. No
  processing of the subsampled VIMP to the original sample size is done.
  Also, the returned VIMP is "standardized" (this means for regression
  families, VIMP is standardized by dividing by the variance of Y and
  multiplying by 100.  For all other families, VIMP is scaled by 100).
  Use \code{standardize=FALSE} if you want unstandardized VIMP.
  
  When printing or plotting results, the default is to standardize
  VIMP. This can be turned off using the option \code{standardize} in
  those wrappers.
  
}
\value{
  A list with the following key components:
  \item{rf}{Original forest grow object.}
  \item{vmp}{Variable importance values for grow forest.}
  \item{vmpS}{Variable importance subsampled values.}
  \item{subratio}{Subratio used.}  
}
\author{
  Hemant Ishwaran and Udaya B. Kogalur
}
\references{

  Ishwaran H. and Lu M.  (2019).  Standard errors and confidence
  intervals for variable importance in random forest regression,
  classification, and survival. \emph{Statistics in Medicine}, 38,
  558-582.

  Politis, D.N. and Romano, J.P. (1994). Large sample confidence
  regions based on subsamples under minimal assumptions. \emph{The
  Annals of Statistics}, 22(4):2031-2050.

  Shao, J. and Wu, C.J. (1989). A general theory for jackknife variance
  estimation. \emph{The Annals of Statistics}, 17(3):1176-1197.

}
\seealso{
  \command{\link{holdout.vimp.rfsrc}}
  \command{\link{plot.subsample.rfsrc}},
  \command{\link{rfsrc}},
  \command{\link{vimp.rfsrc}}
}
\examples{
\donttest{
## ------------------------------------------------------------
## regression
## ------------------------------------------------------------

## traing the forest
reg.o <- rfsrc(Ozone ~ ., airquality)

## default subsample call
reg.smp.o <- subsample(reg.o)

## plot confidence regions
plot.subsample(reg.smp.o)

## summary of results
print(reg.smp.o)

## subsample call with joint vimp and confidence region for error rate
reg.smp.o2 <- subsample(reg.o, performance = TRUE,
           joint = TRUE, xvar.names = c("Day", "Month"))
plot.subsample(reg.smp.o2)

## now try the double bootstrap (slow!!)
reg.dbs.o <- subsample(reg.o, B = 25, bootstrap = TRUE)
print(reg.dbs.o)
plot.subsample(reg.dbs.o)

## ------------------------------------------------------------
## classification
## ------------------------------------------------------------

## 3 non-linear, 15 linear, and 5 noise variables 
if (library("caret", logical.return = TRUE)) {
  d <- twoClassSim(1000, linearVars = 15, noiseVars = 5)

  ## VIMP based on (default) misclassification error
  cls.o <- rfsrc(Class ~ ., d)
  cls.smp.o <- subsample(cls.o, B = 100)
  plot.subsample(cls.smp.o, cex.axis = .7)

  ## same as above, but with VIMP defined using normalized Brier score
  cls.o2 <- rfsrc(Class ~ ., d, perf.type = "brier")
  cls.smp.o2 <- subsample(cls.o2, B = 100)
  plot.subsample(cls.smp.o2, cex.axis = .7)
}

## ------------------------------------------------------------
## class-imbalanced data
## uses RFQ classifier with G-mean VIMP
## ------------------------------------------------------------

if (library("caret", logical.return = TRUE)) {

  ## experimental settings
  n <- 1000
  q <- 20
  ir <- 6
  f <- as.formula(Class ~ .)
 
  ## simulate the data, create minority class data
  d <- twoClassSim(n, linearVars = 15, noiseVars = q)
  d$Class <- factor(as.numeric(d$Class) - 1)
  idx.0 <- which(d$Class == 0)
  idx.1 <- sample(which(d$Class == 1), sum(d$Class == 1) / ir , replace = FALSE)
  d <- d[c(idx.0,idx.1),, drop = FALSE]

  ## q-classifier
  oq <- imbalanced(Class ~ ., d, splitrule = "auc",
              importance = TRUE, block.size = 10)

  ## subsample the q-classifier
  smp.oq <- subsample(oq, B = 100)
  plot(smp.oq, cex.axis = .7)

}

## ------------------------------------------------------------
## survival 
## ------------------------------------------------------------

data(pbc, package = "randomForestSRC")
srv.o <- rfsrc(Surv(days, status) ~ ., pbc)
srv.smp.o <- subsample(srv.o, B = 100)
plot(srv.smp.o)

## ------------------------------------------------------------
## competing risks
## target event is death (event = 2)
## ------------------------------------------------------------

if (library("survival", logical.return = TRUE)) {
  data(pbc, package = "survival")
  pbc$id <- NULL
  cr.o <- rfsrc(Surv(time, status) ~ ., pbc, splitrule = "logrankCR", cause = 2)
  cr.smp.o <- subsample(cr.o, B = 100)
  plot.subsample(cr.smp.o, target = 2)
}

## ------------------------------------------------------------
## multivariate 
## ------------------------------------------------------------

if (library("mlbench", logical.return = TRUE)) {
  ## simulate the data 
  data(BostonHousing)
  bh <- BostonHousing
  bh$rm <- factor(round(bh$rm))
  o <- rfsrc(cbind(medv, rm) ~ ., bh)
  so <- subsample(o)
  plot(so)
  plot(so, m.target = "rm")
}

## ------------------------------------------------------------
## largish data example - use rfsrc.fast for fast forests
## ------------------------------------------------------------

if (library("caret", logical.return = TRUE)) {
  ## largish data set
  d <- twoClassSim(1000, linearVars = 15, noiseVars = 5)

  ## use a subsampled forest with Brier score performance
  ## remember to request forests in rfsrc.fast
  o <- rfsrc.fast(Class ~ ., d, ntree = 100,
           forest = TRUE, perf.type = "brier")
  so <- subsample(o, B = 100)
  plot.subsample(so, cex.axis = .7)
}


}
}
\keyword{subsampling}
\keyword{vimp}
\keyword{confidence interval}

