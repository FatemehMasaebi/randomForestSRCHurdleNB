# randomForestSRCHurdleNB
randomForestSRCHurdleNB is a method for handling zero-inflated and zero-deflated count responses using a set of covariates. In this approach, a random forest tree is constructed with a novel splitting rule so that the best split maximizes the likelihood of child nodes among all allowable splits. randomForestSRCHurdleNB utilizes the "randomForestSRC" package (Ishwaran and Kogalur, 2022) available at <https://cran.r-project.org/package=randomForestSRC> with version 3.1.0. The custom splitting rule feature is employed to implement the proposed splitting rule. This package introduces two types of split rules for handling count data: **Split custom 1**, tailored for count responses following a Poisson distribution, and **Split custom 2**, designed for count responses following a Negative Binomial distribution. The methodology is elaborated in Mathlouthi et al. (2020) and Masaebi et al. (2024).
# Authors
This package is written and maintained by Fatemeh Masaebi, Morteza Mohammadzadeh, Denis Larocque (<denis.larocque@hec.ca>) , Farid Zayeri (<fzayeri@gmail.com>)
# Installation
To install this package, follow these steps:
1. install.packages("devtools")
2. devtools::install_github("FatemehMasaebi/randomForestSRCHurdleNB")
# References
Mathlouthi W, Larocque D, Fredette M. Random forests for homogeneous and non-homogeneous Poisson processes with excess zeros. Statistical Methods in Medical Research. 2020 Aug;29(8):2217-37.
# Example usage:
Below is an example illustrating the utilization of the **rfsrc** function with two custom split rules and extracting parameter estimates.

```

library(randomForestSRCHurdleNB)
library(countreg)

# Load the data
data("NB_test", package = "randomForestSRCHurdleNB")
data("NB_train", package = "randomForestSRCHurdleNB")

# Build a RF with the truncated Poisson split rule with the training data
rfPois = rfsrc(y~x1+x2+x3+x4+x5+x6+x7+x8+x9, splitrule="custom1", data=NB_train)

# Get estimation for mu for the test data (the estimation for phi is 0)
yhat0=predict(rfPois,newdata=NB_test)$predicted
yhat0 = as.integer(yhat0)
model_Pois = zerotrunc(yhat0 ~ 1, dist = "poisson")
muhat_Pois = exp(model_Pois$coefficients[[1]])

# Build a RF with the truncated NB split rule with the training data
rfNB = rfsrc(y~x1+x2+x3+x4+x5+x6+x7+x8+x9, splitrule="custom2, data=NB_train)

# Get estimation for mu and phi for the test data
yhat0=predict(rfNB,newdata=NB_test)$predicted
yhat0 = as.integer(yhat0)
model_NB = zerotrunc(yhat0 ~ 1, dist ="negbin")
muhat_nb=exp(model_NB$coefficients[[1]])
thetahat_nb=model_NB$theta
phihat_nb=1/model_NB$theta
 
```






























