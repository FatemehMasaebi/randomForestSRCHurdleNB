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

# Build a RF with the truncated Poisson split rule with the training data.
# For simplicity in this example, the value of "ntree" is set to 5.
rfztpois= rfsrc(y~x1+x2+x3+x4+x5+x6+x7+x8+x9, ntree = 5, splitrule="custom1", membership = T, data=dat_train)
# Get estimation for mu for the test data (the estimation for phi is 0)
predictNB(rfztpois, dat_test, dist = "poisson")

# Build a RF with the truncated NB split rule with the training data
rfztNB= rfsrc(y~x1+x2+x3+x4+x5+x6+x7+x8+x9, ntree = 5, splitrule="custom2", membership = T, data=dat_train)
# Get estimation for mu and phi for the test data
predictNB(rfztNB, dat_test, dist = "negbin")

# References
Mathlouthi W, Larocque D, Fredette M. Random forests for homogeneous and non-homogeneous Poisson processes with excess zeros. Statistical Methods in Medical Research. 2020 Aug;29(8):2217-37.





























