# randomForestSRCHurdleNB
randomForestSRCHurdleNB is a method for handling Zero-Inflated and Zero-Deflated count responses using a set of covariates. In this approach, a random forest tree is constructed with a novel splitting rule so that the best split maximizes the likelihood of child nodes among all allowable splits. randomForestSRCHurdleNB utilizes the "randomForestSRC" package (Ishwaran and Kogalur, 2022) available at <https://cran.r-project.org/package=randomForestSRC> with version 3.1.0. The custom splitting rule feature is employed to implement the proposed splitting rule. This package introduces two types of split rules for handling count data: **Split custom 1**, tailored for count responses following a Poisson distribution, and **Split custom 2**, designed for count responses following a Negative Binomial distribution. The methodology is elaborated in Mathlouthi et al. (2020) and Masaebi et al. (2024).
# Authors
This package is written and maintained by Fatemeh Masaebi, Morteza Mohammadzadeh, Denis Larocque (<denis.larocque@hec.ca>) , Farid Zayeri (<fzayeri@gmail.com>)
# Installation
To install this package, follow these steps:
1. install.packages("devtools")
2. devtools::install_github("FatemehMasaebi/randomForestSRCHurdleNB")"
# References
Mathlouthi W, Larocque D, Fredette M. Random forests for homogeneous and non-homogeneous Poisson processes with excess zeros. Statistical Methods in Medical Research. 2020 Aug;29(8):2217-37.
