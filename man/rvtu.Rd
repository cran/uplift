\name{rvtu}
\alias{rvtu}

\title{Response Variable Transform for Uplift Modeling}

\description{This function transforms the data frame supplied in the function call by creating a new response variable and an equal number of control and treated observations. This transformed data set can be subsequently used with any conventional supervised learning algorithm to model uplift.}
\usage{
rvtu(formula, data, subset, na.action = na.pass, 
     method = c("undersample", "oversample", "weights", "none"))
}

\arguments{

  \item{formula}{a formula expression of the form response ~ predictors. A special term of the form \code{trt()} must be used in the model equation to identify the binary treatment variable. For example, if the treatment is represented by a variable named \code{treat}, then the right hand side of the formula must include the term +\code{trt(treat)}.}
  \item{data}{a data.frame in which to interpret the variables named in the formula.}
  \item{subset}{expression indicating which subset of the rows of data should be included. All observations are included by default.}
  \item{na.action}{a missing-data filter function. This is applied to the model.frame after any subset argument has been used. Default is \code{na.action = na.pass}.}
  \item{method}{the method used to create the transformed data set. It must be one of "undersample", "oversample", "weights" or "none", with no default. See details.}
}

\details{

The transformed response variable \eqn{z} equals 1 if the observation has a response value of 1 and has been treated, or if it has a response value of 0 and has not been treated. Intuitively, \eqn{z} equals 1 if we know that, for a given
case, the outcome in the treatment group would have been at least as good as in the control group, had we known for this case the outcome in both groups. Under equal proportion of control and treated observations, it is easy to prove that \eqn{ 2 * Prob(z=1|x) - 1 = Prob(y=1|treated, x) - Prob(y=1|control, x)} (Jaskowski and Jaroszewicz, 2012).

If the data has an equal number of control and treated observations, then \code{method = "none"} must be used. Otherwise, any of the other methods must be used. 

If \code{method = "undersample"}, a random sample without replacement is drawn from the treated class (i.e., treated/control) with the majority of observations, such that the returned data frame will have balanced treated/control proportions. 

If \code{method = "oversample"}, a random sample with replacement is drawn from the treated class with the minority of observations, such that the returned data frame will have balanced treated/control proportions. 

If \code{method = "weights"}, the returned data frame will have a weight variable \eqn{w} assigned to each observation. The weight assigned to the treated (control) equals 1 - proportion of treated observations (proportion of treated observations). 
}

\value{
A data frame including the predictor variables (RHS of the formula expression),  the treatment (\eqn{ct=1}) and control (\eqn{ct=0}) assignment, the original response variable (LHS of the formula expression), and the transformed response variable for uplift modeling \eqn{z}. If \code{method = "weights"} an additional weight variable \eqn{w} is included. 
}
\references{
Jaskowski, M. and Jaroszewicz, S. (2012)  Uplift Modeling for Clinical Trial Data. In ICML 2012 Workshop on Machine Learning for Clinical Data Analysis, Edinburgh, Scotland. 

Guelman, L., Guillen, M., and Perez-Marin A.M. (2013). Optimal personalized treatment rules for marketing interventions: A review of methods, a new proposal, and an insurance case study. \emph{Submitted}.
}

\author{
Leo Guelman <leo.guelman@gmail.com>
}

\examples{

library(uplift)

### Simulate data

set.seed(1)
dd <- sim_pte(n = 1000, p = 20, rho = 0, sigma =  sqrt(2), beta.den = 4)
dd$treat <- ifelse(dd$treat == 1, 1, 0) 

### Transform response variable for uplift modeling
dd2 <- rvtu(y ~ X1 + X2 + X3 + X4 + X5 + X6 + trt(treat), data = dd, method = "none")  

### Fit a Logistic model to the transformed response
glm.uplift <- glm(z ~ X1 + X2 + X3 + X4 + X5 + X6, data = dd2, family = "binomial")

### Test fitted model on new data
dd_new <- sim_pte(n = 1000, p = 20, rho = 0, sigma =  sqrt(2), beta.den = 4)
dd_new$treat <- ifelse(dd_new$treat == 1, 1, 0) 
pred <- predict(glm.uplift, dd_new, type = "response")
perf <- performance(2 * pred - 1, rep(0, length(pred)), dd_new$y, dd_new$treat, direction = 1)
perf

}

