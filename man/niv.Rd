\name{niv}
\alias{niv}
\title{Adjusted Net Information Value}
\description{
This function produces an adjusted net information value for each variable specified in the right hand side of the formula. This can be a helpful exploratory tool to (preliminary) determine the predictive power of each variable for uplift.}
\usage{
niv(formula, data, subset, na.action = na.pass, B = 10, direction = 1, 
nbins = 10, continuous = 4, plotit = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula expression of the form response ~ predictors. A special term of the form \code{trt()} must be used in the model equation to identify the binary treatment variable. For example, if the treatment is represented by a variable named \code{treat}, then the right hand side of the formula must include the term +\code{trt(treat)}.}
\item{data}{a data.frame in which to interpret the variables named in the formula.}
  \item{subset}{expression indicating which subset of the rows of data should be included. All observations are included by default.}
  \item{na.action}{a missing-data filter function. This is applied to the model.frame after any subset argument has been used. Default is \code{na.action = na.pass}.}
  \item{B}{the number of bootstrap samples used to compute the adjusted net information value.}
  \item{direction}{if set to \code{1} (default), the net weight of evidence is computed as the difference between the weight of evidence of the treatment and control groups, or if \code{2}, it is computed as the difference between the weight of evidence of the control and treatment groups. This will not change the adjusted net information value, but only the sign of the net weight of evidence values.}
  \item{nbins}{the number of bins created from numeric predictors. The bins are created based on quantiles, with a default value of 10 (deciles).}
  \item{continuous}{specifies the threshold for when a variable is considered to be continuous (when there are at least \code{continuous} unique values). The default is 4. Factor variables are always considered to be categorical no matter how many levels they have.}
  \item{plotit}{plot the adjusted net information value for each variable?}
  \item{\dots}{additional arguments passed to \code{barplot}.}
}
\details{
The ordinary information value (commonly used in credit scoring applications) is given by

\deqn{IV = \sum_{i=1}^{G} \left (P(x=i|y=1) - P(x=i|y=0) \right) \times WOE_i}

where \eqn{G} is the number of groups created from a numeric predictor or categories from a categorical predictor, and \eqn{WOE_i = ln (\frac{P(x=i|y=1)}{P(x=i|y=0)})}.

The net information value is the natural extension of the IV for the case of uplift. It is computed as

\deqn{NIV = 100 \times \sum_{i=1}^{G}(P(x=i|y=1)^{T} \times P(x=i|y=0)^{C}  - 
                           P(x=i|y=0)^{T} \times P(x=i|y=1)^{C}) \times NWOE_i}

where \eqn{NWOE_i = WOE_i^{T} - WOE_i^{C}}

The adjusted net information value is computed as follows:

1. Take \eqn{B} bootstrap samples and compute the NIV for each variable on each sample

2. Compute the mean of the NIV (\eqn{NIV_{mean}}) and sd of the NIV (\eqn{NIV_{sd}}) for each variable over all the \eqn{B} bootstraps

3. The adjusted NIV for a given variable is computed by adding a penalty term to the mean NIV: \eqn{NIV_{mean} - \frac{NIV_{sd}}{\sqrt{B}}}. 

}
\value{
A list with two components:
\item{niv_val}{a matrix with the following columns: niv (the average net information value for each variable over all bootstrap samples), penalty (the penalty term calculated as described in the details above), the adjusted information value (the difference between the prior two colums)}

\item{nwoe}{a list of matrices, one for each variable. The columns represent: the distribution of the responses (y=1) over the treated group (\code{ct1.y1}), the distribution of the non-responses (y=0) over the treated group (\code{ct1.y0}), the distribution of the responses (y=1) over the control group (\code{ct0.y1}), the distribution of the non-responses (y=0) over the control group (\code{ct0.y0}), the weight-of-evidence over the treated group (\code{ct1.woe}), the weight-of-evidence over the control group \code{ct0.woe}, and the net weigh-of-evidence (\code{nwoe}).
}
}
\references{
Larsen, K. (2009). Net lift models. In: M2009 - 12th Annual SAS Data Mining Conference.
}
\author{
Leo Guelman <leo.guelman@gmail.com>
}

\examples{
library(uplift)

set.seed(12345)
dd <- sim_pte(n = 1000, p = 20, rho = 0, sigma =  sqrt(2), beta.den = 4)
dd$treat <- ifelse(dd$treat == 1, 1, 0) 

niv.1 <- niv(y ~ X1 + X2 + X3 + X4 + X5 + X6 + trt(treat), data = dd)            
niv.1$niv
niv.1$nwoe
}
\keyword{uplift}
\keyword{trees}
