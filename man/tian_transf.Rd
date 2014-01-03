\name{tian_transf}
\alias{tian_transf}
\title{Modify Covariates for Uplift Modeling
}
\description{This function transforms the data frame supplied in the function call by creating a new set of modified covariates and an equal number of control and treated observations. This transformed data set can be subsequently used with any conventional supervised learning algorithm to model uplift.
}

\usage{
tian_transf(formula, data, subset, na.action = na.pass,
 method = c("undersample", "oversample", "none"),
 standardize = TRUE, cts = FALSE)
}

\arguments{
\item{formula}{a formula expression of the form response ~ predictors. A special term of the form \code{trt()} must be used in the model equation to identify the binary treatment variable. For example, if the treatment is represented by a variable named \code{treat}, then the right hand side of the formula must include the term +\code{trt(treat)}.}
  \item{data}{a data.frame in which to interpret the variables named in the formula.}
  \item{subset}{expression indicating which subset of the rows of data should be included. All observations are included by default.}
  \item{na.action}{a missing-data filter function. This is applied to the model.frame after any subset argument has been used. Default is \code{na.action = na.pass}.}
  \item{method}{the method used to create the transformed data set. It must be one of "undersample", "oversample" or "none", with no default. See details.}
  \item{standardize}{If TRUE, each variable is standardized to have unit L2 norm, otherwise it is left alone. Default is TRUE.}
  \item{cts}{if TRUE, contrasts for factors are created in a special way. See details. Default is FALSE.}
}

\details{

The covariates \eqn{x} supplied in the RHS of the model formula are transformed as \eqn{w = z * T/2}, where \eqn{T=[-1,1]} is the treatment indicator and \eqn{z} is the matrix of standardize \eqn{x} variables.

If \code{cts = TRUE}, factors included in the formula are converted to dummy variables in a special way that is more appropriate when the returned model frame is used to fit a penalized regression. In this case, contrasts used for factors are given by penalized regression contrasts from the \code{penalized} package. Unordered factors are turned into as many dummy variables as the factor has levels, except when the number of levels is 2, in which case it returns a single contrast. This ensures a symmetric treatment of all levels and guarantees that the fit does not depend on the ordering of the levels. See \code{help(contr.none)} in \code{penalized} package. Ordered factors are turned into dummy variables that code for the difference between successive levels (one dummy less than the number of levels). See \code{help(contr.diff)}  in \code{penalized package}.

If the data has an equal number of control and treated observations, then \code{method = "none"} should be used. Otherwise, any of the other methods should be used. 

If \code{method = "undersample"}, a random sample without replacement is drawn from the treated class (i.e., treated/control) with the majority of observations, such that the returned data frame will have balanced treated/control proportions. 

If \code{method = "oversample"}, a random sample with replacement is drawn from the treated class with the minority of observations, such that the returned data frame will have balanced treated/control proportions. 

}
\value{
A model frame, including the modified covariates \eqn{w} (the prefix "T_" is added to the name of each covariate to denote it has been modified), the treatment (\eqn{ct=1}) and control (\eqn{ct=0}) assignment and the response variable (LHS of model formula). The intercept is omitted from the model frame. 
}
\references{
Tian, L., Alizadeh, A., Gentles, A. and Tibshirani, R. 2012. A simple method for detecting
interactions between a treatment and a large number of covariates. Submitted on Dec 2012.
arXiv:1212.2995 [stat.ME].

Guelman, L., Guillen, M., and Perez-Marin A.M. (2013). Optimal personalized treatment rules for marketing interventions: A review of methods, a new proposal, and an insurance case study. \emph{Submitted}.
}
\author{
Leo Guelman <leo.guelman@gmail.com>
}

\examples{
library(uplift)

set.seed(1)
dd <- sim_pte(n = 1000, p = 20, rho = 0, sigma =  sqrt(2), beta.den = 4)
dd$treat <- ifelse(dd$treat == 1, 1, 0) 

dd2 <- tian_transf(y ~  X1 + X2 + X3 + trt(treat), data =dd, method = "none")
head(dd2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{uplift}