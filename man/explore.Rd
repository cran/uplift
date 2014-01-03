\name{explore}
\alias{explore}

\title{Explore Data for Uplift Modeling}

\description{This function provides a basic exploratory tool for uplift modeling, by computing the average value of the response variable for each predictor and treatment assignment.}
\usage{
explore(formula, 
        data, 
        subset,
        na.action = na.pass,
        nbins = 4, 
        continuous = 4, 
        direction = 1)
}

\arguments{

 \item{formula}{a formula expression of the form response ~ predictors. A special term of the form \code{trt()} must be used in the model equation to identify the binary treatment variable. For example, if the treatment is represented by a variable named \code{treat}, then the right hand side of the formula must include the term +\code{trt(treat)}.}

  \item{data}{a data.frame in which to interpret the variables named in the formula.}

  \item{subset}{expression indicating which subset of the rows of data should be included. All observations are included by default.}

  \item{na.action}{a missing-data filter function. This is applied to the model.frame after any subset argument has been used. Default is \code{na.action = na.pass}.}
 
   \item{nbins}{the number of bins created from numeric predictors. The bins are created based on quantiles, with a default value of 4 (quartiles).}

 \item{continuous}{specifies the threshold for when a variable is considered to be continuous (when there are at least \code{continuous} unique values). The default is 4. Factor variables are always considered to be categorical no matter how many levels they have.}

   \item{direction}{possible values are \code{1} (default) if uplift should be computed as the difference in the average response between treatment and control, or \code{2} between control and treatment. This only affects the uplift calculation as produced in the output.}

}

\value{
A list of matrices, one for each variable. The columns represent: the number of responses over the control group, the number of the responses over the treated group, the average response for the control, the average response for the treatment, and the uplift (difference between treatment and control average response).
}
\author{
Leo Guelman <leo.guelman@gmail.com>
}

\examples{
library(uplift)

set.seed(12345)
dd <- sim_pte(n = 1000, p = 20, rho = 0, sigma =  sqrt(2), beta.den = 4)
dd$treat <- ifelse(dd$treat == 1, 1, 0) 

eda <- explore(y ~ X1 + X2 + X3 + X4 + X5 + X6 + trt(treat), data = dd)            
}
\keyword{uplift}
\keyword{trees}

