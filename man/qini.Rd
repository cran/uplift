\name{qini}
\alias{qini}
\alias{qini.default}
\alias{qini.performance}

\title{Computes the Qini Coefficient Q
}
\description{This function computes the Qini coefficient from a \code{performance} object (as created by the function \code{performance}). 
}
\usage{
\method{qini}{performance}(x, direction = 1, plotit = TRUE, ...)
}
\arguments{
  \item{x}{an object of class \code{performance}.}
  \item{direction}{possible values are \code{1} (default) if the objective is to maximize the difference in the response for Treatment minus Control, and \code{2} for Control minus Treatment.}
  \item{plotit}{plot the incremental gains from the fitted model?}
  \item{\dots}{additional arguments passed to \code{plot}.}
}
\details{
Qini coefficients represent a natural generalizations of the Gini coefficient to the case of uplift. Qini is defined as the area between the actual incremental gains curve from the fitted model and the area under the diagonal corresponding to random targeting. See the references for details. 
}
\value{
A list with the following components
\item{Qini}{the Qini coefficient as defined above.}
\item{inc.gains}{the incremental gain values from the fitted model.}
\item{random.inc.gains}{the random incremental gains.}
}

\references{
Radcliffe, N. and Surry, P. (2011). Real-World Uplift Modelling with Significance-Based Uplift Trees. Portrait Technical Report, TR-2011-1.

Radcliffe, N. (2007). Using control groups to target on predicted lift: Building and assessing uplift models. Direct Marketing Analytics Journal, An Annual Publication from the Direct Marketing Association Analytics Council, pages 14-21.
}
\author{
Leo Guelman <leo.guelman@gmail.com>
}

\examples{
library(uplift)

### simulate data for uplift modeling

set.seed(123)
dd <- sim_pte(n = 1000, p = 20, rho = 0, sigma =  sqrt(2), beta.den = 4)
dd$treat <- ifelse(dd$treat == 1, 1, 0) 

### fit uplift random forest

fit1 <- upliftRF(y ~ X1 + X2 + X3 + X4 + X5 + X6 + trt(treat),
                 data = dd, 
                 mtry = 3,
                 ntree = 100, 
                 split_method = "KL",
                 minsplit = 200, # need small trees as there is strong uplift effects in the data
                 verbose = TRUE)
print(fit1)
summary(fit1)

### predict on new data 

dd_new <- sim_pte(n = 2000, p = 20, rho = 0, sigma =  sqrt(2), beta.den = 4)
dd_new$treat <- ifelse(dd_new$treat == 1, 1, 0)  

pred <- predict(fit1, dd_new)

### evaluate model performance

perf <- performance(pred[, 1], pred[, 2], dd_new$y, dd_new$treat, direction = 1)

### compute Qini coefficient

Q <- qini(perf, plotit = TRUE)
Q
}
\keyword{uplift}
\keyword{tree}
