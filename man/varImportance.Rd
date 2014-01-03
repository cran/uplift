\name{varImportance}
\alias{varImportance}
\alias{varImportance.default}
\alias{varImportance.upliftRF}
\title{Extract Variable Importance from \code{upliftRF} or \code{ccif} Fitted Objects
}
\description{
This is the extractor function for variable importance of predictors.
}

\usage{
\method{varImportance}{upliftRF}(x, n.trees = x$ntree, plotit = TRUE, normalize = TRUE, ...)
}

\arguments{
  \item{x}{an object of class \code{\link{upliftRF}} or \code{\link{ccif}}.}
  \item{n.trees}{number of trees used in the prediction; The default is \code{x$ntree}.} 	
  \item{plotit}{plot variable importance?}
  \item{normalize}{if set to \code{TRUE}, the importance is scaled to add up to 100.}
  \item{\dots}{additional arguments passed to \code{barplot}.}
}
\details{
At each split in each tree, the improvement in the split-criterion is the
importance measure attributed to the splitting variable, and is accumulated
over all the trees in the forest separately for each variable.
} 
\value{
A numeric vector with the variable importance.
}
\references{
Guelman, L., Guillen, M., and Perez-Marin A.M. (2013). Uplift random forests. Cybernetics & Systems, \emph{forthcoming}.
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
                 minsplit = 200, 
                 verbose = TRUE)
print(fit1)

### get variable importance 

varImportance(fit1, plotit = TRUE, normalize = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{uplift}
\keyword{trees}% __ONLY ONE__ keyword per line
