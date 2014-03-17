\name{upliftKNN}
\alias{upliftKNN}

\title{Uplift k-Nearest Neighbor}

\description{
\code{upliftKNN} implements k-nearest neighbor for uplift modeling. 
}
\usage{
upliftKNN(train, test, y, ct, k = 1, dist.method = "euclidean", 
          p = 2, ties.meth = "min", agg.method = "mean")
}

\arguments{
  \item{train}{a matrix or data frame of training set cases.}
  \item{test}{a matrix or data frame of test set cases. A vector will be interpreted as a   row vector for a single case.}
  \item{y}{a numeric response variable (must be coded as 0/1 for binary response).}
  \item{ct}{factor or numeric vector representing the treatment to which each train case is assigned. At least 2 groups are required (e.g. treatment and control). Multi-treatments are also supported.}
  \item{k}{number of neighbors considered.}
  \item{dist.method}{the distance to be used in calculating the neighbors. Any method supported in function \code{\link{dist}} is valid.}
  \item{p}{the power of the Minkowski distance.}
  \item{ties.meth}{method to handle ties for the kth neighbor. The default is "min" which uses all ties. Alternatives include "max" which uses none if there are ties for the k-th nearest neighbor, "random" which selects among the ties randomly and "first" which uses the ties in their order in the data.}
  \item{agg.method}{method to combine responses of the nearest neighbors, defaults to "mean". The alternative is "majority".}
}
\details{
k-nearest neighbor for uplift modeling for a test set from a training set. For each case in the test set, the k-nearest training set vectors for each treatment type are found. The response value for the k-nearest training vectors is aggregated based on the function specified in \code{agg.method}. For "majority", classification is decided by majority vote (with ties broken at random).
}
\value{
A matrix of predictions for each test case and value of \code{ct}
}

\note{
The code logic follows closely the \code{knn} and \code{knnflex} packages, the later currently discontinued from CRAN.
}

\references{
Su, X., Kang, J., Fan, J., Levine, R. A., and Yan, X. (2012). Facilitating score and
causal inference trees for large observational studies. Journal of Machine Learning
Research, 13(10): 2955-2994.

Guelman, L., Guillen, M., and Perez-Marin A.M. (2013). Optimal personalized treatment rules for marketing interventions: A review of methods, a new proposal, and an insurance case study. \emph{Submitted}.
}

\author{
Leo Guelman <leo.guelman@gmail.com>}

\examples{
library(uplift)

### simulate data for uplift modeling

set.seed(1)

train <- sim_pte(n = 500, p = 10, rho = 0, sigma =  sqrt(2), beta.den = 4)
train$treat <- ifelse(train$treat == 1, 1, 0) 

### Fit an Uplift k-Nearest Neighbor on test data

test <- sim_pte(n = 100, p = 10, rho = 0, sigma =  sqrt(2), beta.den = 4)
test$treat <- ifelse(test$treat == 1, 1, 0) 

fit1 <- upliftKNN(train[, 3:8], test[, 3:8], train$y, train$treat, k = 1, 
          dist.method = "euclidean", p = 2, ties.meth = "min",   agg.method = "majority")
head(fit1)          
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{uplift}
\keyword{tree}% __ONLY ONE__ keyword per line
