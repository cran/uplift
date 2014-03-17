\name{predict.upliftRF}
\alias{predict.upliftRF}
\title{Predictions from a Fitted Uplift Random Forest Model}
\description{
prediction of new data using uplift random forest.
}
\usage{
\method{predict}{upliftRF}(object, newdata, n.trees = object$ntree, predict.all = FALSE, ...)
}
\arguments{
  \item{object}{an object of class \code{upliftRF}, as that
    created by the function \code{upliftRF}.}
  \item{newdata}{a data frame containing the values at which predictions are required.}
  \item{n.trees}{number of trees used in the prediction; The default is \code{object$ntree}.}
  \item{predict.all}{should the predictions of all trees be kept?}
  \item{\dots}{not used.}	
}
\details{
At the moment, all predictors passed for fitting the uplift model must also be present in \code{newdata}, even if they are not used as split variables by any of the trees in the forest.
}
\value{
If \code{predict.all = FALSE}, a matrix of predictions containing the conditional class probabilities: \code{pr.y1_ct1} represents \eqn{Prob(y=1|treated, x)} and \code{pr.y1_ct0} represents \eqn{Prob(y=1|control, x)}. This is computed as the average of the individual predictions over all trees.

If \code{predict.all = TRUE}, the returned object is a list with two
components: \code{pred.avg} is the prediction (as described above) and \code{individual} is a list of matrices containing the individual predictions from each tree.
}
 
\references{
Guelman, L., Guillen, M., and Perez-Marin A.M. (2013). Uplift random forests. Cybernetics & Systems, \emph{forthcoming}.
}

\author{
Leo Guelman <leo.guelman@gmail.com>
}

\seealso{
\code{\link{upliftRF}}
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
summary(fit1)

### predict on new data 

dd_new <- sim_pte(n = 2000, p = 20, rho = 0, sigma =  sqrt(2), beta.den = 4)
dd_new$treat <- ifelse(dd_new$treat == 1, 1, 0)  

pred <- predict(fit1, dd_new)     
head(pred)

}
\keyword{uplift}
\keyword{trees}% __ONLY ONE__ keyword per line
