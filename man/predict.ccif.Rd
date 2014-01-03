\name{predict.ccif}
\alias{predict.ccif}

\title{Predictions from a Fitted Causal Conditional Inference Forest Model}

\description{
prediction of new data using causal conditional inference forest.
}

\usage{
\method{predict}{ccif}(object, newdata, n.trees = object$ntree, predict.all = FALSE, ...)
}

\arguments{
  \item{object}{an object of class \code{ccif}, as that
    created by the function \code{ccif}.}
  \item{newdata}{a data frame containing the values at which predictions are required.}
  \item{n.trees}{number of trees used in the prediction; The default is \code{object$ntree}.}
  \item{predict.all}{should the predictions of all trees be kept?}
  \item{\dots}{not used.}  
}

\details{At the moment, all predictors passed for fitting the uplift model must also be present in \code{newdata}, even if they are not used as split variables by any of the trees in the forest.
}

\value{
If \code{predict.all = FALSE}, a matrix of predictions containing the conditional class probabilities: \code{pr.y1_ct1} represents \eqn{Prob(y=1|treated, x)} and \code{pr.y1_ct0} represents \eqn{Prob(y=1|control, x)}. This is computed as the average of the individual predictions over all trees.

If \code{predict.all = TRUE}, the returned object is a list with two
components: \code{pred.avg} is the prediction (as described above) and \code{individual} is a list of matrices containing the individual predictions from each tree.
}

\references{
Guelman, L., Guillen, M., and Perez-Marin A.M. (2013). Optimal personalized treatment rules for marketing interventions: A review of methods, a new proposal, and an insurance case study. \emph{Submitted}.
}

\author{
Leo Guelman <leo.guelman@gmail.com>
}

\examples{
library(uplift)

### Simulate train data

set.seed(12345)
dd <- sim_pte(n = 100, p = 6, rho = 0, sigma =  sqrt(2), beta.den = 4)
dd$treat <- ifelse(dd$treat == 1, 1, 0) 

### Fit model

form <- as.formula(paste('y ~', 'trt(treat) +', paste('X', 1:6, sep = '', collapse = "+"))) 

fit1 <- ccif(formula = form,
             data = dd, 
             ntree = 50, 
             split_method = "Int",
             pvalue = 0.05,
             verbose = TRUE)

### Predict on new data

dd_new <- sim_pte(n = 200, p = 20, rho = 0, sigma =  sqrt(2), beta.den = 4)

pred <- predict(fit1, dd_new)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{uplift}
\keyword{trees}% __ONLY ONE__ keyword per line
