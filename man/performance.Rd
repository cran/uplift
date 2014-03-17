\name{performance}
\alias{performance}
\title{Performance Assessment for Uplift Models
}
\description{Provides a method for assessing performance for uplift models.
}
\usage{
performance(pr.y1_ct1, pr.y1_ct0, y, ct, direction = 1, groups = 10)
}
\arguments{
  \item{pr.y1_ct1}{the predicted probability \eqn{Prob(y=1|treated, x)}.}
  \item{pr.y1_ct0}{the predicted probability \eqn{Prob(y=1|control, x)}.}
  \item{y}{the actual observed value of the response.}
  \item{ct}{a binary (numeric) vector representing the treatment assignment (coded as 0/1).}
  \item{direction}{possible values are \code{1} (default) if the objective is to maximize the difference in the response for Treatment minus Control, and \code{2} for Control minus Treatment.} 
  \item{groups}{number of groups of equal observations in which to partition the data set to show results. The default value is 10 (deciles). Other possible values are 5 and 20.}
}
\details{
Model performance is estimated by: 1. computing the difference in the predicted conditional class probabilities \eqn{Prob(y=1|treated, x)} and \eqn{Prob(y=1|control, x)}, 2. ranking the difference and grouping it into 'buckets' with equal number of observations each, and 3. computing the actual difference in the mean of the response variable between the treatment and the control groups for each bucket.
}
\value{
An object of class \code{performance}, which is a matrix  with the following columns: (\code{group}) the number of groups, (\code{n.ct1}) the number of observations in the treated group, (\code{n.ct0}) the number of observations in the control group, (\code{n.y1_ct1}) the number of observation in the treated group with response = 1, (\code{n.y1_ct0}) the number of observation in the control group with response = 1, (\code{r.y1_ct1}) the mean of the response for the treated group, (\code{r.y1_ct0}) the mean of the response for the control group, and (\code{uplift}) the difference between \code{r.y1_ct1} and \code{r.y1_ct0} (if \code{direction = 1}).
}
\references{
Guelman, L., Guillen, M., and Perez-Marin A.M. (2013). Uplift random forests. Cybernetics & Systems, \emph{forthcoming}.
}
\author{
Leo Guelman <leo.guelman@gmail.com>
}

\examples{
library(uplift)

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

### get variable importance 

varImportance(fit1, plotit = TRUE, normalize = TRUE)

### predict on new data 

dd_new <- sim_pte(n = 1000, p = 20, rho = 0, sigma =  sqrt(2), beta.den = 4)
dd_new$treat <- ifelse(dd_new$treat == 1, 1, 0)  

pred <- predict(fit1, dd_new)

### evaluate model performance

perf <- performance(pred[, 1], pred[, 2], dd_new$y, dd_new$treat, direction = 1)
plot(perf[, 8] ~ perf[, 1], type ="l", xlab = "Decile", ylab = "uplift")  
}
\keyword{uplift}
\keyword{trees}
