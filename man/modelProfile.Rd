\name{modelProfile}
\alias{modelProfile}

\title{
Profile a Fitted Uplift Model
}
\description{This function can be used to profile a fitted uplift model. Given a vector of scores (uplift predictions), it computes basic summary statistics for each predictor by score quantile. 
}
\usage{
modelProfile(formula, data, groups = 10, 
group_label = c("I", "D"), digits_numeric = 1, digits_factor = 4,
exclude_na = FALSE, LaTex = FALSE)
}

\arguments{
  \item{formula}{a formula expression of the form score ~ predictors, where the LHS of the model formula should include the predictions from a fitted model. 
}
  \item{data}{a data.frame in which to interpret the variables named in the formula.
}
  \item{groups}{number of groups of equal observations in which to partition the data set to show results. The default value is 10 (deciles). Other possible values are 5 and 20.
}
  \item{group_label}{possible values are \code{"I"} or \code{"D"}, for group number labels which are increasing or decreasing with the model score, respectively. 
}
  \item{digits_numeric}{number of digits to show for numeric predictors.
}
  \item{digits_factor}{number of digits to show for factor predictors.
}
  \item{exclude_na}{should the results exclude observations with missing values in any of the variables named in the formula?
}
  \item{LaTex}{should the function output LaTex code?
}
}
\details{
This function ranks the variable supplied in the LHS of the model formula and classifies it into groups of equal number of observations. It subsequently calls the function \code{tabular} from the \code{tables} package to compute the average of each numeric predictor and the distribution of each factor within each group.
}
\value{
An object of S3 class \code{tabular}. See \code{help("tabular")} in the \code{tables} package for details. 
}

\author{
Leo Guelman <leo.guelman@gmail.com>
}

\examples{

library(uplift)

### Simulate data
set.seed(12345)
dd <- sim_pte(n = 1000, p = 5, rho = 0, sigma =  sqrt(2), beta.den = 4)
dd$treat <- ifelse(dd$treat == 1, 1, 0) # required coding for upliftRF

### Fit upliftRF model
fit1 <- upliftRF(y ~ X1 + X2 + X3 + X4 + X5 + trt(treat),
                 data = dd, 
                 mtry = 3,
                 ntree = 50, 
                 split_method = "KL",
                 minsplit = 100, 
                 verbose = TRUE)

### Fitted values on train data
pred <- predict(fit1, dd)

### Compute uplift predictions
uplift_pred <- pred[, 1] - pred[, 2]

### Put together data, predictions and add some dummy factors for illustration only
dd2 <- data.frame(dd, uplift_pred, F1 = gl(2, 50, labels = c("A", "B")), 
                  F2 = gl(4, 25, labels = c("a", "b", "c", "d")))
                     
### Profile data based on fitted model
modelProfile(uplift_pred ~ X1 + X2 + X3 + F1 + F2, 
             data = dd2, 
             groups = 10,
             group_label = "D", 
             digits_numeric = 2,
             digits_factor = 4,
             exclude_na = FALSE, 
             LaTex = FALSE)

}

\keyword{tables}
