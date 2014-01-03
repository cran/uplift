\name{sim_pte}
\alias{sim_pte}

\title{Simulations for Personalized Treatment Effects
}

\description{
Numerical simulation for treatment effect heterogeneity estimation as described in Tian et al. (2012)
}

\usage{
sim_pte(n = 1000, p = 20, rho = 0, sigma = sqrt(2), beta.den = 4)
}

\arguments{
  \item{n}{number of observations.}
  \item{p}{number of predictors.}
  \item{rho}{covariance between predictors.}
  \item{sigma}{multiplier of error term.}
  \item{beta.den}{size of main effects relative to interaction effects. See details.}
}

\details{

\code{sim_pte} simulates data according to the following specification:

\deqn{Y = I(\sum_{j=1}^p \beta_{j}X_{j} + \sum_{j=1}^p \gamma_{j}X_{j}T +\sigma_{0}\epsilon > 0)},

where \eqn{\gamma=(1/2,-1/2,1/2,-1/2, 0,...,0)}, \eqn{\beta=(-1)^{j+1}I(3 \leq j \leq 10) / \code{beta.den}}, \eqn{(X_{1}, \ldots, X_{p})} follows a mean zero multivariate normal distribution with a compound symmetric 
variance-covariance matrix, \eqn{(1-\rho)\mathbf{I}_{p} +\rho \mathbf{1}^{T}\mathbf{1}}, \eqn{T=[-1,1]} is the treatment indicator and \eqn{\epsilon} is \eqn{N(0,1)}.

In this case, the "true" treatment effect score \eqn{(Prob(Y=1|T=1) - Prob(Y=1|T=-1))} is given by 

\deqn{\Phi (\frac{\sum_{j=1}^p (\beta_{j} + \gamma_{j})X_{j}}{\sigma_{0}}) - \Phi (\frac{\sum_{j=1}^p (\beta_{j} - \gamma_{j})X_{j}}{\sigma_{0}})}.

}

\value{
A data frame including the response variable (\eqn{Y}), the treatment (\code{treat=1}) and control (\code{treat=-1}) assignment, the predictor variables (\eqn{X}) and the "true" treatment effect score (\code{ts})
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
### Simulate train data

set.seed(12345)
dd <- sim_pte(n = 1000, p = 10, rho = 0, sigma =  sqrt(2), beta.den = 4)
dd$treat <- ifelse(dd$treat == 1, 1, 0) # required coding for upliftRF

### Fit model

form <- as.formula(paste('y ~', 'trt(treat) +', paste('X', 1:10, sep = '', collapse = "+"))) 

fit1 <- upliftRF(formula = form,
                 data = dd, 
                 ntree = 100, 
                 split_method = "Int",
                 interaction.depth = 3,
                 minsplit = 100, 
                 minbucket_ct0 = 50, 
                 minbucket_ct1 = 50,
                 verbose = TRUE)
summary(fit1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{uplift}
\keyword{personalized treatment learning}
