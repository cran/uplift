\name{checkBalance}
\alias{checkBalance}

\title{Standardized Differences for Stratified Comparisons}

\description{
This function is simply a wrapper for \code{xBalance{RItools}}. Given covariates, a treatment variable, and (optionally) a stratifying factor, it calculates standardized mean differences along each covariate, and tests for conditional independence of the treatment variable and the covariates.}

\usage{
checkBalance(formula, data, report = "all", ...)
}

\arguments{
  \item{formula}{A formula containing an indicator of treatment assignment on the left hand side and covariates at right.}
  \item{data}{A data frame in which the formula and (optionally) strata are to be evaluated.}
  \item{report}{Character vector listing measures to report for each stratification; a subset of
  c("adj.means","adj.mean.diffs", "adj.mean.diffs.null.sd", "chisquare.test",
"std.diffs","z.scores","p.values","all").
P-values reported are two-sided for the null-hypothesis of no effect. 
 The option "all" requests all measures.}
  \item{\dots}{Additional arguments passed to \code{xBalance{RItools}}.}
}

\details{
See \code{help("xBalance")} for details. 
}

\value{
An object of class \code{c("xbal", "list")}. There are \code{plot}, \code{print}, and \code{xtable} methods for class \code{xbal}.
}

\references{
Hansen, B.B. and Bowers, J. (2008). Covariate Balance in Simple, Stratified and Clustered Comparative Studies. Statistical Science 23.

Kalton, G. (1968). Standardization: A technique to control for extraneous variables. Applied Statistics 17, 118-136.
}

\author{
Leo Guelman <leo.guelman@gmail.com>
}

\note{
Evidence pertaining to the hypothesis that a treatment variable is not associated with differences in covariate values is assessed by comparing the differences of means (or regression coefficients), without standardization, to their distributions under hypothetical shuffles of the treatment variable, a permutation or randomization distribution. For the unstratified comparison, this reference distribution consists of differences (more generally, regression coefficients) when the treatment variable is permuted without regard to strata. For the stratified comparison, the reference distribution is determined by randomly permuting the treatment variable within strata, then re-calculating the treatment-control differences (regressions of each covariate on the permuted treatment variable). Significance assessments are based on the large-sample Normal approximation to these reference distributions.
}


\examples{
library(uplift)

set.seed(12345)
dd <- sim_pte(n = 1000, p = 20, rho = 0, sigma =  sqrt(2), beta.den = 4)
dd$treat <- ifelse(dd$treat == 1, 1, 0) 

checkBalance(treat ~ X1 + X2 + X3 + X4 + X5 + X6 , data = dd)
}

\keyword{uplift}
\keyword{xBalance}