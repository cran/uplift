\name{upliftRF}
\alias{upliftRF}
\alias{upliftRF.default}
\alias{upliftRF.formula}
\alias{print.upliftRF}
\alias{summary.upliftRF}

\title{Uplift Random Forests}

\description{
\code{upliftRF} implements Random Forests with split criteria designed for binary uplift modeling tasks.
}

\usage{

\method{upliftRF}{formula}(formula, data, ...)

\method{upliftRF}{default}(
x,  
y,  
ct, 
mtry = floor(sqrt(ncol(x))),
ntree = 100, 
split_method = c("ED", "Chisq", "KL", "L1", "Int"),
interaction.depth = NULL,
bag.fraction = 0.5,
minsplit = 20, 
minbucket_ct0 = round(minsplit/4), 
minbucket_ct1 = round(minsplit/4), 
keep.inbag = FALSE,
verbose = FALSE,
...)  

\method{print}{upliftRF}(x, ...)

}
\arguments{
\item{data}{A data frame containing the variables in the model. It should include 
            a variable reflecting the binary treatment assignment of each observation (coded as 0/1).}
\item{x, formula}{a data frame of predictors or a formula describing the model to be fitted. A special term of the form \code{trt()} must be used in the model equation to identify the binary treatment variable. For example, if the treatment is represented by a variable named \code{treat}, then the right hand side of the formula must include the term +\code{trt(treat)}.}
\item{y}{a binary response (numeric) vector.}
\item{ct}{a binary (numeric) vector representing the treatment assignment (coded as 0/1).}
\item{mtry}{the number of variables to be tested in each node;  the default is floor(sqrt(ncol(x))).}
\item{ntree}{the number of trees to generate in the forest; default is ntree = 100.}
\item{split_method}{the split criteria used at each node of each tree; Possible values are: "ED" (Euclidean distance), "Chisq" (Chi-squared divergence), "KL" (Kullback-Leibler divergence), "Int" (Interaction method).}
\item{interaction.depth}{The maximum depth of variable interactions. 1 implies an additive model, 2
implies a model with up to 2-way interactions, etc. The default is to grow trees to maximal depth, constrained on the arguments specified in \code{minsplit} and \code{minbucket}.}
\item{bag.fraction}{the fraction of the training set observations randomly selected for the purpose of fitting each tree in the forest.}
\item{minsplit}{the minimum number of observations that must exist in a node in order for a split to be attempted.}
\item{minbucket_ct0}{the minimum number of control observations in any terminal <leaf> node.}
\item{minbucket_ct1}{the minimum number of treatment observations in any terminal <leaf> node.} 
\item{keep.inbag}{if set to \code{TRUE}, an nrow(x) by ntree matrix is returned, whose entries are the "in-bag" samples in each tree.}
\item{verbose}{print status messages?}
\item{...}{optional parameters to be passed to the low level function upliftRF.default.}
}

\details{
Uplift Random Forests estimate \emph{personalized treatment effects} (a.k.a. uplift) by binary recursive partitioning. The algorithm and split methods are described in Guelman et al. (2013a, 2013b).
}
 
\value{
 An object of class \code{upliftRF}, which is a list with the
  following components:

\item{call}{the original call to \code{upliftRF}}
\item{trees}{the tree structure that was learned}
\item{split_method}{the split criteria used at each node of each tree}
\item{ntree}{the number of trees used} 
\item{mtry}{the number of variables tested at each node}  
\item{var.names}{a character vector with the name of the predictors} 
\item{var.class}{a character vector containing the class of each predictor variable}
\item{inbag}{an nrow(x) by ntree matrix showing the in-bag samples used by each tree}
}   
 

\references{
Guelman, L., Guillen, M., and Perez-Marin A.M. (2013a). Uplift random forests. Cybernetics & Systems, \emph{forthcoming}.

Guelman, L., Guillen, M., and Perez-Marin A.M. (2013b). Optimal personalized treatment rules for marketing interventions: A review of methods, a new proposal, and an insurance case study. \emph{Submitted}.

Su,  X., Tsai, C., Wang, H., Nickerson, D., and Li, B. (2009). Subgroup Analysis via Recursive Partitioning. Journal of Machine Learning Research,  10,  141-158.
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
summary(fit1)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{uplift}
\keyword{tree}% __ONLY ONE__ keyword per line
