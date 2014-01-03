\name{ccif}
\alias{ccif}
\alias{ccif.default}
\alias{ccif.formula}
\alias{print.ccif}
\alias{summary.ccif}

\title{Causal Conditional Inference Forest}

\description{\code{ccif} implements recursive partitioning in a causal conditional inference framework.}

\usage{

\method{ccif}{formula}(formula, data, ...)

\method{ccif}{default}(
x, 
y, 
ct, 
mtry = floor(sqrt(ncol(x))), 
ntree = 100, 
split_method = c("ED", "Chisq", "KL", "L1", "Int"), 
interaction.depth = NULL, 
pvalue = 0.05, 
bonferroni = FALSE, 
minsplit = 20, 
minbucket_ct0 = round(minsplit/4),
minbucket_ct1 = round(minsplit/4), 
keep.inbag = FALSE, 
verbose = FALSE, 
...)

\method{print}{ccif}(x, ...)

}

%- maybe also 'usage' for other objects documented here.
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
implies a model with up to 2-way interactions, etc.}

\item{pvalue}{the maximum acceptable pvalue required in order to make a split.}
  
\item{bonferroni}{apply a bonferroni adjustment to pvalue.}

\item{minsplit}{the minimum number of observations that must exist in a node in order for a split to be attempted.}
 
\item{minbucket_ct0}{the minimum number of control observations in any terminal <leaf> node.}

\item{minbucket_ct1}{the minimum number of treatment observations in any terminal <leaf> node.} 

\item{keep.inbag}{if set to \code{TRUE}, an nrow(x) by ntree matrix is returned, whose entries are the "in-bag" samples in each tree.}

\item{verbose}{print status messages?}

\item{\dots}{Additional arguments passed to \code{independence_test{coin}}. See details.}

}

\details{
Causal conditional inference trees estimate \emph{personalized treatment effects} (a.k.a. uplift) by binary recursive partitioning in a conditional inference framework. Roughly, the algorithm works as follows: 1)
For each terminal node in the tree we test the global null hypothesis of no interaction effect between the treatment \eqn{T} and any of the \eqn{n} covariates selected at random from the set of \eqn{p} covariates (\eqn{n \leq p}). Stop if this hypothesis cannot be rejected. Otherwise select the input variable with strongest interaction effect. The interaction effect is measured by a p-value corresponding to a permutation test (Strasser and Weber, 1999) for the partial null hypothesis of independence between each input variable and a transformed response. Specifically, the response is transformed so the impact of the input variable on the response has a causal interpretation for the treatment effect (see details in Guelman et al. 2013) 2) Implement a binary split in the selected input variable. 3) Recursively repeate steps 1) and 2).

The independence test between each input and the transformed response is performed by calling \code{independence_test{coin}}. Additional arguments may be passed to this function via `\eqn{\ldots}'. 

All split methods are described in Guelman et al. (2013a, 2013b).

This function is very slow at the moment. It was built as a prototype in R. A future version of this package will provide an interface to C++ for this function, which is expected to significantly improve speed. 

}


\value{
 An object of class \code{ccif}, which is a list with the
  following components:

\item{call}{the original call to \code{ccif}}
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

Hothorn, T., Hornik, K. and Zeileis, A. (2006).Unbiased recursive partitioning: A conditional inference framework. Journal of Computational and Graphical Statistics,  15(3): 651-674.

Strasser, H. and Weber, C. (1999). On the asymptotic theory of permutation statistics. Mathematical Methods of Statistics, 8: 220-250.

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
             distribution = approximate (B=999),
             pvalue = 0.05,
             verbose = TRUE)
print(fit1)
summary(fit1)
}
