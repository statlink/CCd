\name{loc0.test}
\alias{loc0.test}

\title{
Log-likelihood ratio test for zero location parameter
}

\description{
Log-likelihood ratio test for zero location parameter.
}

\usage{
loc0.test(y, tol = 1e-7)
}

\arguments{
\item{y}{
A vector with integer values.
}
\item{tol}{
The tolerance value to terminate the maximization algorithm.
}
}

\details{
The probability density function of the generalized Poisson distribution is the following (Nikoloulopoulos & Karlis, 2008):
\deqn{
P(Y=y|\theta, \lambda)=\theta(\theta+\lambda y)^{y-1}\frac{e^{-\theta-\lambda y}}{y!},
\ \ y=0,1... \ \ \theta >0, \ \ 0 \leq \lambda \leq 1.
}
To ensure that \eqn{\theta} is positive we use the "log" link and for \eqn{\lambda} to lie within 0 and 1
we use the "logit" link within the \code{\link{optim}} function.
}

\value{
A vector with the test statistic and its associated p-value.
}

\references{
Papadatos N. (2022). The characteristic function of the discrete Cauchy distribution In Memory of T. Cacoullos.
Journal of Statistical Theory and Practice, 16(3): 47.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

\seealso{
\code{ \link{cc.mle}, \link{dcc}
}
}

\examples{
y <- round( rcauchy(100, 3, 10) )
loc0.test(y)

y <- round( rcauchy(100, 0, 10) )
loc0.test(y)
}


