\name{cc.mle}
\alias{cc.mle}
\alias{cc.mle0}

\title{
Maximum likelihood estimation of the CC distribution
}

\description{
Maximum likelihood estimation of the CC distribution.
}

\usage{
cc.mle(y, tol = 1e-7)
cc.mle0(y, tol = 1e-7)
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
A vector with three numbers, the \eqn{\theta} and \eqn{\lambda} parameters and the value of the log-likelihood.
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
\code{ \link{loc0.test}, \link{dcc}
}
}

\examples{
y <- round( rcauchy(100, 3, 10) )
cc.mle(y)

y <- round( rcauchy(100, 0, 10) )
cc.mle0(y)
}


