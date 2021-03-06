% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fixef_vicc.R
\name{fixef.vicc}
\alias{fixef.vicc}
\alias{fixef}
\title{Extract Fixed Effects}
\usage{
\method{fixef}{vicc}(object, cred = 0.9, ...)
}
\arguments{
\item{object}{An object of class \code{vicc}.}

\item{cred}{Numeric. Credible interval width (defaults to \code{0.90})}

\item{...}{Currently ignored.}
}
\value{
Summarized fixed effects
}
\description{
Summarize the fixed effects.
}
\examples{
\donttest{
# data
Y <- flanker

# congruent trials
congruent <- subset(Y, cond == 0)

# subset 25 from each group
dat <- congruent[unlist(tapply(1:nrow(congruent),
                            congruent$id,
                            head, 25)), ]

fit <- vicc(
  y  = dat$rt,
  group = dat$id,
  iter = 250,
  burnin = 10,
  type =  "pick_none"
  )

fixef(fit)
}
}
