#' @title Posterior Inclusion Probabilities
#'
#' @description Extract the posterior inclusion probabilities (PIP) for either the
#'              random intercepts for sigma or the random effects standard deviation
#'              for sigma.
#'
#' @param object Ab object of class \code{vicc}.
#'
#' @param ... Currently ignored.
#'
#' @return A data frame.
#'
#' @note The PIPs indicate whether the groups differ from the fixed effect, or average,
#'       within-group variance. If the PIP is large, this indicates there is high probability
#'       that group differs from the common variance. A marginal Bayes factor can be computed
#'       as PIP / (1 - PIP), assuming that \code{prior_prob = 0.5}.
#'
#' @export
#'
#' @examples
#' \donttest{
#'
#' # congruent trials
#' congruent <- subset(flanker, cond == 0)
#'
#' # subset 25 from each group
#' dat <- congruent[unlist(tapply(1:nrow(congruent),
#'                             congruent$id,
#'                             head, 25)), ]
#'
#' # fit model
#' fit <- vicc(y  = dat$rt,
#'             group = dat$id,
#'             iter = 250,
#'             burnin = 10,
#'             type =  "pick_group")
#'
#' pip(fit)
#' }
pip <- function(object, ...){

  samps <- posterior_samples(object)

  if (object$type == "pick_group") {
    J <- object$model$data()$J
    pips <-
      colMeans(samps[, grep("pick_id", x = colnames(samps))])
    pip_summary <-
      data.frame(Parameter =  paste0("RE_", 1:J),
                 Group = 1:J,
                 PIP = pips)
    row.names(pip_summary) <- NULL

  } else if (object$type == "pick_tau") {
    pips <- mean(samps[, grep("pick_tau", x = colnames(samps))])

    pip_summary <- data.frame(Param = "RE_sd_sigma", PIP = pips)
    row.names(pip_summary) <- NULL

  } else {
    stop("type not supported.")
  }
  returned_object <- list(pip_summary = pip_summary)
  class(returned_object) <- "pip"
  return(returned_object)
}


#' Print \code{pip} Objects
#'
#' @param x An object of class \code{pip}.
#' @param ... Currently ignored.
#'
#' @export
print.pip <- function(x, ...){
  cat("Posterior Inclusion Probabilities:\n\n")
  print(x$pip_summary, row.names = FALSE, right = FALSE)
  cat("\n")
  cat("------")
}
