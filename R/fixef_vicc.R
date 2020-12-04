#' Extract Fixed Effects
#'
#' @aliases fixef
#'
#' @description Summarize the fixed effects.
#'
#' @param object An object of class \code{vicc}.
#'
#' @param cred Numeric. Credible interval width (defaults to \code{0.90})
#'
#' @param ... Currently ignored.
#'
#' @return Summarized fixed effects
#'
#' @method fixef vicc
#' @export
#' @export fixef
#' @importFrom nlme fixef
#'
#' @examples
#' \donttest{
#' Y <- flanker
#' # congruent trials
#' congruent <- subset(Y, cond == 0)
#'
#' # subset 25 from each group
#' dat <- congruent[unlist(tapply(1:nrow(congruent),
#'                             congruent$id,
#'                             head, 25)), ]
#'
#' fit <- vicc(
#'   y  = dat$rt,
#'   group = dat$id,
#'   iter = 250,
#'   burnin = 10,
#'   type =  "pick_none"
#'   )
#'
#' fixef(fit)
#' }
fixef.vicc <- function(object, cred = 0.95, ...) {

  samps <- posterior_samples(object)

  lb <- (1 - cred) / 2
  ub <-  1 - lb

  if (object$type == "customary") {
    fixef_summary <-  cbind(mean(samps$fe_mu),
                                  sd(samps$fe_mu),
                                  t(quantile(
                                    samps$fe_mu, probs = c(lb, ub)
                                  )))
    row.names(fixef_summary) <- c("intercept_mean")

    } else {

      fixef_summary <-  rbind(cbind(mean(samps$fe_mu),
                                    sd(samps$fe_mu),
                                    t(quantile(
                                      samps$fe_mu, probs = c(lb, ub)
                                    ))),
                              cbind(mean(exp(samps$fe_sd)),
                                    sd(exp(samps$fe_sd)),
                                    t(quantile(
                                      exp(samps$fe_sd), probs = c(lb, ub)
                                    ))))

      row.names(fixef_summary) <- c("intercept_mean",
                                   "intercept_sigma")


  }
  colnames(fixef_summary) <- c("Post.mean",
                               "Post.sd",
                               "Cred.lb",
                               "Cred.ub")
  return(fixef_summary)
}
