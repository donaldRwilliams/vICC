#' @title  Extract Posterior Samples
#'
#' @description Extract posterior samples for \code{vicc} objects
#'
#' @param object An object of class \code{vicc}
#'
#' @return An object of class  \code{data.frame}
#'
#' @export
#'
#' @importFrom coda as.mcmc.list
#'
#' @examples
#' \donttest{
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
#'             type =  "customary")
#'
#' samps <- posterior_samples(fit)
#'}
posterior_samples <- function(object){

  if(!is(object, "vicc")){
    stop("object must be of class 'vicc'")
  }
  samples <- do.call(rbind.data.frame,
                     lapply(1:object$chains, function(x)
                     coda::as.mcmc.list(object$fit)[[x]][-c(1:object$burnin), ]))
  return(samples)
}
