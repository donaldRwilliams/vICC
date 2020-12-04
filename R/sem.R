#' @title Standard Error of Measurement
#'
#' @description Compute the standard error of measurment, possibly for
#'              each group.
#'
#' @param object An object of class \code{vicc}.
#'
#' @param cred Numeric. Credible interval width (defaults to \code{0.90}).
#'
#' @param ... Currently ignored.
#'
#' @note Due to the hierarchical model formulation, the group-level sem is
#'       partially pooled (i.e., shrinkage) to the average,
#'
#'
#' @return An object of class \code{sem}.
#'
#' @export
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
#' fit <- vicc(
#'   y  = dat$rt,
#'   group = dat$id,
#'   iter = 500,
#'   burnin = 10,
#'   type =  "pick_group"
#'   )
#'
#' sem(fit)
#'}
sem <- function(object, cred = 0.95, ...){
  if(!is(object, "vicc")){
    stop("object must be of class 'vicc'")
  }
  samps <- posterior_samples(object)
  lb <- (1 - cred) / 2
  ub <-  1 - lb
  if(object$type == "customary"){
    sem <- samps$sigma
    sem_summary <- data.frame(Post.mean = mean(sem),
                              Post.sd = sd(sem),
                              t(quantile(sem, probs = c(lb, ub)) ))

    colnames(sem_summary)[3:4] <- c("Cred.lb", "Cred.ub")
    class(sem_summary) <- c("sem",
                            "data.frame")
    returned_object <- sem_summary
  } else {
    sem <- exp(samps$fe_sd)
    sem_summary <- data.frame(Post.mean = mean(sem),
                              Post.sd = sd(sem),
                              t(quantile(sem, probs = c(lb, ub)) ))
    coefs <- coef(object, cred = cred)
    colnames(sem_summary)[3:4] <- c("Cred.lb", "Cred.ub")
    returned_object <- list(sem_summary = sem_summary,
                            coefs = coefs)
    class(returned_object) <- c("sem", "list")
  }
  return(returned_object)
}




print.sem <- function(x, ...){
  cat("Standard Error of Measurement\n\n")
  if(is(x, "list")){
    cat("Fixed Effect:\n\n")
    print(as.data.frame(x$sem_summary),
          row.names = FALSE,
          digits = 3,
          right = FALSE)
    cat("\n")
    cat("------\n")
    cat("Group-Level:\n\n")
    print(as.data.frame(x$coefs$group[,,"sd"]),
          row.names = FALSE,
          digits = 3,
          right = FALSE)
  } else {
    print(as.data.frame(x),
          row.names = FALSE,
          digits = 3,
          right = FALSE)
  }
  cat("------")
}
