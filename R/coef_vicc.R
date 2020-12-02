#' @title Extract the Group-Specific Parameters
#'
#' @description Extract the group-specific parameter estimates.
#
#' @param object An object of class \code{vicc}
#'
#' @param cred Numeric. Credible interval width (defaults to \code{0.90})
#'
#' @param ... Currently ignored.
#'
#' @return An array with the summarized parameters
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
#' # fit model
#' fit <- vicc(y  = dat$rt,
#'             group = dat$id,
#'             iter = 250,
#'             burnin = 10,
#'             type =  "customary")
#'
#' coef(fit)
#'
#' }
coef.vicc <- function(object,
                      cred = 0.90,
                      ...){

  lb <- (1 - cred) / 2
  ub <-  1 - lb
  J <- object$model$data()$J

  if(object$type == "customary"){
    samps <- posterior_samples(object)
    array_collect <- array(0, dim = c(J, 4, 2))
    dimnames(array_collect)[[1]] <- 1:J
    dimnames(array_collect)[[3]] <- c("mean", "icc1")
    ranefs <- samps[paste0("beta[", 1:J, "]")]
    post_mean <- apply(ranefs, 2, mean)
    post_sd <- apply(ranefs, 2, sd)
    post_cred <- apply(ranefs, 2, quantile, probs = c(lb, ub))
    array_collect[,,1] <- cbind(post_mean, post_sd,  t(post_cred))


    icc <- (samps$tau_mu^2 / (samps$tau_mu^2 + samps$sigma^2))
    post_mean_icc <- mean(icc)
    post_sd_icc <- sd(icc)
    post_cred_icc <- quantile(icc, probs = c(lb, ub))


    array_collect[,,2] <-  matrix(rep(cbind(post_mean_icc,
                                            post_sd_icc,
                                            t(post_cred_icc)), J),
                                  nrow = J,
                                  ncol = 4,
                                  byrow =TRUE)
    } else {

    samps <- posterior_samples(object)

    obs_per_group <- tapply(object$model$data()$ID,
                            object$model$data()$ID,
                            length)

    array_collect <- array(0, dim = c(J, 4, 4))
    dimnames(array_collect)[[1]] <- 1:J
    dimnames(array_collect)[[3]] <- c("mean",
                                      "sd",
                                      "icc1",
                                      "icc2")

    # location
    ranefs_l <- samps[paste0("beta_l[", 1:J, "]")]
    post_mean_l <- apply(ranefs_l, 2, mean)
    post_sd_l <- apply(ranefs_l, 2, sd)
    post_cred_l <- apply(ranefs_l, 2, quantile, probs = c(lb, ub))
    array_collect[,,1] <- cbind(post_mean_l,
                                post_sd_l,
                                t(post_cred_l))

    # scale
    ranefs_s <- exp(samps[paste0("beta_s[", 1:J, "]")])
    post_mean_s <- apply(ranefs_s, 2, mean)
    post_sd_s <- apply(ranefs_s, 2, sd)
    post_cred_s <- apply(ranefs_s, 2, quantile, probs = c(lb, ub))
    array_collect[,,2] <- cbind(post_mean_s, post_sd_s,
                                t(post_cred_s))

    # ICC
    ranefs_icc1 <- apply(ranefs_s, MARGIN = 2, FUN = function(x) {
      samps$tau_mu^2  /  (samps$tau_mu^2  + x^2)
      })

    post_mean_icc1 <- apply(ranefs_icc1, 2, mean)
    post_sd_icc1 <- apply(ranefs_icc1, 2, sd)
    post_cred_icc1 <- apply(ranefs_icc1, 2, quantile, probs = c(lb, ub))
    array_collect[, , 3] <- cbind(post_mean_icc1,
                                  post_sd_icc1,
                                  t(post_cred_icc1))


    ranefs_icc2 <- sapply(1:J, FUN = function(x) {
      samps$tau_mu^2  /  (samps$tau_mu^2  + (ranefs_s[,x]^2 / obs_per_group[x]) )
    })

    post_mean_icc2 <- apply(ranefs_icc2, 2, mean)
    post_sd_icc2 <- apply(ranefs_icc2, 2, sd)
    post_cred_icc2 <- apply(ranefs_icc2, 2, quantile, probs = c(lb, ub))
    array_collect[, , 4] <- cbind(post_mean_icc2,
                                   post_sd_icc2,
                                   t(post_cred_icc2))



    }

  dimnames(array_collect)[[2]] <-  c("Post.mean",
                                     "Post.sd",
                                     "Cred.lb",
                                     "Cred.ub")

  return(array_collect)

}
