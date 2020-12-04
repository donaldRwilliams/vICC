#' Varying Intraclass Correlation Coefficients
#'
#' @description Compute varying intraclass correlation coefficients
#'
#' @param y Numeric vector. The outcome variable.
#'
#' @param group Numeric vector. The grouping variable (e.g., subjects).
#'
#' @param type Character string. Which model should be fitted
#'              (defaults to \code{pick_group})? The options are
#'              described in \code{Details}.
#'
#' @param iter Numeric. The number of posterior samples per chain (excluding \code{burnin}).
#'
#' @param chains Numeric. The number of chains (defaults to \code{2}).
#'
#' @param burnin Numeric. The number of burnin samples, which are discarded
#'               (defaults to \code{500}).
#'
#' @param prior_scale Numeric. The prior distribution scale parameter
#'                    (defaults to \code{1}). Note the prior is a
#'                    half student-t distribution with 10 degrees of freedom.
#'
#' @param prior_prob Numeric. The prior inclusion probability (defaults to \code{0.5}). This
#'                   is used for \code{type = "pick_tau"} or \code{type = "pick_group"} and ignored
#'                   otherwise.
#'
#' @return An object of class \code{vicc}.
#'
#' @importFrom rjags jags.model coda.samples
#'
#' @export
#'
#' @examples
#' \donttest{
#'
#' flanker <- vICC::flanker
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
#'             type =  "customary")
#'}
vicc <- function(y, group,
                 type = "pick_group",
                 iter = 5000,
                 chains = 2,
                 burnin = 500,
                 prior_scale = 1,
                 prior_prob = 0.5){


  # outcome
  y <- y
  N <- length(y)
  # groups
  ID <- group
  # number of groups
  J <- length(unique(ID))


  if(type == "pick_group") {

    model <-
      rjags::jags.model(
        file = base::textConnection(ICC_pick_id),
        n.chains = chains,
        inits = list(fe_mu = mean(y)),
        data = list(
          y = y,
          ID = ID,
          J = J,
          N = N,
          prior_scale = prior_scale,
          inc_prob = prior_prob
        )
      )

    fit <- rjags::coda.samples(
      model,
      n.iter = iter + burnin,
      variable.names = c(
        "fe_mu",
        "fe_sd",
        "tau_mu",
        "pick_id",
        "tau_sd",
        "beta_s",
        "beta_l",
        "rho12"
      )
    )
  } else if (type == "pick_tau"){

    model <- rjags::jags.model(
      base::textConnection(ICC_pick_tau),
      n.chains = chains,
      inits = list(fe_mu = mean(y)),
      data = list(
        y = y,
        ID = ID,
        J = J,
        N = N,
        prior_scale = prior_scale,
        inc_prob = prior_prob
      )
    )

    # fit for motivating example
    fit <- coda.samples(
      model = model,
      n.iter = iter + burnin,
      variable.names = c(
        "fe_mu",
        "fe_sd",
        "tau_mu",
        "pick_tau",
        "tau_sd",
        "beta_s",
        "beta_l",
        "rho12"
      )
    )


  } else if(type == "pick_none"){


    model <- rjags::jags.model(
      base::textConnection(ICC_lsm),
      n.chains = chains,
      inits = list(fe_mu = mean(y)),
      data = list(
        y = y,
        ID = ID,
        J = J,
        N = N,
        prior_scale = prior_scale
      )
    )

    fit <- rjags::coda.samples(
      model = model,
      n.iter = iter + burnin,
      variable.names = c("fe_mu",
                         "fe_sd",
                         "tau_mu",
                         "tau_sd",
                         "beta_s",
                         "beta_l",
                         "rho12")
    )
  } else if(type == "customary"){
    file <- textConnection(ICC_customary)
    model <- rjags::jags.model(
      file = file,
      n.chains = chains,
      inits = list(fe_mu = mean(y)),
      data = list(
        y = y,
        ID = ID,
        J = J,
        N = N,
        prior_scale = prior_scale
      )
    )

    fit <- rjags::coda.samples(
      model = model,
      n.iter = iter + burnin,
      variable.names = c("sigma",
                         "beta",
                         "tau_mu",
                         "fe_mu")
    )
    close(file)
    } else {
      stop("model not supported. see documentation")
  }

  returned_object <- list(fit = fit,
                          type = type,
                          model = model,
                          chains = chains,
                          iter = iter,
                          burnin = burnin)

  class(returned_object) <- "vicc"
  return(returned_object)


}


#' Print \code{vicc} Objects
#'
#' @param x An object of class \code{vicc}.
#'
#' @param cred Numeric. Credible interval width (defaults to \code{0.90}).
#'
#' @param ... Currently ignored
#'
#' @export
print.vicc <- function(x, cred = 0.95, ...){
  lb <- (1 - cred) / 2
  ub <-  1 - lb
  cat("vICC: Varying Intraclass Correlaton Coefficients\n")
  # cat("-----\n")
  samps <- posterior_samples(x)

  if(x$type == "customary"){
    cat("Type:", x$type, "\n")
    cat("-----\n")
    cat("Random Effects:\n")
    re_sd <- samps$tau_mu
    re_summary <- data.frame(Post.mean = mean(re_sd),
                             Post.sd = sd(re_sd), t(quantile(re_sd, c(lb, ub))))
    row.names(re_summary) <- "RE.sd.mean"
    colnames(re_summary)[3:4] <- c("Cred.lb", "Cred.ub")
    print(round(re_summary, 4), right = FALSE)
    cat("\n")
    cat("Fixed Effects:\n")
    fe_mu <- samps$fe_mu
    fe_summary <- data.frame(Post.mean = mean(fe_mu),
                             Post.sd = sd(fe_mu),
                             t(quantile(fe_mu, c(lb, ub))))
    row.names(fe_summary) <- "FE.mean"
    colnames(fe_summary)[3:4] <- c("Cred.lb", "Cred.ub")
    print(round(fe_summary, 4), right = FALSE)

    sigma <- samps$sigma
    sigma_summary <- data.frame(Post.mean = mean(sigma),
                                Post.sd = sd(sigma),
                                t(quantile(sigma, c(lb, ub))))
    row.names(sigma_summary) <- "sigma"
    colnames(sigma_summary)[3:4] <- c("Cred.lb", "Cred.ub")
    cat("\n")
    cat("Residual SD:\n")
    print(round(sigma_summary, 4), right = FALSE)

  } else {

    cat("Type:", x$type, "\n")
    cat("-----\n")
    cat("Random Effects:\n")
    re_sd_mean <- samps$tau_mu
    re_sd_sd <- samps$tau_sd
    re_cor <- samps$rho12
    re_summary <- rbind.data.frame(

      data.frame(Post.mean = mean(re_sd_mean),
                 Post.sd = sd(re_sd_mean),
                 t(quantile(re_sd_mean, c(lb, ub)))),

      data.frame(Post.mean = mean(re_sd_sd),
                 Post.sd = sd(re_sd_sd),
                 t(quantile(re_sd_sd, c(lb, ub)))),

      data.frame(Post.mean = mean(re_cor),
                 Post.sd = sd(re_cor),
                 t(quantile(re_cor, c(lb, ub))))

    )
    row.names(re_summary) <- c("RE.sd.mean", "RE.sd.sigma", "Cor(mean,sigma)")
    colnames(re_summary)[3:4] <- c("Cred.lb", "Cred.ub")
    print(round(re_summary, 4), right = FALSE)
    cat("\n")
    cat("Fixed Effects:\n")

    fe_mu <- samps$fe_mu
    fe_sd <- exp(samps$fe_sd)
    fe_summary <-
      rbind.data.frame(
        data.frame(Post.mean = mean(fe_mu),
                   Post.sd = sd(fe_mu),
                   t(quantile(fe_mu, c(lb, ub)))),

        data.frame(Post.mean = mean(fe_sd),
                   Post.sd = sd(fe_sd),
                   t(quantile(fe_sd, c(lb, ub))))

      )
    row.names(fe_summary) <- c("FE.mean", "FE.sigma")
    colnames(fe_summary)[3:4] <- c("Cred.lb", "Cred.ub")
    print(round(fe_summary, 4), right = FALSE)

  }
  cat("-----\n")
}
