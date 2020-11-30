#' Varying Intraclass Correlation Coefficients
#'
#'
#' @param y
#' @param group
#' @param model
#' @param iter
#' @param chains
#' @param burnin
#' @param prior_scale
#' @param prior_prob
#'
#' @return
#' @export
#'
#' @examples
vicc <- function(y, group,
                 model = "pick_group",
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


  if(model == "pick_group") {
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
        "beta_l"
      )
    )
  }


  return(fit)


}
