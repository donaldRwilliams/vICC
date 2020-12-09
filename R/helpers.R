#' @importFrom methods is
#' @importFrom stats quantile sd coef
#' @importFrom utils packageVersion
ICC_lsm <- "model{

  for(j in 1:J){

    # latent betas
    beta_raw_l[j] ~  dnorm(0, 1)

    # random effect
    beta_l[j] <- fe_mu + tau_mu * beta_raw_l[j]

    # cholesky
    z2[j] ~ dnorm(0, 1)
    beta_raw_s[j] = rho12 * beta_raw_l[j] + sqrt(1 - rho12^2) * z2[j]

    beta_s[j] <- fe_sd + tau_sd * beta_raw_s[j]

  }

  for(i in 1:N){

    # likelihood
    y[i] ~ dnorm(beta_l[ID[i]], 1/exp(beta_s[ID[i]])^2)

  }


  # fixed effects priors
  fe_mu ~ dnorm(mean_start, 0.0001)
  fe_sd ~ dnorm(0, 0.0001)

  # random effects priors
  tau_mu ~ dt(0, pow(prior_scale,-2), 10)T(0,)
  tau_sd ~ dt(0, pow(prior_scale,-2), 10)T(0,)



  # prior for RE correlation
  fz ~ dnorm(0, 1)
  rho12 = tanh(fz)

}"


ICC_customary <- "model{

for(j in 1:J){
  # latent betas
  beta_raw[j] ~  dnorm(0, 1)

  # random effect
  beta[j] <- fe_mu + tau_mu * beta_raw[j]
}

for(i in 1:N){

  # likelihood
  y[i] ~ dnorm(beta[ID[i]], prec)

}


# fixed effects priors
fe_mu ~ dnorm(mean_start, 0.001)

# random effects priors
tau_mu ~ dt(0, pow(prior_scale,-2), 10)T(0,)
prec ~ dgamma(1.0E-4,1.0E-4)

sigma <- 1/sqrt(prec)

}"

ICC_pick_tau <- "model{

    for(j in 1:J){

      # latent betas
      beta_raw_l[j] ~  dnorm(0, 1)

      # random effect
      beta_l[j] <- fe_mu + tau_mu * beta_raw_l[j]

      # cholesky
      z2[j] ~ dnorm(0, 1)
      beta_raw_s[j] = rho12 * beta_raw_l[j] + sqrt(1 - rho12^2) * z2[j]

      beta_s[j] <- fe_sd + tau_new * beta_raw_s[j]

    }

    for(i in 1:N){

      # likelihood
      y[i] ~ dnorm(beta_l[ID[i]], 1/exp(beta_s[ID[i]])^2)

    }


    # fixed effects priors
    fe_mu ~ dnorm(0, 1)
    fe_sd ~ dnorm(0, 1)

    # random effects priors
    tau_mu ~ dgamma(1.0E-4,1.0E-4)

    tau_sd ~ dt(0, pow(prior_scale,-2), 10)T(0,)

    pick_tau ~ dbern(inc_prob)

    tau_new <- tau_sd * pick_tau


    # prior for RE correlation
    fz ~ dnorm(0, 1)
    rho12 = tanh(fz)

}"



ICC_pick_id <- "model{

for(j in 1:J){

      pick_id[j] ~ dbern(inc_prob)

      # latent betas
      beta_raw_l[j] ~  dnorm(0, 1)

      # random effect
      beta_l[j] <- fe_mu + tau_mu * beta_raw_l[j]

      # cholesky
      z2[j] ~ dnorm(0, 1)

      beta_raw_s[j] = rho12 * beta_raw_l[j] + sqrt(1 - rho12^2) * z2[j]

      beta_new[j] <- beta_raw_s[j] * pick_id[j]
      beta_s[j] <- fe_sd + (tau_sd * beta_new[j])

}

for(i in 1:N){
# likelihood
y[i] ~ dnorm(beta_l[ID[i]], 1/exp(beta_s[ID[i]])^2)
}


# fixed effects priors
fe_mu ~ dnorm(0, 1)
fe_sd ~ dnorm(0, 1)

# random effects priors
tau_mu ~   dt(0, pow(prior_scale,-2), 10)T(0,)
tau_sd ~   dt(0, pow(prior_scale,-2), 10)T(0,)



# prior for RE correlation
fz ~ dnorm(0, 1)
rho12 = tanh(fz)

}"

globalVariables(c("group_color",
                  "group",
                  "Post.mean",
                  "Cred.lb",
                  "Cred.ub",
                  "PIP"))


viccStartupMessage <- function(){
  msg <- c(paste0(

    "
            _______ _____ _____
              /||   //   //
     ____    //||  //   //
       \\\\   // || ||   ||
        \\\\ //  || ||   ||
         \\ /   ||  \\\\   \\\\
          / ___||__ \\\\___\\\\____
          ", "\nVersion ", packageVersion("vICC")),
    "\nType 'citation(\"vICC\")' for citing this R package.")
  return(msg)
}

.onAttach <- function(lib, pkg){
  # startup message
  msg <- viccStartupMessage()
  if(!interactive())
    msg[1] <- paste("Package 'vICC' version", packageVersion("vICC"))
  packageStartupMessage(msg)
  invisible()
}
