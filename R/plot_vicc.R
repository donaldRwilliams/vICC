#' @title Plot \code{vicc} Objects
#'
#' @description Plot the group-specific coefficients or the random effects.
#'
#' @param x An object of class \code{vicc}.
#'
#' @param type Character string. Which parameters should be plotted? The options are
#'             \code{ranef} and \code{coef} (the default).
#'
#' @param ... Currently ignored.
#'
#' @return A \code{ggplot} object.
#'
#' @export
#'
#' @importFrom ggplot2 aes geom_hline geom_point geom_errorbar ggtitle ggplot
#'
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
#' plts <- plot(fit)
#'}
plot.vicc <- function(x , type = "coef", ...){
  # posterior samples
  samps <- posterior_samples(x)

  obs_per_group <- tapply(x$model$data()$ID,
                          x$model$data()$ID,
                          length)
  if(type == "coef"){

    if(x$type == "customary"){

      icc1 <- mean((samps$tau_mu ^ 2 / (samps$tau_mu ^ 2 + (samps$sigma)^2)))

      icc2 <- mean((samps$tau_mu^2 / (samps$tau_mu^2 + (samps$sigma)^2 / mean(obs_per_group))))

      # mean
      coefs <- coef(x)
      n_plots <- dim(coefs$group)[3]
      plots <- list()
      temp <- coefs$group[, , 1]
      dat <-   as.data.frame(temp[order(temp[, 1]), ])
      dat$group <- 1:nrow(dat)

      dat$group_color <-
        as.factor(ifelse(dat$Cred.lb < fixef(x)[1, 1] &
                           dat$Cred.ub > fixef(x)[1, 1], 0 , 1))

      plot_mean <- ggplot(dat, aes(
        x = as.factor(group),
        y = Post.mean,
        color = group_color
      )) +
        geom_hline(yintercept =  fixef(x)[1, 1]) +
        geom_point() +
        geom_errorbar(aes(ymin = Cred.lb,
                          ymax = Cred.ub)) +
        ggtitle(dimnames(coefs$group)[[3]][1])



      # icc1
      coefs <- coef(x)
      n_plots <- dim(coefs$group)[3]
      plots <- list()
      temp <- coefs$group[, , 2]
      dat <-   as.data.frame(temp[order(temp[, 1]), ])
      dat$group <- 1:nrow(dat)

      plot_icc1 <- ggplot(dat, aes(
        x = as.factor(group),
        y = Post.mean
      )) +
        geom_hline(yintercept =  icc1) +
        geom_point() +
        geom_errorbar(aes(ymin = Cred.lb,
                          ymax = Cred.ub)) +
        ggtitle(dimnames(coefs$group)[[3]][2])

      # icc2
      coefs <- coef(x)
      n_plots <- dim(coefs$group)[3]
      plots <- list()
      temp <- coefs$group[, , 3]
      dat <-   as.data.frame(temp[order(temp[, 1]), ])
      dat$group <- 1:nrow(dat)

      plot_icc2 <- ggplot(dat, aes(
        x = as.factor(group),
        y = Post.mean
      )) +
        geom_hline(yintercept =  icc2) +
        geom_point() +
        geom_errorbar(aes(ymin = Cred.lb,
                          ymax = Cred.ub)) +
        ggtitle(dimnames(coefs$group)[[3]][3])


      returned_object <- list(plot_mean = plot_mean,
                              plot_icc1 = plot_icc1,
                              plot_icc2 = plot_icc2)

    } else {

      icc1 <- mean((samps$tau_mu ^ 2 / (samps$tau_mu ^ 2 + exp(samps$fe_sd)^2)))

      icc2 <- mean((samps$tau_mu^2 / (samps$tau_mu^2 + exp(samps$fe_sd)^2 / mean(obs_per_group))))

      coefs <- coef(x)

      # mean
      temp <- coefs$group[,,1]
      dat <-   as.data.frame( temp[order(temp[,1]),])
      dat$group <- 1:nrow(dat)

      dat$group_color <-
        as.factor(
          ifelse(dat$Cred.lb < fixef(x)[1,1] & dat$Cred.ub > fixef(x)[1,1], 0 ,1)
        )

      plot_mean <- ggplot(dat, aes(x = as.factor(group),
                                   y = Post.mean,
                                   color = group_color)) +
        geom_hline(yintercept =  fixef(x)[1,1]) +
        geom_point() +
        geom_errorbar(aes(ymin = Cred.lb,
                          ymax = Cred.ub)) +
        ggtitle(  dimnames( coefs$group)[[3]][1])


      # sd
      coefs <- coef(x)
      temp <- coefs$group[,,2]
      dat <-   as.data.frame( temp[order(temp[,1]),])
      dat$group <- 1:nrow(dat)

      dat$group_color <-
        as.factor(
          ifelse(dat$Cred.lb < fixef(x)[2,1] & dat$Cred.ub > fixef(x)[2,1], 0 ,1)
        )

      plot_sd <- ggplot(dat, aes(x = as.factor(group),
                                 y = Post.mean,
                                 color = group_color)) +
        geom_hline(yintercept =  fixef(x)[2,1]) +
        geom_point() +
        geom_errorbar(aes(ymin = Cred.lb,
                          ymax = Cred.ub)) +
        ggtitle(  dimnames( coefs$group)[[3]][2])

      # icc1
      coefs <- coef(x)
      temp <- coefs$group[,,3]
      dat <-   as.data.frame( temp[order(temp[,1]),])
      dat$group <- 1:nrow(dat)

      dat$group_color <-
        as.factor(
          ifelse(dat$Cred.lb < icc1 & dat$Cred.ub > icc1, 0 ,1)
        )

      plot_icc1 <- ggplot(dat, aes(x = as.factor(group),
                                   y = Post.mean,
                                   color = group_color)) +
        geom_hline(yintercept =  icc1) +
        geom_point() +
        geom_errorbar(aes(ymin = Cred.lb,
                          ymax = Cred.ub)) +
        ggtitle(  dimnames( coefs$group)[[3]][3])


      # icc2
      coefs <- coef(x)
      temp <- coefs$group[,,4]
      dat <-   as.data.frame( temp[order(temp[,1]),])
      dat$group <- 1:nrow(dat)

      dat$group_color <-
        as.factor(
          ifelse(dat$Cred.lb < icc2 & dat$Cred.ub > icc2, 0 ,1)
        )

      plot_icc2 <- ggplot(dat, aes(x = as.factor(group),
                                   y = Post.mean,
                                   color = group_color)) +
        geom_hline(yintercept =  icc2) +
        geom_point() +
        geom_errorbar(aes(ymin = Cred.lb,
                          ymax = Cred.ub)) +
        ggtitle(  dimnames( coefs$group)[[3]][4])

      returned_object <- list(plot_mean = plot_mean,
                              plot_sd = plot_sd,
                              plot_icc1 = plot_icc1,
                              plot_icc2 = plot_icc2)
    }

  } else if (type == "ranef"){


    if(x$type == "customary"){
      # mean
      coefs <- ranef(x)

      temp <- coefs$group[, , 1]
      dat <-   as.data.frame(temp[order(temp[, 1]), ])
      dat$group <- 1:nrow(dat)

      dat$group_color <-
        as.factor(ifelse(dat$Cred.lb < 0 &
                           dat$Cred.ub > 0, 0 , 1))

      plot_mean <- ggplot(dat, aes(
        x = as.factor(group),
        y = Post.mean,
        color = group_color
      )) +
        geom_hline(yintercept =  0) +
        geom_point() +
        geom_errorbar(aes(ymin = Cred.lb,
                          ymax = Cred.ub)) +
        ggtitle(dimnames(coefs$group)[[3]][1])




      returned_object <- list(plot_mean = plot_mean)

    } else {

      coefs <- ranef(x)

      # mean
      temp <- coefs$group[,,1]
      dat <-   as.data.frame( temp[order(temp[,1]),])
      dat$group <- 1:nrow(dat)

      dat$group_color <-
        as.factor(
          ifelse(dat$Cred.lb < 0 & dat$Cred.ub > 0, 0 ,1)
        )

      plot_mean <- ggplot(dat, aes(x = as.factor(group),
                                   y = Post.mean,
                                   color = group_color)) +
        geom_hline(yintercept =  0) +
        geom_point() +
        geom_errorbar(aes(ymin = Cred.lb,
                          ymax = Cred.ub)) +
        ggtitle(  dimnames( coefs$group)[[3]][1])


      # sd
      temp <- coefs$group[,,2]
      dat <-   as.data.frame( temp[order(temp[,1]),])
      dat$group <- 1:nrow(dat)

      dat$group_color <-
        as.factor(
          ifelse(dat$Cred.lb < 0 & dat$Cred.ub > 0, 0 ,1)
        )

      plot_sd <- ggplot(dat, aes(x = as.factor(group),
                                 y = Post.mean,
                                 color = group_color)) +
        geom_hline(yintercept =  0) +
        geom_point() +
        geom_errorbar(aes(ymin = Cred.lb,
                          ymax = Cred.ub)) +
        ggtitle(  dimnames( coefs$group)[[3]][2])

      # icc1
      temp <- coefs$group[,,3]
      dat <-   as.data.frame( temp[order(temp[,1]),])
      dat$group <- 1:nrow(dat)

      dat$group_color <-
        as.factor(
          ifelse(dat$Cred.lb < 0 & dat$Cred.ub > 0, 0 ,1)
        )

      plot_icc1 <- ggplot(dat, aes(x = as.factor(group),
                                   y = Post.mean,
                                   color = group_color)) +
        geom_hline(yintercept =  0) +
        geom_point() +
        geom_errorbar(aes(ymin = Cred.lb,
                          ymax = Cred.ub)) +
        ggtitle(  dimnames( coefs$group)[[3]][3])


      # icc2
      temp <- coefs$group[,,4]
      dat <-   as.data.frame( temp[order(temp[,1]),])
      dat$group <- 1:nrow(dat)

      dat$group_color <-
        as.factor(
          ifelse(dat$Cred.lb < 0 & dat$Cred.ub > 0, 0 ,1)
        )

      plot_icc2 <- ggplot(dat, aes(x = as.factor(group),
                                   y = Post.mean,
                                   color = group_color)) +
        geom_hline(yintercept =  0) +
        geom_point() +
        geom_errorbar(aes(ymin = Cred.lb,
                          ymax = Cred.ub)) +
        ggtitle(  dimnames( coefs$group)[[3]][4])

      returned_object <- list(plot_mean = plot_mean,
                              plot_sd = plot_sd,
                              plot_icc1 = plot_icc1,
                              plot_icc2 = plot_icc2)


    }
    } else {
    stop("type not supported. must be 'coef' or 'ranef'")
  }
  return(returned_object)
}
