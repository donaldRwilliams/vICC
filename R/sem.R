
sem <- function(object, cred = 0.95, ...){
  if(!is(object, "vicc")){
    stop("object must be of class 'vicc'")
  }

  samps <- posterior_samples(object)
  lb <- (1 - cred) / 2
  ub <-  1 - lb

  if(object$type == "customary"){
    obs_per_group <- tapply(object$model$data()$ID,
                            object$model$data()$ID,
                            length)

    sem <- samps$sigma / sqrt(mean(obs_per_group))
    sem_summary <- data.frame(Post.mean = mean(sem),
                              Post.sd = sd(sem),
                              t(quantile(sem, probs = c(lb, ub)) ))

    colnames(sem_summary)[3:4] <- c("Cred.lb", "Cred.ub")
    class(sem_summary) <- c("sem",
                            "data.frame")
    returned_object <- sem_summary
  } else {
    obs_per_group <- tapply(object$model$data()$ID,
                            object$model$data()$ID,
                            length)

    sem <- exp(samps$fe_sd) / sqrt(mean(obs_per_group))

    sem_summary <- data.frame(Post.mean = mean(sem),
                              Post.sd = sd(sem),
                              t(quantile(sem, probs = c(lb, ub)) ))


   df <- t(as.matrix(obs_per_group))

  mat <- sqrt(t(do.call(rbind,
             lapply(df, rep, nrow(samps))
   )))

    array_collect <- array(0, c(length(obs_per_group), 4, 1 ))
    sem <- exp(samps[, grep("beta_s", colnames(samps))]) /mat
    post_mean_sem <- apply(sem, 2, mean)
    post_sd_sem <- apply(sem, 2, sd)
    post_q_sem <- apply(sem, 2, quantile, probs = c(lb, ub))
    array_collect[, , 1] <- cbind(post_mean_sem,
                                  post_sd_sem,
                                  t(post_q_sem))

     dimnames(array_collect)[[3]] <- c("sem")
     dimnames(array_collect)[[2]] <- c("Post.mean", "Post.sd", "Cred.lb", "Creb.ub")

     coefs <- list(group = array_collect)


    # colnames(sem_summary)[3:4] <- c("Cred.lb", "Cred.ub")

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
    print(as.data.frame(x$coefs$group[,,"sem"]),
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
