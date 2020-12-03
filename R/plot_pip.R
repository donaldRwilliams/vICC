#' Plot \code{pip} Objects
#'
#' @description Bar plot for the posterior inclusion probabilities, which corresponds to
#'              the probability that each group differs from the average within-group
#'              variance.
#'
#' @param x An object of class \code{pip}
#' @param color Character string. Which color for the bars
#'              (defaults to \code{black})?
#'
#' @param width Numeric. The width for the bars (defaults to \code{0.5}).
#'
#' @param ... Currently ignored
#'
#' @return A \code{ggplot} object.
#'
#' @export
#'
#' @importFrom ggplot2 geom_bar scale_y_discrete ylab
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
#' fit <- vicc(
#'   y  = dat$rt,
#'   group = dat$id,
#'   iter = 500,
#'   burnin = 10,
#'   type =  "pick_group"
#' )
#'
#' pips <- pip(fit)
#'
#' plot(pips)
#'
#' }
plot.pip <- function(x, color = "black",
                     width = 0.5, ...){
  dat_plot <- x$pip_summary
  if(nrow(dat_plot) ==1){
    stop("type not supported. must be 'tick_group'")
  }

  dat_plot <- dat_plot[order(dat_plot$PIP),]
  dat_plot$group <- 1:nrow(dat_plot)
  plt <- ggplot(dat_plot, aes(x = PIP, y = as.factor(group))) +
    geom_bar(stat = "identity", width = 0.5, color = color) +
    scale_y_discrete(labels = row.names(dat_plot)) +
    ylab("Group")
  return(plt)
}







