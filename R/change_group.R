#' Change Group ID
#'
#' @description Change the group ID to be consequetive numbers, starting at 1, which is
#'              required for model fitting.
#'
#' @param group Numeric Vector. The grouping variable (e.g., subjects).
#'
#' @return Updated group ID.
#'
#' @examples
#' # congruent trials
#' dat <- subset(flanker,  id %in% c(39, 23, 2))
#' change_group(dat$id)
#' @export
change_group <- function(group){
   x <- rle(group)$lengths
   new_id <- rep(x = seq_along(x),
                times = x)
 return(new_id)
}
