#' Plot objects of class \code{rseg}
#'
#' \code{plot} method for objects of class \code{rseg}.
#'
#' Does currently not support plotting of multivariate outcomes and censored outcomes for \link{eseg} and \link{rseg}.
#'
#' @return
#'
#' Plot of a \code{rseg} object.
#'
#' @param x an object of class \code{rseg} fit by \link{cseg}, \link{eseg} or \link{rseg}.
#' @param ... additional arguments passed to \link[partykit]{plot.constparty}.
#'
#' @export
#' @import graphics
#'
#' @examples
#' airq <- subset(airquality, !is.na(Ozone))
#' airct <- cseg(Ozone ~ ., data = airq)
#' airct
#' plot(airct)
plot.rseg <- function(x, ...) {
  if (inherits(x,"mob")) {
    dat <- x[[1]][[1]]$data
    fit <- x[[1]][[3]]
    dat$segment <- as.factor(predict(x, type = "segment"))
    tree <- mob(update(formula(x[[1]][[1]]), ~ . | segment), data = dat, fit = fit, control = mob_control(minsplit = 2, minbucket = 1, maxdepth = 2, alpha = 1, catsplit = "multiway"))
    plot(tree, ip_args = list(pval = FALSE, id = FALSE), tp_args = list(id = FALSE), ...)
  } else {
    if (attr(formula(x[[1]][[1]]), "response") == 0) stop("plot function does currently not support multivariate outcomes")
    if (nrow(x[[1]][[1]]$data) == 0) {
      dat <- x[[1]][[1]]$data.save
      dat$segment <- as.factor(predict(x, type = "segment"))
      tree <- ctree(update(formula(x[[1]][[1]]), ~ segment), data = dat, minsplit = 2, minbucket = 1, stump = TRUE, mincriterion = 0, multiway = TRUE)
    } else {
      dat <- x[[1]][[1]]$data
      dat$segment <- as.factor(predict(x, type = "segment"))
      tree <- ctree(dat[, capture.output(formula(x[[1]][[1]])[[2]])] ~ segment, data = dat, minsplit = 2, minbucket = 1, stump = TRUE, mincriterion = 0, multiway = TRUE)
    }
    plot(tree, ip_args = list(pval = FALSE, id = FALSE), tp_args = list(id = FALSE), ...)
  }
}
