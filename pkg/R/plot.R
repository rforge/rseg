#' Plot objects of class \code{segmentation}
#'
#' \code{plot} method for objects of class \code{segmentation}.
#'
#' Does currently not support plotting of multivariate outcomes and censored outcomes for \link{eSeg} and \link{rSeg}.
#'
#' @return
#'
#' Plot of a \code{segmentation} object.
#'
#' @param x an object of class \code{segmentation} fit by \link{cSeg}, \link{eSeg} or \link{rSeg}.
#' @param ... additional arguments passed to \link[partykit]{plot.constparty}.
#'
#' @export
#' @import graphics
#'
#' @examples
#' airq <- subset(airquality, !is.na(Ozone))
#' airct <- cSeg(Ozone ~ ., data = airq)
#' airct
#' plot(airct)
plot.segmentation <- function(x, ...) {
  if (attr(formula(x[[1]][[1]]), "response") == 0) stop("plot function does currently not support multivariate outcomes")
  if (nrow(x[[1]][[1]]$data) == 0) {
    dat <- x[[1]][[1]]$data.save
    dat$segment <- as.factor(predict.segmentation(x, type = "segment"))
    tree <- ctree(update(formula(x[[1]][[1]]), ~ segment), data = dat, minsplit = 2, minbucket = 1, stump = TRUE, mincriterion = 0, multiway = TRUE)
    } else {
      dat <- x[[1]][[1]]$data
      dat$segment <- as.factor(predict.segmentation(x, type = "segment"))
      tree <- ctree(dat[, capture.output(formula(x[[1]][[1]])[[2]])] ~ segment, data = dat, minsplit = 2, minbucket = 1, stump = TRUE, mincriterion = 0, multiway = TRUE)
    }
    plot(tree, ip_args = list(pval = FALSE, id = FALSE), tp_args = list(id = FALSE), ...)
}
