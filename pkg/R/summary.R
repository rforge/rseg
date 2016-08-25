#' Summary of objects of class \code{segmentation}
#'
#' \code{summary} method for objects of class \code{segmentation}.
#'
#' This function provides some information about \code{segmentation} objects.
#'
#' @return
#'
#' A list of three elements: 1) \code{segments} contains the number of segments in \code{object}, \code{rules} contains a vector of the decision rules that define the segments in \code{object} and 3) \code{size} contains a vector of segment sizes in \code{object}.
#'
#' @param object an object of class \code{segmentation} fit by \link{cSeg}, \link{eSeg} or \link{rSeg}.
#' @param ... not used.
#'
#' @export
#' @import partykit
#'
#' @examples
#' airq <- subset(airquality, !is.na(Ozone))
#' airct <- cSeg(Ozone ~ ., data = airq)
#' summary(airct)

summary.segmentation <- function(object, ...) {
  segs <- length(object)
  rules <- c(sapply(1:(segs - 1), function(z) unlist(partykit:::.list.rules.party(object[[z]][[1]])[as.character(object[[z]][[2]])])), "complement")
  names(rules) <- 1:segs
  support <- sapply(1:segs, function(z) unlist(sum(predict(object[[z]][[1]], type = "node") == object[[z]][[2]])))
  if (segs > 1) {
    list("segments" = segs, "rules" = rules, "size" = support)
  }
  else list("segments" = segs, "rules" = "none", "size" = support)
}
