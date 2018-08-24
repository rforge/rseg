#' Summary of objects of class \code{rseg}
#'
#' \code{summary} method for objects of class \code{rseg}.
#'
#' This function provides some information about \code{rseg} objects.
#'
#' @return
#'
#' A list of three to four elements: 1) \code{segments} contains the number of segments in \code{object}, \code{rules} contains a vector of the decision rules that define the segments in \code{object} and 3) \code{size} contains a vector of segment sizes in \code{object}. 4) In case of Model-based Recursive Segementation there is a fourth element containing the summaries of the models fit to the segments.
#'
#' @param object an object of class \code{rseg} fit by \link{cseg}, \link{eseg} or \link{rseg}.
#' @param ... not used.
#'
#' @export
#' @import partykit
#'
#' @examples
#' airq <- subset(airquality, !is.na(Ozone))
#' airct <- cseg(Ozone ~ ., data = airq)
#' summary(airct)

summary.rseg <- function(object, ...) {
  segs <- length(object)
  rules <- c(sapply(1:(segs - 1), function(z) unlist(partykit:::.list.rules.party(object[[z]][[1]])[as.character(object[[z]][[2]])])), "complement")
  names(rules) <- 1:segs
  support <- sapply(1:segs, function(z) unlist(sum(predict(object[[z]][[1]], type = "node") == object[[z]][[2]])))
  if (segs > 1) {
    thelist <- list("segments" = segs, "rules" = rules, "size" = support)
  }
  else thelist <- list("segments" = segs, "rules" = "none", "size" = support)
  if (inherits(object,"mob")) {
    thelist[[4]] <- lapply(1:segs, function(z) summary(object[[z]][[1]], node = object[[z]][[2]]))
    names(thelist)[4] <- "models"
  } 
  thelist
}
