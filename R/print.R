#' Print objects of class \code{segmentation}
#'
#' \code{print} method for objects of class \code{segmentation}.
#'
#'
#'
#' @param x an object of class \code{segmentation} fit by \link{cSeg}, \link{eSeg} or \link{rSeg}.
#' @param ... not used.
#'
#' @export
#' @import partykit
#'
#' @examples
#' airq <- subset(airquality, !is.na(Ozone))
#' airct <- cSeg(Ozone ~ ., data = airq)
#' airct

print.segmentation <- function(x, ...) {
  segs <- length(x)
  theformula <- formula(x[[1]][[1]])
  attributes(theformula) <- NULL
  cat("Model formula:\n")
  print(theformula)
  if (segs > 1) {
    cat("\nThere are", segs, "segments.\n\n")
    rules <- sapply(1:(segs - 1), function(z) unlist(partykit:::.list.rules.party(x[[z]][[1]])[as.character(x[[z]][[2]])]))
    ns <- sapply(1:segs, function(z) unlist(sum(predict(x[[z]][[1]], type = "node") == x[[z]][[2]])))
    cat("Rules:", "\n")
    sapply(1:(length(x) - 1), function(z) cat("segment", z, ": ", rules[z], " ( n =", ns[z], ")\n"))
    cat("segment", segs, ":  The complement of segments", 1, "to", segs-1, " ( n =", ns[segs], ")")
  }
  else cat("\nThere are no rules. All observations belong to a single segment.\n")
}
