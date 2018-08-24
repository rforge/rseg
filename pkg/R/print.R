#' Print objects of class \code{rseg}
#'
#' \code{print} method for objects of class \code{rseg}.
#'
#'
#'
#' @param x an object of class \code{rseg} fit by \link{cseg}, \link{eseg} or \link{rseg}.
#' @param ... not used.
#'
#' @export
#' @import partykit
#'
#' @examples
#' airq <- subset(airquality, !is.na(Ozone))
#' airct <- cseg(Ozone ~ ., data = airq)
#' airct

print.rseg <- function(x, ...) {
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
    if (segs == 2) {
      cat("segment", segs, ":  The complement of segment 1", " ( n =", ns[segs], ")")
    } else cat("segment", segs, ":  The complement of segments", 1, "to", segs-1, " ( n =", ns[segs], ")\n")
  }
  else cat("\nThere are no rules. All observations belong to a single segment.\n")
}
