#' Extract tree model
#'
#' Extract a tree model from an object of class \code{rseg}.
#'
#' Recursive segmentation by \link{cseg}, \link{eseg} or \link{rseg} fits models by iterative application of the functions \link[partykit]{ctree}, \link[evtree]{evtree} or \link[rpart]{rpart}, respectively. Therefore, it is possible to extract a respective tree model from an object of class \code{rseg}.
#'
#' @return
#'
#' A tree model fit by the functions \link[partykit]{ctree}, \link[evtree]{evtree} or \link[rpart]{rpart}.
#'
#' @param x an object of class \code{rseg} fit by \link{cseg}, \link{eseg} or \link{rseg}.
#'
#' @export
#'
#' @examples
#' airq <- subset(airquality, !is.na(Ozone))
#' airct <- cseg(Ozone ~ ., data = airq)
#' aircttree <- gettree(airct)
#' aircttree
#' plot(aircttree)

gettree <- function(x) {
  x[[1]][[1]]
}
