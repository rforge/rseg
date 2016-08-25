#' Extract tree model
#'
#' Extract a tree model from an object of class \code{segmentation}.
#'
#' Recursive segmentation by \link{cSeg}, \link{eSeg} or \link{rSeg} fits models by iterative application of the functions \link[partykit]{ctree}, \link[evtree]{evtree} or \link[rpart]{rpart}, respectively. Therefore, it is possible to extract a respective tree model from an object of class \code{segmentation}.
#'
#' @return
#'
#' A tree model fit by the functions \link[partykit]{ctree}, \link[evtree]{evtree} or \link[rpart]{rpart}.
#'
#' @param x an object of class \code{segmentation} fit by \link{cSeg}, \link{eSeg} or \link{rSeg}.
#'
#' @export
#'
#' @examples
#' airq <- subset(airquality, !is.na(Ozone))
#' airct <- cSeg(Ozone ~ ., data = airq)
#' aircttree <- gettree(airct)
#' aircttree
#' plot(aircttree)

gettree <- function(x) {
  x[[1]][[1]]
}
