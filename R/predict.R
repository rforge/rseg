#' Predictions by recursive segmentation
#'
#' \code{predict} method for objects of class \code{segmentation}.
#'
#'
#' @return
#'
#' A list, matrix or vector of predictions.
#'
#' @param object an object of class \code{segmentation} fit by \link{cSeg}, \link{eSeg} or \link{rSeg}.
#' @param newdata optional data to use for pediction. If omitted, the fitted values in \code{object} are used.
#' @param type the type of prediction to be returned. For "response", this is the mean of a numeric response, the predicted class for a categorical response or the median survival time for a censored response. For "prob", there are two options: 1) in case of a categorical response this is a matrix of conditional class probabilities or a list with the conditional class probabilities for each observation. 2) In case of numeric and censored responses, this is a list with the empirical cumulative distribution functions and empirical survivor functions (Kaplan-Meier estimate). For "segment", this is an integer vector of terminal node identifiers.
#' @param ... not used.
#'
#' @export
#'
#' @examples
#' ### survival analysis
#' if (require("TH.data")) {
#' data("GBSG2", package = "TH.data")
#' GBSG2seg <- cSeg(Surv(time, cens) ~ ., data = GBSG2)
#' print(GBSG2seg)
#' predict(GBSG2seg, type = "response")
#' predict(GBSG2seg, type = "prob")
#' predict(GBSG2seg, type = "segment")
#' }

predict.segmentation <- function(object, newdata = NULL, type = c("response", "prob", "segment")[1], ...) {
  if (!is.null(newdata)) {
    dat <- newdata} else if (nrow(object[[1]][[1]]$data) == 0) {
      dat <- object[[1]][[1]]$data.save} else dat <- object[[1]][[1]]$data
  prediction <- vector(mode = "list", length = nrow(dat))
  for (i in 1:length(object)) {
    if (type == "segment") preds <- i else preds <- predict(object[[i]][[1]], id = object[[i]][[2]], newdata = NULL, type = type)
    for (j in which(unlist(lapply(prediction, is.null)) & predict(object[[i]][[1]], newdata = dat, type = "node") == object[[i]][[2]])) {
      prediction[[j]] <- preds
    }
  }
  prediction <- simplify2array(prediction, higher = FALSE)
  if (is.matrix(prediction)) {
    prediction <- t(prediction)
    colnames(prediction) <- colnames(preds)
  }
  prediction
}
