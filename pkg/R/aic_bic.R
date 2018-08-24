#' AIC and BIC guided aggregation of segments
#'
#' Functions to aggregate the last segments in a \code{rseg} object in order to optimize the AIC or BIC.
#'
#' Depending on the value of the \code{family} argument, the AIC and BIC are extracted from a generalized linear model with identity link function (= \code{"gaussian"}), logit link function (= \code{"binomial"}) or a Cox proportional hazards regression model (= \code{"censored"}). The segmentation provided by \code{x} is used as factor variable in these models.
#'
#' @return
#'
#' An object of class \code{rseg}.
#'
#' @param x an object of class \code{rseg} fit by \link{cseg}, \link{eseg} or \link{rseg}.
#' @param criterion type of information criterion used. Accepted values are \code{"AIC"} (= default) and \code{"BIC"}.
#' @param family a description of the error distribution/scale of the outcome. Accepted values are \code{"gaussian"}, \code{"binomial"} and \code{"censored"}.
#'
#' @export
#' @import stats
#' @import survival
#' @import partykit
#'
#' @examples
#' airq <- subset(airquality, !is.na(Ozone))
#' irisseg <- cseg(Ozone ~ ., minsplit = 10L, minbucket = 3L, data = airq)
#' irisseg
#'
#' ## The AIC does not suggest aggregation of segments
#' aic(irisseg, family = 'gaussian')
#'
#' ## The stricter BIC suggests aggregation of segments
#' aic(irisseg, criterion = 'BIC', family = 'gaussian')
#' ## This is equivalent to
#' bic(irisseg, family = 'gaussian')
#'
aic <- function(x, criterion = c("AIC", "BIC")[1], family = c("gaussian", "binomial", "censored")[1]) {
    aloc <- aloc_const <- predict.rseg(x, type = "segment")
    penalty <- ifelse(criterion == "AIC", 2, log(nrow(fitted(x[[1]][[1]]))))
    Y <- c(fitted(x[[1]][[1]])[, "(response)"])
    if (family == "binomial") {
      Y <- as.factor(Y)
      levels(Y) <- 0:1
    }
    aics <- c()
    for(z in length(x):2) {
      if (family != "censored") {
        aics <- c(aics, extractAIC(glm(Y ~ as.factor(aloc), family = family), k = penalty)[2])
      } else {aics <- c(aics, extractAIC(coxph(Y ~ as.factor(aloc)), k = penalty)[2])}
      aloc[aloc == z] <- z - 1
    }
    if (family != "censored") {
      aics <- c(aics, extractAIC(glm(Y ~ 1, family = family), k = penalty)[2])
    } else {aics <- c(aics, extractAIC(coxph(Y ~ 1), k = penalty)[2])}
    aic.min <- which.min(rev(aics))
    if (aic.min == 1) mytrees <- list(list(ctree(Y ~ as.factor(rep(1, length(Y))), minsplit = length(Y) + 1), 1))
    if (aic.min >  1) {
      mytrees <- x[1:(aic.min - 1)]
      Y_remain <- Y[!is.element(aloc_const, 1:(aic.min - 1))]
      mytrees[[aic.min]] <- list(ctree(Y_remain ~ as.factor(rep(1, length(Y_remain))), minsplit = length(Y_remain) + 1), 1)
    }
    class(mytrees) <- "rseg"
    mytrees
}
#' @rdname aic
#' @export
bic <- function(x, family = c("gaussian", "binomial", "censored")[1]) aic(x = x, criterion = "BIC", family = family)
