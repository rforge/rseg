#' Recursive Segmentation by Evolutionary Learning
#'
#' Recursive segmentation based on evolutionary trees.
#'
#' The algorithm makes use of \link[evtree]{evtree} to construct a recursive segmentation model. There is a \link[=predict.segmentation]{predict}, \link[=predict.segmentation]{plot}, \link[=predict.segmentation]{summary} and \link[=print.segmentation]{print} function. See the corresponding documentation for details. The function \link[evtree]{evtree} does currently not allow missing values in the covariates.
#'
#' @return
#'
#' An object of class \code{segmentation}.
#'
#' @param formula a symbolic description of the model to be fit.
#' @param data a data frame that contains the variables in the model.
#' @param maxdepth maximal depth of the tree models used for recursive segmentation. The number of decision rules that define a segment can be controled this way.
#' @param minsplit minimal size of a subset to allow for furhter segmentation.
#' @param minbucket minimal size of a segment.
#' @param ... further arguments passed to \link[evtree]{evtree.control}.
#'
#' @export
#' @import stats
#' @import evtree
#'
#' @examples
#' ### regression
#' airq <- subset(airquality, !is.na(Ozone) & complete.cases(airquality))
#' set.seed(1234)
#' airseg <- eSeg(Ozone ~ ., data = airq)
#' airseg
#' plot(airseg)
#'
#' ### classification
#' set.seed(1234)
#' irisseg <- eSeg(Species ~ .,data = iris)
#' irisseg
#' plot(irisseg)

eSeg <- function(formula, data, maxdepth = 10L, minsplit = 20L, minbucket = 7L, ...) {
  nouts <- length(unlist(strsplit(as.character(formula[2]), "[+]"))) # determine the number of outcome variables
  if (nouts > 1) stop("the evtree routine does currently not support multivariate outcomes")
  terminal_ids <- 999  # dummy to enable start of the loop
  mytrees <- list()  # list to contain the segments
  dat <- data
  i <- 0  # counter to fill in the list
  while(nrow(dat) > minsplit & max(terminal_ids) > 3 & length(unique(dat[, all.vars(formula)[1]])) > 1) {
    i <- i + 1
    mytree <- evtree(formula, data = dat, minsplit = minsplit, minbucket = minbucket, maxdepth = maxdepth, ...)
    terminal_ids <- nodeids(mytree, terminal = TRUE)
    dat$aloc <- predict(mytree, type = "node")
    if (max(terminal_ids) == 3) {
      mytrees[[i]] <- list(mytree, 3)
    } else {
    node.select <- nodeapply(ctree(update(formula, as.formula(paste("~ factor(aloc == ", paste(terminal_ids, collapse = ") + factor(aloc == "), ")"))), data = dat, minsplit = 2, minbucket = 1, stump = TRUE, mincriterion = 0))
    mytrees[[i]] <- list(mytree, terminal_ids[unlist(node.select[[1]])["split.varid"] - nouts])  # "- nouts" because the outcome variables are counted
    }
    dat <- subset(dat, select = -aloc, subset = aloc != mytrees[[i]][[2]])
  }
  mytrees[[i + 1]] <- list(ctree(formula, data = dat, minsplit = nrow(dat) + 1), 1)
  class(mytrees) <- "segmentation"
  mytrees
}
