#' Model-based Recursive Segementation
#'
#' Recursive segmentation based on Model-based Recursive Partitioning.
#'
#' The algorithm makes use of \link[partykit]{mob} to construct a model-based recursive segmentation model. There is a \link[=predict.rseg]{predict}, \link[=plot.rseg]{plot}, \link[=summary.rseg]{summary} and \link[=print.rseg]{print} function. The \link[=gettree]{gettree} function can be used to extract the correspoding tree model. See the corresponding documentation for details.
#'
#'
#'
#' @return
#'
#' An object of class \code{rseg}.
#'
#' @param formula a symbolic description of the model to be fit.
#' @param data a data frame that contains the variables in the model.
#' @param fit A function for fitting the model within each node. For details see \link[partykit]{mob}.
#' @param maxsegs maximal number of segments
#' @param maxdepth maximal depth of the tree models used for recursive segmentation. The number of decision rules that define a segment can be controled this way.
#' @param minsplit minimal size of a subset to allow for furhter segmentation.
#' @param minbucket minimal size of a segment.
#' @param ... further arguments passed to \link[partykit]{mob_control}.
#'
#' @export
#' @import stats
#' @import partykit
#'
#' @references{
#' \insertRef{Hapfelmeier2018}{rseg}
#' }
#' @importFrom Rdpack reprompt
#'
#' @examples
#' if(require("AER")) {
#'   
#'   data("TeachingRatings", package = "AER")
#'   tr <- subset(TeachingRatings, credits == "more")
#'   
#'   myfit <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
#'     glm(y ~ x, family = gaussian, ...)
#'   }
#'   segmob(eval ~ beauty | minority + age + gender + division + native + tenure, 
#'     data = tr, fit = myfit, weights = students, caseweights = FALSE)
#' }

segmob <- function(formula, data, fit, weights = NULL, maxsegs = Inf, maxdepth = 10L, minsplit = 20L, minbucket = 7L, ...) {
  if (maxsegs <= 1) stop("'maxsegs' needs to be >1")
  formula <- Formula::as.Formula(formula)
  terminal_ids <- 999  # dummy to enable start of the loop
  mytrees <- list()  # list to contain the segments
  dat <- data  # data used in the loops
  i <- 0  # counter to fill in the list
  while(max(terminal_ids) > 1) {
    i <- i + 1
    mytree <- eval(substitute(mob(formula, data = dat, fit = fit, weights = weights, control = mob_control(minsplit = minsplit, minbucket = minbucket, maxdepth = maxdepth, ...))))
    terminal_ids <- nodeids(mytree, terminal = TRUE)
    if (max(terminal_ids) == 1) {
      mytrees[[i]] <- list("tree" = mytree, "selected.node" = 1)
    } else {
      dat$.aloc <- predict(mytree, type = "node")
      if (max(terminal_ids) == 3) {
        mytrees[[i]] <- list("tree" = mytree, "selected.node" = 3)
      } else {
        node.select <- nodeapply(eval(substitute(mob(update(formula, as.formula(paste("~ . | factor(.aloc == ", paste(terminal_ids, collapse = ") + factor(.aloc == "), ")"))), data = dat, fit = fit, weights = weights, control = mob_control(minsplit = 2, minbucket = 1, maxdepth = 2, alpha = 1)))))
        mytrees[[i]] <- list("tree" = mytree, "selected.node" = unlist(node.select[[1]])[["split.varid"]])
      }
      dat <- dat[dat$.aloc != mytrees[[i]][[2]], -ncol(dat)]
      if (length(mytrees) == maxsegs-1) {
        mytrees[[i + 1]] <- list("tree" = eval(substitute(mob(formula, data = dat, fit = fit, weights = weights, control = mob_control(minsplit = nrow(dat) + 1)))), "selected.node" = 1)
        break
      }
    }
  }
  class(mytrees) <- c("rseg", "mob")
  mytrees[[1]][[3]] <- fit
  names(mytrees) <- paste("segment", 1:length(mytrees))
  mytrees
}
