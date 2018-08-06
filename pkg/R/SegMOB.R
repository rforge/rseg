#' Model-based Recursive Segementation
#'
#' Recursive segmentation based on Model-based Recursive Partitioning.
#'
#' The algorithm makes use of \link[partykit]{mob} to construct a model-based recursive segmentation model. There is a \link[=predict.segmentation]{predict}, \link[=plot.segmentation]{plot}, \link[=summary.segmentation]{summary} and \link[=print.segmentation]{print} function. The \link[=gettree]{gettree} function can be used to extract the correspoding tree model. See the corresponding documentation for details.
#'
#'
#'
#' @return
#'
#' An object of class \code{segmentation}.
#'
#' @param formula a symbolic description of the model to be fit.
#' @param data a data frame that contains the variables in the model.
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
#' if(require("mlbench")) {
#'   
#'   ## Pima Indians diabetes data
#'   data("PimaIndiansDiabetes", package = "mlbench")
#'   
#'   ## a simple basic fitting function (of type 1) for a logistic regression
#'   logit <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
#'     glm(y ~ 0 + x, family = binomial, start = start, ...)
#'   }
#'   
#'   ## set up a logistic regression tree
#'   pid_tree <- SegMOB(diabetes ~ glucose | pregnant + pressure + triceps + insulin +
#'                     mass + pedigree + age, data = PimaIndiansDiabetes, fit = logit)
#'   
#'   ## print tree
#'   print(pid_tree)
#'   
#'   ## visualization
#'   plot(pid_tree)
#'  
#'   ## summary
#'   summary(pid_tree)
#'   
#'   ## predicted nodes
#'   predict(pid_tree, newdata = head(PimaIndiansDiabetes, 6), type = "segment")
#' }

SegMOB <- function(formula, data, fit, maxsegs = Inf, maxdepth = 10L, minsplit = 20L, minbucket = 7L, ...) {
  if (maxsegs <= 1) stop("'maxsegs' needs to be >1")
  formula <- Formula::as.Formula(formula)
  terminal_ids <- 999  # dummy to enable start of the loop
  mytrees <- list()  # list to contain the segments
  dat <- data  # data used in the loops
  i <- 0  # counter to fill in the list
  while(max(terminal_ids) > 1) {
    i <- i + 1
    mytree <- mob(formula, data = dat, fit = fit, control = mob_control(minsplit = minsplit, minbucket = minbucket, maxdepth = maxdepth, ...))
    terminal_ids <- nodeids(mytree, terminal = TRUE)
    if (max(terminal_ids) == 1) {
      mytrees[[i]] <- list(mytree, 1)
    } else {
      dat$aloc <- predict(mytree, type = "node")
      if (max(terminal_ids) == 3) {
        mytrees[[i]] <- list(mytree, 3)
      } else {
        node.select <- nodeapply(mob(update(formula, as.formula(paste("~ . | factor(aloc == ", paste(terminal_ids, collapse = ") + factor(aloc == "), ")"))), data = dat, fit = fit, control = mob_control(minsplit = 2, minbucket = 1, maxdepth = 2, alpha = 1)))
        mytrees[[i]] <- list(mytree, unlist(node.select[[1]])[["split.varid"]])
      }
      dat <- subset(dat, select = -aloc, subset = aloc != mytrees[[i]][[2]])
      if (length(mytrees) == maxsegs-1) {
        mytrees[[i + 1]] <- list(mob(formula, data = dat, fit = fit, control = mob_control(minsplit = nrow(dat) + 1)), 1)
        break
      }
    }
  }
  class(mytrees) <- c("segmentation", "mob")
  mytrees[[1]][[3]] <- fit
  mytrees
}
