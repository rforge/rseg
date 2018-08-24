#' Recursive Segmentation by Evolutionary Learning
#'
#' Recursive segmentation based on evolutionary trees.
#'
#' The algorithm makes use of \link[evtree]{evtree} to construct a recursive segmentation model. There is a \link[=predict.rseg]{predict}, \link[=plot.rseg]{plot}, \link[=summary.rseg]{summary} and \link[=print.rseg]{print} function. The \link[=gettree]{gettree} function can be used to extract the correspoding tree model. See the corresponding documentation for details. The function \link[evtree]{evtree} does currently not allow missing values in the covariates.
#'
#' @return
#'
#' An object of class \code{rseg}.
#'
#' @param formula a symbolic description of the model to be fit.
#' @param data a data frame that contains the variables in the model.
#' @param maxsegs maximal number of segments
#' @param maxdepth maximal depth of the tree models used for recursive segmentation. The number of decision rules that define a segment can be controled this way.
#' @param minsplit minimal size of a subset to allow for furhter segmentation.
#' @param minbucket minimal size of a segment.
#' @param ... further arguments passed to \link[evtree]{evtree.control}.
#'
#' @export
#' @import stats
#' @import evtree
#'
#' @references{
#' \insertRef{Hapfelmeier2018}{rseg}
#' }
#' @importFrom Rdpack reprompt
#'
#' @examples
#' ### regression
#' airq <- subset(airquality, complete.cases(airquality))
#' set.seed(1234)
#' airseg <- eseg(Ozone ~ ., data = airq)
#' airseg
#' plot(airseg)
#'
#' ### classification
#' set.seed(1234)
#' irisseg <- eseg(Species ~ ., data = iris)
#' irisseg
#' plot(irisseg)

eseg <- function(formula, data, maxsegs = Inf, maxdepth = 10L, minsplit = 20L, minbucket = 7L, ...) {
  if (maxsegs <= 1) stop("'maxsegs' needs to be >1")
  nouts <- length(unlist(strsplit(as.character(formula[2]), "[+]"))) # determine the number of outcome variables
  if (nouts > 1) stop("the evtree routine does currently not support multivariate outcomes")
  terminal_ids <- 999  # dummy to enable start of the loop
  mytrees <- list()  # list to contain the segments
  dat <- data
  i <- 0  # counter to fill in the list
  while(nrow(dat) > minsplit & max(terminal_ids) > 3 & length(unique(dat[, all.vars(formula)[1]])) > 1 & length(mytrees) < maxsegs-1) {
    i <- i + 1
    mytree <- evtree(formula, data = dat, minsplit = minsplit, minbucket = minbucket, maxdepth = maxdepth, ...)
    terminal_ids <- nodeids(mytree, terminal = TRUE)
    dat$.aloc <- predict(mytree, type = "node")
    if (max(terminal_ids) == 3) {
      mytrees[[i]] <- list("tree" = mytree, "selected.node" = 3)
    } else {
    node.select <- nodeapply(ctree(update(formula, as.formula(paste("~ factor(.aloc == ", paste(terminal_ids, collapse = ") + factor(.aloc == "), ")"))), data = dat, minsplit = 2, minbucket = 1, stump = TRUE, mincriterion = 0))
    mytrees[[i]] <- list("tree" = mytree, "selected.node" = terminal_ids[unlist(node.select[[1]])["split.varid"] - nouts])  # "- nouts" because the outcome variables are counted
    }
    dat <- dat[dat$.aloc != mytrees[[i]][[2]], -ncol(dat)]
  }
  mytrees[[i + 1]] <- list("tree" = ctree(formula, data = dat, minsplit = nrow(dat) + 1), "selected.node" = 1)
  names(mytrees) <- paste("segment", 1:length(mytrees))
  class(mytrees) <- "rseg"
  mytrees
}
