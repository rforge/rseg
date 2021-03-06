#' Recursive Segementation by Conditional Inference
#'
#' Recursive segmentation based on conditional inference trees.
#'
#' The algorithm makes use of \link[partykit]{ctree} to construct a recursive segmentation model. There is a \link[=predict.rseg]{predict}, \link[=plot.rseg]{plot}, \link[=summary.rseg]{summary} and \link[=print.rseg]{print} function. The \link[=gettree]{gettree} function can be used to extract the correspoding tree model. See the corresponding documentation for details.
#'
#'
#'
#' @return
#'
#' An object of class \code{rseg}.
#'
#' @param formula a symbolic description of the model to be fit.
#' @param data a data frame that contains the variables in the model.
#' @param maxsegs maximal number of segments
#' @param maxdepth maximal depth of the tree models used for recursive segmentation. The number of decision rules that define a segment can be controled this way.
#' @param minsplit minimal size of a subset to allow for further segmentation.
#' @param minbucket minimal size of a segment.
#' @param ... further arguments passed to \link[partykit]{ctree_control}.
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
#' ### regression
#' airq <- subset(airquality, !is.na(Ozone))
#' airseg <- cseg(Ozone ~ ., data = airq)
#' airseg
#' plot(airseg)
#'
#' # Here, observations with missing values are processed by surrogate splits
#' airseg2 <- cseg(Ozone ~ ., data = airq, maxsurrogate = 1)
#' airseg2
#' plot(airseg2)
#'
#' ### classification
#' irisseg <- cseg(Species ~ ., data = iris)
#' irisseg
#' plot(irisseg)
#'
#' ### survival analysis
#' if (require("TH.data")) {
#' data("GBSG2", package = "TH.data")
#' GBSG2seg <- cseg(Surv(time, cens) ~ ., data = GBSG2)
#' print(GBSG2seg)
#' plot(GBSG2seg)
#' }
#'
#' ### multivariate responses
#' airseg3 <- cseg(Ozone + Temp ~ ., data = airq)
#' airseg3

cseg <- function(formula, data, maxsegs = Inf, maxdepth = 10L, minsplit = 20L, minbucket = 7L, ...) {
  if (maxsegs <= 1) stop("'maxsegs' needs to be >1")
  nouts <- length(unlist(strsplit(as.character(formula[2]), "[+]"))) # determine the number of outcome variables
  terminal_ids <- 999  # dummy to enable start of the loop
  mytrees <- list()  # list to contain the segments
  dat <- data  # data used in the loops
  i <- 0  # counter to fill in the list
  while(max(terminal_ids) > 1) {
    i <- i + 1
    mytree <- ctree(formula, data = dat, minsplit = minsplit, minbucket = minbucket, maxdepth = maxdepth, ...)
    terminal_ids <- nodeids(mytree, terminal = TRUE)
    if (max(terminal_ids) == 1) {
      mytrees[[i]] <- list("tree" = mytree, "selected.node" = 1)
    } else {
      dat$.aloc <- predict(mytree, type = "node")
      if (max(terminal_ids) == 3) {
        mytrees[[i]] <- list("tree" = mytree, "selected.node" = 3)
      } else {
        node.select <- nodeapply(ctree(update(formula, as.formula(paste("~ factor(.aloc == ", paste(terminal_ids, collapse = ") + factor(.aloc == "), ")"))), data = dat, minsplit = 2, minbucket = 1, stump = TRUE, mincriterion = 0))
        mytrees[[i]] <- list("tree" = mytree, "selected.node" = terminal_ids[unlist(node.select[[1]])["split.varid"] - nouts])  # "- nouts" because the outcome variables are counted
      }
      dat <- dat[dat$.aloc != mytrees[[i]][[2]], -ncol(dat)]
      if (length(mytrees) == maxsegs-1) {
        mytrees[[i + 1]] <- list("tree" = ctree(formula, data = dat, minsplit = nrow(dat) + 1), "selected.node" = 1)
        break
      }
    }
  }
  names(mytrees) <- paste("segment", 1:length(mytrees))
  class(mytrees) <- "rseg"
  mytrees
}
