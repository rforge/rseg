#' Recursive Segmentation by CART
#'
#' Recursive segmentation based on the CART algorithm.
#'
#' The algorithm makes use of \link[rpart]{rpart} to construct a recursive segmentation model. The \link[rpart]{prune} funktion is used to determine CART trees of optimal size by the minimal cross-validated error estimate. There is a \link[=predict.rseg]{predict}, \link[=plot.rseg]{plot}, \link[=summary.rseg]{summary} and \link[=print.rseg]{print} function. The \link[=gettree]{gettree} function can be used to extract the correspoding tree model. See the corresponding documentation for details.
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
#' @param ... further arguments passed to \link[rpart]{rpart.control}.
#'
#' @export
#' @import stats
#' @import rpart
#'
#' @references{
#' \insertRef{Hapfelmeier2018}{rseg}
#' }
#' @importFrom Rdpack reprompt
#'
#' @examples
#' ### regression
#' airq <- subset(airquality, !is.na(Ozone))
#' set.seed(1234)
#' airseg <- rseg(Ozone ~ ., data = airq)
#' airseg
#' plot(airseg)
#'
#' ### classification
#' set.seed(1234)
#' irisseg <- rseg(Species ~ ., data = iris)
#' irisseg
#' plot(irisseg)
#'
#' ### survival analysis
#' if (require("TH.data")) {
#' data("GBSG2", package = "TH.data")
#' set.seed(1234)
#' GBSG2seg <- rseg(Surv(time, cens) ~ ., data = GBSG2)
#' print(GBSG2seg)
#' plot(GBSG2seg)
#' }

rseg <- function(formula, data, maxsegs = Inf, maxdepth = 10L, minsplit = 20L, minbucket = 7L, ...) {
  if (maxsegs <= 1) stop("'maxsegs' needs to be >1")
  nouts <- length(unlist(strsplit(as.character(formula[2]), "[+]"))) # determine the number of outcome variables
  if (nouts > 1) stop("the rpart routine does currently not support multivariate outcomes")
  terminal_ids <- 999  # dummy to enable start of loop
  mytrees <- list()  # list to contain the segments
  rSegdat <- droplevels(data, except = all.vars(formula)[1])  # data used in the loops
  i <- 0  # counter to fill in the list
  while(max(terminal_ids) > 3 & length(mytrees) < maxsegs-1) {
    i <- i + 1
    assign("rSegdat", rSegdat, .GlobalEnv)
    mytree <- rpart::rpart(formula, data = rSegdat, minsplit = minsplit, minbucket = minbucket, maxdepth = maxdepth, ...)
    mytree <- prune(mytree, cp = mytree$cptable[which.min(mytree$cptable[,"xerror"]),"CP"])
    mytree <- as.party(mytree)
    terminal_ids <- nodeids(mytree, terminal = TRUE)
    if (max(terminal_ids) == 1) {
      mytrees[[i]] <- list("tree" = ctree(formula, data = rSegdat, minsplit = nrow(rSegdat) + 1), "selected.node" = 1)
    } else {
      rSegdat$.aloc <- predict(mytree, type = "node")
      if (max(terminal_ids) == 3) {
        mytrees[[i]] <- list("tree" = mytree, "selected.node" = 3)
        mytrees[[i + 1]] <- list("tree" = ctree(formula, data = rSegdat[rSegdat$.aloc != 3, -ncol(rSegdat)], minsplit = nrow(rSegdat) + 1), "selected.node" = 1)
      } else {
        assign("rSegdat", rSegdat, .GlobalEnv)
        node.select <- nodeapply(as.party(rpart(update(formula, as.formula(paste("~ factor(.aloc == ", paste(terminal_ids, collapse = ") + factor(.aloc == "), ")"))), data = rSegdat, minsplit = 2, minbucket = 1, maxdepth = 1, cp = -1)))
        mytrees[[i]] <- list("tree" = mytree, "selected.node" = terminal_ids[unlist(node.select[[1]])["split.varid"] - nouts])  # "- nouts" because the outcome variables are counted
        rSegdat <- droplevels(rSegdat[rSegdat$.aloc != mytrees[[i]][[2]], -ncol(rSegdat)], except = all.vars(formula)[1])
        if (length(unique(rSegdat[, all.vars(formula)[1]])) == 1 | length(mytrees) == maxsegs-1) { # when the remaining outcome is unique | maxseg-1 is reached
          mytrees[[i + 1]] <- list("tree" = ctree(formula, data = rSegdat, minsplit = nrow(rSegdat) + 1), "selected.node" = 1)
          break
        }
      }
    }
  }
  if (nrow(mytrees[[1]][[1]]$data) == 0) mytrees[[1]][[1]]$data.save <- data # when there is only a root
  rm(rSegdat)
  names(mytrees) <- paste("segment", 1:length(mytrees))
  class(mytrees) <- "rseg"
  mytrees
}
