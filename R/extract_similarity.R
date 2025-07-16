#' Extract Random Forest Similarity Scores
#'
#' Calculates similarity scores between new observations and reference data points based on a
#' fitted random forest model. The similarity reflects the frequency with which each new data point
#' shares terminal nodes with each reference data point across the forest, weighted appropriately.
#' This corresponds to the adaptive nearest neighbors interpretation of random forests.
#'
#' Supports random forest models fitted with either the \pkg{ranger} or \pkg{randomForest} package.
#'
#' @param object A fitted random forest model. Must be an object of class \code{"ranger"} (from the
#' \pkg{ranger} package) or \code{"randomForest"} (from the \pkg{randomForest} package).
#' @param newdata A data frame or matrix of new observations to compare against \code{refdata}.
#' @param refdata A data frame or matrix containing the reference data points (e.g., training data
#' or held-out data).
#' @param match_training Logical. If \code{TRUE}, uses in-bag sampling frequencies to weight the
#' similarity scores based on how often each reference point appeared in each bootstrap sample.
#' Must be \code{TRUE} if \code{refdata} was used to train the model and you want the similarity to
#' reflect the adaptive nearest neighbor weighting. For this to work, \code{object} must have kept
#' track of the training inbag counts, i.e. \code{keep.inbag = TRUE}, which is not default behavior.
#'
#' @return A numeric matrix of dimension \code{nrow(newdata)} by \code{nrow(refdata)}, where each
#' entry represents the similarity score between a new point and a reference point. The rows sum
#' (approximately) to 1.
#'
#' @details
#' For each tree in the forest, new and reference points are considered similar if they fall into
#' the same terminal node. Similarity scores are calculated as the normalized frequency with which
#' pairs share terminal nodes, adjusted by the in-bag counts of reference points when
#' \code{match_training = TRUE}. This weighting ensures that predictions made using these
#' similarity scores match the forest's behavior when trained using bootstrap samples.
#'
#' The function requires that \code{inbag.counts} (for \code{ranger}) or \code{inbag}
#' (for \code{randomForest}) be available in the fitted object when \code{match_training = TRUE}.
#'
#' @examples
#' \dontrun{
#' # With ranger
#' library(ranger)
#' rf_ranger <- ranger(Sepal.Length ~ ., data = iris, num.trees = 100, keep.inbag = TRUE)
#' sim_ranger <- extract_similarity(rf_ranger, newdata = iris, refdata = iris)
#'
#' # With randomForest
#' library(randomForest)
#' rf_rf <- randomForest(Sepal.Length ~ ., data = iris, ntree = 100, keep.inbag = TRUE)
#' sim_rf <- extract_similarity(rf_rf, newdata = iris, refdata = iris)
#' }
#'
#' @export
extract_similarity <- function(object, newdata, refdata, match_training = TRUE) {

  if ("ranger" %in% class(object)) {
    if (match_training) {
      if (!is.null(object$inbag.counts)) {
        ref_multiplier <- do.call(cbind, object$inbag.counts)
      } else {
        stop("`object` does not include inbag counts (necessary for `match_training` = TRUE)")
      }
    }
    num_trees <- object$num.trees
    terminal_nodes_new <- stats::predict(object, data = newdata, type = "terminalNodes")$predictions
    terminal_nodes_ref <- stats::predict(object, data = refdata, type = "terminalNodes")$predictions

  } else if ("randomForest" %in% class(object)) {
    if (match_training) {
      if (!is.null(object$inbag)) {
        ref_multiplier <- object$inbag
      } else {
        stop("`object` does not include inbag counts (necessary for `match_training` = TRUE)")
      }
    }
    num_trees <- object$ntree
    terminal_nodes_new <- attr(stats::predict(object, newdata = newdata, nodes = TRUE), "nodes")
    terminal_nodes_ref <- attr(stats::predict(object, newdata = refdata, nodes = TRUE), "nodes")

  } else {
    stop("`object` must be either a fitted `ranger` model or a fitted `randomForest` model")
  }

  if (!match_training) {
    ref_multiplier <- matrix(1, nrow = nrow(refdata), ncol = num_trees)
  }

  similarity_matrix <- matrix(0, nrow = nrow(newdata), ncol = nrow(refdata))

  for (t in seq_len(num_trees)) {

    node_new <- terminal_nodes_new[, t]
    node_ref <- terminal_nodes_ref[, t]
    tree_weight <- ref_multiplier[, t]

    node_map_new <- split(seq_len(nrow(newdata)), node_new)
    node_map_ref <- split(seq_len(nrow(refdata)), node_ref)

    for (node in intersect(names(node_map_new), names(node_map_ref))) {
      idx_new <- node_map_new[[node]]
      idx_ref <- node_map_ref[[node]]

      if (length(idx_ref) > 0) {
        w <- tree_weight[idx_ref]
        w_norm <- (w / sum(w)) / num_trees
        similarity_matrix[idx_new, idx_ref] <- similarity_matrix[idx_new, idx_ref] +
          matrix(w_norm, nrow = length(idx_new), ncol = length(w_norm), byrow = TRUE)
      }
    }
  }

  similarity_matrix
}
