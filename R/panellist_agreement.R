#' Compute the Agreement between Panellists
#'
#' Compute the agreement between the panellists through correlations (Pearson or Spearman). This correlation is measured against the entire panel or the panel without that panellist.
#'
#' @param dataset Data frame to use
#' @param firstvar Integer: position of the first sensory attribute
#' @param lastvar Integer: position of the last sensory attribute
#' @param cor Character: 'full' or 'without' to consider or not the panellist in the panel scores
#' @param type Character: 'Pearson' or 'Spearman' to determine the type of correlation to consider
#'
#' @returns The table of Correlation
#' @export
#'
#' @examples NULL
panellist_agreement <- function(dataset, firstvar=6, lastvar=ncol(dataset), cor="without", type="Pearson"){

  # Setting up the dataset and getting some info
  attribute <- colnames(dataset)[firstvar:lastvar]
  nbatt <- length(attribute)
  dataset <- dataset %>%
    as.data.frame() %>%
    dplyr::mutate(Judge = as.factor(Judge)) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(attribute), as.character)) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(attribute), as.numeric))
  juge <- levels(dataset$Judge)
  nbjuge <- length(juge)

  # Preparing the results
  res_cor <- matrix(NA, nbjuge, nbatt, dimnames=list(juge,attribute))

  for (j in 1:nbjuge){

    # Getting the data from the assessor
    data_j <- dataset %>%
      dplyr::filter(Judge == juge[j]) %>%
      dplyr::select(Product, all_of(attribute)) %>%
      as.data.frame() %>%
      tibble::column_to_rownames(var="Product")

    # Getting the data from the panel
    # Options for cor are 'without' (cor vs. panel without that panelist) or 'full' (cor vs. panel incl. that panelist as well)
    if (cor == "without"){

      data_p <- dataset %>%
        dplyr::filter(Judge != juge[j]) %>%
        dplyr::select(Product, all_of(attribute)) %>%
        dplyr::group_by(Product) %>%
        dplyr::summarize_all(list(~mean(.))) %>%
        dplyr::ungroup() %>%
        tibble::column_to_rownames(var="Product")

    } else if (cor == "full"){

      data_p <- dataset %>%
        dplyr::select(Product, all_of(attribute)) %>%
        dplyr::group_by(Product) %>%
        dplyr::summarize_all(list(~mean(.))) %>%
        dplyr::ungroup() %>%
        tibble::column_to_rownames(var="Product")

    }

    # Getting and Storing the correlations
    if (type == "Pearson"){
      res_cor[juge[j],] <- diag(cor(data_p, data_j[rownames(data_p),]))
    } else if (type == "Spearman"){
      res_cor[juge[j],] <- diag(cor(data_p, data_j[rownames(data_p),], method="spearman"))
    }
  }

  return(res_cor)
}
