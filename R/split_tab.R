#' Split Tables
#'
#' Split tables into its value component and color (column ends with '_col') component.
#'
#' @param data Full data to be split
#'
#' @returns The 2 sets of table
#' @export
#'
#' @examples NULL
split_tab <- function(data){

  tab1 <- data %>%
    dplyr::select(-tidyselect::ends_with("_col")) %>%
    dplyr::mutate(dplyr::across(colnames(data)[2:ceiling(ncol(data)/2)], as.numeric))

  tab2 <- data %>%
    dplyr::select(1, tidyselect::ends_with("_col"))

  colnames(tab2) <- colnames(tab1)

  return(list(table=tab1, colour=tab2))
}
