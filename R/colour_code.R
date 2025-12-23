#' Transforms Table into Corresponding Colour Coded Table
#'
#' @param data Data to colour code
#' @param value Vector containing the set of 2 values to use for the colour coding to good/acceptable/bad
#' @param direction Integer: 1 means 'lower than min is good, and higher than max is bad'
#'
#' @returns The corresponding table with the colour code.
#' @export
#'
#' @examples NULL
colour_code <- function(data, value, direction){

  # Direction 1 means lower than min is good, and higher than max is bad
  # Colour code is 1=good, 2=acceptable, 3=bad

  tabcol <- data %>%
    tidyr::pivot_longer(-1, names_to="Attribute", values_to="Scores") %>%
    dplyr::mutate(Scores = as.numeric(Scores)) %>%
    dplyr::mutate(Attribute = factor(Attribute, levels=colnames(data))) %>%
    dplyr::arrange(Attribute) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Colour = dplyr::case_when(
      (direction == 1 & Scores <= min(value)) ~1,
      (direction == 1 & Scores <= max(value)) ~2,
      (direction == 2 & Scores >= max(value)) ~1,
      (direction == 2 & Scores >= min(value)) ~2,
      .default = 3)) %>%
    dplyr::select(-Scores) %>%
    dplyr::mutate(Colour = tidyr::replace_na(Colour, 0)) %>%
    tidyr::pivot_wider(names_from=Attribute, values_from=Colour, values_fill=0)

  return(tabcol)
}
