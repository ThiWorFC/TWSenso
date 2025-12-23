#' Converts values to colour code
#'
#' Transform values into colour code based on pre-set limits: If x >= value1 -> 1, if x >= value2 --> 2, else 3
#'
#' @param x Integer to convert
#' @param value1 Integer for the lower limit
#' @param value2 Integer for the higher limit
#'
#' @returns Integer that corresponds to the colour code for x based on value1 and value2
#' @export
#'
#' @examples NULL
convert <- function(x, value1, value2){

  dplyr::case_when(x >= value1 ~ 1,
            x >= value2 ~ 2,
            .default = 3)

}
