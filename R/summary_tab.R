#' Build the Summary Tables
#'
#' Build the summary tables for the different abilities.
#'
#' @param data Data to use
#' @param type Character: 'Judge' or 'Attribute'
#'
#' @returns A summary table for either the Judge or the Attribute
#' @export
#'
#' @examples NULL
summary_tab <- function(data, type="Judge"){

  if (type == "Judge"){

    summary <- data %>%
      tibble::enframe(name="Ability", value="Results") %>%
      tidyr::unnest(Results) %>%
      dplyr::mutate(Ability = factor(Ability, levels=names(data))) %>%
      tidyr::pivot_longer(-c(1,2), names_to="Attribute", values_to="Scores") %>%
      tidyr::separate(Judge, c("Judge", "Rep"), sep="_") %>%
      dplyr::group_by(Ability, Judge, Attribute) %>%
      dplyr::summarise(Scores = mean(Scores)) %>%
      dplyr::mutate(Scores = case_when(
        Scores == 0 ~ 0,
        Scores < 2 ~ 1,
        Scores > 2 ~ 3,
        .default=2)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Ability, Judge) %>%
      dplyr::summarise(Results = 100*sum(Scores<3 & Scores>0)/sum(Scores>0)) %>%
      tidyr::pivot_wider(names_from=Ability, values_from=Results) %>%
      dplyr::ungroup()

  } else if (type == "Attribute"){

    summary <- data %>%
      tibble::enframe(name="Ability", value="Results") %>%
      tidyr::unnest(Results) %>%
      dplyr::mutate(Ability = factor(Ability, levels=names(data))) %>%
      pivot_longer(-c(1,2), names_to="Attribute", values_to="Scores") %>%
      tidyr::separate(Judge, c("Judge", "Rep"), sep="_") %>%
      dplyr::group_by(Ability, Judge, Attribute) %>%
      dplyr::summarise(Scores = mean(Scores)) %>%
      dplyr::mutate(Scores = ifelse(Scores < 2, 1, ifelse(Scores > 2, 3, 2))) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Ability, Attribute) %>%
      dplyr::summarise(Results = 100*sum(Scores<3 & Scores>0)/sum(Scores>0)) %>%
      tidyr::pivot_wider(names_from=Ability, values_from=Results) %>%
      dplyr::ungroup()
  }

  return(summary)
}
