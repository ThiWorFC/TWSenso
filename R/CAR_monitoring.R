#' Assess the Panel and Panelist Performance for QDA tests
#'
#' @param dataset Dataset to analyze
#' @param reference Character: informs the hiddent reference sample
#' @param ref_dev Threshold for the deviation from the reference
#' @param incl_duplo Boolean: does the test includes duplicated samples?
#' @param duplicate Vector with the names of the two duplicates
#' @param dup_dev Threshold for the difference between the duplicates
#' @param dis_dev Threshold for the sample discrimination
#' @param cor_val Threshold for the agreement (correlation)
#' @param panel_pmin Min pvalue for the panel discrimination
#' @param panel_pmax Max pvalue for the panel discrimination
#' @param panel_agrmin Min pvalue for the panel agreement
#' @param panel_agrmax Max pvalue for the panel agreement
#' @param sum_min Minimum value for the summary
#' @param sum_max Maximum value for the summary
#'
#' @returns The different tables of results at the panel and panelist level, including summaries (per Judge and per Attribute)
#' @export
#'
#' @examples NULL
CAR_monitoring <- function(dataset, reference=NULL, ref_dev=c(0.5,1),
                           incl_duplo=TRUE, duplicate=NULL, dup_dev=c(1,2),
                           dis_dev=c(2,3), cor_val=c(0.7,0.8),
                           panel_pmin=0.05, panel_pmax=0.1, panel_agrmin=0.1, panel_agrmax=0.2,
                           sum_min=50, sum_max=75){

  # Information regarding the data
  juge <- unique(dataset$Judge)
  nbjuge <- length(juge)
  attribute <- colnames(dataset)[6:ncol(dataset)]
  nbatt=length(attribute)

  dataset <- dataset %>%
    dplyr::filter(Session > 0)

  # Attribute check
  attr_rmv <- dataset %>%
    dplyr::select(tidyselect::all_of(attribute)) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), as.numeric)) %>%
    dplyr::mutate(id = row_number()) %>%
    tidyr::pivot_longer(-id, names_to="variable", values_to="score") %>%
    dplyr::group_by(variable) %>%
    dplyr::count(score) %>%
    dplyr::mutate(prop = n/sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(prop >= 0.9) %>%
    dplyr::pull(variable) %>%
    unique()

  if (length(attr_rmv) > 0){
    attribute <- attribute[!attribute %in% attr_rmv]
    nbatt = length(attribute)
    dataset <- dataset %>%
      dplyr::select(-tidyselect::all_of(attr_rmv))
  }

  ## Panelist ----

  # Hidden Reference
  ref_j <- dataset %>%
    dplyr::filter(Product %in% reference) %>%
    dplyr::select(Judge, tidyselect::all_of(attribute)) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), abs))

  ref_j_col <- ref_j %>%
    colour_code(value = ref_dev, direction=1)

  # Duplicate Samples
  if (incl_duplo){

    dup_j <- dataset %>%
      dplyr::filter(Product %in% duplicate) %>%
      dplyr::select(Judge, Product, tidyselect::all_of(attribute)) %>%
      tidyr::pivot_longer(tidyselect::all_of(attribute), names_to="Attribute", values_to="Score") %>%
      dplyr::mutate(Attribute = factor(Attribute, levels=attribute)) %>%
      dplyr::mutate(Score = as.numeric(Score)) %>%
      tidyr::pivot_wider(names_from=Product, values_from=Score) %>%
      dplyr::mutate(Difference = abs(.data[[duplicate[1]]] - .data[[duplicate[2]]])) %>%
      dplyr::select(Judge, Attribute, Difference) %>%
      tidyr::pivot_wider(names_from=Attribute, values_from=Difference)

    dup_j_col <- dup_j %>%
      colour_code(value = dup_dev, direction=1)

  } else {
    dup_j = NULL
    dup_j_col = NULL
  }

  # Discrimination
  dis_j <- dataset %>%
    dplyr::select(Judge, Product, tidyselect::all_of(attribute)) %>%
    tidyr::pivot_longer(tidyselect::all_of(attribute), names_to="Attribute", values_to="Score") %>%
    dplyr::mutate(Attribute = factor(Attribute, levels=attribute)) %>%
    dplyr::mutate(Score = as.numeric(Score)) %>%
    dplyr::group_by(Judge, Attribute) %>%
    dplyr::summarize(Range = diff(range(Score))) %>%
    tidyr::pivot_wider(names_from=Attribute, values_from=Range) %>%
    dplyr::ungroup()

  dis_j_col <- dis_j %>%
    colour_code(value = dis_dev, direction = 2)

  # Consensus
  agr_j <- panellist_agreement(dataset, firstvar=6, lastvar=ncol(dataset), cor="without") %>%
    tibble::as_tibble(rownames = "Judge") %>%
    dplyr::arrange(Judge)

  agr_j_col <- agr_j %>%
    colour_code(value = cor_val, direction = 2) %>%
    dplyr::arrange(Judge)

  ## Summary ----
  sum_j <- summary_tab(list(Reference = ref_j_col,
                            Discrimination = dis_j_col,
                            Reproducibility = dup_j_col,
                            Agreement = agr_j_col), type="Judge")
  sum_j_col <- sum_j %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) convert(x, sum_max, sum_min)))

  sum_a <- summary_tab(list(Reference = ref_j_col,
                            Discrimination = dis_j_col,
                            Reproducibility = dup_j_col,
                            Agreement = agr_j_col), type="Attribute") %>%
    dplyr::mutate(Attribute = factor(Attribute, levels=attribute)) %>%
    dplyr::arrange(Attribute)
  sum_a_col <- sum_a %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) convert(x, sum_max, sum_min)))

  ## Panel ----
  dataset2 <- dataset %>%
    dplyr::filter(Session > 0) %>%
    dplyr::select(1,2,6:ncol(dataset)) %>%
    tidyr::pivot_longer(-c(1,2), names_to="Attribute", values_to="Score") %>%
    dplyr::mutate(Attribute = factor(Attribute, levels=colnames(dataset)[6:ncol(dataset)])) %>%
    dplyr::mutate(Score = as.numeric(Score))

  # Discrimination
  res_aov <- dataset2 %>%
    dplyr::nest_by(Attribute) %>%
    dplyr::mutate(mod = list(aov(Score ~ Product + Judge, data=data))) %>%
    dplyr::reframe(broom::tidy(mod)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(term == "Product") %>%
    dplyr::select(Attribute, Discrimination = p.value)

  panel <- res_aov

  # Reference
  res_ref <- dataset2 %>%
    dplyr::filter(Product == reference) %>%
    dplyr::nest_by(Attribute) %>%
    dplyr::mutate(mod = list(t.test(data$Score, mu=0))) %>%
    dplyr::reframe(broom::tidy(mod)) %>%
    dplyr::ungroup() %>%
    dplyr::select(Attribute, Reference=p.value) %>%
    dplyr::mutate(Reference = ifelse(is.na(Reference), 1, Reference))

  panel <- panel %>%
    dplyr::full_join(res_ref, by="Attribute")

  # Reproducibility
  if (incl_duplo){
    res_dup <- dataset2 %>%
      dplyr::filter(Product %in% duplicate) %>%
      tidyr::pivot_wider(names_from=Product, values_from=Score) %>%
      dplyr::rename(Duplo1=3, Duplo2=4) %>%
      dplyr::nest_by(Attribute) %>%
      dplyr::mutate(mod = list(t.test(data$Duplo1, data$Duplo2, paired=TRUE))) %>%
      dplyr::reframe(broom::tidy(mod)) %>%
      dplyr::ungroup() %>%
      dplyr::select(Attribute, Reproducibility=p.value)

    panel <- panel %>%
      dplyr::full_join(res_dup, by="Attribute")
  }

  # Agreement
  res_agr <- dataset2 %>%
    dplyr::select(Judge, Product, Attribute, Score) %>%
    dplyr::group_by(Attribute, Judge) %>%
    dplyr::mutate(Score = rank(Score, ties.method = "average")) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from=Judge, values_from=Score) %>%
    dplyr::select(-Product) %>%
    split(.$Attribute) %>%
    purrr::map(function(data){
      DescTools::KendallW(x=data, correct=TRUE, test=TRUE)$estimate
    }) %>%
    tibble::enframe(name="Attribute", value="Agreement") %>%
    tidyr::unnest(Agreement)

  # Final Table
  panel <- panel %>%
    dplyr::full_join(res_agr, by="Attribute")

  panel_col <- panel %>%
    dplyr::mutate(Discrimination = dplyr::case_when(
      Discrimination <= panel_pmin ~ 1,
      Discrimination <= panel_pmax ~ 2,
      .default = 3)) %>%
    dplyr::mutate(Reference = dplyr::case_when(
      Reference >= panel_pmax ~ 1,
      Reference >= panel_pmin ~ 2,
      .default = 3)) %>%
    dplyr::mutate(Agreement = dplyr::case_when(
      Agreement >= panel_agrmax ~ 1,
      Agreement >= panel_agrmin ~ 2,
      .default = 3))

  if (incl_duplo){

    panel_col <- panel_col %>%
      dplyr::mutate(Reproducibility = dplyr::case_when(
        Reproducibility >= panel_pmax ~ 1,
        Reproducibility >= panel_pmin ~ 2,
        .default = 3))

  }

  ## Export ----
  res <- list()

  res$Panel <- panel %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) TWUtils::formatting(x, n=3))) %>%
    dplyr::full_join(panel_col, by="Attribute", suffix=c('','_col')) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))

  res$Panellist$Reference <- ref_j %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) TWUtils::formatting(x, n=2))) %>%
    dplyr::full_join(ref_j_col, by="Judge", suffix=c('','_col')) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))

  res$Panellist$Discrimination <- dis_j %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) TWUtils::formatting(x, n=2))) %>%
    dplyr::full_join(dis_j_col, by="Judge", suffix=c('','_col')) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))

  if (incl_duplo){

    res$Panellist$Reproducibility <- dup_j %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) TWUtils::formatting(x, n=2))) %>%
      dplyr::full_join(dup_j_col, by="Judge", suffix=c('','_col')) %>%
      dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))

  }

  res$Panellist$Agreement <- agr_j %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) TWUtils::formatting(x, n=2))) %>%
    dplyr::full_join(agr_j_col, by="Judge", suffix=c('','_col')) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))

  res$Summary$Panellist <- sum_j %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) TWUtils::formatting(x, n=0))) %>%
    dplyr::full_join(sum_j_col, by="Judge", suffix=c('','_col')) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))

  res$Summary$Attribute <- sum_a %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) TWUtils::formatting(x, n=0))) %>%
    dplyr::full_join(sum_a_col, by="Attribute", suffix=c('','_col')) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))

  res$attr_rmv <- attr_rmv

  return(res)
}
