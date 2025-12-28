#' Assess the Panel and Panelist Performance for ODP tests
#'
#' Automated analysis for the panel and panelist performance in an ODP test. Both the intensity scores and the ranks are evaluated.
#'
#' @param dataset Dataset to analyze
#' @param duplicate Vector with the names of the two duplicates
#' @param lim_score Threshold for the difference between the scores of the duplicates
#' @param lim_rank Threshold for the difference between the ranks of the duplicates
#' @param lim_discri Threshold for the sample discrimination
#' @param lim_cor Threshold for the agreement (correlation)
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
ODP_monitoring <- function(dataset, duplicate, lim_score=20, lim_rank=3,
                           lim_discri=c(10,20), lim_cor=0.8,
                           panel_pmin=0.05, panel_pmax=0.05, panel_agrmin=0.1, panel_agrmax=0.2,
                           sum_min=50, sum_max=75){


    # Checking and Extracting Information from the Dataset
  if (max(dataset$Session)>1){
    dataset <- dataset %>%
      tidyr::unite(Judge, Judge, Session, sep="_", remove=FALSE)
  } else if (max(dataset$Replica)>1){
    dataset <- dataset %>%
      tidyr::unite(Judge, Judge, Replica, sep="_", remove=FALSE)
  }

  juge <- unique(dataset$Judge)
  nbjuge <- length(juge)
  product <- unique(dataset$Product)
  nbprod <- length(product)
  attribute <- colnames(dataset)[6:ncol(dataset)]
  nbatt <- length(attribute)

  ## Panelist ----

  # Discrimination
  panellist_dis <- dataset %>%
    tidyr::pivot_longer(tidyselect::all_of(attribute), names_to="Attribute", values_to="Score") %>%
    dplyr::mutate(Attribute = factor(Attribute, levels=attribute)) %>%
    dplyr::arrange(Judge, Attribute, Product) %>%
    dplyr::mutate(Score = as.numeric(as.character(Score))) %>%
    dplyr::group_by(Judge, Attribute) %>%
    dplyr::summarize(Discrimination = diff(range(Score))) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from=Attribute, values_from=Discrimination)

  panellist_dis_col <- panellist_dis %>%
    colour_code(value = lim_discri, direction = 2)

  # Reproducibility
  panellist_rep <- dataset %>%
    tidyr::pivot_longer(tidyselect::all_of(attribute), names_to="Attribute", values_to="Score") %>%
    dplyr::mutate(Attribute = factor(Attribute, levels=attribute)) %>%
    dplyr::arrange(Judge, Attribute, Product) %>%
    dplyr::mutate(Score = as.numeric(as.character(Score))) %>%
    dplyr::group_by(Judge, Attribute) %>%
    dplyr::mutate(Rank = rank(Score, ties.method="average")) %>%
    dplyr::mutate(min = min(Score), coef=100/(max(Score)-min(Score))) %>%
    dplyr::mutate(coef = ifelse(is.infinite(coef), 1, coef)) %>%
    dplyr::mutate(Score = (Score-min)*coef) %>%
    dplyr::select(-c(min, coef)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(Score:Rank, names_to="Type", values_to="Value") %>%
    dplyr::filter(Product %in% duplicate) %>%
    tidyr::pivot_wider(names_from=Product, values_from=Value) %>%
    dplyr::mutate(Difference = abs(.data[[duplicate[2]]]-.data[[duplicate[1]]])) %>%
    dplyr::select(Judge, Attribute, Type, Difference) %>%
    tidyr::pivot_wider(names_from=Type, values_from=Difference)

  panellist_score <- panellist_rep %>%
    dplyr::arrange(Attribute) %>%
    dplyr::select(-Rank) %>%
    tidyr::pivot_wider(names_from="Attribute", values_from="Score")

  panellist_score_col <- panellist_score %>%
    colour_code(value = lim_score, direction=1)

  panellist_rank <- panellist_rep %>%
    dplyr::arrange(Attribute) %>%
    dplyr::select(-Score) %>%
    tidyr::pivot_wider(names_from="Attribute", values_from="Rank")

  panellist_rank_col <- panellist_rank %>%
    colour_code(value = lim_rank, direction=1)

  # Agreement
  panellist_agree <- round(panellist_agreement(dataset, cor="without"),2) %>%
    tibble::as_tibble(rownames = "Judge") %>%
    dplyr::arrange(Judge)

  panellist_agree_col <- panellist_agree %>%
    colour_code(value = lim_cor, direction = 2) %>%
    dplyr::arrange(Judge)

  ## Summary ----
  j_sum <- summary_tab(list(Discrimination = panellist_dis_col,
                            Score = panellist_score_col,
                            Rank = panellist_rank_col,
                            Agreement = panellist_agree_col), type="Judge")
  j_sum_col <- j_sum %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) convert(x, sum_max, sum_min)))

  a_sum <- summary_tab(list(Discrimination = panellist_dis_col,
                            Score = panellist_score_col,
                            Rank = panellist_rank_col,
                            Agreement = panellist_agree_col), type="Attribute") %>%
    dplyr::mutate(Attribute = factor(Attribute, levels=attribute)) %>%
    dplyr::arrange(Attribute)
  a_sum_col <- a_sum %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) convert(x, sum_max, sum_min)))

  ## Panel ----
  dataset2 <- dataset %>%
    tidyr::pivot_longer(tidyselect::all_of(attribute), names_to="Attribute", values_to="Score") %>%
    dplyr::mutate(Attribute = factor(Attribute, levels=attribute)) %>%
    dplyr::mutate(Score = as.numeric(Score))

  # Discrimination
  data_model <- dataset2 %>%
    dplyr::nest_by(Attribute) %>%
    dplyr::mutate(mod1 = list(aov(Score ~Product + Judge, data=data)),
           mod2 = list(friedman.test(Score ~ Product|Judge, data)))

  ## ANOVA
  res_aov <- data_model %>%
    dplyr::reframe(broom::tidy(mod1)) %>%
    dplyr::filter(term == "Product") %>%
    dplyr::select(Attribute, Discrimination = p.value) %>%
    dplyr::ungroup()

  ## Friedman
  res_friedman <- data_model %>%
    dplyr::reframe(broom::tidy(mod2)) %>%
    dplyr::select(Attribute, `Discrimination (Rank)` = p.value) %>%
    dplyr::ungroup()

  # Reproducibility
  res_dup <- dataset2 %>%
    dplyr::filter(Product %in% duplicate) %>%
    tidyr::pivot_wider(names_from=Product, values_from=Score) %>%
    dplyr::rename(Duplo1=3, Duplo2=4) %>%
    dplyr::nest_by(Attribute) %>%
    dplyr::mutate(mod = list(t.test(data$Duplo1, data$Duplo2, paired=TRUE))) %>%
    dplyr::reframe(broom::tidy(mod)) %>%
    dplyr::ungroup() %>%
    dplyr::select(Attribute, Reproducibility=p.value)

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

  panel <- dplyr::full_join(res_aov, res_friedman, by="Attribute") %>%
    dplyr::full_join(res_dup, by="Attribute") %>%
    dplyr::full_join(res_agr, by="Attribute")

  panel_col <- panel %>%
    dplyr::mutate(Discrimination = dplyr::case_when(
      Discrimination <= panel_pmin ~ 1,
      Discrimination <= panel_pmax ~ 2,
      .default = 3)) %>%
    dplyr::mutate(`Discrimination (Rank)` = dplyr::case_when(
      `Discrimination (Rank)` <= panel_pmin ~ 1,
      `Discrimination (Rank)` <= panel_pmax ~ 2,
      .default = 3)) %>%
    dplyr::mutate(Reproducibility = dplyr::case_when(
      Reproducibility >= panel_pmax ~ 1,
      Reproducibility >= panel_pmin ~ 2,
      .default = 3)) %>%
    dplyr::mutate(Agreement = dplyr::case_when(
      Agreement >= panel_agrmax ~ 1,
      Agreement >= panel_agrmin ~ 2,
      .default = 3))

  ## Export ----
  res <- list()

  res$Panel <- panel %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) TWUtils::formatting(x, n=3))) %>%
    dplyr::full_join(panel_col, by="Attribute", suffix=c('','_col')) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))

  res$Panellist$Discrimination <- panellist_dis %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) TWUtils::formatting(x, n=2))) %>%
    dplyr::full_join(panellist_dis_col, by="Judge", suffix=c('','_col')) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))

  res$Panellist$Score <- panellist_score %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) TWUtils::formatting(x, n=2))) %>%
    dplyr::full_join(panellist_score_col, by="Judge", suffix=c('','_col')) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))

  res$Panellist$Rank <- panellist_rank %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) TWUtils::formatting(x, n=2))) %>%
    dplyr::full_join(panellist_rank_col, by="Judge", suffix=c('','_col')) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))

  res$Panellist$Agreement <- panellist_agree %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) TWUtils::formatting(x, n=2))) %>%
    dplyr::full_join(panellist_rank_col, by="Judge", suffix=c('','_col')) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))

  res$Summary$Panellist <- j_sum %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) TWUtils::formatting(x, n=0))) %>%
    dplyr::full_join(j_sum_col, by="Judge", suffix=c('','_col')) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))

  res$Summary$Attribute <- a_sum %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) TWUtils::formatting(x, n=0))) %>%
    dplyr::full_join(a_sum_col, by="Attribute", suffix=c('','_col')) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))

  return(res)
}
