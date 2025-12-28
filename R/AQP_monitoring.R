#' Assess the Panel and Panelist Performance for QDA tests
#'
#' Automated analysis for the panel and panelist performance in AQP test.
#'
#' @param dataset Dataset to analyze
#' @param hidden_ref Name of the hidden reference sample (or 'none')
#' @param dif_ref Threshold for the difference between the reference and the hidden reference
#' @param incl_duplo Boolean: does the test includes duplicated samples?
#' @param duplicate Vector with the names of the two duplicates
#' @param dif_duplo Threshold for the difference between the duplicates
#' @param dif_range Threshold for the range between the samples
#' @param cor_lev Threshold for the correlation
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
AQP_monitoring <- function(dataset, hidden_ref="none", dif_ref=10,
                           incl_duplo=TRUE, duplicate, dif_duplo=20,
                           dif_range=30, cor_lev=0.70,
                           panel_pmin=0.05, panel_pmax=0.05, panel_agrmin=0.1, panel_agrmax=0.2,
                           sum_min=50, sum_max=75){

  # Info about the data
  attribute <- colnames(dataset)[6:ncol(dataset)]
  nbatt <- length(attribute)
  juge <- levels(factor(dataset$Judge))
  nbjuge <- length(juge)

  # Get the Reference Information
  if (hidden_ref != "none"){
    data_ref <- dataset %>%
      dplyr::mutate(Product = as.character(Product)) %>%
      dplyr::filter(Session == 0) %>%
      dplyr::select(Product, tidyselect::all_of(attribute))

    if (nrow(data_ref)>0){
      use_ref = TRUE

      data_ref = data_ref %>%
        tidyr::pivot_longer(-Product, names_to="Attribute", values_to="Score") %>%
        dplyr::group_by(Product, Attribute) %>%
        dplyr::summarize(Score = mean(Score, na.rm=TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Product = "Reference") %>%
        tidyr::pivot_wider(names_from=Product, values_from=Score)
    } else {
      use_ref = FALSE
    }

  } else {
    use_ref=FALSE
  }

  dataset <- dataset %>%
    dplyr::filter(Session>0)

  ## Panelist Level ----

  # Reference
  if (use_ref){

    j_ref <- dataset %>%
      dplyr::filter(Product == hidden_ref) %>%
      tidyr::pivot_longer(tidyselect::all_of(attribute), names_to="Attribute", values_to="Score") %>%
      dplyr::inner_join(data_ref, by="Attribute") %>%
      dplyr::mutate(Score = Score - Reference) %>%
      dplyr::select(Judge, Attribute, Score) %>%
      tidyr::pivot_wider(names_from="Attribute", values_from="Score")

    j_ref_col <- j_ref %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), abs)) %>%
      colour_code(value=dif_ref, direction=1)

  } else {
    j_ref = NULL
    j_ref_col = NULL
  }

  # Discrimination
  j_discri <- dataset %>%
    dplyr::select(Judge, tidyselect::all_of(attribute)) %>%
    dplyr::group_by(Judge) %>%
    dplyr::summarize(dplyr::across(tidyselect::where(is.numeric()), \(x) diff(range(x))))

  j_discri_col <- j_discri %>%
    colour_code(value=dif_range, direction=2)

  # Reproducibility
  if (incl_duplo){

    j_duplo <- dataset %>%
      dplyr::filter(Product %in% duplicate) %>%
      dplyr::select(Judge, tidyselect::all_of(attribute)) %>%
      dplyr::group_by(Judge) %>%
      dplyr::summarize(dplyr::across(tidyselect::where(is.numeric()), diff))

    j_duplo_col <- j_duplo %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), abs)) %>%
      colour_code(value=dif_duplo, direction=1)

  } else {
    j_duplo = NULL
    j_duplo_col = NULL
  }

  # Agreement
  j_agree <- panellist_agreement(dataset) %>%
    tibble::as_tibble(rownames = "Judge") %>%
    dplyr::arrange(Judge)

  j_agree_col <- j_agree %>%
    colour_code(value=cor_lev, direction=2) %>%
    dplyr::arrange(Judge)

  ## Summary ----
  j_sum <- summary_tab(list(Reference = j_ref_col,
                            Discrimination = j_discri_col,
                            Reproducibility = j_duplo_col,
                            Agreement = j_agree_col), type="Judge")
  j_sum_col <- j_sum %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) convert(x,sum_max,sum_min)))

  a_sum <- summary_tab(list(Reference = j_ref_col,
                            Discrimination = j_discri_col,
                            Reproducibility = j_duplo_col,
                            Agreement = j_agree_col), type="Attribute") %>%
    dplyr::mutate(Attribute = factor(Attribute, levels=attribute)) %>%
    dplyr::arrange(Attribute)

  a_sum_col <- a_sum %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) convert(x,sum_max,sum_min)))

  ## Panel Level ----
  dataset2 <- dataset %>%
    dplyr::select(1,2,6:ncol(.)) %>%
    tidyr::pivot_longer(3:ncol(.), names_to="Attribute", values_to="Score") %>%
    dplyr::mutate(Attribute = factor(Attribute, levels=colnames(dataset)[6:ncol(dataset)])) %>%
    dplyr::arrange(Attribute)

  # ANOVA
  panel_aov <- dataset2 %>%
    dplyr::nest_by(Attribute) %>%
    dplyr::mutate(mod = list(lm(Score ~ Product + Judge, data=data))) %>%
    dplyr::reframe(broom::tidy(anova(mod))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(term == "Product") %>%
    dplyr::select(Attribute, Discrimination = p.value)

  panel <- panel_aov

  # Reference
  if (use_ref){
    panel_ref <- dataset2 %>%
      dplyr::filter(Product == hidden_ref) %>%
      dplyr::inner_join(data_ref, by="Attribute") %>%
      dplyr::nest_by(Attribute) %>%
      dplyr::mutate(mod = list(t.test(data$Score, mu=mean(data$Reference), paired=FALSE))) %>%
      dplyr::reframe(broom::tidy(mod)) %>%
      dplyr::ungroup() %>%
      dplyr::select(Attribute, Reference=p.value)

    panel <- panel %>%
      dplyr::full_join(panel_ref, by="Attribute")
  }

  # Duplicates
  if (incl_duplo){

    panel_dup <- dataset2 %>%
      dplyr::filter(Product %in% duplicate) %>%
      tidyr::pivot_wider(names_from=Product, values_from=Score) %>%
      dplyr::rename(Duplo1=3, Duplo2=4) %>%
      dplyr::nest_by(Attribute) %>%
      dplyr::mutate(mod = list(t.test(data$Duplo1, data$Duplo2, paired=TRUE))) %>%
      dplyr::reframe(broom::tidy(mod)) %>%
      dplyr::ungroup() %>%
      dplyr::select(Attribute, Reproducibility=p.value)

    panel <- panel %>%
      dplyr::full_join(panel_dup, by="Attribute")
  }

  # Agreement
  panel_agr <- dataset2 %>%
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

  # Export Panel
  panel <- panel %>%
    dplyr::full_join(panel_agr, by="Attribute")

  panel_col <- panel %>%
    dplyr::mutate(Discrimination = dplyr::case_when(
      Discrimination <= panel_pmin ~ 1,
      Discrimination <= panel_pmax ~ 2,
      .default = 3)) %>%
    dplyr::mutate(Agreement = dplyr::case_when(
      Agreement >= panel_agrmax ~ 1,
      Agreement >= panel_agrmin ~ 2,
      .default = 3))

  if ("Reference" %in% colnames(panel_col)){
    panel_col <- panel_col %>%
      dplyr::mutate(Reference = dplyr::case_when(
        Reference >= panel_pmax ~ 1,
        Reference >= panel_pmin ~ 2,
        .default = 3))
  }

  if (incl_duplo){
    panel_col <- panel_col %>%
      dplyr::mutate(Reproducibility = dplyr::case_when(
        Reproducibility >= panel_pmax ~ 1,
        Reproducibility >= panel_pmin ~ 2,
        .default = 3))
  }

  ## Export the Results ----
  res <- list()
  res$Panel <- panel %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) TWUtils::formatting(x, n=3))) %>%
    dplyr::full_join(panel_col, by="Attribute", suffix=c('','_col')) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))

  if (use_ref){
    res$Panellist$Reference <- j_ref %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) TWUtils::formatting(x, n=2))) %>%
      dplyr::full_join(j_ref_col, by="Judge", suffix=c('','_col')) %>%
      dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))
  }

  res$Panellist$Discrimination <- j_discri %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) TWUtils::formatting(x, n=2))) %>%
    dplyr::full_join(j_discri_col, by="Judge", suffix=c('','_col')) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))

  if (incl_duplo){
    res$Panellist$Reproducibility <- j_duplo %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) TWUtils::formatting(x, n=2))) %>%
      dplyr::full_join(j_duplo_col, by="Judge", suffix=c('','_col')) %>%
      dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))
  }

  res$Panellist$Agreement <- j_agree %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric()), \(x) TWUtils::formatting(x, n=2))) %>%
    dplyr::full_join(j_agree_col, by="Judge", suffix=c('','_col')) %>%
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
