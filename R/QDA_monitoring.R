#' Assess the Panel and Panelist Performance for QDA tests
#'
#' Automated analysis for the panel and panelist performance in quantitative descriptive analysis (QDA) when test is fully duplicated.
#'
#' @param dataset Dataset to analyze
#' @param col.r Integer: position of the replicate column
#' @param type Character: 'reproducibility' or 'repeatability' to define which model to use
#' @param random Boolean: should the panelist effect be considered as a random effect?
#' @param panel_sign Level of significance for the panel
#' @param panel_sd Threshold for the panel sd
#' @param panellist_sign Level of significance for the panelists
#' @param panellist_sd Threshold for the panelist sd (reproducibility)
#' @param panellist_cor Threshold for the panelist correlation (agreement)
#' @param sum_min Minimum value for the summary
#' @param sum_max Maximum value for the summary
#' @param test Type of test: 'QDA', 'AQP', or 'CAR'
#' @param hidden_ref Name of the hidden reference sample (or 'none')
#' @param dif_ref Threshold for the difference between the reference and the hidden reference
#' @param panel_agrmin Minimum value for the panel agreement
#' @param panel_agrmax Maximum value for the panel agreement
#' @param panel_cormin Minimum value for the panel correlation
#' @param panel_cormax Maximum value for the panel correlation
#'
#' @returns The different tables of results at the panel and panelist level, including summaries (per Judge and per Attribute)
#' @export
#'
#' @examples NULL
QDA_monitoring <- function(dataset, col.r="Replica",
                           type="reproducibility", random=TRUE,
                           panel_sign=c(0.05, 0.10), panel_sd=c(10,12),
                           panellist_sign=c(0.05, 0.10), panellist_sd=c(10,12), panellist_cor=c(0.5,0.7),
                           sum_min=50, sum_max=75, test="QDA", hidden_ref="none", dif_ref=c(5,10),
                           panel_agrmin=0.1, panel_agrmax=0.2, panel_cormin=0.5, panel_cormax=0.7){

  ## Handling the Hidden Ref
  if (test %in% c("AQP","CAR") & hidden_ref != "none"){

    data_ref <- dataset %>%
      dplyr::filter(Session == 0)

    if (nrow(data_ref) > 0){
      run_ref = TRUE

      data_ref <- data_ref %>%
        tidyr::pivot_longer(6:ncol(.), names_to="Attribute", values_to="Ref") %>%
        dplyr::group_by(Attribute) %>%
        dplyr::summarize(Ref = mean(Ref, na.rm=TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::select(Attribute, Ref)

    } else {
      run_ref = FALSE
    }
  } else {
    run_ref = FALSE
  }

  dataset <- dataset %>%
    dplyr::filter(Session > 0)

  ## Information regarding the data
  product <- unique(dataset$Product)
  nbprod <- length(product)
  juge <- unique(dataset$Judge)
  nbjuge <- length(juge)
  attribute <- colnames(dataset)[6:ncol(dataset)]
  nbatt <- length(attribute)

  dataset <- dataset %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(col.r), as.character))

  nbrep <- dataset %>%
    dplyr::pull(tidyselect::all_of(col.r)) %>%
    unique() %>%
    length()

  if (nbrep>1){
    run=TRUE
  } else {
    run=FALSE
  }

  ## Check which attribute to run the analysis on...
  run.att <- rep(TRUE,nbatt)

  for (att in 1:nbatt){
    if (sd(na.omit(unlist(dataset[,(5+att)]))) == 0){
      run.att[att] <- FALSE
    }
  }

  ## Panel Performance ----
  # colour-code: 1=green, 2=orange, 3=red

  if (run){

    ## ODP
    if (test == "ODP"){

      run_ODP = TRUE

      dataset2 <- dataset %>%
        dplyr::mutate(Judge = stringr::str_c(Judge,"_",.data[[col.r]])) %>%
        tidyr::pivot_longer(tidyselect::all_of(attribute), names_to="Attribute", values_to="Score") %>%
        dplyr::mutate(Attribute = factor(Attribute, levels=attribute)) %>%
        dplyr::mutate(Score = as.numeric(Score))

      res_friedman <- dataset2 %>%
        dplyr::nest_by(Attribute) %>%
        dplyr::mutate(mod = list(friedman.test(Score ~ Product|Judge, data))) %>%
        dplyr::reframe(broom::tidy(mod)) %>%
        dplyr::select(Attribute, `Discrimination (Rank)` = p.value) %>%
        dplyr::ungroup()

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
        tibble::enframe(name="Attribute", value="Agreement (Rank)") %>%
        tidyr::unnest(`Agreement (Rank)`)

      res_rep <- dataset %>%
        tidyr::pivot_longer(tidyselect::all_of(attribute), names_to="Attribute", values_to="Score") %>%
        dplyr::mutate(Attribute = factor(Attribute, levels=attribute)) %>%
        dplyr::mutate(Score = as.numeric(Score)) %>%
        dplyr::select(Judge, Product, Rep=tidyselect::all_of(col.r), Attribute, Score) %>%
        dplyr::group_by(Product, Rep, Attribute) %>%
        dplyr::summarize(Score = mean(Score)) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_wider(names_from=Rep, values_from=Score) %>%
        split(.$Attribute) %>%
        purrr::map(function(data){
          cor(data$`1`, data$`2`, method="spearman")
        }) %>%
        tibble::enframe(name="Attribute", value="Reproducibility (Rank)") %>%
        tidyr::unnest(`Reproducibility (Rank)`)

    } else {
      run_ODP = FALSE
    }

    ## 3-way ANOVA Solution
    if (type == "reproducibility"){

      formul.panel <- paste0("~Product*Judge*", col.r, "-Product:Judge:",col.r)

      res_panel <- SensoMineR::panelperf(as.data.frame(dataset), formul=formul.panel,
                                         firstvar=6, lastvar=ncol(dataset), random=random)$p.value %>%
        tibble::as_tibble(rownames="Attribute") %>%
        dplyr::select(Attribute, Discrimination=2, Agreement=5, Reproducibility=6)

      if (run_ref){

        res_ref <- dataset %>%
          dplyr::filter(Product == hidden_ref) %>%
          tidyr::pivot_longer(6:ncol(.), names_to="Attribute", values_to="Score") %>%
          dplyr::full_join(data_ref, by="Attribute") %>%
          dplyr::nest_by(Attribute) %>%
          dplyr::mutate(mod = list(t.test(data$Score, data$Ref))) %>%
          dplyr::reframe(broom::tidy(mod)) %>%
          dplyr::ungroup() %>%
          dplyr::select(Attribute, Reference=p.value)

        res_panel <- dplyr::full_join(res_panel, res_ref, by="Attribute")

      }
      if (run_ODP){

        res_panel <- res_panel %>%
          dplyr::full_join(res_friedman, by="Attribute") %>%
          dplyr::full_join(res_agr, by="Attribute") %>%
          dplyr::full_join(res_rep, by="Attribute")

      }

      res_panel_col <- res_panel %>%
        dplyr::mutate(Discrimination = dplyr::case_when(
          Discrimination <= panel_sign[1] ~ 1,
          Discrimination <= panel_sign[2] ~ 2,
          .default = 3)) %>%
        dplyr::mutate(Agreement = dplyr::case_when(
          Agreement < panel_sign[1] ~ 3,
          Agreement < panel_sign[2] ~ 2,
          .default = 1)) %>%
        dplyr::mutate(Reproducibility = dplyr::case_when(
          Reproducibility < panel_sign[1] ~ 3,
          Reproducibility < panel_sign[2] ~ 2,
          .default = 1))

      if (run_ref){
        res_panel_col <- res_panel_col %>%
          dplyr::mutate(Reference = dplyr::case_when(
            Reference <= panel_sign[1] ~ 3,
            Reference <= panel_sign[2] ~ 2,
            .default = 1))
      }

      if (run_ODP){
        res_panel_col <- res_panel_col %>%
          dplyr::mutate(`Reproducibility (Rank)` = round(`Reproducibility (Rank)`, 1)) %>%
          dplyr::mutate(`Discrimination (Rank)` = dplyr::case_when(
            `Discrimination (Rank)` <= panel_sign[1] ~ 1,
            `Discrimination (Rank)` <= panel_sign[2] ~ 2,
            .default = 3)) %>%
          dplyr::mutate(`Agreement (Rank)` = dplyr::case_when(
            `Agreement (Rank)` >= panel_agrmax ~ 1,
            `Agreement (Rank)` >= panel_agrmin ~ 2,
            .default = 3)) %>%
          dplyr::mutate(`Reproducibility (Rank)` = dplyr::case_when(
            `Reproducibility (Rank)` >= panel_cormax ~ 1,
            `Reproducibility (Rank)` >= panel_cormin ~ 2,
            .default = 3))
      }

      res_panel <- res_panel %>%
        dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) TWUtils::formatting(x, 3)))

      ## 2-way ANOVA Solution
    } else if (type == "repeatability"){

      formul.panel <- "~Product*Judge"
      res_panel <- matrix(NA, nbatt, 3, dimnames=list(attribute, c("Discrimination","Agreement","Repeatability")))

      for (att in 1:nbatt){
        if (run.att[att]){
          res.aov <- anova(lm(as.formula(paste0("`",attribute[att],"`",formul.panel)), data=dataset))
          if (random){
            res.aov[1,4] <- res.aov[1,3]/res.aov[3,3]
            res.aov[1,5] <- pf(res.aov[1,4], res.aov[1,1], res.aov[3,1], lower.tail=FALSE)
          }
          res_panel[att,1:2] <- res.aov[c(1,3),5]
          res_panel[att,3] <- sqrt(res.aov[4,3])
        }
      }

      res_panel <- res_panel %>%
        tibble::as_tibble(rownames="Attribute")

      if (run_ref){

        res_ref <- dataset %>%
          dplyr::filter(Product == hidden_ref) %>%
          tidyr::pivot_longer(6:ncol(.), names_to="Attribute", values_to="Score") %>%
          dplyr::full_join(data_ref, by="Attribute") %>%
          dplyr::nest_by(Attribute) %>%
          dplyr::mutate(mod = list(t.test(data$Score, data$Ref))) %>%
          dplyr::reframe(broom::tidy(mod)) %>%
          dplyr::ungroup() %>%
          dplyr::select(Attribute, Reference=p.value)

        res_panel <- dplyr::full_join(res_panel, res_ref, by="Attribute")
      }

      if (run_ODP){

        res_panel <- res_panel %>%
          dplyr::full_join(res_friedman, by="Attribute") %>%
          dplyr::full_join(res_agr, by="Attribute") %>%
          dplyr::full_join(res_rep, by="Attribute")
      }

      res_panel_col <- res_panel %>%
        dplyr::mutate(Discrimination = dplyr::case_when(
          Discrimination <= panel_sign[1] ~ 1,
          Discrimination <= panel_sign[2] ~ 2,
          .default = 3)) %>%
        dplyr::mutate(Agreement = dplyr::case_when(
          Agreement < panel_sign[1] ~ 3,
          Agreement < panel_sign[2] ~ 2,
          .default = 1)) %>%
        dplyr::mutate(Repeatability = dplyr::case_when(
          Repeatability <= panel_sd[1] ~ 1,
          Repeatability <= panel_sd[2] ~ 2,
          .default = 3))

      if (run_ref){

        res_panel_col <- res_panel_col %>%
          dplyr::mutate(Reference = dplyr::case_when(
            Reference <= panel_sign[1] ~ 3,
            Reference <= panel_sign[2] ~ 2,
            .default = 1))
      }

      if (run_ODP){

        res_panel_col <- res_panel_col %>%
          dplyr::mutate(`Discrimination (Rank)` = dplyr::case_when(
            `Discrimination (Rank)` <= panel_sign[1] ~ 1,
            `Discrimination (Rank)` <= panel_sign[2] ~ 2,
            .default = 3)) %>%
          dplyr::mutate(`Agreement (Rank)` = dplyr::case_when(
            `Agreement (Rank)` >= panel_agrmax ~ 1,
            `Agreement (Rank)` >= panel_agrmin ~ 2,
            .default = 3)) %>%
          dplyr::mutate(`Reproducibility (Rank)` = dplyr::case_when(
            `Reproducibility (Rank)` >= panel_cormax ~ 1,
            `Reproducibility (Rank)` >= panel_cormin ~ 2,
            .default = 3))
      }

      res_panel <- res_panel %>%
        dplyr::mutate(Repeatability = TWUtils::formatting(Repeatability, 2)) %>%
        dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) TWUtils::formatting(x, 3)))
    }
  }

  ## Panelist performance ----

  # Remove Panelist with incomplete data
  j.rm <- NULL
  for (j in 1:nbjuge){
    if (nrow(dataset[dataset$Judge == juge[j],]) <= nbprod){
      j.rm <- c(j.rm, juge[j])
    }
  }
  if (!is.null(j.rm)){
    dataset.j <- dataset[!(dataset$Judge %in% j.rm),] %>%
      dplyr::mutate(Judge = factor(Judge))
  } else {
    dataset.j <- dataset
  }

  # Panelist performance using paneliperf {SensoMineR}
  res_panellist <- SensoMineR::paneliperf(as.data.frame(dataset.j), formul=formul.panel, formul.j="~Product",
                                          col.j=1, firstvar=6, lastvar=ncol(dataset), random=random)

  j_discri <- round(res_panellist$prob.ind,3) %>%
    tibble::as_tibble(rownames = "Judge")
  j_discri_col <- res_panellist$prob.ind %>%
    tibble::as_tibble(rownames = "Judge") %>%
    colour_code(value = panellist_sign, direction=1)

  j_repro <- round(res_panellist$res.ind,3) %>%
    tibble::as_tibble(rownames = "Judge")
  j_repro_col <- res_panellist$res.ind %>%
    tibble::as_tibble(rownames = "Judge") %>%
    colour_code(value = panellist_sd, direction=1)

  j_agree <- round(res_panellist$agree.ind,3) %>%
    tibble::as_tibble(rownames = "Judge")
  j_agree_col <- res_panellist$agree.ind %>%
    tibble::as_tibble(rownames = "Judge") %>%
    colour_code(value = panellist_cor, direction=2)

  if (run_ref){

    j_ref <- dataset %>%
      dplyr::filter(Product == hidden_ref) %>%
      tidyr::pivot_longer(tidyselect::all_of(attribute), names_to="Attribute", values_to="Score") %>%
      dplyr::inner_join(data_ref, by="Attribute") %>%
      dplyr::mutate(Diff = abs(Score - Ref)) %>%
      dplyr::select(Judge, Attribute, Diff) %>%
      tidyr::pivot_wider(names_from=Attribute, values_from=Diff, values_fn=mean)

    j_ref_col <- j_ref %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), abs)) %>%
      colour_code(value=dif_ref, direction=1)

  } else {
    j_ref = NULL
    j_ref_col = NULL
  }

  if (run_ODP){

    j_reprorank <- dataset %>%
      tidyr::pivot_longer(tidyselect::all_of(attribute), names_to="Attribute", values_to="Score") %>%
      dplyr::mutate(Attribute = factor(Attribute, levels=attribute)) %>%
      split(.$Judge) %>%
      purrr::map(function(data){
        data %>%
          dplyr::select(Product, tidyselect::all_of(col.r), Attribute, Score) %>%
          tidyr::pivot_wider(names_from=col.r, values_from=Score) %>%
          split(.$Attribute) %>%
          purrr::map(function(data){
            cor(data$`1`, data$`2`, method="spearman")
          }) %>%
          tibble::enframe(name="Attribute", value="res") %>%
          tidyr::unnest(res)
      }) %>%
      tibble::enframe(name="Judge", value="res") %>%
      tidyr::unnest(res) %>%
      tidyr::pivot_wider(names_from=Attribute, values_from=res)

    j_reprorank_col<- j_reprorank %>%
      colour_code(value = panellist_cor, direction = 2)

    j_agreerank <- dataset %>%
      tidyr::pivot_longer(tidyselect::all_of(attribute), names_to="Attribute", values_to="Score") %>%
      dplyr::mutate(Attribute = factor(Attribute, levels=attribute)) %>%
      dplyr::select(Judge, Product, Attribute, Score) %>%
      tidyr::pivot_wider(names_from=Attribute, values_from=Score, values_fn=mean) %>%
      panellist_agreement(firstvar=3, cor="without", type="Spearman") %>%
      round(., 2) %>%
      tibble::as_tibble(rownames = "Judge")

    j_agreerank_col <- j_agreerank %>%
      colour_code(value = panellist_cor, direction = 2)

  } else {

    j_reprorank <- NULL
    j_reprorank_col <- NULL
    j_agreerank <- NULL
    j_agreerank_col <- NULL
  }

  ## Summary Tables ----
  j_sum <- summary_tab(list(Discrimination=j_discri_col,
                            Reference=j_ref_col,
                            Repeatability=j_repro_col,
                            Agreement=j_agree_col,
                            `Repeatability (Rank)`=j_reprorank_col,
                            `Agreement (Rank)`=j_agreerank_col), type="Judge")
  j_sum_col <- j_sum %>%
    mutate(dplyr::across(tidyselect::where(is.numeric), convert, sum_max, sum_min))

  a_sum <- summary_tab(list(Discrimination=j_discri_col,
                            Reference=j_ref_col,
                            Repeatability=j_repro_col,
                            Agreement=j_agree_col,
                            `Repeatability (Rank)`=j_reprorank_col,
                            `Agreement (Rank)`=j_agreerank_col), type="Attribute") %>%
    dplyr::mutate(Attribute = factor(Attribute, levels=attribute)) %>%
    dplyr::arrange(Attribute)

  a_sum_col <- a_sum %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) convert(x, sum_max, sum_min)))

  ## Export Results ----
  res <- list()

  res$Panel <- res_panel %>%
    dplyr::full_join(res_panel_col, by="Attribute", suffix=c('','_col'))%>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"),  \(x) tidyr::replace_na(x, 0)))

  res$Panellist$Discrimination <- j_discri %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) TWUtils::formatting(x, n=3))) %>%
    dplyr::full_join(j_discri_col, by="Judge", suffix=c('','_col')) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))

  if (run_ref){

    res$Panellist$Reference <- j_ref %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) TWUtils::formatting(x, n=2))) %>%
      dplyr::full_join(j_ref_col, by="Judge", suffix=c('','_col')) %>%
      dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))
  }

  res$Panellist$Reproducibility <- j_repro %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) TWUtils::formatting(x, n=2))) %>%
    dplyr::full_join(j_repro_col, by="Judge", suffix=c('','_col')) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))

  res$Panellist$Agreement <- j_agree %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) TWUtils::formatting(x, n=2))) %>%
    dplyr::full_join(j_agree_col, by="Judge", suffix=c('','_col')) %>%
    dplyr::mutate(dplyr::across(ends_with("_col"), \(x) replace_na(x, 0)))

  if (run_ODP){
    res$Panellist$`Reproducibility (Rank)` <- j_reprorank %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) TWUtils::formatting(x , n=2))) %>%
      dplyr::full_join(j_reprorank_col, by="Judge", suffix=c('','_col')) %>%
      dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))

    res$Panellist$`Agreement (Rank)` <- j_agreerank %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) TWUtils::formatting(x , n=2))) %>%
      dplyr::full_join(j_agreerank_col, by="Judge", suffix=c('','_col')) %>%
      dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))
  }

  res$Summary$Panellist <- j_sum %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) TWUtils::formatting(x , n=0))) %>%
    dplyr::full_join(j_sum_col, by="Judge", suffix=c('','_col'))%>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))

  res$Summary$Attribute <- a_sum %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) TWUtils::formatting(x, n=0))) %>%
    dplyr::full_join(a_sum_col, by="Attribute", suffix=c('','_col'))%>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("_col"), \(x) tidyr::replace_na(x, 0)))

  return(res)
}
