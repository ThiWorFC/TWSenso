#' Visualize Results for CAR data
#'
#' Visualize the scores provided by the panelists in CAR test.
#'
#' @param dataset Data to analyze
#' @param reference Name of the reference sample
#' @param duplicate Names of the duplicate samples
#'
#' @returns The Plot of interest
#' @export
#'
#' @examples NULL
CAR_plot <- function(dataset, reference=NULL, duplicate=NULL){

  # Information regarding the Data
  attribute <- colnames(dataset)[6:ncol(dataset)]
  nbatt <- length(attribute)

  # Prepare the Data to Plot
  data_plot <- dataset %>%
    dplyr::select(Product, tidyselect::all_of(attribute)) %>%
    tidyr::pivot_longer(tidyselect::all_of(attribute), names_to="Attribute", values_to="Scores") %>%
    dplyr::mutate(Attribute = factor(Attribute, levels=attribute)) %>%
    dplyr::group_by(Product, Attribute) %>%
    dplyr::summarize(Mean=mean(Scores, na.rm=TRUE)) %>%
    dplyr::mutate(`Type of Sample` = dplyr::case_when(
      Product == reference ~ "Reference",
      Product %in% duplicate ~ "Duplicate",
      .default = "Sample"))

  # Create the Plot
  p <- ggplot2::ggplot(data_plot, aes(x=Attribute, y=Mean, label=Product, color=`Type of Sample`))+
    ggplot2::geom_point(cex=2)+
    ggrepel::geom_text_repel(show.legend=FALSE)+
    ggplot2::geom_vline(xintercept=c(1:nbatt), color="grey50")+
    ggplot2::geom_hline(yintercept=0, color="grey75")+
    ggplot2::geom_rect(xmin=-Inf, xmax=Inf, ymin=-1, ymax=1, alpha=0.015, fill="grey80", show.legend=FALSE, inherit.aes=FALSE)+
    ggplot2::labs(x="")+
    ggplot2::scale_y_continuous(name="", limits=c(-3,3), breaks=seq(-3,3,1))+
    ggplot2::scale_alpha(range=c(0,1))+
    ggplot2::coord_flip()+
    ggplot2::ggtitle("Results of the CAR test for the Panel")+
    ggplot2::theme_minimal()+
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.line = ggplot2::element_line(color="grey60"))

  # Export the Plot
  return(p)
}
