#' Visualize Results for ODP data
#'
#' Visualize the scores provided by the panelists in ODP test.
#'
#' @param dataset Data to analyze
#' @param duplicate Names of the duplicate samples
#'
#' @returns The Plot of interest
#' @export
#'
#' @examples NULL
ODP_plot <- function(dataset, duplicate){

  # Setting up the Dataset
  attribute <- colnames(dataset)[6:ncol(dataset)]
  nbatt <- length(attribute)

  # Creating the Data for the Plot
  data_plot <- dataset %>%
    dplyr::select(Product, tidyselect::all_of(attribute)) %>%
    tidyr::pivot_longer(-Product, names_to="Attribute", values_to="Score") %>%
    dplyr::mutate(Attribute = factor(Attribute, levels=attribute), Score = as.numeric(Score)) %>%
    dplyr::group_by(Product, Attribute) %>%
    dplyr::summarize(Score = mean(Score)) %>%
    dplyr::mutate(Duplo = ifelse(Product %in% duplicate, "Duplo", "Sample"))

  # Create the Plot
  p <- ggplot2::ggplot(data_plot, aes(x=Attribute, y=Score, label=Product, colour=Duplo))+
    ggplot2::geom_point(cex=4)+
    ggrepel::geom_text_repel()+
    ggplot2::scale_y_continuous(name="", limits=c(0,100), breaks=seq(0,100,20))+
    ggplot2::labs(x="")+
    ggplot2::coord_flip()+
    ggplot2::ggtitle("Results of ODP data at the Panel level")+
    ggplot2::theme_bw()

  for (att in 1:nbatt){
    p <- p+
      ggplot2::geom_segment(x=att, y=0, xend=att, yend=100, arrow=ggplot2::arrow(length=unit(0.3,"cm")), lwd=1, colour="black")
  }

  p <- p+
    ggplot2::geom_point(cex=4)

  return(p)
}
