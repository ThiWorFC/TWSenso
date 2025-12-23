#' Visualize Results for AQP data
#'
#' Visualize the scores provided by the panelists in AQP test.
#'
#' @param dataset Data to analyze
#' @param hidden_ref Name of the hidden reference
#' @param duplicate Names of the duplicate samples
#'
#' @returns The Plot of interest
#' @export
#'
#' @examples NULL
AQP_plot <- function(dataset, hidden_ref="none", duplicate=NULL){

  # Info about the data
  attribute <- colnames(dataset)[6:ncol(dataset)]
  nbatt <- length(attribute)
  xmax <- 10*ceiling(max(as.data.frame(dataset[,attribute]))/10)

  # Check if there is a reference
  if (hidden_ref != "none"){

    data_ref <- dataset %>%
      dplyr::filter(Session==0)

    if (nrow(data_ref) > 0){
      ref <- data_ref %>%
        dplyr::mutate(Product = as.character(Product)) %>%
        dplyr::pull(Product) %>%
        unique()
    } else {
      ref = "no ref"
    }

  } else {

    dataset <- dataset %>%
      dplyr::mutate(Product = as.character(Product)) %>%
      dplyr::filter(Session>0)

    ref = "no ref"
  }

  # Prepare the Dataset
  data_plot <- dataset %>%
    dplyr::select(Product, tidyselect::all_of(attribute)) %>%
    dplyr::filter(!is.na(Product)) %>%
    dplyr::group_by(Product) %>%
    dplyr::summarise_all(list(~mean(.,na.rm=TRUE))) %>%
    tidyr::pivot_longer(tidyselect::all_of(attribute), names_to="Attribute", values_to="Score") %>%
    dplyr::mutate(Attribute = factor(Attribute, levels=attribute)) %>%
    dplyr::mutate(Type = dplyr::case_when(
      Product %in% hidden_ref ~ "Hidden Ref.",
      Product %in% ref ~ "Reference",
      Product %in% duplicate ~ "Duplicate",
      .default = "Sample"))

  # Create the Plot
  p <- ggplot2::ggplot(data_plot, aes(x=Attribute, y=Score, color=Type, label=Product))+
    ggplot2::geom_point(cex=2)+
    ggrepel::geom_text_repel(show.legend=FALSE)+
    ggplot2::scale_y_continuous(name="", limits=c(0,xmax))+
    ggplot2::labs(x="")+
    ggplot2::scale_color_manual(values=c("Sample"="black", "Duplicate"="firebrick2", "Reference"="blue", "Hidden Ref."="deepskyblue"))+
    ggplot2::coord_flip()+
    ggplot2::ggtitle("Results of the AQP test for the panel level")+
    ggplot2::theme_minimal()+
    ggplot2::theme(axis.line = ggplot2::element_line(color="grey60"), panel.grid = ggplot2::element_blank())

  for (att in 1:nbatt){
    p <- p +
      ggplot2::geom_segment(x=att, y=0, xend=att, yend=xmax, arrow=ggplot2::arrow(length=unit(0.3,"cm")), lwd=1, colour="grey50", alpha=0.5)
  }

  # Export the Graph
  return(p)
}
