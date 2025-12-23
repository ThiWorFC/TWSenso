#' Build the Tucker Plot for QDA data
#'
#' Visualize the agreement between panelist for the different attributes in a PCA plot.
#'
#' @param dataset Data to analyze
#' @param firstvar Integer: position of the first attribute
#' @param lastvar Integer: position of the last attribute
#' @param attr_show Vector containing the attribute to show
#' @param type_vis Character: 'ind' or 'var'
#'
#' @returns The Plot of interest
#' @export
#'
#' @examples NULL
QDA_plot <- function(dataset, firstvar=6, lastvar=ncol(dataset), attr_show="none", type_vis="var"){

  # Extracting information regarding the data
  attribute <- colnames(dataset)[6:ncol(dataset)]
  nbatt <- length(attribute)

  # Preparing the data for the analysis
  res_pca <- dataset %>%
    tidyr::pivot_longer(all_of(attribute), names_to="Attribute", values_to="Score") %>%
    dplyr::group_by(Judge, Product, Attribute) %>%
    dplyr::summarize(Score = mean(Score, na.rm=TRUE)) %>%
    tidyr::unite(Attribute, c(Attribute, Judge), sep="___") %>%
    tidyr::pivot_wider(names_from=Attribute, values_from=Score) %>%
    as.data.frame() %>%
    tibble::column_to_rownames(var="Product") %>%
    FactoMineR::PCA(graph=FALSE)

  if (type_vis == "ind"){

    p <- FactoMineR::plot.PCA(res_pca, choix="ind")

  } else if (type_vis == "var"){

    data_plot <- res_pca$var$coord %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var="Attribute") %>%
      tidyr::separate(Attribute, c("Attribute","Judge"), sep="___") %>%
      dplyr::mutate(AttrShow = ifelse(Attribute == attr_show, Attribute, ""))

    xleg <- paste0("Dim 1 (",round(res_pca$eig[1,2],2),"%)")
    yleg <- paste0("Dim 2 (",round(res_pca$eig[2,2],2),"%)")

    p <- ggplot2::ggplot(data_plot)+
      ggforce::geom_circle(aes(x0=0, y0=0, r=1), color="black")+
      ggforce::geom_circle(aes(x0=0, y0=0, r=0.5), color="grey", linetype=2)+
      ggplot2::geom_vline(xintercept=0, lty=2)+
      ggplot2::geom_hline(yintercept=0, lty=2)+
      ggplot2::coord_fixed()+
      ggplot2::geom_point(aes(x=Dim.1, y=Dim.2, color=Attribute), cex=1.5)+
      ggrepel::geom_text_repel(aes(x=Dim.1, y=Dim.2, label=AttrShow, color=Attribute), show.legend=FALSE)+
      ggplot2::scale_x_continuous(name=xleg, limits=c(-1.1,1.1), breaks=seq(-1, 1, 0.5))+
      ggplot2::scale_y_continuous(name=yleg, limits=c(-1.1,1.1), breaks=seq(-1, 1, 0.5))+
      ggplot2::ggtitle("Tucker-1 plot")+
      ggplot2::theme_bw()
  }

  return(p)
}
