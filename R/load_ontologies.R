#' Load Ontologies
#'
#' This function loads the 'molon' ontology which describes mechanisms driving
#' mutational profiles
#'
#' @return A data frame containing the ontologies.
#' @export
#'
#' @examples
#' ontologies <- load_ontologies()
#' head(ontologies)
load_ontology <- function(){
  path = system.file("terms.tsv", package = "molon")
  read.csv(path, header = TRUE, sep = "\t")
}


#' Graph the MOLON ontology
#'
#' Visualise the MOLON ontology that describes mechanisms
#' driving distinct mutational profiles in cancer.
#' @return A \code{ggplot2} object representing the ontology graph.
#' @export
#'
#' @examples
#' ontology_graph <- graph_ontologies()
#' print(ontology_graph)
graph_ontology <- function(){
  df_ontology <- load_ontology()
  graph <- tidygraph::as_tbl_graph(df_ontology)
  #graph
  ggraph <- ggraph::ggraph(graph, layout = "igraph", algorithm = "sugiyama") +
    ggraph::geom_edge_diagonal() + #(strength = 1) +
    ggraph::geom_node_label(
      ggplot2::aes(label = name, fill = name),show.legend = FALSE,
      col = "black", fontface = "bold", hjust = "inward"
    ) +
    ggplot2::scale_fill_manual(values = c(Endogenous = "lightblue", Exogenous = "lightblue", Other = "lightblue"), na.value = "white") +
    #geom_node_text(aes(label = name, colour = depth), repel = TRUE, size = 10) +
    #scale_color_brewer(palette="Set1") +
    ggplot2::theme_void() +
    ggplot2::coord_flip()

  return(ggraph)
}
