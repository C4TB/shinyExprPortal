plotNetwork <- function(edgelist,
                        nodes_table,
                        colors = NULL,
                        font_colors = NULL,
                        node_name = NULL) {
  vnd <- visNetwork::toVisNetworkData(edgelist)
  # Get additional info from nodes_table
  vnd$nodes <-
    merge(vnd$nodes, nodes_table, by.x = "id", by.y = "name")
  if (!is.null(colors)) {
    vnd$nodes$color <- colors[vnd$nodes$group]
  }
  if (!is.null(font_colors)) {
    vnd$nodes$font.color <- font_colors[vnd$nodes$group]
  }
  vnd$nodes$font.size <- 14
  net <- visNetwork::visNetwork(vnd$nodes, vnd$edges) %>%
    visNetwork::visEdges(color = list(color = "gray", opacity = 0.5)) %>%
    visNetwork::visNodes(shape = "box") %>%
    visNetwork::visIgraphLayout()
  
  net <- net %>%
    visNetwork::visOptions(
        nodesIdSelection = if (!is.null(node_name)) list(
        selected = node_name,
        main = "Select node to highlight:"
      ) else FALSE,
      highlightNearest = list(
        enabled = TRUE,
        degree = 1,
        hover = TRUE
      )
    )
  net
}
