

plotNetwork <- function(edgelist, nodes_table, node_name = NULL) {
  vnd <- visNetwork::toVisNetworkData(edgelist)
  # Get additional info from nodes_table
  vnd$nodes <-
    merge(vnd$nodes, nodes_table, by.x = "id", by.y = "name")
  vnd$nodes$font.size <- 14
  net <- visNetwork::visNetwork(vnd$nodes, vnd$edges) %>%
    visNetwork::visIgraphLayout()
  if (not_null(node_name)) {
    net <- net %>%
      visNetwork::visOptions(
        nodesIdSelection = list(selected = node_name,
                                main = "Select node to highlight:"),
        highlightNearest = list(
          enabled = T,
          degree = 1,
          hover = T
        )
      )
  }
}
