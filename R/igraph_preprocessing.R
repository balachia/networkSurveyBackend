#' Extract network directionalities from master igraph
#'
#' Builds directed, mutual, and collapsed (any direction) networks from a complete master network
#'
#' @param g master igraph
#' @param slug name slug for this graph
#' @export
build_network_directions <- function(g, slug = 'g') {
    # "directed" networks (preserving weight)
    #g.d <- as.undirected(g, 'collapse', edge.attr.comb = 'sum') %>%
    #    add_centralities(g)
    E(g)$width <- 1
    g.d <- g %>% add_centralities(g)
    # undirected networks
    g.mc <- as.undirected(g, 'collapse', edge.attr.comb = 'sum') %>%
        add_centralities(g)
    g.mm <- as.undirected(g, 'mutual') %>%
        add_centralities(g)
    # reset widths
    E(g.d)$width <- 1
    E(g.mc)$width <- 1 + 2 * (E(g.mc)$width > 1)
    E(g.mm)$width <- 1
    # export
    res <- list(g.d, g.mc, g.mm)
    names(res) <- paste0(slug, c('.directed', '.any', '.mutual'))
    res
}

#' Adds centrality metrics to graph
#'
#' Builds centrality metrics from inbound tie information
#'
#' @param g target graph to augment
#' @param g0 master igraph with full graph directionality info
#' @export
add_centralities <- function(g, g0) {
    # TODO: add `constraint` (Burt's constraint)?
    V(g)$centrality.outdegree <- igraph::degree(g0, mode = 'out')
    V(g)$centrality.indegree <- igraph::degree(g0, mode = 'in')
    suppressWarnings({
        V(g)$centrality.close <- igraph::closeness(g0, mode = 'in')
    })
    V(g)$centrality.between <- igraph::betweenness(g0)
    V(g)$centrality.eigen <- igraph::eigen_centrality(g0, directed = TRUE)$vector
    V(g)$centrality.cluster <- igraph::transitivity(g0, type = 'barrat', isolates = 'zero')
    g
}

#' Produce pseudo-network containing all possible edges
#'
#' This allows different edges to be dynamically hidden/masked, without reloading a different graph
#'
#' @param g master igraph
#' @import igraph
#' @export
build_edge_network <- function(g) {
    # 1) build all directed edges
    gd <- igraph::as.directed(g)
    E(gd)$directed <- TRUE
    df.d <- igraph::as_data_frame(gd)
    # 2) build all undirected edges
    gu <- igraph::as.undirected(g, mode = 'collapse', edge.attr.comb = sum) %>%
        igraph::as.directed(mode = 'arbitrary')
    E(gu)$directed <- FALSE
    df.u <- igraph::as_data_frame(gu)
    # 3) combine graphs, and assign each edge an id
    ge <- igraph::graph_from_data_frame(rbind(df.d, df.u))
    E(ge)$id <- 1:length(E(ge))
    # 4) create selector edge attributes
    # advice network
    E(ge)$advice.directed <- E(ge)$directed * E(ge)$advice
    #E(ge)$advice.any      <- (!E(ge)$directed) * (E(ge)$advice > 0)
    E(ge)$advice.any      <- (!E(ge)$directed) * E(ge)$advice
    E(ge)$advice.mutual   <- (!E(ge)$directed) * (E(ge)$advice == 2)
    # support network
    E(ge)$support.directed <- E(ge)$directed * E(ge)$support
    #E(ge)$support.any      <- (!E(ge)$directed) * (E(ge)$support > 0)
    E(ge)$support.any      <- (!E(ge)$directed) * E(ge)$support
    E(ge)$support.mutual   <- (!E(ge)$directed) * (E(ge)$support == 2)
    # done
    ge
}

