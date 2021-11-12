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
    # modified closeness centrality
    suppressWarnings({
        V(g)$centrality.close <- igraph::closeness(g0, mode = 'in')
    })
    V(g)$centrality.between <- igraph::betweenness(g0)
    V(g)$centrality.eigen <- igraph::eigen_centrality(g0, directed = TRUE)$vector
    V(g)$centrality.cluster <- igraph::transitivity(g0, type = 'barrat', isolates = 'zero')
    # hierarchical community detection
    # collapse to undirected
    g0.undirected <- igraph::as.undirected(g0, 'collapse')
    #communities <- igraph::cluster_edge_betweenness(g0.undirected)
    communities <- igraph::cluster_fast_greedy(g0.undirected)
    # build up colors from most-merged
    parent <- igraph::cut_at(communities, 1)
    g <- igraph::set_vertex_attr(g,
                                 name = paste0('community.', 1),
                                 value = parent)
    for(no in 2:min(10, length(V(g0)) - 1)) {
        child <- igraph::cut_at(communities, no)
        # find splitted color
        split.color <- which(sapply(1:(no-1),
                                    function(x) length(unique(child[parent == x]))) > 1)
        stopifnot(length(split.color) == 1)
        # find splitted units (recolor the smaller of the split groups)
        newcols <- unique(child[parent == split.color])
        n.c1 <- sum(child == newcols[1])
        n.c2 <- sum(child == newcols[2])
        newcol <- if(n.c1 >= n.c2) newcols[2] else newcols[1]
        #split.lgl <- child == splitted.col
        # reproduce split in the parent
        parent[child == newcol] <- no
        g <- igraph::set_vertex_attr(g,
                                     name = paste0('community.', no),
                                     value = parent)
    }
    # adjust 'isolates'
    isolates <- V(g)$centrality.indegree == 0
    range.connected.close <- range(V(g)$centrality.close[!isolates])
    gap.cc <- range.connected.close[2] - range.connected.close[1]
    V(g)$centrality.close[isolates] <- range.connected.close[1] - 0.5 * gap.cc
    # adjust 'nonrespondents'
    nonrespondents <- V(g)$centrality.outdegree == 0
    V(g)$centrality.close[nonrespondents] <- NA
    #return
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

