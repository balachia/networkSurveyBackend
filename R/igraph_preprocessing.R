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

#' Hierarchical community detection coloring
#'
#' Assign consistent colors to nodes according to hierarchical community detection.
#'
#' @param g target graph to augment
#' @param g0 master igraph with full graph directionality info
build_color_merge_tree <- function(g, g0) {
    g0.undirected <- igraph::as.undirected(g0, 'collapse')
    # hierarchical community detection
    communities <- igraph::cluster_fast_greedy(g0.undirected)
    # stats
    net.size <- igraph::gorder(g0.undirected)
    mtree <- igraph::merges(communities)
    # algorithm outline:
    #   read merge tree forwards:
    #       1) assign subcommunity ids
    #       2) build subcommunity->vertices map
    #   read merge tree backwards:
    #       1) assign color indices from bottom up
    #       1a) for convenience, maintain running subcommunity->index mapping
    #       2) create color splits: first merged group [,1] gets new color
    #           and second merged group [,2] gets old color
    #           it appears as though the second group tends to be the one merged into (bigger)
    
    # augment mtree with community ids
    nmerges <- nrow(mtree)
    ncommunities <- net.size + nmerges
    mtree <- cbind(mtree, (1:nmerges) + net.size)
    # build subcommunity -> vertices map
    community.list <- lapply(1:ncommunities, identity)
    for(i in 1:nmerges) {
        community.list[[net.size + i]] <- c(community.list[[ mtree[i,1] ]], community.list[[ mtree[i, 2] ]])
    }
    # build color indices
    # all vertices start at 1
    vcolors <- rep(1, net.size)
    com.colors <- rep(1, ncommunities)
    # run splits from full merge (only run 10 splits for now)
    nsplits <- 10
    res <- matrix(0, nrow = nsplits + 1, ncol = net.size)
    res[1, ] <- vcolors
    for(i in 1:nsplits) {
        # find i'th split
        si <- nmerges - i + 1
        # assign new colors
        new.col <- i + 1
        old.col <- com.colors[ mtree[si, 3] ]
        com.colors[ mtree[si, 1] ] <- new.col
        com.colors[ mtree[si, 2] ] <- old.col
        # assign colors to vertices
        vcolors[ community.list[[ mtree[si, 1] ]] ] <- new.col
        res[i + 1, ] <- vcolors
    }
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
    #g0.undirected <- igraph::as.undirected(g0, 'collapse')
    #communities <- igraph::cluster_edge_betweenness(g0.undirected)
    #communities <- igraph::cluster_fast_greedy(g0.undirected)
    community.res <- build_color_merge_tree(g, g0)
    #print("############################################################")
    #print("new graph")
    #print(community.res)
    ## build up colors from most-merged
    ## adjust for number of disconnected components
    #print("############################################################")
    #print("new graph")
    #excess.disconnects <- igraph::gorder(g0.undirected) - nrow(igraph::merges(communities)) - 1
    #print(excess.disconnects)
    #print(igraph::is_connected(g0.undirected))
    #print(communities)
    #print(igraph::merges(communities))
    ##print(igraph::gorder(g0.undirected))
    #parent <- igraph::cut_at(communities, 1 + excess.disconnects)
    #g <- igraph::set_vertex_attr(g,
    #                             name = paste0('community.', 1),
    #                             value = parent)
    ## set up min/max color indices
    #min.split <- 2 + excess.disconnects
    #max.split <- min(10, length(V(g0)) - 1) + excess.disconnects
    ##for(no in 2:min(10, length(V(g0)) - 1)) {
    #for(no in min.split:max.split) {
    #    child <- igraph::cut_at(communities, no + excess.disconnects)
    #    # find splitted color
    #    split.color <- which(sapply(1:(no-1),
    #                                function(x) length(unique(child[parent == x]))) > 1)
    #    #stopifnot(length(split.color) == 1)
    #    # find splitted units (recolor the smaller of the split groups)
    #    newcols <- unique(child[parent == split.color])
    #    n.c1 <- sum(child == newcols[1])
    #    n.c2 <- sum(child == newcols[2])
    #    newcol <- if(n.c1 >= n.c2) newcols[2] else newcols[1]
    #    #split.lgl <- child == splitted.col
    #    # reproduce split in the parent
    #    parent[child == newcol] <- no
    #    g <- igraph::set_vertex_attr(g,
    #                                 name = paste0('community.', no),
    #                                 value = parent)
    #}
    for(i in 1:nrow(community.res)) {
        g <- igraph::set_vertex_attr(g,
                                     name = paste0('community.', i),
                                     value = community.res[i, ])
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

