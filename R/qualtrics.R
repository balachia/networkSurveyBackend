#' Load qualtrics survey from zip or csv file
#' 
#' @param file_qualtrics list(datapath :: string path, type :: mime type)
#' @export
load_qualtrics_file <- function(file_qualtrics) {
    # TODO: is filetype checking robust??
    #   can maybe wrap everything in a tryCatch
    suppressMessages({
        fHandle <- file_qualtrics$datapath
        if(file_qualtrics$type == "application/zip") { fHandle <- utils::unzip(fHandle) }
        df.qualtrics <- qualtRics::read_survey(fHandle)
    })
    df.qualtrics
}

#' Convert survey dataframe into an igraph network for the rest of the pipeline
#'
#' @param df.qualtrics dataframe derived from qualtrics file
#' @param qidNetwork QID for network roster question
#' @param qidSudo QID for pseudonym question
#' @param qidConsent QID for anonymity consent question
#' @param ansAdvice survey response for an advice network edge
#' @param ansSupport survey response for a support network edge
#' @param ansConsent survey response for anonymity consent
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @export
qualtrics_to_igraph <- function(df.qualtrics,
                                qidNetwork = 'network', qidSudo = 'pseudonym', qidConsent = 'consent',
                                ansAdvice = 'advice', ansSupport = 'support', ansConsent = 'I agree') {
    df.qualtrics <- df.qualtrics %>% mutate(ego = NodeID)
    # TODO: keep only valid rows (i.e. actual responses, not tests or previews)
    # extract and longen network data
    net.prefix <- paste0(qidNetwork, '_')
    df.net <- df.qualtrics %>%
        pivot_longer(cols = starts_with(net.prefix),
                     names_prefix = net.prefix,
                     names_to = 'alter') %>%
        mutate(alter = as.numeric(alter))
    # extract networks
    df.net$advice <- grepl(ansAdvice, df.net$value)
    df.net$support <- grepl(ansSupport, df.net$value)
    # drop self-ties
    df.net <- df.net %>%
        filter(ego != alter)
    # fix pseudonyms
    if(!(qidSudo %in% colnames(df.qualtrics))) {
        df.qualtrics <- df.qualtrics %>%
            mutate("{qidSudo}" := "")
    }
    # merge required node characteristics from survey into data
    df.nodes <- data.frame(id = unique(df.net$alter)) %>%
        left_join(df.qualtrics, by = c('id' = 'NodeID')) %>%
        rename(consent = all_of(qidConsent), sudo = all_of(qidSudo)) %>%
        select(id, FullName, consent, sudo)
    # add names, fixing missing nodes
    df.nodes <- df.nodes %>%
        mutate(maybeLabel = ifelse((consent == ansConsent) & !(is.na(consent)), FullName, '???'),
               bConsent = (consent == ansConsent) & !(is.na(consent))) %>%
        select(id, maybeLabel, bConsent, sudo) %>%
        rename(consent = bConsent)
    # build igraph
    ntwk <- df.net %>%
        select(ego, alter, advice, support) %>%
        igraph::graph_from_data_frame(directed = TRUE, vertices = df.nodes)
    #E(ntwk)$width <- 1
    ntwk
}

