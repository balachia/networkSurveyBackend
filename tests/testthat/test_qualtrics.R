library(networkSurveyBackend)

file.sudo <- list(type = 'text/csv',
                  datapath = system.file("extdata", "ZKC_Qualtrics_Pseudonyms.csv", package = "networkSurveyBackend"))
df.sudo <- load_qualtrics_file(file.sudo)

test_that("csv loads", {
    expect_equal(nrow(df.sudo), 34)
    expect_false(is.null(df.sudo$pseudonym))
})

test_that("qualtrics converts to igraph", {
    g <- qualtrics_to_igraph(df.sudo)
    vas <- names(igraph::vertex.attributes(g))
    eas <- names(igraph::edge.attributes(g))
    expect_true('name' %in% vas)
    expect_true('maybeLabel' %in% vas)
    expect_true('consent' %in% vas)
    expect_true('sudo' %in% vas)
    expect_true('advice' %in% eas)
    expect_true('support' %in% eas)
})
