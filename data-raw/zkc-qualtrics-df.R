## code to prepare `zkc.qualtrics.df` dataset goes here
library(networkSurveyBackend)

file.sudo <- list(type = 'text/csv',
                  datapath = system.file("extdata", "ZKC_Qualtrics_Pseudonyms.csv", package = "networkSurveyBackend"))
zkc.qualtrics.df <- load_qualtrics_file(file.sudo)

usethis::use_data(zkc.qualtrics.df, overwrite = TRUE)
