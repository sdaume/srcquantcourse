# Project: SRC Quantitative Methods PhD Course
#
# Script purpose: This fits one set of topic models with different topic numbers
#                 (K) and evaluates with regard to four metrics (heldout,
#                 residuals, semantic coherence and exclusivity).
#                 The heldouts, models and metrics are saved in appropriately
#                 named '.Rdata' files indicating the topic modelling variation.
#                 This is setup to be run on a server with multiple cores.
#
# Stefan Daume

library(dplyr)
library(stm)
library(quanteda)
library(quanteda.textstats)
library(future)
library(furrr)
library(purrr)


###############################################################################################
# SETUP SOME CONTROL AND LABELLING VARIABLES
###############################################################################################

MAX_PARALLEL_CORES <- 5 #set this to a reasonable number based on the server spec

# setup some variables controlling the runs and outputs
base_result_label <- "preprints_covid"

seed_heldout <- 7648689
seed_stm <- 9868467

min_K <- 5
max_K <- 100
K_steps <- 5


###############################################################################################
# LOAD THE TEXT DATA
###############################################################################################

# we have to load and save the data differently depending on where we run the script
preprints <- NULL
remote_context <- FALSE

print(paste(Sys.time(), "Loading document data ..."))

if(remote_context) {
  load("~/data/preprintdata/preprints_raw.Rdata")
  results_dir <- paste("~/data/topicevals/", base_result_label, "/", sep = "")
} else {
  load("./data-raw/preprints_raw.Rdata")
  results_dir <- paste("./data-raw/topicevals/", base_result_label, "/", sep = "")
}


###############################################################################################
# FILTER AND PREPROCESS THE DOCUMENT SET
###############################################################################################

print(paste(Sys.time(), "Preparing DFM ..."))

preprints <- preprints_raw %>%
  group_by(doi) %>%
  filter(version == max(version)) %>%
  ungroup() %>%
  distinct(doi, .keep_all = TRUE) %>%
  mutate(published = stringr::str_trim(published)) %>%
  mutate(published = na_if(published, "NA")) %>%
  mutate(is_published = as.numeric(!is.na(published))) %>%
  mutate(year = lubridate::year(date)) %>%
  filter(year >= 2020 & year <= 2023) %>%
  select(doi, server, title, abstract, date, year, version, is_published)

# create a subset filtered by keywords in titles and abstracts
keywords <- c("sars-cov", "covid")

search_pattern <- stringr::regex(paste(keywords, collapse = "|"), ignore_case = TRUE)

preprints <- preprints %>%
  filter(stringr::str_detect(title, pattern = search_pattern) |
           stringr::str_detect(abstract, pattern = search_pattern))

preprint_stats <- preprints %>%
  group_by(server, year) %>%
  summarise(n_pubs = n()) %>%
  ungroup()

# Tokenize and create a document-feature-matrix as input to stm
pubs_dfm <- preprints %>%
  quanteda::corpus(docid_field = "doi", text_field = "abstract") %>%
  quanteda::tokens(remove_punct = TRUE,
                   remove_symbols = TRUE,
                   remove_numbers = TRUE,
                   remove_url = TRUE,
                   remove_separators = TRUE,
                   split_hyphens = TRUE) %>%
  quanteda::dfm() %>%
  quanteda::dfm_remove(pattern = quanteda::stopwords("english")) #%>%
  #quanteda::dfm_wordstem()

dfm_stats <- textstat_frequency(pubs_dfm)

# Filter the document feature matrix based on feature and document properties
# NOTE: these cutoffs are reasonable choices that reduce the matrix size, but
# they are just examples; it is always a advisable to explore the vocabulary and
# resulting documents to assess options for a specific document collection
pubs_dfm <- pubs_dfm %>%
  #quanteda::dfm_remove(pattern = c("sars", "cov", "covid")) %>%
  quanteda::dfm_remove(min_nchar = 2) %>%
  quanteda::dfm_trim(min_docfreq = 2, docfreq_type = "count") %>%
  quanteda::dfm_subset(quanteda::ntoken(.) > 4) %>%
  quanteda::dfm_trim(min_docfreq = 2, termfreq_type = "count")

dfm_stats <- textstat_frequency(pubs_dfm)

# when using the native document representation of STM (which is required when
# evaluating models in relation to a held-out, we need to assemble and supply
# the metadata (i.e. covars) separately, below we create a dataframe with covars
# (or docvars) for the documents remaining after the DFM filtering

pubs_undropped <- data.frame(doi = pubs_dfm@Dimnames$docs) %>%
  left_join(preprints, by = "doi")

covars_data <- pubs_undropped %>%
  select(doi, server, year)


###############################################################################################
# SETUP AND STORE THE HELDOUT
###############################################################################################

print(paste(Sys.time(), "Creating heldout DFM ..."))

K_values <- tibble::tibble(K = seq.int(from = min_K, to = max_K, by = K_steps))

# create a heldout corpus for evaluation of model fit
# (a seed is needed for replicability)
pubs_dfm_heldout <- make.heldout(pubs_dfm, seed = seed_heldout)

# save to replicate evaluation without running the preparations again
save(pubs_dfm_heldout,
     file = paste(results_dir,
                  base_result_label, "_dfm_heldout_K",
                  min_K, "_", max_K, ".Rdata", sep = ""))

print(paste(Sys.time(), "Preparing topic modelling ..."))

# run multiple parallel model evaluations, leave always one core unused
if (!is.null(MAX_PARALLEL_CORES) && MAX_PARALLEL_CORES > 0 &&
    MAX_PARALLEL_CORES < parallelly::availableCores()) {
  used_workers <- MAX_PARALLEL_CORES
} else {
  useable_cores <- parallelly::availableCores() - 1
  used_workers <- ifelse(nrow(K_values) <= useable_cores, nrow(K_values), useable_cores)
}


print(paste(Sys.time(), "Launched", used_workers, "parallel workers."))

print(paste(Sys.time(), "Fitting", nrow(K_values), "models in the range", min_K, "-", max_K))

#plan(sequential)
plan("multisession", workers = used_workers)

# fit the STM models with different Ks
pubs_covid_models <- K_values %>%
  mutate(topic_model = future_map(K, ~stm(documents = pubs_dfm_heldout$documents,
                                          vocab = pubs_dfm_heldout$vocab,
                                          prevalence = ~ server * s(year),
                                          data = covars_data,
                                          K = .,
                                          #emtol = 1e-06,
                                          #control = list("allow.neg.change" = FALSE),
                                          verbose = FALSE,
                                          seed = seed_stm),
                                  .options = furrr_options(seed = seed_stm)))

# save the models
save(pubs_covid_models,
     file = paste(results_dir,
                  base_result_label, "_models_K",
                  min_K, "_", max_K, ".Rdata", sep = ""))

print(paste(Sys.time(), "Finished fitting topic models, starting evaluation ..."))

# extract model fit metrics
pubs_covid_metrics <- pubs_covid_models %>%
  mutate(exclusivity = future_map(topic_model, exclusivity),
         semantic_coherence = future_map(topic_model, semanticCoherence,
                                         pubs_dfm_heldout$documents),
         eval_heldout = future_map(topic_model, eval.heldout,
                                   pubs_dfm_heldout$missing),
         residual = future_map(topic_model, checkResiduals,
                               pubs_dfm_heldout$documents))

plan("sequential")

# save to explore metrics
save(pubs_covid_metrics,
     file = paste(results_dir,
                  base_result_label, "_metrics_K",
                  min_K, "_", max_K, ".Rdata", sep = ""))



print(paste(Sys.time(), "DONE!"))
