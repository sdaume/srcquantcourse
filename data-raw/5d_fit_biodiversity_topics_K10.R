# Project: SRC Quantitative Methods PhD Course
#
# Script purpose: Fit a K=10 topic model for biodiversity preprints
#
# Stefan Daume

library(dplyr)
library(stm)
library(quanteda)
library(quanteda.textstats)


###############################################################################################
# SETUP SOME CONTROL AND LABELLING VARIABLES
###############################################################################################

# setup some variables controlling the run and outputs
base_result_label <- "preprint_models"

results_dir <- paste("./data-raw/", base_result_label, "/", sep = "")

seed_stm <- 9868467


###############################################################################################
# LOAD THE TEXT DATA
###############################################################################################

load("./data-raw/preprints_raw.Rdata")


###############################################################################################
# FILTER AND PREPROCESS THE DOCUMENT SET
###############################################################################################

preprints <- preprints_raw %>%
  group_by(doi) %>%
  filter(version == max(version)) %>%
  ungroup() %>%
  distinct(doi, .keep_all = TRUE) %>%
  mutate(published = stringr::str_trim(published)) %>%
  mutate(published = na_if(published, "NA")) %>%
  mutate(is_published = as.numeric(!is.na(published))) %>%
  mutate(is_published = case_when(is_published == 1 ~ "published",
                                  is_published == 0 ~ "not published",
                                  TRUE ~ "undefined")) %>%
  mutate(year = lubridate::year(date)) %>%
  #filter(server == "biorxiv") %>%
  filter(year >= 2017 & year <= 2021) %>%
  select(doi, server, title, abstract, date, year, version, is_published)

# create a subset filtered by keywords in titles and abstracts
keywords <- c("biodiversity")

search_pattern <- stringr::regex(paste(keywords, collapse = "|"), ignore_case = TRUE)

preprints <- preprints %>%
  filter(stringr::str_detect(title, pattern = search_pattern) |
           stringr::str_detect(abstract, pattern = search_pattern))

preprint_stats <- preprints %>%
  group_by(year, is_published) %>%
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
                   split_hyphens = TRUE) %>% #!!!!
  quanteda::dfm() %>%
  quanteda::dfm_remove(pattern = quanteda::stopwords("english")) #%>%
  #quanteda::dfm_wordstem()

dfm_stats <- textstat_frequency(pubs_dfm)

# Filter the document feature matrix based on feature and document properties
# NOTE: these cutoffs are reasonable choices that reduce the matrix size, but
# they are just examples; it is always a advisable to explore the vocabulary and
# resulting documents to assess options for a specific document collection
pubs_dfm <- pubs_dfm %>%
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
  select(doi, server, year, is_published)


###############################################################################################
# FIT TOPIC MODEL
###############################################################################################

print(paste(Sys.time(), "Preparing topic modelling ..."))

# for consistency we use the native STM format; the stm() function actually calls
# quanteda::convert() internally when a DFM is passed, but in order to ensure
# that misalignments of docs and metadata are avoided when reproducing any parts
# of the analysis we explicitly work with the native format
biodiv_stm_docs <- quanteda::convert(pubs_dfm, to = "stm")

save(biodiv_stm_docs,
     file = paste(results_dir,
                  #base_result_label,
                  "biodiv_stm_docs",
                  #K,
                  ".Rdata", sep = ""))

# fit the STM models with different Ks
biodiv_model_K10 <- stm(documents = biodiv_stm_docs$documents,
                    vocab = biodiv_stm_docs$vocab,
                    data = biodiv_stm_docs$meta,
                    prevalence = ~ is_published * year,
                    K = 10,
                    #emtol = 1e-06,
                    #control = list("allow.neg.change" = FALSE),
                    verbose = TRUE,
                    seed = seed_stm)


summary(biodiv_model_K10)

plot(biodiv_model_K10)

# save the models
save(biodiv_model_K10,
     file = paste(results_dir,
                  #base_result_label,
                  "biodiv_model_K10",
                  #min_K, "_", max_K,
                  ".Rdata", sep = ""))

print(paste(Sys.time(), "Finished fitting topic model ..."))


###############################################################################################
# EVALUATE COVARIATE EFFECT
###############################################################################################

print(paste(Sys.time(), "Evaluating effect of covariates ..."))

biodiv_effect_K10 <- estimateEffect(1:10 ~ is_published * year,
                                   stmobj = biodiv_model_K10,
                                   metadata = biodiv_stm_docs$meta)

summary(biodiv_effect_K10)
# save to explore metrics
save(biodiv_effect_K10,
     file = paste(results_dir,
                  #base_result_label,
                  "biodiv_effect_K10",
                  #K,
                  ".Rdata", sep = ""))


print(paste(Sys.time(), "DONE!"))

