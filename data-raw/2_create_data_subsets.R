# Project: SRC Quantitative Methods PhD Course
#
# Script purpose: Create copies of thematic subsets of preprints. This includes
#                 deduplicating, filtering by year and adding inferred meta-data
#                 variables (year and publication status).
#
# NOTE: ALL steps in this script must be run to create data sets that can be
#       used with other scripts in this package.
#
# Stefan Daume - 15.02.2024 (initial version)


library(dplyr)
library(stringr)
library(lubridate)

###############################################################################################
# CREATE THEMATIC PREPRINT SUBSETS
###############################################################################################

load("./data-raw/preprints_raw.Rdata")

preprints <- preprints_raw %>%
  group_by(doi) %>%
  filter(version == max(version)) %>%
  ungroup() %>%
  distinct(doi, .keep_all = TRUE) %>%
  mutate(published = stringr::str_trim(published)) %>%
  mutate(published = na_if(published, "NA")) %>%
  mutate(is_published = as.numeric(!is.na(published))) %>%
  mutate(year = lubridate::year(date)) %>%
  filter(year >= 2019 & year <= 2023) %>%
  select(doi, server, title, abstract, date, year, version, is_published)


# create an AI/machine learning subset
keywords <- c("machine learning", "deep learning", "artificial intelligence")
search_pattern <- stringr::regex(paste(keywords, collapse = "|"), ignore_case = TRUE)

ml_preprints <- preprints %>%
  filter(stringr::str_detect(title, pattern = search_pattern) |
           stringr::str_detect(abstract, pattern = search_pattern))

save(ml_preprints, file = "./data-raw/ml_preprints.Rdata")




# create a sustainability science subset
keywords <- c("sustainability", "sustainable development", "socio-ecological",
              "social-ecological", "biosphere", "resilience")

search_pattern <- stringr::regex(paste(keywords, collapse = "|"), ignore_case = TRUE)

ses_preprints <- preprints %>%
  filter(stringr::str_detect(title, pattern = search_pattern) |
           stringr::str_detect(abstract, pattern = search_pattern))

save(ses_preprints, file = "./data-raw/ses_preprints.Rdata")




# create a covid subset
keywords <- c("sars-cov", "covid")

search_pattern <- stringr::regex(paste(keywords, collapse = "|"), ignore_case = TRUE)

covid_preprints <- preprints %>%
  filter(year >= 2020 & year <= 2023) %>%
  filter(stringr::str_detect(title, pattern = search_pattern) |
           stringr::str_detect(abstract, pattern = search_pattern))

save(covid_preprints, file = "./data-raw/covid_preprints.Rdata")
