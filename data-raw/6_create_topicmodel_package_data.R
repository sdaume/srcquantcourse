# Project: SRC Quantitative Methods PhD Course
#
# Script purpose: Add sample topic models as package data for reuse in
#                 documentation.
#
# Stefan Daume


library(usethis)
library(tidyr)
library(stringr)

# topic models
# covid preprints
load("./data-raw/preprint_models/covid_model_K20.Rdata")
load("./data-raw/preprint_models/covid_effect_K20.Rdata")

usethis::use_data(covid_model_K20, overwrite = FALSE)
usethis::use_data(covid_effect_K20, overwrite = FALSE)

# biodiversity preprints
load("./data-raw/preprint_models/biodiv_model_K10.Rdata")
load("./data-raw/preprint_models/biodiv_effect_K10.Rdata")

usethis::use_data(biodiv_model_K10, overwrite = FALSE)
usethis::use_data(biodiv_effect_K10, overwrite = FALSE)


# publication stats for illustration
load("./data-raw/covid_preprints.Rdata")

covid_preprint_stats <- covid_preprints %>%
  group_by(server, year, is_published) %>%
  summarise(n_pubs = n()) %>%
  ungroup() %>%
  #group_by(server, year) %>%
  #mutate(server_all = sum(n_pubs)) %>%
  #ungroup() %>%
  pivot_wider(id_cols = c("year"),
              names_from = c("server", "is_published"),
              values_from = c("n_pubs")) %>%
  mutate(biorxiv_all = biorxiv_0 + biorxiv_1) %>%
  mutate(medrxiv_all = medrxiv_0 + medrxiv_1)

