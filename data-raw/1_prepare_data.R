# Project: SRC Quantitative Methods PhD Course
#
# Script purpose: Retrieve preprint data from bioRxiv.
#
# NOTE: ALL steps in this script must be run to create a data set that can be
#       used with other scripts in this package.
#
# Stefan Daume - 15.02.2024 (initial version)


library(dplyr)
library(medrxivr)


###############################################################################################
# GET PREPRINT DATA
###############################################################################################

# get publications from medRxiv and bioRxiv
pubs_biorxiv_raw <- medrxivr::mx_api_content(server = "biorxiv",
                                             #from_date = "2019-01-01",
                                             to_date = "2023-12-31")

pubs_medrxiv_raw <- medrxivr::mx_api_content(server = "medrxiv",
                                             #from_date = "2019-01-01",
                                             to_date = "2023-12-31")

ppe <- preprints_raw %>%
  group_by(server, date) %>%
  summarise(n_pubs = n()) %>%
  ungroup() %>%
  arrange(server, desc(date))

pubs_biorxiv_raw <- pubs_biorxiv_raw %>%
  mutate(server = "biorxiv")

pubs_medrxiv_raw <- pubs_medrxiv_raw %>%
  mutate(server = "medrxiv")

preprints_raw <- rbind(pubs_biorxiv_raw, pubs_medrxiv_raw)

save(preprints_raw, file = "./data-raw/preprints_raw.Rdata")
