# Project: SRC Quantitative Methods PhD Course
#
# Script purpose: Summarize topic model evaluations for different preprint
#                 subsets and store them as a package datasets for use in the
#                 package documentation.
#
# Stefan Daume


library(ggplot2)
library(dplyr)
library(purrr)

load("./data-raw/topicevals/preprints/preprints_metrics_K5_100.Rdata")
load("./data-raw/topicevals/preprints_covid/preprints_covid_metrics_K5_100.Rdata")
load("./data-raw/topicevals/preprints_ml/preprints_ml_metrics_K5_100.Rdata")
load("./data-raw/topicevals/preprints_ses/preprints_ses_metrics_K5_100.Rdata")
load("./data-raw/topicevals/preprints_biodiv/preprints_biodiv_metrics_K5_100.Rdata")

pubs_all_metrics <- pubs_all_metrics %>%
  mutate(subset_label = "All preprints")

pubs_covid_metrics <- pubs_covid_metrics %>%
  mutate(subset_label = "Covid preprints")

pubs_ml_metrics <- pubs_ml_metrics %>%
  mutate(subset_label = "ML/AI preprints")

pubs_ses_metrics <- pubs_ses_metrics %>%
  mutate(subset_label = "SES preprints")

pubs_biodiv_metrics <- pubs_biodiv_metrics %>%
  mutate(subset_label = "Biodiversity preprints")

pubs_metrics <- dplyr::bind_rows(pubs_all_metrics, pubs_covid_metrics, pubs_ml_metrics,
                                 pubs_ses_metrics, pubs_biodiv_metrics)

topicmodel_evaluations <- pubs_metrics %>%
  group_by(subset_label) %>%
  transmute(K,
            iterations = map_dbl(map(topic_model, "convergence"), "its"),
            converged = map_lgl(map(topic_model, "convergence"), "converged"),
            heldout_likelihood = map_dbl(eval_heldout, "expected.heldout"),
            semantic_coherence = map_dbl(semantic_coherence, median),
            residuals = map_dbl(residual, "dispersion"),
            exclusivity = map_dbl(exclusivity, median)) %>%
  ungroup() %>%
  tidyr::gather(model_metric, value, -K, -subset_label) %>%
  mutate(model_metric = factor(model_metric,
                               levels = c("converged", "iterations",
                                          "heldout_likelihood", "semantic_coherence",
                                          "residuals", "exclusivity"),
                               labels = c("Converged", "Iterations",
                                          "Held-out likelihood", "Semantic coherence",
                                          "Residuals", "Exclusivity")))

# change overwrite to TRUE, to update the package dataset
usethis::use_data(topicmodel_evaluations, overwrite = FALSE)
