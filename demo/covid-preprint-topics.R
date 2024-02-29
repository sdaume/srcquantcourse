# Project: SRC Quantitative Methods PhD Course
#
# Script purpose: For convenience this script replicates the code in the
#                 package vignette (see /vignettes/covid-preprint-topics.Rmd).
#                 Alternatively, in order to replicate or adapt this example,
#                 copy the code from package built package documentation
#                 (see https://sdaume.github.io/srcquantcourse/) or copy the
#                 Rmd file and work interactively with that copy.
#                 The latest code in any '.Rmd' file can also be extracted into
#                 an '.R' file with knitr::purl().
#
# Stefan Daume

library(srcquantcourse)
#library(medrxivr)
library(dplyr)
library(tidyr)
library(tidytext)
library(tibble)
library(stringr)
library(quanteda)
library(quanteda.textstats)
library(stm)
library(stminsights)
library(ggplot2)

# for this example we assume that you have downloaded the data and created a
# local copy to replicate the analysis
preprint_rdata_url <- url("ENTER_RDATA_LOCATION_HERE")

load(preprint_rdata_url)

## FOR ILLUSTRATION - RUN ONLY IF YOU HAVE NOT YET CREATED THE RDATA SET!
## library(dplyr)
## library(medrxivr)
##
## # get publications from medRxiv and bioRxiv
## pubs_biorxiv_raw <- medrxivr::mx_api_content(server = "biorxiv",
##                                              #from_date = "2019-01-01",
##                                              to_date = "2023-12-31")
##
## pubs_medrxiv_raw <- medrxivr::mx_api_content(server = "medrxiv",
##                                              #from_date = "2019-01-01",
##                                              to_date = "2023-12-31")
##
## pubs_biorxiv_raw <- pubs_biorxiv_raw %>%
##   mutate(server = "biorxiv")
##
## pubs_medrxiv_raw <- pubs_medrxiv_raw %>%
##   mutate(server = "medrxiv")
##
## preprints_raw <- dplyr::bind_rows(pubs_biorxiv_raw, pubs_medrxiv_raw)
##
## save(preprints_raw, file = "./data-raw/preprints_raw.Rdata")


## Clean, filter and annotate the preprint data
preprints_cleaned <- preprints_raw %>%
  group_by(doi) %>%
  filter(version == max(version)) %>%
  ungroup() %>%
  distinct(doi, .keep_all = TRUE)


preprints <- preprints_cleaned %>%
  mutate(published = stringr::str_trim(published)) %>%
  mutate(published = na_if(published, "NA")) %>%
  mutate(is_published = as.numeric(!is.na(published))) %>%
  mutate(is_published = case_when(is_published == 1 ~ "published",
                                  is_published == 0 ~ "not published",
                                  TRUE ~ "undefined")) %>%
  mutate(year = lubridate::year(date)) %>%
  filter(year >= 2020 & year <= 2023) %>%
  select(doi, server, title, abstract, date, year, version, is_published)


keywords <- c("sars-cov", "covid")

search_pattern <- stringr::regex(paste(keywords, collapse = "|"),
                                 ignore_case = TRUE)

covid_preprints <- preprints %>%
  filter(stringr::str_detect(title, pattern = search_pattern) |
           stringr::str_detect(abstract, pattern = search_pattern))


## Preparing and preprocessing the documents for text analysis

## Create a corpus
pubs_corpus <- covid_preprints %>%
  quanteda::corpus(docid_field = "doi", text_field = "abstract")

# echo the corpus
pubs_corpus


## Tokenize and preprocess
pubs_tokens <- pubs_corpus %>%
  quanteda::tokens(remove_punct = TRUE,
                   remove_symbols = TRUE,
                   remove_numbers = TRUE,
                   remove_url = TRUE,
                   remove_separators = TRUE,
                   split_hyphens = TRUE)


## Create a Document-feature matrix
pubs_dfm <- pubs_tokens %>%
  quanteda::dfm()


## Filter terms and documents
pubs_dfm <- pubs_dfm %>%
  quanteda::dfm_remove(pattern = quanteda::stopwords("english")) #%>%
  #quanteda::dfm_wordstem()


pubs_dfm <- pubs_dfm %>%
  quanteda::dfm_remove(min_nchar = 2) %>%
  quanteda::dfm_trim(min_docfreq = 2, docfreq_type = "count") %>%
  quanteda::dfm_subset(quanteda::ntoken(.) > 4)

## Topic modeling

## Fitting the STM topic model
covid_stm_docs <- quanteda::convert(pubs_dfm, to = "stm")

covid_model_K20 <- stm(documents = covid_stm_docs$documents,
                       vocab = covid_stm_docs$vocab,
                       data = covid_stm_docs$meta,
                       prevalence = ~ server * s(year),
                       K = 20,
                       verbose = TRUE,
                       seed = 9868467)


## Estimating the effect of document covariates
covid_effect_K20 <- estimateEffect(1:20 ~ server * s(year),
                                   stmobj = covid_model_K20,
                                   metadata = covid_stm_docs$meta)


## Analysing and interpreting the topic model

## Basic topic model information
plot(covid_model_K20, n = 5)

summary(covid_model_K20)



## Topic-document and term-topic distributions

# retrieve the 'gamma' matrix
gamma <- tidytext::tidy(covid_model_K20, matrix = "gamma")

glimpse(gamma)

# retrieve the 'beta' matrix
beta <- tidytext::tidy(covid_model_K20, matrix = "beta")

glimpse(beta)


## Understanding and labeling topics

# get the top FREX words
frex_top20 <- as.data.frame(labelTopics(covid_model_K20, n = 20)$frex) %>%
  rownames_to_column(var = "topic") %>%
  pivot_longer(starts_with("V"), values_to = "term") %>%
  mutate(is_frex = 1) %>%
  select(-name)

topic_words <- tidytext::tidy(covid_model_K20, matrix = "beta") %>%
  #filter(!(term %in% c("sars", "cov", "covid"))) %>%
  mutate(topic = as.character(topic)) %>%
  group_by(topic) %>%
  arrange(-beta) %>%
  slice_head(n = 50) %>%
  mutate(beta_norm = (beta - min(beta)) / (max(beta) - min(beta))) %>%
  ungroup() %>%
  left_join(frex_top20, by = c("topic", "term")) %>%
  mutate(is_frex = ifelse(is.na(is_frex), "0", "1")) %>%
  filter(!(term %in% c("sars", "cov", "covid"))) %>%
  mutate(topic = paste("Topic", topic))

ggplot(topic_words, aes(label = term, size = beta_norm, color = is_frex)) +
  ggwordcloud::geom_text_wordcloud_area(shape = "square",
                                        family = "Arial",
                                        rm_outside = TRUE) +
  scale_radius(range = c(4, 15)) +
  scale_color_manual(values = c("0" = "black", "1" = "#D55E00")) +
  facet_wrap(~topic, ncol = 4)


## ----eval=FALSE--------------------------------------------------------------------------------------------------------------------
## # might be useful to add an example for stm::findThoughts() here


## Covariate effects
summary(covid_effect_K20)



## Covariate: Publication year
plot(covid_effect_K20,
     covariate = "year",
     method = "continuous",
     model = covid_model_K20,
     topics = c(1, 16, 11),
     xaxt = "n",
     main = 'Effect of publication year on prevalence of Topic 1 ("epidemic \nmodels"), Topic 16 ("testing") and Topic 11 ("vaccines")',
     labeltype = "prob",
     xlab = "Publication year")
axis(1, at = c("2020","2021","2022","2023"), labels = c(2020, 2021, 2022, 2023))


## Covariate: Publication year (using `stminsights`)
year_effect <- get_effects(estimates = covid_effect_K20,
                           variable = "year",
                           type = "continuous")

year_effect %>%
  mutate(topic = as.character(topic)) %>%
  mutate(topic = paste("Topic", topic)) %>%
    ggplot(aes(x = value, y = proportion)) +
      geom_line() +
      geom_ribbon(aes(ymin = lower, ymax = upper),
                  alpha = 0.2, linetype = 0)  +
      xlab("Publication year") +
      ylab("Topic prevalence") +
      facet_wrap(~topic, ncol = 4) +
      theme_minimal()


## Covariate: preprint server (treatment effect medrxiv)
plot(covid_effect_K20,
     covariate = "server",
     #topics = c(9, 10, 16, 1),
     model = covid_model_K20,
     method = "difference",
     cov.value1 = "medrxiv", cov.value2 = "biorxiv",
     xlab = "higher biorxiv prevalence ... higher medrxiv prevalence",
     xlim = c(-0.19, 0.1),
     #labeltype = "prob",
     main = "Effect of preprint server ('treatment medrxiv')")


## Covariate: preprint server (prevalence individual topic)
plot(covid_effect_K20,
     covariate = "server",
     topics = c(9),
     model = covid_model_K20,
      method = "pointestimate",
     xlab = "Topical prevalence",
     xlim = c(-0.04, 0.2),
     #labeltype = "prob",
     main = "Effect of preprint server covariate for Topic 9")



## Covariate effect: preprint server and publication year
plot(covid_effect_K20,
     topics = c(17),
     covariate = "year",
     model = covid_model_K20,
     ci.level = 0.95,
     method = "continuous",
     moderator = "server",
     moderator.value = "medrxiv",
     linecol = "#619CFF", lwd = 4,
     xlab = "Publication year",
     ylim = c(0, .15),
     main = 'Effect of preprint server on Topic 17 ("virus variants")',
     xaxt = "n",
     printlegend = F)
plot(covid_effect_K20,
     topics = c(17),
     covariate = "year",
     model = covid_model_K20,
     method = "continuous",
     moderator = "server",
     moderator.value = "biorxiv",
     linecol = "#F8766D", lwd = 2,
     add = T,
     printlegend = F)
legend(2020, .15, c("medRxiv", "bioRxiv"), lwd = 2, col = c("#619CFF", "#F8766D"))
axis(1, at = c("2020","2021","2022","2023"), labels = c(2020, 2021, 2022, 2023))


## Covariate effect: preprint server and publication year (`stminsights`)
biorxiv_effect <- get_effects(covid_effect_K20,
                              variable = "year", type = "continuous",
                              moderator = "server", modval = "biorxiv")

medrxiv_effect <- get_effects(covid_effect_K20,
                              variable = "year", type = "continuous",
                              moderator = "server", modval = "medrxiv")

server_effects <- bind_rows(biorxiv_effect, medrxiv_effect)

server_effects %>%
  mutate(topic = as.character(topic)) %>%
  mutate(topic = paste("Topic", topic)) %>%
    ggplot(aes(x = value, y = proportion, color = moderator,
               group = moderator, fill = moderator)) +
      geom_line() +
      geom_ribbon(aes(ymin = lower, ymax = upper,
                      fill = moderator), alpha = 0.2, linetype = 0) +
      xlab("Publication year") +
      ylab("Topic prevalence") +
      facet_wrap(~topic, ncol = 4) +
      theme_minimal() +
      theme(legend.position = "bottom")


## Exploring the topic structure
## Topic correlations
covid_topic_correlations <- topicCorr(covid_model_K20)

plot(covid_topic_correlations)

