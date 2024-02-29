#' An STM topic model with 20 topics
#'
#' This package data set provides a sample STM topic model with K = 20 topics
#' (see [stm::stm()]).
#' The model has been fit to bioRxiv and medRxiv preprints from 2020 to 2023 that
#' contain the terms 'sars-cov' or 'covid' in the title or abstract. The model
#' has been fit with the respective preprint server and publication year as
#' topic prevalence covariates. The included `covid_effect_K20` dataset captures
#' the regression parameters of these document covariates.
#'
#' @format An STM topic model object
#'
"covid_model_K20"


#' An STM regression object for a topic model with 20 topics
#'
#' This package data set provides the results of applying the
#' [stm::estimateEffect()] function to the included `covid_effect_K20` topic
#' model object.
#'
#' @format An STM regression object
#'
"covid_effect_K20"


#' Evaluation results for topic models with different K
#'
#' This is a convenience dataset to include the results of evaluating topic
#' models with different topic numbers (*K*) for thematic subsets of the
#' bioRxiv and medRxiv preprints. Scripts to run the actual evaluations are
#' included in the package repo.
#'
#' The dataset specifies the preprint subset, used K, type of evaluation metric
#' and the value of that metric.
#'
#' @format A dataframe with four variables
#'
"topicmodel_evaluations"
