# README

This folder contains scripts to collect data for the course examples supported by and documented in this package. 

Preprint metadata (including abstracts) from *bioRxiv* and *medRxiv* is used to build the examples. *bioRxiv* explicitly supports the use of preprints for text mining (see [https://www.biorxiv.org/tdm](https://www.biorxiv.org/tdm)) but the terms stipulate that access to the bulk data is not intended for redistribution or rehosting. For this reason only the results of the preprint text analysis are shared in this repository but not the raw preprint data. Instead scripts are provided to retrieve the raw data and create subsets as expected by the sample analysis and exercises. Those have to be run before running the analysis scripts.


## Retrieve preprint data

The script indexed '1' can be used to collect the most recent preprint information via the official *bioRxiv* API (https://api.biorxiv.org/) using the `[medrxivr](https://docs.ropensci.org/medrxivr/)` package. NOTE that it can take several hours to retrieve this data.


## Preprint subsets

The course examples provide several subsets of the preprint data for analysis: 1) preprints related to Covid-19, 2) preprints containing terms related to AI and machine learning, 3) preprints containing terms related to sustainability science. The script indexed '2' can be used to create standalone copies of those two datasets. 


## Topic model evaluations

The examples and exercises create topic models with a specific number of topics (*'K'*). In order to make reasonable choices, topic model evaluations for several preprint subsets were run and evaluated. The scripts indexed '3a-e' can be used to replicate those evaluations including preprocessing of the documents. Those (long-running and parallelized) scripts should typically be run on a server with sufficient CPU and memory capacity. Adapt the scripts to match your setup and directory structure.

The raw results of those evaluations have not been included here due to size. The script indexed '4' can be used to process the results of those multiple topic model evaluations. The extracted metrics have been included as a package dataset `topicmodel_evaluations`. The results are referenced in the package documentation.



