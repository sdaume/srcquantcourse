
<!-- README.md is generated from README.Rmd. Please edit that file -->

# srcquantcourse

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of `srcquantcourse` is to provide sample analysis, replication
scripts and exercises for the data analysis module of the *SRC
Quantitative Methods PhD Course*; this part of the module concentrates
on *probabilistic topic modelling*.

## Installation

You can install the development version of srcquantcourse from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sdaume/srcquantcourse")
```

Alternatively, clone this repo
(<https://github.com/sdaume/srcquantcourse>) in order to work on and
modify the scripts.

## License, credits and acknowledgements

The package is shared under an [MIT License](LICENSE.md).

The package relies on several other R packages that are listed in the
package [DESCRIPTION](DESCRIPTION) and accompanying scripts. The work of
these packages’ authors is hereby gratefully acknowledged.

To illustrate the methods covered in this package preprint meta-data
from [bioRxiv](https://www.biorxiv.org/) and
[medRxiv](https://www.medrxiv.org/) has been used. Those preprint
servers provide [API access](http://api.biorxiv.org/) to preprints which
is explicitly [intended to support text
mining](https://www.biorxiv.org/tdm); this is hereby gratefully
acknowledged. The actual preprints cannot be shared in this package, but
scripts utilizing the
[`medrxivr`](https://github.com/ropensci/medrxivr/) package are provided
to obtain the preprint data and replicate the results.

This package has been developed to support education and research at the
[Stockholm Resilience Centre](https://www.stockholmresilience.org); the
research has benefited from funding by the [Swedish Research Council for
Sustainable Development (Formas)](https://formas.se/).

## Disclaimer

The package author(s) are not associated with the *bioRxiv* or
*medRxiv*. This package has been developed as a reusable tool for
education and the author(s) own research and comes with no guarantee for
the correctness of results or included package functions.