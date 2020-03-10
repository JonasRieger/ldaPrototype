# ldaPrototype

[![Build Status](https://travis-ci.org/JonasRieger/ldaPrototype.svg?branch=master)](https://travis-ci.org/JonasRieger/ldaPrototype) 
[![CRAN](https://www.r-pkg.org/badges/version/ldaPrototype)](https://cran.r-project.org/package=ldaPrototype)
[![Coverage Status](https://coveralls.io/repos/github/JonasRieger/ldaPrototype/badge.svg?branch=master)](https://coveralls.io/github/JonasRieger/ldaPrototype?branch=master)
[![DOI](https://zenodo.org/badge/187803702.svg)](https://zenodo.org/badge/latestdoi/187803702)

## Prototype of Multiple Latent Dirichlet Allocation Runs
Determine a Prototype from a number of runs of Latent Dirichlet Allocation (LDA) measuring its similarities with S-CLOP: A procedure to select the LDA run with highest mean pairwise similarity, which is measured by S-CLOP (Similarity of multiple sets by Clustering with Local Pruning), to all other runs. LDA runs are specified by its assignments leading to estimators for distribution parameters. Repeated runs lead to different results, which we encounter by choosing the most representative LDA run as prototype.

## Installation
```{R}
install.packages("ldaPrototype")
```
For the development version use [devtools](https://cran.r-project.org/package=devtools):
```{R}
devtools::install_github("JonasRieger/ldaPrototype")
```

## Related Software
* [tm](https://CRAN.R-project.org/package=tm) is useful for preprocessing text data.
* [lda](https://CRAN.R-project.org/package=lda) offers a fast implementation of the Latent Dirichlet Allocation and is used by ``ldaPrototype``.
* [quanteda](https://quanteda.io/) is a framework for "Quantitative Analysis of Textual Data".
* [stm](https://www.structuraltopicmodel.com/) is a framework for Structural Topic Models.
* [tosca](https://CRAN.R-project.org/package=tosca) is a framework for statistical methods in content analysis including visualizations and validation techniques. It is also useful for managing and manipulating text data to a structure requested by ``ldaPrototype``.
* [topicmodels](https://CRAN.R-project.org/package=topicmodels) is another framework for various topic models based on the Latent Dirichlet Allocation and Correlated Topics Models.

## Contribution
This R package is licensed under the [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html).
For bug reports and feature requests please use the [issue tracker](https://github.com/JonasRieger/ldaPrototype/issues).
