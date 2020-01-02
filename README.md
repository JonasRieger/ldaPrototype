# ldaPrototype

## Determine a Prototype from a Number of Runs of Latent Dirichlet Allocation Measuring Its Similarities with S-CLOP
A procedure to select the LDA run with highest mean pairwise similarity, which is measured by S-CLOP, to all other runs. LDA runs are specified by its assignments leading to estimators for distribution parameters. Repeated runs lead to different results, which we encounter by choosing the most representative LDA run as prototype.

## Installation
A realease on CRAN is intended.
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
For wishes, issues, and bugs please use the [issue tracker](https://github.com/JonasRieger/ldaPrototype/issues).

[![Build Status](https://travis-ci.org/JonasRieger/ldaPrototype.svg?branch=master)](https://travis-ci.org/JonasRieger/ldaPrototype) 
[![Coverage Status](https://coveralls.io/repos/github/JonasRieger/ldaPrototype/badge.svg?branch=master)](https://coveralls.io/github/JonasRieger/ldaPrototype?branch=master)
