# ldaPrototype

## Determine a Prototype from a number of Runs of Latent Dirichlet Allocation measuring its similarities with S-CLOP
A procedure to select the LDA run with highest mean pairwise similarity, which is measured by S-CLOP, to all other runs. LDA runs are specified by its assignments leading to estimators for distribution parameters. Repeated runs lead to different results, which we encounter by choosing the most representative LDA run as prototype.

## Installation
A realease on CRAN is intended.
For the development version use [devtools](https://cran.r-project.org/package=devtools):
```{R}
devtools::install_github("JonasRieger/ldaPrototype")
```

## Contribution
For wishes, issues, and bugs please use the [issue tracker]https://github.com/JonasRieger/ldaPrototype/issues.

[![Build Status](https://travis-ci.org/JonasRieger/ldaPrototype.svg?branch=master)](https://travis-ci.org/JonasRieger/ldaPrototype) 
[![Coverage Status](https://coveralls.io/repos/github/JonasRieger/ldaPrototype/badge.svg?branch=master)](https://coveralls.io/github/JonasRieger/ldaPrototype?branch=master)
