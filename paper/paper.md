---
title: 'ldaPrototype: A method in R to get a Prototype of multiple Latent Dirichlet Allocations'
authors:
- affiliation: 1
  name: Jonas Rieger
  orcid: 0000-0002-0007-4478
date: "10 March 2020"
output: pdf_document
bibliography: paper.bib
tags:
- R
- topic modeling
- natural language processing
- text data
- tuning
- model selection
- high performance computing
- parallelization
affiliations:
- index: 1
  name: TU Dortmund University
---

# Summary

Topic Modeling [@blei2012] is one of the biggest subjects in the field of text data analysis. Here, the Latent Dirichlet Allocation [@blei2003] takes a special position. A large part of scientific text data analyses are based on this model (LDA). The LDA method has a far-reaching disadvantage. Random initialization and conditional reassignments within the iterative process of the Gibbs sampler [@griffiths2004] can result in fundamentally different models when executed several times on the same data and with identical parameter sets. This fact greatly limits the scientific reproducibility.

Up to now, the so-called eye-balling method has been used in practice to select suitable results. From a set of models, subjective decisions are made to select the model that seems to fit the data best or, in the worst case, the result that best supports one's hypothesis is chosen. This contradicts basically good scientific practice. A different method of objective and automated selection has also become established. A model from a set of LDAs can be determined optimizing the log-likelihood using the perplexity on held-out data. The [`R`](https://www.r-project.org/) [@R] package [`topicmodels`](https://CRAN.R-project.org/package=topicmodels) [@topicmodels] provides a workflow for this procedure. However, @chang2009 were able to show that this selection mechanism by optimizing likelihood-based measures does not correspond to the human perception of a well-adapted model of text data. Instead, the authors propose a so-called intruder procedure based on human codings. The corresponding methodology is implemented in the package [`tosca`](https://github.com/Docma-TU/tosca) [@tosca].

The [`R`](https://www.r-project.org/) package [`ldaPrototype`](https://github.com/JonasRieger/ldaPrototype) on the other hand determines a prototypical LDA by automated selection from a set of LDAs. The method improves reliability of findings drawn from LDA results [@rieger2020], which is achieved following a typical statistical approach. For a given combination of parameters, a number of models is calculated (usually about 100), from which that LDA is determined that is most similar to all other LDAs from a set of models. For this purpose pairwise model similarities are calculated using the S-CLOP measure (**S**imilarity of Multiple Sets by **C**lustering with **Lo**cal **P**runing), which can be determined by a clustering procedure of the individual topic units based on topic similarities of the two LDA results considered. The package offers visualization possibilities for comparisons of LDA models based on the clustering of the associated topics. Furthermore, the package supports the repetition of the modeling procedure of the LDA by a simple calculation of the repeated LDA runs.

In addition to the possibility of local parallel computation by connecting to the package [`parallelMap`](https://github.com/berndbischl/parallelMap) [@parallelMap], there is the possibility to calculate using batch systems on high performance computing (HPC) clusters by integrating helpful functions from the package [`batchtools`](https://github.com/mllg/batchtools) [@batchtools]. This is especially helpful if the text corpora contains several hundred of thousands articles and the sequential calculation of 100 or more LDA runs would extend over several days. The modeling of single LDA runs is done with the help of the computation time optimized [`R`](https://www.r-project.org/) package [`lda`](https://CRAN.R-project.org/package=lda) [@lda], which implements the calculation in `C++` code. In general, the package [`ldaPrototype`](https://github.com/JonasRieger/ldaPrototype) is based on `S3` objects and thus extends the packages [`lda`](https://CRAN.R-project.org/package=lda) and [`tosca`](https://github.com/Docma-TU/tosca) by user-friendly display and processing options. Other [`R`](https://www.r-project.org/) packages for estimating LDA are [`topicmodels`](https://CRAN.R-project.org/package=topicmodels) and  [`mallet`](https://github.com/mimno/RMallet) [@mallet], whereas [`stm`](https://www.structuraltopicmodel.com/) [@stm] offers a powerful framework for Structural Topic Models and [`quanteda`](https://quanteda.io/) [@quanteda] is a popular framework for preprocessing and quantitative analysis of text data.

# References
