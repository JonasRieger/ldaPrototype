# ldaPrototype
[![status](https://joss.theoj.org/papers/ecce89f69453dc2ee0c697fbcb776de8/status.svg)](https://joss.theoj.org/papers/ecce89f69453dc2ee0c697fbcb776de8)
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

## (Quick Start) Example
Load the package and the example dataset from Reuters consisting of 91 articles - [tosca::LDAprep](https://github.com/Docma-TU/tosca/blob/master/R/LDAprep.R) can be used to manipulate text data to the format requested by ``ldaPrototype``.
```{R}
library("ldaPrototype")
data(reuters_docs)
data(reuters_vocab)
```
Run the shortcut function to create a LDAPrototype object. It consists of the LDAPrototype of 4 LDA runs (with specified seeds) with 10 topics each. The LDA selected by the algorithm can be retrieved using ``getPrototype`` or ``getLDA``.
```{R}
res = LDAPrototype(docs = reuters_docs, vocabLDA = reuters_vocab, n = 4, K = 10, seeds = 1:4)
proto = getPrototype(res) #= getLDA(res)
```
The same result can also be achieved by executing the following lines of code in several steps, which can be useful for interim evaluations.
```{R}
reps = LDARep(docs = reuters_docs, vocab = reuters_vocab,
  n = 4, K = 10, seeds = 1:4)
topics = mergeTopics(reps, vocab = reuters_vocab)
jacc = jaccardTopics(topics)
sclop = SCLOP.pairwise(jacc)
res2 = getPrototype(reps, sclop = sclop)

proto2 = getPrototype(res2) #= getLDA(res2)

identical(res, res2)
```
To get an overview of the workflow, the associated functions and getters for each type of object, the following call is helpful:
```{R}
?`ldaPrototype-package`
```

## (Slightly more detailed) Example
Similar to the quick start example, the shortcut of one single call is again compared with the step-by-step procedure. 
We model 5 LDAs with ``K = 12`` topics, hyperparameters ``alpha = eta = 0.1`` and seeds ``1:5``. We want to calculate the log likelihoods for the 20 iterations after 5 burn-in iterations and topic similarities should be based on ``atLeast = 3`` words (see Step 3 below). In addition, we want to keep all interim calculations, which would be discarded by default to save memory space.
```{R}
res = LDAPrototype(docs = reuters_docs, vocabLDA = reuters_vocab,
  n = 5, K = 12, alpha = 0.1, eta = 0.1, compute.log.likelihood = TRUE,
  burnin = 5, num.iterations = 20, atLeast = 3, seeds = 1:5,
  keepLDAs = TRUE, keepSims = TRUE, keepTopics = TRUE)
```
Based on ``res`` we can have a look at several getter functions:
```{R}
getID(res)
getPrototypeID(res)

getParam(res)
getParam(getLDA(res))

getLDA(res, all = TRUE)
getLDA(res)

est = getEstimators(getLDA(res))
est$phi[,1:3]
est$theta[,1:3]
getLog.likelihoods(getLDA(res))

getSCLOP(res)
getSimilarity(res)[1:5, 1:5]
tosca::topWords(getTopics(getLDA(res)), 5)
```
#### Step 1: LDA Replications
In the first step we simply run the LDA procedure five times with the given parameters. This can also be done with support of [batchtools](https://github.com/mllg/batchtools) using ``LDABatch`` instead of ``LDARep`` or [parallelMap](https://github.com/mlr-org/parallelMap) setting the ``pm.backend`` and (optionally) ``ncpus`` argument(s).
```{R}
reps = LDARep(docs = reuters_docs, vocab = reuters_vocab,
  n = 5, K = 12, alpha = 0.1, eta = 0.1, compute.log.likelihood = TRUE,
  burnin = 5, num.iterations = 20, seeds = 1:5)
```
#### Step 2: Merging Topic Matrices of Replications
The topic matrices of all replications are merged and reduced to the vocabulary given in ``vocab``. By default the vocabulary of the first topic matrix is used as a simplification of the case that all LDAs contain the same vocabulary set.
```{R}
topics = mergeTopics(reps, vocab = reuters_vocab)
```
#### Step 3: Topic Similarities
We use the merged topic matrix to calculate pairwise topic similarites using the Jaccard coefficient with parameters adjusting the consideration of words. A word is taken as relevant for a topic if its count passes thresholds given by ``limit.rel`` and ``limit.abs``. A word is considered for calculation of similarities if it's relevant for the topic or if it belongs to the (``atLeast =``) 3 most common words in the corresponding topic.
```{R}
jacc = jaccardTopics(topics, limit.rel = 1/500, limit.abs = 10, atLeast = 3)
getSimilarity(jacc)[1:3, 1:3]
```
We can check the number of relevant and considered words using the ad-hoc getter. The difference between ``n1`` and ``n2`` can become larger than (``atLeast =``) 3 if there are ties in the count of words, which is negligible for large sample sizes.
```{R}
n1 = getRelevantWords(jacc)
n2 = getConsideredWords(jacc)
(n2-n1)[n2-n1 != 0]
```
#### Step 3.1: Representation of Topic Similarities as Dendrogram
It is possible to represent the calulcated pairwise topic similarities as dendrogram using ``dendTopics`` and related ``plot`` options.
```{R}
dend = dendTopics(jacc)
plot(dend)
```
The S-CLOP algorithm results in a pruning state of the dendrogram, which can be retrieved calling ``pruneSCLOP``. By default each of the topics is colorized by its LDA run belonging; but the cluster belongings can also be visualized by the colors or by vertical lines with freely chosen parameters.
```{R}
pruned = pruneSCLOP(dend)
plot(dend, pruned)
plot(dend, pruning = pruned, pruning.par = list(type = "both", lty = 1, lwd = 2, col = "red"))
```
#### Step 4: Pairwise LDA Model Similarities (S-CLOP)
For determination of the LDAPrototype the pairwise S-CLOP similarities of the 5 LDA runs are needed.
```{R}
sclop = SCLOP.pairwise(jacc)
```
#### Step 5: Determine LDAPrototype
In the last step the LDAPrototype itself is determined by maximizing the mean pairwise S-CLOP per LDA.
```{R}
res2 = getPrototype(reps, sclop = sclop)
```
There are several possibilites for using shortcut functions to summarize steps of the procedure. For example, we can determine the LDAPrototype after Step 1:
```{R}
res3 = getPrototype(reps, atLeast = 3)
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
For bug reports (lack of documentation, misleading or wrong documentation, unexpected behaviour, ...) and feature requests please use the [issue tracker](https://github.com/JonasRieger/ldaPrototype/issues).
Pull requests are welcome and will be included at the discretion of the author.
