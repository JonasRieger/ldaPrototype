# ldaPrototype
## Determine a Prototype from a number of Runs of Latent Dirichlet Allocation measuring its similarities with S-CLOP

A procedure to select the LDA run with highest mean pairwise similarity, which is measured by S-CLOP, to all other runs.
LDA runs are specified by its assignments leading to estimators for distribution parameters.
Repeated runs lead to different results, which we encounter by choosing the most representative LDA run as prototype.
