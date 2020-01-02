context("dendTopics")

data("reuters_docs")
data("reuters_vocab")

res = LDARep(docs = reuters_docs, K = 10, vocab = reuters_vocab, n = 3, num.iterations = 5)
jacc = jaccardTopics(mergeTopics(res))

dend = dendTopics(jacc)
dend2 = dendTopics(getSimilarity(jacc))

dend3 = dendTopics(jacc, ind = "LDARep1")

pruned = pruneSCLOP(dend)
pruned2 = pruneSCLOP(dend2)

pruned3 = pruneSCLOP(dend3)

pairs = SCLOP.pairwise(jacc)
pairs2 = SCLOP.pairwise(getSimilarity(jacc))

test_that("dendTopics_pruneSCLOP", {
  expect_true(is.dendrogram(dend))
  expect_equal(dend, dend2)
  expect_equal(pruned, pruned2)
  expect_equal(length(unlist(lapply(pruned, labels))), nrow(getSimilarity(jacc)))
  expect_equal(length(unlist(lapply(pruned, labels))), length(labels(dend)))

  expect_equal(length(unlist(lapply(pruned3, labels))), length(labels(dend3)))
})

test_that("SCLOP_disparitSum", {
  expect_equal(SCLOP(dend), SCLOP(dend2))
  sclop = SCLOP(dend)
  expect_true(is.numeric(sclop))
  expect_true(sclop >= 0 && sclop <= 1)
  expect_length(sclop, 1)

  expect_equal(disparitySum(dend), disparitySum(dend2))
  ds = disparitySum(dend)
  expect_true(ds >= 0)
  expect_length(ds, 1)

  expect_true(is.na(SCLOP(dend3)))
})

test_that("SCLOP.pairwise", {
  expect_true(all(is.na(diag(pairs))))
  expect_true(isSymmetric(pairs))
  expect_true(all(pairs[lower.tri(pairs)] <= 1 & pairs[lower.tri(pairs)] >= 0))
  expect_identical(pairs, pairs2)
})

test_that("dend_sclop_errors", {
  expect_error(dendTopics(res))
  expect_error(pruneSCLOP(res))
  expect_error(SCLOP(res))
  expect_error(SCLOP.pairwise(res))
  expect_error(SCLOP.pairwise(dend))
})

test_that("print.dendrogram", {
  expect_output(print(dend), "'dendrogram' with 2 branches and")
  expect_output(print(dend2), "'dendrogram' with 2 branches and")
})

test_that("print.PruningSCLOP", {
  expect_output(print(pruned), "PruningSCLOP Object consisting of")
  expect_output(print(pruned2), "PruningSCLOP Object consisting of")
})

test_that("plot.TopicDendrogram_plot.PruningSCLOP", {
  expect_silent(plot(dend))
  expect_silent(plot(dend, pruned))
  expect_silent(plot(pruned, dend))
  expect_silent(plot(dend, pruned, pruning.par = list(type = "color")))
  expect_silent(plot(pruned, dend, pruning.par = list(type = "both")))
  expect_silent(plot(pruned, dend, pruning.par = list(type = "both", lty = 1, lwd = 2, col = "red")))
})
