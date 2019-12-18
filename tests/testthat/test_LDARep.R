context("LDARep object")

data("reuters_docs")
data("reuters_vocab")

args = list(K = 10, num.iterations = 20, alpha = 0.1, eta = 0.1,
  compute.log.likelihood = TRUE,
  documents = reuters_docs, vocab = reuters_vocab)
param = args[1:5]

oldseed = .Random.seed
set.seed(1895)
result = do.call(lda::lda.collapsed.gibbs.sampler, args)
.Random.seed <<- oldseed
result2 = getLDA(LDARep(docs = reuters_docs, vocab = reuters_vocab, n = 1,
  seeds = 1895, K = 10, num.iterations = 20, alpha = 0.1, eta = 0.1,
  compute.log.likelihood = TRUE))

lda = LDA(x = result, param = param)
lda2 = as.LDA(
  param = getParam(lda),
  assignments = getAssignments(lda),
  topics = getTopics(lda),
  document_sums = getDocument_sums(lda),
  document_expects = getDocument_expects(lda),
  log.likelihoods = getLog.likelihoods(lda))
lda3 = LDA(result2)
lda4 = LDA(lda, topics = NULL)
lda3.manip = lda3
lda3.manip$topics = NULL
lda3.manip = as.LDA(lda3.manip)
lda5 = LDA(topics = getTopics(lda))
lda6 = LDA()

est = getEstimators(lda)

test_that("LDA_as.LDA", {
  expect_identical(names(lda), names(lda6))
  expect_identical(names(lda), names(lda4))
  expect_identical(names(lda), names(lda3.manip))
  expect_identical(names(lda), names(lda5))

  expect_equal(lda, lda2)
  expect_equal(lda, lda3)
  expect_equal(lda4, lda3.manip)
})

test_that("is.LDA", {
  expect_true(is.LDA(lda))
  expect_true(is.LDA(lda2))
  expect_true(is.LDA(lda3))
  expect_true(is.LDA(lda3.manip))
  expect_true(is.LDA(lda4))
  expect_true(is.LDA(lda5))
  expect_true(is.LDA(lda6))
})

test_that("print.LDA", {
  expect_output(print(lda), "LDA Object")
  expect_output(print(lda2), "LDA Object")
  expect_output(print(lda3), "LDA Object")
  expect_output(print(lda3.manip), "LDA Object")
  expect_output(print(lda4), "LDA Object")
  expect_output(print(lda5), "LDA Object")
  expect_output(print(lda6), "LDA Object")
})

test_that("LDA_getter", {
  expect_equal(result$topics, getTopics(lda))
  expect_equal(result$assignments, getAssignments(lda))
  expect_equal(result$document_sums, getDocument_sums(lda))
  expect_equal(result$document_expects, getDocument_expects(lda))
  expect_equal(result$log.likelihoods, getLog.likelihoods(lda))

  expect_equal(getParam(lda), param)
  expect_equal(getK(lda), param$K)
  expect_equal(getEta(lda), param$eta)
  expect_equal(getAlpha(lda), param$alpha)
  expect_equal(getNum.iterations(lda), param$num.iterations)

  expect_named(est, c("phi", "theta"))
  expect_true(is.matrix(est$phi) && is.matrix(est$theta))
  expect_true(all(est$phi > 0) && all(est$phi < 1) &&
      all(est$theta > 0) && all(est$theta < 1))
  expect_equal(dim(est$phi), c(getK(lda), length(reuters_vocab)))
  expect_equal(dim(est$theta), c(getK(lda), length(reuters_docs)))
  expect_equal(rowSums(est$phi), rep(1, getK(lda)))
  expect_equal(colSums(est$theta), rep(1, length(reuters_docs)))
})
