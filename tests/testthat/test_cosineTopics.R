context("cosineTopics")

data("reuters_docs")
data("reuters_vocab")

mtopics = mergeTopics(LDARep(docs = reuters_docs, vocab = reuters_vocab, n = 2, K = 10, num.iterations = 5))
cosine = cosineTopics(mtopics)
cosine2 = cosineTopics(mtopics, pm.backend = "socket")
cosine3 = cosineTopics(mtopics, progress = FALSE)

test_that("cosineTopics_success", {
  tmp = 20 # n*K
  expect_equal(getParam(cosine), list(type = "Cosine Similarity"))
  expect_equal(names(getRelevantWords(cosine)), names(getConsideredWords(cosine)))
  expect_equal(length(getRelevantWords(cosine)), tmp)
  expect_equal(length(getConsideredWords(cosine)), tmp)
  expect_true(all(getRelevantWords(cosine) == nrow(mtopics)))
  expect_true(all(getConsideredWords(cosine) == nrow(mtopics)))
  expect_true(all(as.integer(getRelevantWords(cosine)) == c(getRelevantWords(cosine))))
  expect_true(all(as.integer(getConsideredWords(cosine)) == c(getConsideredWords(cosine))))
  expect_equal(dim(getSimilarity(cosine)), rep(tmp, 2))
  expect_true(all(is.na(getSimilarity(cosine)[upper.tri(getSimilarity(cosine))])))
  expect_true(all(is.na(diag(getSimilarity(cosine)))))
  expect_true(all(getSimilarity(cosine)[lower.tri(getSimilarity(cosine))] >= 0))
  expect_true(all(getSimilarity(cosine)[lower.tri(getSimilarity(cosine))] <= 1))
  expect_equal(cosine, cosine2)
  expect_equal(cosine, cosine3)
})

test_that("cosineTopics_errors", {
  expect_error(cosineTopics(mtopics, ncpus = -1, pm.backend = "socket"))
  expect_error(cosineTopics(mtopics, ncpus = 3.2, pm.backend = "socket"))
  expect_error(cosineTopics(mtopics, pm.backend = TRUE))
  expect_error(cosineTopics(mtopics, pm.backend = ""))
  expect_error(cosineTopics(mtopics, progress = "TRUE"))
  colnames(mtopics)[1] = ""
  expect_error(cosineTopics(mtopics))
  colnames(mtopics)[1:2] = "LDARep1.1"
  expect_error(cosineTopics(mtopics))
  colnames(mtopics)[2] = "LDARep1.2"
  expect_silent(cosineTopics(mtopics))
  expect_error(cosineTopics(mtopics-1))
  expect_error(cosineTopics(as.data.frame(mtopics)))
  mtopics[sample(seq_len(nrow(mtopics)), 1), sample(seq_len(ncol(mtopics)), 1)] = NA
  expect_error(cosineTopics(mtopics))
  expect_error(cosineTopics(1:100))
  expect_error(cosineTopics())
})

test_that("print.TopicSimilarity", {
  expect_output(print(cosine), "TopicSimilarity Object")
  expect_output(print(cosine), "type: Cosine Similarity")
  expect_output(print(cosine2), "TopicSimilarity Object")
  expect_output(print(cosine2), "type: Cosine Similarity")
  expect_output(print(cosine3), "TopicSimilarity Object")
  expect_output(print(cosine3), "type: Cosine Similarity")
})
