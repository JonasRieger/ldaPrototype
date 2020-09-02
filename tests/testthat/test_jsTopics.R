context("jsTopics")

data("reuters_docs")
data("reuters_vocab")

mtopics = mergeTopics(LDARep(docs = reuters_docs, vocab = reuters_vocab, n = 2, K = 10, num.iterations = 5))
js = jsTopics(mtopics)
js2 = jsTopics(mtopics, pm.backend = "socket")

js3 = jsTopics(mtopics, epsilon = 1, progress = FALSE)
js4 = jsTopics(mtopics, epsilon = 1, pm.backend = "socket")

test_that("jsTopics_success", {
  tmp = 20 # n*K
  expect_equal(getParam(js), list(type = "Jensen-Shannon Divergence", epsilon = 1e-6))
  expect_equal(names(getRelevantWords(js)), names(getConsideredWords(js)))
  expect_equal(length(getRelevantWords(js)), tmp)
  expect_equal(length(getConsideredWords(js)), tmp)
  expect_true(all(getRelevantWords(js) == nrow(mtopics)))
  expect_true(all(getConsideredWords(js) == nrow(mtopics)))
  expect_true(all(as.integer(getRelevantWords(js)) == c(getRelevantWords(js))))
  expect_true(all(as.integer(getConsideredWords(js)) == c(getConsideredWords(js))))
  expect_equal(dim(getSimilarity(js)), rep(tmp, 2))
  expect_true(all(is.na(getSimilarity(js)[upper.tri(getSimilarity(js))])))
  expect_true(all(is.na(diag(getSimilarity(js)))))
  expect_true(all(getSimilarity(js)[lower.tri(getSimilarity(js))] >= 0))
  expect_true(all(getSimilarity(js)[lower.tri(getSimilarity(js))] <= 1))
  expect_equal(js, js2)

  expect_equal(getParam(js3), list(type = "Jensen-Shannon Divergence", epsilon = 1))
  expect_equal(names(getRelevantWords(js3)), names(getConsideredWords(js3)))
  expect_equal(length(getRelevantWords(js3)), tmp)
  expect_equal(length(getConsideredWords(js3)), tmp)
  expect_true(all(getRelevantWords(js) == nrow(mtopics)))
  expect_true(all(getConsideredWords(js) == nrow(mtopics)))
  expect_true(all(as.integer(getRelevantWords(js3)) == c(getRelevantWords(js3))))
  expect_true(all(as.integer(getConsideredWords(js3)) == c(getConsideredWords(js3))))
  expect_equal(dim(getSimilarity(js3)), rep(tmp, 2))
  expect_true(all(is.na(getSimilarity(js3)[upper.tri(getSimilarity(js3))])))
  expect_true(all(is.na(diag(getSimilarity(js3)))))
  expect_true(all(getSimilarity(js3)[lower.tri(getSimilarity(js3))] >= 0))
  expect_true(all(getSimilarity(js3)[lower.tri(getSimilarity(js3))] <= 1))
  expect_equal(js3, js4)
})

test_that("jsTopics_errors", {
  expect_error(jsTopics(mtopics, epsilon = -1))
  expect_error(jsTopics(mtopics, ncpus = -1, pm.backend = "socket"))
  expect_error(jsTopics(mtopics, ncpus = 3.2, pm.backend = "socket"))
  expect_error(jsTopics(mtopics, pm.backend = TRUE))
  expect_error(jsTopics(mtopics, pm.backend = ""))
  expect_error(jsTopics(mtopics, progress = "TRUE"))
  colnames(mtopics)[1] = ""
  expect_error(jsTopics(mtopics))
  colnames(mtopics)[1:2] = "LDARep1.1"
  expect_error(jsTopics(mtopics))
  colnames(mtopics)[2] = "LDARep1.2"
  expect_silent(jsTopics(mtopics))
  expect_error(jsTopics(mtopics-1))
  expect_error(jsTopics(as.data.frame(mtopics)))
  mtopics[sample(seq_len(nrow(mtopics)), 1), sample(seq_len(ncol(mtopics)), 1)] = NA
  expect_error(jsTopics(mtopics))
  expect_error(jsTopics(1:100))
  expect_error(jsTopics())
})


test_that("print.TopicSimilarity", {
  expect_output(print(js), "TopicSimilarity Object")
  expect_output(print(js), "type: Jensen-Shannon Divergence")
  expect_output(print(js2), "TopicSimilarity Object")
  expect_output(print(js2), "type: Jensen-Shannon Divergence")
  expect_output(print(js3), "TopicSimilarity Object")
  expect_output(print(js3), "type: Jensen-Shannon Divergence")
})
