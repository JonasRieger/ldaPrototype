context("mergeTopics")

data("reuters_docs")
data("reuters_vocab")

res = LDARep(docs = reuters_docs, vocab = reuters_vocab, n = 3, K = 10, num.iterations = 5)
lda = getLDA(res)
id = getID(res)
nores = res
nores$id = 1

mtopics = mergeTopics(res)
mtopics2 = mergeTopics(res, vocab = reuters_vocab)
mtopics3 = mergeRepTopics(lda = lda, vocab = reuters_vocab, id = id)
mtopics4 = mergeRepTopics(lda)

voc = sample(reuters_vocab, length(reuters_vocab)/2)
mtopics5 = mergeRepTopics(lda, vocab = voc)
mtopics6 = mergeTopics(res, vocab = voc)
mtopics7 = mergeTopics(res, vocab = voc, progress = FALSE)


test_that("mergeTopics_success", {
  expect_true(all(mtopics >= 0))
  expect_true(all(as.integer(mtopics) == c(mtopics)))
  expect_equal(ncol(mtopics), sum(sapply(lda, getK)))
  expect_equal(nrow(mtopics), length(reuters_vocab))
  expect_equal(mtopics, mtopics2)
  expect_equal(mtopics2, mtopics3)
  expect_equal(mtopics3, mtopics4)
  expect_equal(ncol(mtopics5), sum(sapply(lda, getK)))
  expect_equal(nrow(mtopics5), length(voc))
  expect_equal(mtopics5, mtopics6)
  expect_equal(mtopics6, mtopics7)
})

test_that("mergeTopics_errors", {
  expect_error(mergeTopics(nores))
  expect_warning(mergeRepTopics(lda, id = 1:15))
  expect_silent(mergeTopics(res, vocab = 1:10))
})
