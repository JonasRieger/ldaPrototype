context("Prototype_calculation")

data("reuters_docs")
data("reuters_vocab")

res = LDARep(docs = reuters_docs, K = 22, vocab = reuters_vocab, n = 3, seeds = 1:3, num.iterations = 5)
lda = getLDA(res)
voc = sample(reuters_vocab, length(reuters_vocab)/2)
mtopics = mergeTopics(res)
mtopics2 = mergeTopics(res, voc)
jacc = jaccardTopics(mtopics)
jacc2 = jaccardTopics(mtopics2)
sclop = SCLOP.pairwise(jacc)
sclop2 = SCLOP.pairwise(jacc2)

proto = LDAPrototype(docs = reuters_docs, K = 22, vocabLDA = reuters_vocab, n = 3, seeds = 1:3, num.iterations = 5)
proto2 = getPrototype(res)
proto3 = getPrototype(lda)
proto3$jobs[, seed := 1:3]
proto4 = getPrototype(res, sclop = sclop)

protoall = getPrototype(res, keepTopics = TRUE, keepSims = TRUE, keepLDAs = TRUE)
protoall2 = LDAPrototype(reuters_docs, reuters_vocab, voc, n = 3, seeds = 1:3, num.iterations = 5, K = 22,
  keepTopics = TRUE, keepSims = TRUE, keepLDAs = TRUE)

protoall.manip = protoall
protoall.manip$lda = getLDA(protoall.manip, reduce = FALSE)
protoall.manip["topics"] = list(NULL)
protoall.manip["sims"] = list(NULL)
protoall.manip["wordslimit"] = list(NULL)
protoall.manip["wordsconsidered"] = list(NULL)

protolda = getPrototype(proto)
protolda2 = getLDA(proto)
protolda3 = getLDA(proto, job = getPrototypeID(proto))


test_that("Prototype_success", {
  expect_identical(proto, proto2)
  expect_identical(proto, proto3)
  expect_identical(proto, proto4)
  expect_identical(proto, protoall.manip)

  expect_identical(protolda, protolda2)
  expect_identical(protolda, protolda3)
  expect_true(is.LDA(protolda))

  expect_identical(getLDA(protoall, job = getJob(res)), getLDA(protoall, all = TRUE))
  expect_identical(getLDA(protoall, all = TRUE), getLDA(res))
  expect_identical(getID(protoall), getID(res))
  expect_identical(getSimilarity(protoall), getSimilarity(jacc))
  expect_identical(getRelevantWords(protoall), getRelevantWords(jacc))
  expect_identical(getConsideredWords(protoall), getConsideredWords(jacc))
  expect_identical(getSCLOP(protoall), SCLOP.pairwise(jacc))
  expect_identical(getMergedTopics(protoall), mtopics)

  expect_identical(getSimilarity(protoall2), getSimilarity(jacc2))
  expect_identical(getRelevantWords(protoall2), getRelevantWords(jacc2))
  expect_identical(getConsideredWords(protoall2), getConsideredWords(jacc2))
  expect_identical(getSCLOP(protoall2), SCLOP.pairwise(jacc2))
  expect_identical(getMergedTopics(protoall2), mtopics2)

  expect_true(is.LDA(getPrototype(LDAPrototype(docs = reuters_docs, vocabLDA = reuters_vocab, n = 5, num.iterations = 5, K = 25))))
})

test_that("Prototype_not_true_error", {
  # parameter vocabLDA and vocabMerge, not vocab
  expect_error(LDAPrototype(LDAPrototype(docs = reuters_docs, vocab = reuters_vocab)))

  expect_error(getPrototype(mtopics))
  expect_error(getPrototype(jacc))
  expect_error(getPrototype(sclop))
  expect_error(LDAPrototype(res))
  expect_error(LDAPrototype(lda))
})

test_that("print.LDAPrototype", {
  expect_output(print(proto), "PrototypeLDA Object with elements")
  expect_output(print(protoall), "PrototypeLDA Object with elements")
  expect_output(print(LDAPrototype(reuters_docs, reuters_vocab, K = 4, n = 10, num.iterations = 5)),
    "PrototypeLDA Object with elements")
})
