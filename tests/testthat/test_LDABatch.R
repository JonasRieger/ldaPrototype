context("LDABatch")

data("reuters_docs")
data("reuters_vocab")

resrep = LDARep(docs = reuters_docs, vocab = reuters_vocab, n = 3,
  K = 10:12, num.iterations = 5, seeds = 1:3)
resbatch = LDABatch(docs = reuters_docs, vocab = reuters_vocab, n = 3,
  K = 10:12, num.iterations = 5, seeds = 1:3)
resbatch2 = LDABatch(docs = reuters_docs, vocab = reuters_vocab, n = 3,
  K = 10:12, num.iterations = 5, seeds = rep(4,3), load = TRUE, chunk.size = 3, resources = list(memory = 1024))

test_that("LDABatch_registry", {
  # no load = TRUE
  expect_error(LDABatch(docs = reuters_docs, vocab = reuters_vocab, n = 3,
    K = 10:12, num.iterations = 5, seeds = 1:3))
  #expect_warning(is.LDABatch(resbatch))
  #expect_error(is.LDABatch(resbatch))
  expect_true(suppressWarnings(is.LDABatch(resbatch)))
  expect_silent(is.LDABatch(resbatch2))
  expect_true(is.LDABatch(resbatch2))
})

lda1 = getLDA(resbatch)
lda2 = getLDA(resbatch2)

test_that("LDABatch_registry2", {
  # getLDA refreshes the registry
  expect_silent(is.LDABatch(resbatch))
  expect_true(is.LDABatch(resbatch))
  expect_silent(is.LDABatch(resbatch2))
  expect_true(is.LDABatch(resbatch2))
})

resbatchrep = as.LDARep(resbatch)
resbatchbatch = as.LDABatch(job = getJob(resbatch))

proto = getPrototype(resrep)
proto2 = getPrototype(resbatchrep, keepLDAs = TRUE, keepSims = TRUE, keepTopics = TRUE)
proto3 = getPrototype(resbatchbatch, keepLDAs = TRUE, keepSims = TRUE, keepTopics = TRUE)

proto2.manip = proto2
proto2.manip$lda = getLDA(proto2.manip, reduce = FALSE)
proto2.manip["topics"] = list(NULL)
proto2.manip["sims"] = list(NULL)
proto2.manip["wordslimit"] = list(NULL)
proto2.manip["wordsconsidered"] = list(NULL)
proto2.manip$id = proto$id
colnames(proto2.manip$sclop) = rownames(proto2.manip$sclop) = colnames(proto$sclop)

test_that("as.LDABatch", {
  expect_true(is.LDARep(resbatchrep))
  expect_equal(proto, proto2.manip)
  expect_identical(proto2, proto3)
})

resbatchbatch2 = as.LDABatch(job = getJob(resbatch2)$job.id)
proto4 = getPrototype(resbatch2)
proto5 = getPrototype(resbatchbatch2)


test_that("as.LDABatch", {
  expect_identical(proto4, proto5)
})

resbatchcomplete = as.LDABatch()

test_that("setFileDir",{
  # registries: call by reference, not call by value
  tmp = resbatchcomplete
  tmp2 = as.LDABatch()
  expect_silent(setFileDir(resbatchcomplete, "LDABatch"))
  expect_equal(getRegistry(resbatchcomplete)$file.dir, "LDABatch")
  expect_identical(getRegistry(resbatchcomplete)$file.dir, getRegistry(tmp)$file.dir)
  expect_false(getRegistry(resbatchcomplete)$file.dir == getRegistry(tmp2)$file.dir)
})

test_that("is.LDABatch", {
  expect_true(is.LDABatch(resbatch))
  expect_true(is.LDABatch(resbatchbatch, verbose = TRUE))
  expect_true(is.LDABatch(resbatch2, verbose = TRUE))
  expect_true(is.LDABatch(resbatchbatch2))
  expect_true(is.LDABatch(resbatchcomplete))
  expect_false(is.LDABatch(resbatchrep))

  expect_true(is.LDARep(resbatchrep))
  expect_false(is.LDARep(resbatch))
  expect_false(is.LDARep(resbatchbatch, verbose = TRUE))
  expect_false(is.LDARep(resbatch2, verbose = TRUE))
  expect_false(is.LDARep(resbatchbatch2))
  expect_false(is.LDARep(resbatchcomplete))

  res = resbatch

  # id
  nores = res
  nores$id = c("id1", "id2")
  expect_false(is.LDABatch(nores, verbose = TRUE))
  nores$id = 1
  expect_false(is.LDABatch(nores, verbose = TRUE))

  # jobs
  nores = res
  nores$jobs = getJob(res)[-1,]
  expect_true(is.LDABatch(nores)) # although the names arent matching
  nores = res
  nores$jobs$job.id = as.character(getJob(res)$job.id)
  expect_false(is.LDABatch(nores, verbose = TRUE))
  nores$jobs = rbind(getJob(res), getJob(res))
  expect_false(is.LDABatch(nores, verbose = TRUE))
  nores$jobs = as.data.frame(getJob(res))
  expect_false(is.LDABatch(nores, verbose = TRUE))

  # general
  nores = res
  nores$id = NULL
  expect_false(is.LDABatch(nores, verbose = TRUE))
  nores = 1
  class(nores) = "LDABatch"
  expect_false(is.LDABatch(nores, verbose = TRUE))
  nores = res
  class(nores) = "abc"
  expect_false(is.LDABatch(nores, verbose = TRUE))

  # registry
  nores = res
  setFileDir(nores, "a")
  #expect_true(is.LDABatch(nores, verbose = TRUE))
  expect_false(is.LDABatch(nores, verbose = TRUE))
  expect_error(getLDA(nores))

  # repeated num.iterations
  expect_warning(res0 <- LDABatch(docs = reuters_docs,
    vocab = reuters_vocab, n = 1, num.iterations = 10, num.iterations = 5, id = "tmp"))
  expect_true(is.LDABatch(res0))
  expect_equal(getNum.iterations(getLDA(res0)), 5)
  expect_equal(getK(getLDA(res0)), 100)
  expect_output(print(res0), "LDABatch Object")
})

test_that("print.LDABatch", {
  expect_output(print(resbatch), "LDABatch Object")
  expect_output(print(resbatch2), "LDABatch Object")
  expect_output(print(resbatchbatch), "LDABatch Object")
  expect_output(print(resbatchbatch2), "LDABatch Object")
  expect_output(print(resbatchcomplete), "LDABatch Object")
})
