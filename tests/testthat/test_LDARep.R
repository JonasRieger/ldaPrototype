context("LDARep object")

data("reuters_docs")
data("reuters_vocab")

res = LDARep(docs = reuters_docs, vocab = reuters_vocab, n = 3,
  K = 10:12, num.iterations = 5)
resrepsocket = LDARep(docs = reuters_docs, vocab = reuters_vocab, seeds = rep(1,3), n = 3,
  K = 10, num.iterations = 5, pm.backend = "socket")

lda = getLDA(resrepsocket, job = 1)
ldawoparam = lda
ldawoparam$param = NULL
class(ldawoparam) = NULL
# missing id, missing names, missing job
res2 = as.LDARep(lda = list(lda))
# missing id, missing names, but job (data.frame)
res3 = as.LDARep(lda = list(lda), job = as.data.frame(c(job.id = 1, getParam(lda))))
# job vector
res4 = as.LDARep(lda = list(lda), job = unlist(getParam(lda)))
# job data.table, construct LDA object
res5 = as.LDARep(lda = list(LDA(ldawoparam, param = getParam(lda))),
  job = as.data.table(c(job.id = 1, getParam(lda))))

test_that("as.LDARep", {
  expect_equal(res, as.LDARep(res))
  expect_true(is.LDA(lda))
  expect_equal(getLDA(resrepsocket, 1), getLDA(resrepsocket, 2))
  expect_equal(getLDA(resrepsocket, 2), getLDA(resrepsocket, 3))
  expect_equal(res2, res3)
  expect_equal(res3, res4)
  expect_equal(res4, res5)
  # not all param vector
  expect_error(as.LDARep(lda = list(lda), job = c(K = 10)))
  # not all param data.table
  expect_error(as.LDARep(lda = list(lda), job = as.data.table(getParam(lda))))
  # ids not fitting data.frame
  expect_error(as.LDARep(lda = list("1" = lda), job = as.data.frame(c(job.id = 2, getParam(lda)))))
  # names lda not integerish (only a warning!!)
  expect_warning(as.LDARep(lda = list("abc" = lda)))
  # no param in lda to interprete and no job given
  expect_error(as.LDARep(lda = list(ldawoparam)))
  # duplicated names
  expect_error(as.LDARep(lda = list("1" = lda, "1" = lda)))
})

test_that("print.LDARep", {
  expect_output(print(res), "LDARep Object")
  expect_output(print(resrepsocket), "LDARep Object")
})

test_that("x", {

})

test_that("x", {

})

test_that("x", {

})
