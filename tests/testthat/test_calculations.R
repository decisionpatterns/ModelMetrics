
context("Calculation Tests")
data(testDF)
glmModel <- glm(y ~ ., data=testDF, family="binomial")
Preds <- predict(glmModel, type = 'response')


test_that("logLoss returns correct values", {

  expect_equal(logLoss(testDF$y, Preds), 0.1546854, tolerance = .000001)
  expect_equal(logLoss(testDF$y, Preds, 'poisson'), 0.6910357, tolerance = .000001)
  expect_equal(logLoss(glmModel), 0.1546854, tolerance = .000001)

})


test_that("auc returns correct values", {

  expect_equal(auc(testDF$y, Preds), 0.9872666, tolerance = .000001)
  expect_equal(auc(c(testDF$y,testDF$y), c(Preds, Preds)), 0.9872666, tolerance = .000001)
  expect_equal(auc(glmModel), 0.9872666, tolerance = .000001)

})

test_that("gini returns correct values", {

  expect_equal(gini(testDF$y, Preds), 0.9745332, tolerance = .000001)
  expect_equal(gini(c(testDF$y,testDF$y), c(Preds, Preds)), 0.9745332, tolerance = .000001)
  expect_equal(gini(glmModel), 0.9745332, tolerance = .000001)

})


test_that("rmse returns correct values", {

  expect_equal(rmse(testDF$y, Preds), 0.2188343, tolerance = .000001)
  expect_equal(rmse(glmModel), 0.2188343, tolerance = .000001)

})


test_that("mse returns correct values", {

  expect_equal(mse(testDF$y, Preds), 0.04788846, tolerance = .000001)
  expect_equal(mse(glmModel), 0.04788846, tolerance = .000001)

})


test_that("ppv returns correct values", {

  expect_equal(ppv(testDF$y, Preds, .5), 0.9365079, tolerance = .000001)
  expect_equal(precision(testDF$y, Preds, .5), 0.9365079, tolerance = .000001)

})


test_that("npv returns correct values", {

  expect_equal(npv(testDF$y, Preds, .5), 0.9189189, tolerance = .000001)

})


test_that("specificity returns correct values", {

  tempTab <- table(testDF$y, Preds > .5)
  SPC <- tempTab[1,1]/sum(tempTab[1,])

  expect_equal(specificity(testDF$y, Preds, .5), SPC, tolerance = .000001)
  expect_equal(tnr(testDF$y, Preds, .5), SPC, tolerance = .000001)

})


test_that("sensitivity returns correct values", {

  expect_equal(recall(testDF$y, Preds, .5), 0.9516129, tolerance = .000001)
  expect_equal(sensitivity(testDF$y, Preds, .5), 0.9516129, tolerance = .000001)
  expect_equal(tpr(testDF$y, Preds, .5), 0.9516129, tolerance = .000001)

})

test_that("f1 score returns correct values", {

  expect_equal(f1Score(testDF$y, Preds, .5), 0.944, tolerance = .000001)

})

test_that("f1 score and F score agree with beta 1 (default value)", {

  expect_equal(f1Score(testDF$y, Preds, .5), fScore(testDF$y, Preds, .5, 1), tolerance = .000001)

})

test_that("mcc returns correct values", {

  expect_equal(mcc(testDF$y, Preds, .5), 0.8508762, tolerance = .000001)

})


test_that("brier returns correct values", {

  expect_equal(brier(testDF$y, Preds), 0.04788846, tolerance = .000001)
  expect_equal(brier(glmModel), 0.04788846, tolerance = .000001)

})



test_that("mae returns correct values", {

  expect_equal(mae(testDF$y, Preds), 0.09440662, tolerance = .000001)
  expect_equal(mae(glmModel), 0.09440662, tolerance = .000001)

})

test_that("mape returns correct values", {

  expect_equal(mape(testDF$y, Preds), 0.09440662, tolerance = .000001)
  expect_equal(mape(glmModel), 0.09440662, tolerance = .000001)

})

test_that("msle returns correct values", {

  expect_equal(msle(testDF$y, Preds), 0.02318011, tolerance = .000001)
  expect_equal(msle(glmModel), 0.02318011, tolerance = .000001)

})


test_that("rmsle returns correct values", {

  expect_equal(rmsle(testDF$y, Preds), 0.1522501, tolerance = .000001)
  expect_equal(rmsle(glmModel), 0.1522501, tolerance = .000001)


})


test_that("rmsle returns correct values", {

  A <- c(rep(1, 63), rep(0, 31))
  B <- c(rep(1, 61), rep(0, 25), rep(1, 6), rep(0, 2))

  tab <- table(A, B)

  a = tab[2,2]
  b = tab[2,1]
  c = tab[1,2]
  d = tab[1,1]

  marginA = ((a + b)*(a + c))/(a + b + c + d)
  marginB = ((c + d)*(b + d))/(a + b + c + d)

  Pe = (marginA + marginB)/(a + b + c + d)
  Po = (a + d)/(a + b + c + d)

  manualKappa = (Po - Pe)/(1 - Pe)

  expect_equal(kappa(A, B), manualKappa, tolerance = .000001)


})


