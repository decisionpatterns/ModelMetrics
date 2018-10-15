#' @title Mean absolute percent error
#' @description Calculates the mean absolute percent error
#'
#' @param actual A vector actual responses
#' @param predicted A vector of predicted values
#' @param \dots additional parameters to be passed the the s3 methods
#' @param modelObject the model object. Currently supported \code{glm, randomForest, glmerMod, gbm}
#'
#' @examples
#'
#'  actual <- 1:3
#'  predicted <- actual / 2
#'
#'  mape(actual,predicted)
#'
#' @export

mape <- function(...){
  UseMethod("mape")
}

#' @rdname mape
#' @export
mape.default <- function(actual, predicted, ...){
  mape_(actual, predicted)
}


#' @rdname mape
#' @export
mape.glm <- function(modelObject, ...){

  family <- family(modelObject)[[1]]
  if(any(family %in% c('binomial', 'poisson'))){
    actual <- modelObject$y
    predicted <- modelObject$fitted.values
  } else {
    stop(paste0("family: ", family, " is not currently supported"))
  }

  mape.default(actual, predicted)
}

#' @rdname mape
#' @export
mape.randomForest <- function(modelObject, ...){

  actual <- as.numeric(modelObject$y) - 1
  predicted <- predict(modelObject, type = 'prob')[,2]

  mape.default(actual, predicted)
}

#' @rdname mape
#' @export
mape.glmerMod <- function(modelObject, ...){

  actual <- modelObject@resp$y
  predicted <- modelObject@resp$mu

  mape.default(actual, predicted)
}

#' @rdname mape
#' @export
mape.gbm <- function(modelObject, ...){

  actual <- modelObject$data$y
  predicted <- modelObject$fit

  mape.default(actual, predicted)
}

#' @rdname mape
#' @export
mape.rpart <- function(modelObject, ...){

  actual <- modelObject$y
  predicted <- predict(modelObject)

  mape.default(actual, predicted)
}

