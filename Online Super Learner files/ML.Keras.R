#' ML.Keras
#'
#'  Doc here
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom keras to_categorical normalize  keras_model_sequential
#' @section Methods: 
#' \describe{  
#'   \item{\code{initialize() }}{ 
#'     
#'   } 
#' }  
#' @export
ML.Keras <- R6Class("ML.Keras",
inherit = ML.Base,
public =
  list(
    fitfunname='keras-net',
    lmclass='keras-net-lm',
    initialize = function(layers = NULL, epoch = 200, validation_split = 0.2, batch_size = 32) {
      if (is.null(layers)){
        layers <- list(
          list(units = 8, activation = 'relu'),
          list(units = 4, activation = 'relu'),
          list(units = 2, activation = 'softmax')
        )
        
      }
      private$optimizer <- optimizer_rmsprop()
      private$metrics <- c('accuracy')
      private$loss <- "binary_crossentropy"
      private$layers <- layers
      private$epoch <- epoch
      private$validation_split <- validation_split
      private$batch_size <- batch_size
    }
  ),
active =
  list(
  ),
private =
  list(
    layers = NULL,
    epoch = NULL,
    validation_split = NULL,
    batch_size = NULL,
    loss = NULL,
    optimizer = NULL,
    metrics = NULL,
    do.fit = function(X_mat, Y_vals, coef = NULL) {
      input_shape <- ncol(X_mat)
      train_data <- as.matrix(X_mat)
      train_labels <- to_categorical(Y_vals)
      model <- keras_model_sequential()
      first <- TRUE
      for (layer in private$layers) {
        if (first) {
          model %>% layer_dense(units = layer$units, activation = layer$activation, input_shape = input_shape)
          first <- FALSE
        } else {
          model %>% layer_dense(units = layer$units, activation = layer$activation)
        }
      }
      
      model %>% compile(
        loss = private$loss,
        optimizer = private$optimizer,
        metrics = private$metrics
      )
      browser()
      model %>%
        fit(train_data,
            train_labels,
            epoch = private$epoch,
            batch_size = private$batch_size,
            validation_split = private$validation_split)
      
      return(model)
    },
    
    do.update = function(X_mat, Y_vals, m.fit, ...) {
      labels <- to_categorical(Y_vals)
      m.fit %>% fit(
        X_mat, labels, 
        epochs = private$epochs, 
        batch_size = private$batch_size, 
        validation_split = private$validation_split
      )
      return(m.fit)
    },
    
    do.predict = function(X_mat, m.fit) {
      m.fit %>% predict_proba(X_mat)
    }
  )
)

