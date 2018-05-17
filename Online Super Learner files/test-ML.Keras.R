context("ML.Keras.R")
described.class <- ML.Keras

context(" initialize")
#==========================================================
test_that("it should initialize", {
  expect_error(described.class$new(), NA)
})

context(" do.fit")
#==========================================================
test_that("it should fit a basic model", {
  data <- data.table(
    x=c(1,2,3,4,5,6,7,8,9,10)
  )
  y <- c(0,0,0,0,0,0,1,0,1,1)
  subject <- described.class$new()
  subject$perform_fit(X_mat = data, Y_vals = y)
})