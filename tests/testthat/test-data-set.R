context("DataSet")

obsDataFile <- getTestDataFilePath("obs_data.pkml")
obsData <- loadDataRepositoryFromPKML(obsDataFile)

test_that("it can set the name of the data set", {
  dataSet <- DataSet$new(obsData)
  dataSet$name <- "TOTO"
  expect_equal(dataSet$name, "TOTO")
})

test_that("it can create a new data set from scratch", {
  dataSet <- DataSet$new()
  expect_false(is.null(dataSet))
  expect_identical(dataSet$xValues, numeric(0))
})


test_that("it can create a new data set from an existing repository", {
  dataSet <- DataSet$new(obsData)
  expect_false(is.null(dataSet))
})

test_that("it can return and set the xValues", {
  dataSet <- DataSet$new()
  dataSet$xValues <- c(1, 2, 3, 4, 5)
  expect_equal(dataSet$xValues, c(1, 2, 3, 4, 5))
})

test_that("it can return and set the yValues", {
  dataSet <- DataSet$new()
  dataSet$xValues <- c(10, 20, 40)
  dataSet$yValues <- c(1, 2, 4)
  expect_equal(dataSet$yValues, c(1, 2, 4))
})

test_that("it can update the unit of the xValues and this does not change the returned value", {
  dataSet <- DataSet$new()
  #this will be in h
  dataSet$xValues <- c(1, 2, 3, 4, 5)
  dataSet$xUnit <- ospUnits$Time$min
  expect_equal(dataSet$xValues, c(1, 2, 3, 4, 5))
})

test_that("it can update the dimension of the xValues and this does not change the returned value", {
  dataSet <- DataSet$new()
  #this will be Time in h
  dataSet$xValues <- c(1, 2, 3, 4, 5)
  dataSet$xDimension <- ospDimensions$Ampere
  expect_equal(dataSet$xValues, c(1, 2, 3, 4, 5))
  expect_equal(dataSet$xUnit, ospUnits$Ampere$A)
})

test_that("it can update the dimension of the yValues and this does not change the returned value", {
  dataSet <- DataSet$new()
  #this will be Time in min
  dataSet$xValues <- c(10, 20, 40)
  dataSet$yValues <- c(1, 2, 4)
  dataSet$yDimension <- ospDimensions$Ampere
  expect_equal(dataSet$yValues, c(1, 2, 4))
  expect_equal(dataSet$yUnit, ospUnits$Ampere$A)
})
