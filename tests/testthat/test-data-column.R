context("DataColumn")

baseGrid <- DataColumn$new(rClr::clrNew("OSPSuite.Core.Domain.Data.BaseGrid", "baseGrid", getDimensionByName(ospDimensions$Time)))

# Passing concentration (mass) for dimension for now
column <- DataColumn$new(rClr::clrNew("OSPSuite.Core.Domain.Data.DataColumn", "column", getDimensionByName(ospDimensions$`Concentration (mass)`), baseGrid$ref))

test_that("it can set and retrieve the dimension of a data column", {
  expect_equal(baseGrid$dimension, ospDimensions$Time)
  baseGrid$dimension <- ospDimensions$Amount
  expect_equal(baseGrid$dimension, ospDimensions$Amount)
})

test_that("it can set and retrieve the display unit of a data column", {
  baseGrid$dimension <- ospDimensions$Time
  expect_equal(baseGrid$displayUnit, ospUnits$Time$h)
  baseGrid$displayUnit <- ospUnits$Time$`day(s)`
  expect_equal(baseGrid$displayUnit,ospUnits$Time$`day(s)`)
})

test_that("it can set and retrieve the values of the column", {
  baseGrid$values <- c(1, 2, 3)
  expect_equal(baseGrid$values, c(1, 2, 3))
})

test_that("it throws an error when setting a unit thast does not exist in the dimension", {
  baseGrid$dimension <- ospDimensions$Time
  expect_error(baseGrid$displayUnit <- ospUnits$Amount$mmol)
})
