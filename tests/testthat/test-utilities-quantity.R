
context("getAllQuantitiesMatching")

sim <- loadTestSimulation("S1")

test_that("It can retrieve quantities with absolute path", {
  quantities <- getAllQuantitiesMatching(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim)
  expect_equal(length(quantities), 1)
})

test_that("It can retrieve quantities with generic path path", {
  quantities <- getAllQuantitiesMatching(toPathString(c("Organism", "Liv*", "Intracellu*", "Vol*")), sim)
  expect_equal(length(quantities), 1)
})

context("getQuantity")

test_that("It can retrieve a single quantity by path if it exists", {
  quantity <- getQuantity(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim)
  expect_equal(quantity$name, "Volume")
})

test_that("It returns null if the quantity by path does not exist", {
  quantity <- getQuantity(toPathString(c("Organism", "Liver", "Intracellular", "Length")), sim)
  expect_null(quantity)
})

test_that("It throws an error when trying to retrieve a quantity by path that would result in multiple quantities", {
  expect_that(getQuantity(toPathString(c("Organism", "Liver", "*")), sim), throws_error())
})