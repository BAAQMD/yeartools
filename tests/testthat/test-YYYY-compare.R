test_that("equality works", {

  expect_true(CY(2011) == CY(2011))
  expect_true(CY(2011) == "CY2011")
  expect_true("CY2011" == CY(2011))

  expect_equal(
    CY(2011:2015) == paste0("CY", 2011:2015),
    c(TRUE, TRUE, TRUE, TRUE, TRUE))

  expect_false(CY(2011) == 2011)
  expect_false(CY(2011) == "CY2012")

  expect_true(CY(2011) != CY(2012))

})

test_that("different timelines are not comparable", {

  expect_false(RY(2011) == PY(2011))

})

test_that("ordering works", {

  expect_true(CY(2011) < CY(2012))
  expect_true(CY(2011) < "CY2012")
  expect_true("CY2011" < CY(2012))

  expect_false(CY(2011) < CY(2011))
  expect_false(CY(2011) < "CY2011")
  expect_false("CY2011" < CY(2011))

  expect_true(CY(2011) <= CY(2011))
  expect_true(CY(2011) <= "CY2011")
  expect_true("CY2011" <= CY(2011))

  expect_true(CY(2011) >= CY(2011))
  expect_true(CY(2011) >= "CY2011")
  expect_true("CY2011" >= CY(2011))

  expect_true(CY(2012) >= CY(2011))
  expect_true(CY(2012) >= "CY2011")
  expect_true("CY2012" >= CY(2011))

})