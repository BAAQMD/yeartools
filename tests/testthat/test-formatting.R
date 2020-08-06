context("formatting")

test_that("pillar formatting works", {

  CY_data <-
    tibble::tibble(
      year = CY(2010:2015))

  verify_output(
    "test-formatting-expected.txt",
    print(CY_data))

})
