out <- link_section("R6 Employment Status", label = "Check HMIS Data Standards Manual")

test_that("link_section generates a shiny 'a' tag", {

  expect_s3_class(out, "shiny.tag")

  expect_equal(out$name, "a")

})

test_that("link_section generates proper href", {

  expected <- 'https://files.hudexchange.info/resources/documents/HMIS-Data-Standards-Manual-2024.pdf#R6%20Employment%20Status'
  actual <- out$attribs$href

  expect_equal(expected, actual)

})

test_that("link_section generates proper target", {

  expect_equal("_blank", out$attribs$target)

})

test_that("link_section generates proper label", {

  expect_equal("Check HMIS Data Standards Manual", out$children[[1]])

})
