context("xml")

library(TEIdytext)
library(testthat)
library(dplyr)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("simplest XML", {
  xml2::read_xml("<w>foo</w>") %>% TEIdytext:::traverseXML %>%
  expect_length(2)
})


test_that("nested XML", {
  xml2::read_xml("<w>foo<c>foo bar</c></w>") %>% traverseXML %>%
    expect_length(3)
})




test_that("kennan", {
  system.file("extdata", "kennan.xml", package="TEIdytext") %>%
  xml2::read_xml() %>% traverseXML
})

kennan = TEIdy(system.file("extdata", "kennan.xml", package="TEIdytext"))

not_all_na <- function(x) {!all(is.na(x))}

kennan

z = env()

combine = function(left, right, fill = "repeat") {
  ll = envir$.length

  lr = length(right)[[1]]

}

combine_data = function(listset) {
  out = listset[[1]]
  l = length(out)


  lengths = listset %>% map(pluck, 1)  %>% map_int(length)
  keys = listset %>% map(names) %>% flatten %>% unique
  keys %>% map(function(k) {
    listset %>% map(pluck, k) %>% map2(lengths, ~ ifelse(!is.null(.x), list(.x), list(rep(NA, times=.y)))) %>% unlist
  }) %>%
    set_names(keys) %>%
    return
}

listset = list(list("a" = c(1,2,3), "b" = c(4,5,6)), list("c"=c(1:5), "b" = 5:9) )
listset %>% combine_data()
