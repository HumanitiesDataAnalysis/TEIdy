context("xml")

library(TEIdytext)
library(testthat)
library(dplyr)
library(purrr)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("simplest XML", {
  z = xml2::read_xml("<w>foo</w>") %>% TEIdytext:::traverseXML()
  do.call(data_frame, as.list(z))[1:z$.length,]
  ls(z) %>%
  expect_length(2)
})




test_that("nested XML", {
  z = xml2::read_xml("<w>foo<c>foo bar</c></w>") %>% TEIdytext:::traverseXML() %>% TEIdytext:::bundle_env_to_frame() %>%
    nrow %>% expect_equal(2)
  do.call(data_frame, as.list(z))[1:z$.length,]

})

test_that("Federalist", {
  path = system.file("extdata", "federalist.papers.xml", package="TEIdytext")
  library(TEIdytext)
  TEIdy(path)
  p

})

test_that("empty produce metadata", {
  xml2::read_xml("<pb id='foo'/>") %>% TEIdytext:::traverseXML() %>%
    TEIdytext:::bundle_env_to_frame() %>% nrow %>% expect_equal(1)
})

xml2::read_xml("<div><a>a</a><b>b</b><c>c</c></div>") %>% TEIdytext:::traverseXML() %>%
  TEIdytext:::bundle_env_to_frame()

test_that("empty tags fill out a row", {
  data = xml2::read_xml("<div><pb id='foo' /><span>foo</span><pb id='tef' /></div>") %>% TEIdytext:::traverseXML() %>%
    TEIdytext:::bundle_env_to_frame()
  data %>% nrow %>% expect_equal(3)
  data %>% ncol %>% expect_equal(6)
})

test_that("frus", {
  z = system.file("extdata", "frus1946v06.xml", package="TEIdytext") %>%
     xml2::read_xml() %>% TEIdytext:::traverseXML() %>%
      TEIdytext:::bundle_env_to_frame()
  z %>% select(basetext) %>% slice(110:139)

})

test_that("nested XML", {
  z = xml2::read_xml("<w>foo<c>foo <pb id=\"13\"></pb>bar</c><c>foo</c></w>") %>% TEIdytext:::traverseXML() %>%
    TEIdytext:::bundle_env_to_frame()
  z
  do.call(data_frame, as.list(z))[1:z$.length,]

  ls(z) %>%
    expect_length(3)
})

test_that("macbeth act 1 loads", {
  z = system.file("extdata", "Mac.xml", package="TEIdytext") %>%
    xml2::read_xml() %>%
    xml2::xml_contents() %>%
    purrr::pluck(2) %>%
    xml2::xml_contents() %>%
    purrr::pluck(2) %>%
    xml2::xml_contents() %>%
    purrr::pluck(6)

  f = z %>% TEIdytext:::traverseXML() %>% TEIdytext:::bundle_env_to_frame()

  f
})

test_that("Macbeth", {
  z = system.file("extdata", "Mac.xml", package="TEIdytext") %>%
    xml2::read_xml() %>%
    xml2::xml_contents() %>%
    purrr::pluck(2) %>%
    TEIdytext:::traverseXML() %>% TEIdytext:::bundle_env_to_frame()

})

test_that("kennan", {
  z = system.file("extdata", "kennan.xml", package="TEIdytext") %>%
  xml2::read_xml()

  f = z %>% TEIdytext:::traverseXML() %>% TEIdytext:::bundle_env_to_frame()
  f %>% View
})


listset = list(list("a" = c(1,2,3), "b" = c(4,5,6)), list("c"=c(1:5), "b" = 5:9) )
left = listset[[1]] %>% TEIdytext:::as_env()
right = listset[[2]] %>% TEIdytext:::as_env()

TEIdytext:::combine_env(left, right)
left$.length
left$b
combine_env(left, right)
left$c
?ls

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

