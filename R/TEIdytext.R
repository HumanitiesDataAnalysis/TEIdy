#' TEIdytext: Work with XML-tagged documents in the tidytext framework.
#'
#' @section Parsing
#'
#' The workhorse function is TEIdy. It takes the
#' location of an xml file, and returns a tibble
#'
#' @section Shortcomings
#'
#' This is not a fully-thought representation of all TEI or XML tags.
#' Instead, it tries to offer a rough approximation of the DOM tree.
#'
#' It is very slow for what it does. Probably it would be best to write the
#' whole thing in C++.
#'
#' @docType package
#'
#' @name TEIdytext
#'
#' @import dplyr
#' @import purrr

NULL
