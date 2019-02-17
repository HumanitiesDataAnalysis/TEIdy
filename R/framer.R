library(purrr)

#' Load an XML document as a data.frame
#'
#' @param fname A filename
#' @param ignore Tags that, when encountered, will not be added as columns to the tidy representation.
#' @param discard Tags that, when encountered, will be discarded from the tidy representation. discard='teiHeader', for
#'        example, will prevent header information from being included.
#' @param keep Tags to *keep* in the tidy representation. If this is used, 'ignore' and 'discard' arguments will apply
#'     only inside tags defined by keep.
#'
#' @return A tibble, with a 'text' column indicating the lowest of text address found in the document.
#'
#' @export
#'
#' @examples
#'
#' tomorrow = system.file("extdata", "Tomorrow_and_tomorrow.xml", package="TEIdy")
#' TEIdy(tomorrow)
#'
TEIdy = function(fname, ignore = c(), discard = c(), keep = c()) {
  node = xml2::read_xml(fname)
  traverseXML(node, ignore = ignore, discard = discard, keep = keep, mode = "complete") %>%
    bundle_env_to_frame()
}


TEIsummary = function(node, ignore = c(), discard = c()) {
  traverseXML(node, ignore = ignore, discard = discard, mode = "summarize") %>%
    .summaryFinalize
}

.summaryFinalize = function(f) {f %>%
  bind_rows %>%
  group_by(tag, value) %>%
  summarize(children = sum(children), n = sum(n))
}


childNodes = function(node, discard=c(), ignore = c(), keep = c(), mode = "complete") {
  xml2::xml_contents(node) %>%
    map(traverseXML, discard = discard, ignore = ignore, keep = keep, mode = mode) %>%
    discard(is.null)
}

combine_data = function(listset) {
  lengths = listset %>% map(pluck, 1)  %>% map_int(length)
  keys = listset %>% map(names) %>% flatten %>% unique
  keys %>% map(function(k) {
    listset %>% map(pluck, k) %>% map2(lengths, ~ ifelse(!is.null(.x), list(.x), list(rep(NA, times=.y)))) %>% unlist
  }) %>%
    set_names(keys) %>%
    return
}


lift_data = function(data, from = NULL, to = NULL) {
  # Lifts up data that is undefined for tag across the group.
  # Especially useful if the text is present.
  from = enquo(from)
  newfield = enquo(to)
  if (quo_is_null(newfield)) {
    newfield = from
  }
  data %>% mutate(!!newfield := !!from %>% discard(is.na) %>% paste(collapse = "--"))
}


as_env = function(l) {
  if (is.list(l)) {
    out = rlang::as_environment(l)
    length = length(out[[ls(envir = out)[1]]])
    out$.length = length
    out$.allocated = length
  }
  out
}

combine_env = function(left, right) {
  left_names = ls(envir = left)
  right_names = ls(envir = right)
  if ((left$.allocated - left$.length) < right$.length) {
    # Overallocated a bunch of NAs.
    adding = max(2 * left$.length + right$.length, 511)
    for (name in left_names) {
      left[[name]] = c(left[[name]], rep(NA, adding))
    }
    left$.allocated = left$.allocated + adding
  }
  for (name in right_names) {

    if (is.null(left[[name]])) {
      left[[name]] = c(rep(NA, left$.allocated))
    }

    left[[name]][(left$.length + 1):(left$.length + right$.length)] = right[[name]][1:right$.length]
  }
  left$.length = left$.length + right$.length
  return(left)
}

bundle_env_to_frame = function(z) {
  do.call(data_frame, as.list(z))[1:z$.length,]
}

traverseXML = function(node, ignore = c("basetext"), discard = c(), keep = c(), mode = "complete") {
  combiner = combine_env
  if (class(node)[1] == "xml_nodeset") {
    return (map(node, traverseXML, ignore = ignore, discard = discard, keep = keep, mode = mode) %>% discard(is.null) %>% combiner)
  }

  if (xml2::xml_type(node) == "text") {
    if (mode=="summarize") {return(data_frame(tag="basetext", value="text", children=1, n = 1))}
    return(env(plaintext = xml2::xml_text(node), .length = 1, .allocated = 1))
  }

  name = xml2::xml_name(node)

  if (name %in% discard) return(NULL)

  children = childNodes(node, discard, ignore, keep, mode)
  if (length(children)==0) {
    children = list(env("empty" = name, .length = 1, .allocated = 1))
  }

  reduced = reduce(children, combiner)

  if (name %in% ignore) {
    return(reduced)
  }

  meta2 = xml2::xml_attrs(node) %>% as.list

  if (length(meta2[["type"]])) {
    name = paste(name, meta2[["type"]], sep=".")
  }

  metadata = list()
  metadata[[name]] = "empty"

  meta2 %>% imap(function(.x, .y) {
    if (.y != 'type') {
      if (.y == 'id') {
        metadata[[name]] <<- .x
      } else {
        metadata[[paste(name, .y, sep='.')]] <<- .x
      }
    }
  })

  metadata %>% imap(function(val, key) {
    if (reduced$.allocated < reduced$.length) {
      stop("Corrupted data")
    }
    reduced[[key]] = c(rep(val, reduced$.length), rep(NA, reduced$.allocated - reduced$.length))
  })

  return(reduced)

}


