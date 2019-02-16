library(purrr)

.xpl = function(x, i) Bxml2::xml_contents(x) %>% pluck(i)


tomorrow = system.file("extdata", "Tomorrow_and_tomorrow.xml", package="TEIdy")

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
    do.call(data_frame, .)
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


traverseXML = function(node, ignore = c(), discard = c(), keep = c(), mode = "complete") {
  combiner = .summaryFinalize
  if (mode=="complete") {
      combiner = combine_data
  }
  if (class(node)[1] == "xml_nodeset") {
    return (node %>% map(traverseXML, ignore = ignore, discard = discard, keep = keep, mode = mode) %>% discard(is.null) %>% combiner)
  }

  if (xml2::xml_type(node) == "text") {
    if (mode=="summarize") {return(data_frame(tag="text", value="text", children=1, n = 1))}
    return(list(text = xml2::xml_text(node)))
  }

  name = xml2::xml_name(node)

  if (name %in% discard) {
    return(NULL)
  }

  children = childNodes(node, discard, ignore, keep, mode)

  if (length(children)==0) {
    if (mode != "summarize") {
      return(NULL)
    } else {
      children = data_frame()
    }
  }

  children = children %>% combiner

  if (name %in% ignore) {
    return(children)
  }


  meta2 = xml2::xml_attrs(node) %>% as.list

  if (length(meta2[["type"]])) {
    name = paste(name, meta2[["type"]], sep=".")
  }

  metadata = list()
  metadata[[name]] = TRUE

  meta2 %>% imap(function(.x, .y) {
    if (.y != 'type') {
      metadata[[paste(name, .y, sep='.')]] <<- .x
    }
  })


  if(mode == "summarize") {
    if (length(children) > 1 && !is.null(ncol(children))) {
      child_count = sum(children$children)
    } else {
      child_count = 1
    }
    return(
    data_frame(tag = names(metadata), value = unlist(metadata) %>% as.character, children = child_count, n = 1) %>% bind_rows(children)
      )
  }
  child_length = length(children[[1]])
  metadata %>% map(~rep(., child_length)) %>% c(children) %>% return

}


