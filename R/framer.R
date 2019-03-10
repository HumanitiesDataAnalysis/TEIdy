#' Load an XML document as a data.frame
#'
#' @param fname A filename
#' @param ignore Tags that, when encountered, will not be added as columns to the tidy representation. Data inside these tags
#' will be kept, but the tag will not be column.
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
    bundle_env_to_frame() %>%
    mutate_at(vars(matches("^[^.]+$")), .numberUnique) %>%
    select(.text, .hierarchy, everything())
}

.numberUnique = function(x) match(x, unique(x[!is.na(x)]))

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


#' Lift up a piece of data
#'
#' @description Frequently a TEI document will have useful information about
#' subheadings located inside another tag: for instance, consider this XML.
#' The author is "HAMILTON", but authorship applies to the entire 'div.'
#'
#' This function allows you to lift out the contents of any field up to every
#' element in the same document (where 'document' is defined by the current
#' tibble grouping.) Now every element in the div will have 'Hamilton' as author.
#'
#' <div>
#' <title>FEDERALIST No. 1</title>
#' <p>General Introduction</p>
#' <p>For the Independent Journal.</p>
#' <author>HAMILTON</author>
#' <text>
#' <p>To the People of the State of New York:</p>
#' [...]
#' </text>
#' </div>
#'
#' The 'from' and 'to' arguments give finer-grained control, but generally it
#' can be OK to populate the text from one tag to the full document.
#'
#' @param data A tibble.
#' @param where The tag where a value is located; eg, 'author'.
#' @param from The tag to extract a value from. By default '.text'; i.e., the data is in the text field.
#' @param to The new field to put the value into. By default, it overwrites
#'
#' @return A copy of the original tibble with one altered or new column.
#'
#' @importFrom rlang quo enquo quo_is_null
#'
#' @export
#'
#'
lift_data = function(data, where, from = NULL, to = NULL) {
  # Lifts up data that is undefined for tag across the group.
  # Especially useful if the text is present.
  tag = enquo(where)
  value = enquo(from)
  if (quo_is_null(value)) {
    value = quo(.text)
  }
  newfield = enquo(to)
  if (quo_is_null(newfield)) {
    newfield = tag
  }
  data %>%
    mutate(!!newfield :=
             map2(!!tag, !!value,  ~ list(.x, .y)) %>%
             # Find the rows thing where 'where' is present
             discard(~is.na(.[[1]])) %>%
             # Grab the
             map(pluck,2) %>%
             # Paste all matching elements together with a '--'.
             # Ideally there should only be one.
             compact %>% paste(collapse = "--"))
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
  left_names = c(ls(envir = left), ".text")
  right_names = c(ls(envir = right), ".text")
  if ((left$.allocated - left$.length) < right$.length) {
    # Overallocate at least 511 NAs.
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
  list_from_env = as.list(z)
  list_from_env$.text = z$.text
  list_from_env$.hierarchy = z$.hierarchy
  do.call(tibble, list_from_env)[1:z$.length,]
}

traverseXML = function(node, ignore = c("basetext"), discard = c(), keep = c(), mode = "complete") {
  combiner = combine_env
  if (class(node)[1] == "xml_nodeset") {
    return (map(node, traverseXML, ignore = ignore, discard = discard, keep = keep, mode = mode) %>% discard(is.null) %>% combiner)
  }

  if (xml2::xml_type(node) == "text") {
    if (mode=="summarize") {return(data_frame(tag="basetext", value="text", .hierarchy = "", children=1, n = 1))}
    return(rlang::env(.text = xml2::xml_text(node), .length = 1, .allocated = 1))
  }

  name = xml2::xml_name(node)

  if (name %in% discard) return(NULL)

  children = childNodes(node, discard, ignore, keep, mode)
  if (length(children)==0) {
    children = list(rlang::env(.text = NA, .length = 1, .allocated = 1))
  }

  reduced = reduce(children, combiner)

  if (name %in% ignore) {
    return(reduced)
  }

  meta2 = xml2::xml_attrs(node) %>% as.list

  if (length(meta2[["type"]])) {
    name = paste(name, meta2[["type"]], sep=".")
  }

  reduced$.hierarchy = paste0(">", name, reduced$.hierarchy)

  metadata = list()
  metadata[[name]] =
    stringi::stri_rand_strings(1, 10, '[0-z]')

  meta2 %>% imap(function(.x, .y) {
    if (.y != 'type') {
        metadata[[paste(name, .y, sep='.')]] <<- .x
      }
    })


  metadata %>% imap(function(val, key) {
    if (reduced$.allocated < reduced$.length) {
      stop("Corrupted data; insufficient space allocated.")
    }
    reduced[[key]] = c(rep(val, reduced$.length), rep(NA, reduced$.allocated - reduced$.length))
  })

  return(reduced)

}

#' Add TF-IDF summary based on groupings.
#'
#' @description The TF-IDF function in tidytext requires an explicit 'doc'
#' parameter; this applies it on the existing dataset groups.
#'
#' @param frame A grouped data from
#' @param word The unquoted variable name storing terms for frequency
#' @param count The unquoted variable storing the count
#'
#' @return A data_frame with a column tfidf added.
#' @export
summarize_tf_idf = function(frame, word, count) {
  token = enquo(word)
  count = enquo(count)
  groupings = groups(f)
  n_docs = frame %>% distinct(!!!groupings) %>% nrow
  frame %>%
    distinct(!!!groupings, token) %>%
    group_by(!!token) %>%
    summarize(idf = log(n_docs/n())) %>%
    inner_join(frame) %>%
    group_by(!!!groupings) %>%
    mutate(doc_total = sum(!!count)) %>%
    group_by(!!token, add = TRUE) %>%
    summarize(count = sum(!!count), tfidf = count/doc_total[1]*idf[1])
}


#' Summarize the log-likelihood ratio across a grouped data frame
#'
#' @param data A data frame
#' @param token The column indicating a token.
#' @param count The column indicating wordcount data.
#'
#' @citation Ted Dunning, Accurate Statistics.
#'
#' @return A dataframe with the supplied grouping and a log-likelihood for each token in that grouping.
#' Strongly positive numbers are over-represented; strongly negative numbers are under-represented.
#' @export
#'
summarize_llr = function(data, token, count = rep(1, n())) {
  token = enquo(token)
  count = enquo(count)
  groupings = groups(data)

  data %>%
    group_by(!!token, add = TRUE) %>%
    # Numeric to fix some integer overflow problems.
    summarize(count=sum(!!count) %>% as.numeric) %>%
    group_by(!!token) %>% mutate(grandtot = sum(count)) %>%
    group_by(!!!groupings) %>%
    # Dunning scores
    mutate(count.x = count, count.y = grandtot - count) %>%
    mutate(c = sum(count.x), d = sum(count.y), totalWords = c + d) %>%
    mutate(count.y = ifelse(count.y==0, 1, count.y)) %>%
    mutate(exp.x = c*grandtot/totalWords, exp.y = d * grandtot/totalWords) %>%
    mutate(score = 2*(
      (count*log(
        count/exp.x)) +
        count.y*log(count.y/exp.y))
    ) %>%
    mutate(score = ifelse((count.y - exp.y) > 0, -score, score)) %>%
    select(!!!groupings, !!token, dunning_llr = score) %>%
    ungroup
}

