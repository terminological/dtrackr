#' Convert `Graphviz` dot content to a SVG
#'
#' Convert a `graphviz` dot digraph as string to `SVG` as string
#'
#' @param dot a `graphviz` dot string
#'
#' @return the SVG as a string
#' @export
#'
#' @examples
#' dot2svg("digraph { A->B }")
dot2svg <- function(dot) {
  if (!requireNamespace("V8", quietly = TRUE)) {
    stop("V8 package is required", call. = FALSE)
  }
  if (!(utils::packageVersion("V8") >= "1.0")) {
    stop(
      'your "V8" package is too old. The DOT package requires V8
        package version 1.0 or newer',
      call. = FALSE
    )
  }
  # remove the "\n" from DOT source

  scriptDOT <- gsub("[\r\n]", " ", dot)
  scriptDOT <- gsub("'", "\"", scriptDOT)
  dot <- paste("'", scriptDOT, "'", sep = "")

  JS <- system.file("lib/viz.js", package = "dtrackr")
  # this is an old vis.js
  # newer versions are here https://github.com/aduh95/viz.js but use npm and assume node.

  ENV <- V8::v8()
  ENV$source(JS)
  call <- paste("image = Viz(", dot, ")", sep = "")
  content <- ENV$eval(call)
  return(content)

  # viewer <- getOption("viewer")
  # if (!is.null(viewer) & display) {
  #   viewer(dotFile)
  # }
}

#' Standard paper sizes
#'
#' A list of standard paper sizes for outputting flowcharts or other dot
#' graphs. These include width and height dimensions in inches and can be
#' used as one way to specify the output size of a dot graph, including
#' flowcharts (see the `size` parameter of [dtrackr::flowchart()]).
#'
#' The sizes available are `A4`, `A5`, `full` (fits a portrait A4 with margins), `half` (half an
#' A4 with margins), `third`, `two_third`, `quarter`, `sixth` (all with reference to
#' an A4 page with margins). There are 2 landscape sizes `A4_landscape` and `full_landscape` which
#' fit an A4 page with or without margins. There are also 2 slide dimensions,
#' to fit with standard presentation software dimensions.
#'
#' This is just a convenience. Similar effects can be achieved by providing `width` and `height`
#' parameters to [dtrackr::flowchart()] directly.
#'
#' @export
std_size = list(
  A4 = list(width = 8.25, height = 11.75, rot = 0),
  A5 = list(width = 5 + 7 / 8, height = 8.25, rot = 0),
  full = list(width = 5.9, height = 8, rot = 0),
  half = list(width = 5.9, height = 4, rot = 0),
  third = list(width = 5.9, height = 3, rot = 0),
  two_third = list(width = 5.9, height = 6, rot = 0),
  quarter = list(width = 5.9, height = 2, rot = 0),
  sixth = list(width = 3, height = 3, rot = 0),
  A4_landscape = list(width = 8.25, height = 11.75, rot = 270),
  full_landscape = list(width = 5.9, height = 8, rot = 270),
  slide_16_9 = list(width = 9.32, height = 4.5, rot = 0),
  slide_4_3 = list(width = 9.32, height = 6, rot = 0)
)

#' Save DOT content to a file
#'
#' Convert a digraph in dot format to SVG and save it to a range of output file types
#'
#' @param dot a `graphviz` dot string
#' @param filename the full path of the file name (minus extension for multiple
#'   formats)
#' @param size a named list with 3 elements, length and width in inches and
#'   rotation. A predefined set of standard sizes are available in the
#'   [dtrackr::std_size] object.
#' @param maxWidth a width (on the paper) in inches if `size` is not defined
#' @param maxHeight a height (on the paper) in inches if `size` is not defined
#' @param formats some of `pdf`,`dot`,`svg`,`png`,`ps`
#' @param landscape rotate the output by 270 degrees into a landscape format.
#'   `maxWidth` and `maxHeight` still apply and refer to the paper width to fit
#'   the flowchart into after rotation. (you might need to flip width and height)
#' @param ... ignored
#'
#' @return a list with items `paths` with the absolute paths of the saved files
#'   as a named list, and `svg` as the SVG string of the rendered dot file.
#' @export
#'
#' @examples
#' save_dot("digraph {A->B}",tempfile())
save_dot = function(
  dot,
  filename,
  size = std_size$half,
  maxWidth = size$width,
  maxHeight = size$height,
  formats = c("dot", "png", "pdf", "svg"),
  landscape = size$rot != 0,
  ...
) {
  tmp = filename %>% fs::path_ext()
  if (tmp %in% c("dot", "png", "pdf", "svg", "ps")) {
    formats = tmp
  }
  filename = filename %>% fs::path_ext_remove()
  fname = function(extn, suffix = NULL) {
    normalizePath(
      paste0(c(filename, suffix), collapse = "_") %>% fs::path_ext_set(extn),
      mustWork = FALSE
    )
  }

  if ("dot" %in% formats) {
    # message("saving dot to: ",fname("dot"))
    suppressMessages(unlink(fname("dot")))
    dot %>% writeChar(fname("dot"))
  }

  svg = dot %>% dot2svg()
  svg = svg %>% .scale_svg(maxWidth, maxHeight, landscape)

  if ("pdf" %in% formats) {
    svg %>%
      charToRaw() %>%
      rsvg::rsvg_pdf(
        file = fname("pdf")
      )
    try(
      grDevices::embedFonts(fname("pdf")),
      silent = TRUE
    )
  }

  if ("png" %in% formats) {
    # this is written to pdf first then converted to png and I think this
    # was also done because of a difference in PDF and PNG scaling in rsvg
    # versions prior to 2.4.0, In 2.4.0 this was changed for PNG but to make it
    # easy we are sticking with this and scaling the SVG printing to PDF
    # and rescaling the PDF to PNG at 300 dpi.

    if (!fs::file_exists(fname("pdf"))) {
      tmp = tempfile()
      svg %>% charToRaw() %>% rsvg::rsvg_pdf(file = tmp)
    } else {
      tmp = fname("pdf")
    }
    pdftools::pdf_render_page(tmp, page = 1, dpi = 300) %>%
      png::writePNG(fname("png"))
  }

  if ("svg" %in% formats) {
    # With this approach we don't need to "render" the SVG
    # but that then requires the correct fonts are available on the
    # rendering system. RSVG will convert fonts to glphs and remove this dependency
    # but at the cost of preventing further editing.
    svg %>% writeChar(fname("svg"))
    # svg %>% charToRaw() %>% rsvg::rsvg_svg(
    #   file = fname("svg")
    # )
  }

  if ("ps" %in% formats) {
    svg %>%
      charToRaw() %>%
      rsvg::rsvg_ps(
        file = fname("ps")
      )
  }

  paths = as.list(sapply(formats, fname))
  return(
    list(
      paths = paths,
      svg = svg
    )
  )
}


.scale_svg = function(
  svg,
  maxWidth = NULL,
  maxHeight = NULL,
  landscape = FALSE,
  dpi = 72
) {
  defaultWidth = (svg %>% stringr::str_match("width=\"([0-9]*)pt\""))[1, 2] %>%
    as.numeric()
  defaultHeight = (svg %>% stringr::str_match("height=\"([0-9]*)pt\""))[
    1,
    2
  ] %>%
    as.numeric()

  if (is.null(maxWidth)) {
    maxWidth = (if (landscape) defaultHeight else defaultWidth) / 72
  }
  if (is.null(maxHeight)) {
    maxHeight = (if (landscape) defaultWidth else defaultHeight) / 72
  }

  # Calculate a minimum scale based on xscale and yscale then apply that to
  # both X and Y. Then rotating things using svg. the origin of rotation requries
  # a translation (which is on the original scale of the image)
  # The small transform offsets are needed to align the content to the viewport
  dpi = 72
  if (landscape) {
    scale = min(
      1,
      maxHeight * 72 / defaultWidth,
      maxWidth * 72 / defaultHeight
    ) *
      dpi /
      72
    targetWidth = defaultWidth * scale
    targetHeight = defaultHeight * scale
    transform = sprintf(
      "scale(%1.3f %1.3f) rotate(270) translate(-%1.3f %1.3f)",
      scale,
      scale,
      defaultWidth - 4,
      defaultHeight - 4
    )
    viewBox = sprintf("0 0 %1.3f %1.3f", targetHeight, targetWidth)
    width = sprintf("%1.3fpt", targetHeight)
    height = sprintf("%1.3fpt", targetWidth)
  } else {
    scale = min(
      1,
      maxWidth * 72 / defaultWidth,
      maxHeight * 72 / defaultHeight
    ) *
      dpi /
      72
    targetWidth = defaultWidth * scale
    targetHeight = defaultHeight * scale
    transform = sprintf(
      "scale(%1.3f %1.3f) rotate(0) translate(4 %1.3f)",
      scale,
      scale,
      defaultHeight - 4
    )
    viewBox = sprintf("0 0 %1.3f %1.3f", targetWidth, targetHeight)
    width = sprintf("%1.3fpt", targetWidth)
    height = sprintf("%1.3fpt", targetHeight)
  }

  # perform all scaling within the SVG
  svg = svg %>%
    stringr::str_replace_all(
      "width=\"([0-9]*)pt\"",
      sprintf("width=\"%s\"", width)
    ) %>%
    stringr::str_replace_all(
      "height=\"([0-9]*)pt\"",
      sprintf("height=\"%s\"", height)
    ) %>%
    stringr::str_replace_all(
      "viewBox=\"[^\"]+\"",
      sprintf("viewBox=\"%s\"", viewBox)
    ) %>%
    stringr::str_replace_all(
      "transform=\"[^\"]+\"",
      sprintf("transform=\"%s\"", transform)
    )

  return(svg)
}
