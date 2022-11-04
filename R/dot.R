
#' Convert Graphviz dot content to a SVG
#'
#' Convert a graphviz dot digraph as string to SVG as string
#'
#' @param dot a graphviz dot string
#'
#' @return the SVG as a string
#' @export
#'
#' @examples
#' dot2svg("digraph { A->B }")
dot2svg <- function(dot) {

  if(!requireNamespace("V8", quietly = TRUE)) stop("V8 package is required", call. = FALSE)
  if (!(utils::packageVersion("V8") >= "1.0")) {
    stop('your "V8" package is too old. The DOT package requires V8
        package version 1.0 or newer', call. = FALSE)
  }
  # remove the "\n" from DOT source

  scriptDOT <- gsub("[\r\n]", " ", dot)
  scriptDOT <- gsub("'", "\"", scriptDOT)
  dot <- paste("'", scriptDOT, "'", sep="")

  JS <- system.file("lib/viz.js", package = "dtrackr")
  # this is an old vis.js
  # newer versions are here https://github.com/aduh95/viz.js but use npm and assume node.

  ENV <- V8::v8();
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
#' graphs
#'
#' @export
std_size = list(
  A4 = list(width=8.25,height=11.75,rot=0),
  A5 = list(width=5+7/8,height=8.25,rot=0),
  full =  list(width=5.9,height=8,rot=0),
  half =  list(width=5.9,height=4,rot=0),
  third =  list(width=5.9,height=3,rot=0),
  two_third = list(width=5.9,height=6,rot=0),
  quarter = list(width=5.9,height=2,rot=0),
  sixth = list(width=3,height=3,rot=0),
  A4_landscape = list(width=11.75, height=8.25,rot=0),
  full_landscape =  list(width=8,height=5.9,rot=0),
  slide_16_9 = list(width=9.32,height=4.5,rot=0),
  slide_4_3 = list(width=9.32,height=6,rot=0)
)

#' Save DOT content to a file
#'
#' Convert a digraph in dot format to SVG and save it to a range of output file types
#'
#' @param dot a graphviz dot string
#' @param filename the full path of the filename (minus extension for multiple formats)
#' @param size a named list with 2 elements, length and width in inches. A predefined set
#'   of standard sizes are available in the [dtrackr::std_size] object
#' @param maxWidth a width in inches is size is not defined
#' @param maxHeight a height in inches if size is not defined
#' @param formats some of "pdf","dot","svg","png","ps"
#' @param ... ignored
#'
#' @return a list with items `paths` with the absolute paths of the saved files as a named list, and `svg` as the SVG string of the rendered dot file.
#' @export
#'
#' @examples
#' save_dot("digraph {A->B}",tempfile())
save_dot = function(dot, filename, size = std_size$half, maxWidth = size$width, maxHeight = size$height, formats=c("dot","png","pdf","svg"), ...) {

  tmp = filename %>% fs::path_ext()
  if(tmp %in% c("dot","png","pdf","svg","ps")) formats=tmp
  filename = filename %>% fs::path_ext_remove()

  fname = function(extn) normalizePath(filename %>% fs::path_ext_set(extn),mustWork = FALSE)

  if ("dot" %in% formats) {
    # message("saving dot to: ",fname("dot"))
    suppressMessages(unlink(fname("dot")))
    dot %>% writeChar(fname("dot"))
  }

  svg = dot %>% dot2svg()
  defaultWidth = (svg %>% stringr::str_match("width=\"([0-9]*)pt\""))[1,2] %>% as.numeric()
  defaultHeight = (svg %>% stringr::str_match("height=\"([0-9]*)pt\""))[1,2] %>% as.numeric()
  aspectRatio = defaultWidth / defaultHeight
  widthIn = min(maxWidth,maxHeight*aspectRatio,defaultWidth/72)
  heightIn = min(maxHeight,maxWidth/aspectRatio,defaultHeight/72)

  # resize = function(dpi) {
  #   svg %>%
  #     stringr::str_replace_all("width=\"([0-9]*)pt\"", sprintf("width=\"%1.0fpt\"", widthIn*dpi)) %>%
  #     stringr::str_replace_all("height=\"([0-9]*)pt\"", sprintf("height=\"%1.0fpt\"", heightIn*dpi)) # %>%
  #     # stringr::str_replace_all("viewBox=\"[^\"]+\"", sprintf("viewBox=\"0.00 0.00 %1.2f %1.2f\"", widthIn*dpi, heightIn*dpi))
  # }

  if ("pdf" %in% formats) {
    svg %>% charToRaw() %>% rsvg::rsvg_pdf(
      file = fname("pdf"),
      width = widthIn*72,
      height = heightIn*72
    )
    try(
      grDevices::embedFonts(fname("pdf")),
      silent=TRUE
    );
    # if (rot!=0 & rlang::is_installed("staplr")) {
    #   staplr::rotate_pdf(page_rotation = rot,
    #     input_filepath = fname("pdf"),
    #     output_filepath = fname("pdf"),
    #     overwrite = TRUE)
    # }
  }

  if ("png" %in% formats) {
    if (!fs::file_exists(fname("pdf"))) {
      tmp = tempfile()
      svg %>% charToRaw() %>% rsvg::rsvg_pdf(file = tmp,
         width = widthIn*72,
         height = heightIn*72)
    } else {
      tmp = fname("pdf")
    }
    pdftools::pdf_render_page(tmp,page = 1,dpi=300) %>%
      png::writePNG(fname("png"))
    # if (rot != 0 & rlang::is_installed("magick")) {
    #   magick::image_rotate(
    #     magick::image_read(
    #       fname("png")
    #     )
    #     ,rot
    #   ) %>% magick::image_write(
    #     fname("png")
    #   )
    # }
  }

  if ("svg" %in% formats) {
    svg %>% charToRaw() %>% rsvg::rsvg_svg(
      file = fname("svg"),
      width = widthIn*72,
      height = heightIn*72
    )
  }

  if ("ps" %in% formats) {
    svg %>% charToRaw() %>% rsvg::rsvg_ps(
        file = fname("ps"),
        width = widthIn*72,
        height = heightIn*72
    )
  }

  paths =  as.list(sapply(formats,fname))
  return(
    list(
      paths = paths,
      svg = svg
  ))


}



