# library(rsvg)
#
# svg_text <- '<?xml version="1.0" encoding="UTF-8" standalone="no"?>
# <svg
#    width="100"
#    height="100"
#    viewBox="0 0 100 100"
#    version="1.1"
#    xmlns="http://www.w3.org/2000/svg"
#    xmlns:svg="http://www.w3.org/2000/svg">
#    <g>
#     <rect
#        style="fill:#b3b3b3;stroke:#000000;"
#        width="90"
#        height="90"
#        x="5"
#        y="5" />
#     <rect
#        style="fill:#b3b3b3;stroke:#000000;"
#        width="20"
#        height="20"
#        x="60"
#        y="60" />
#   </g>
# </svg>
# '
#
# # a simple box centred in the pdf with a smaller box in the bottom right
# expected = tempfile(fileext = ".expected.pdf")
# rsvg::rsvg_pdf(charToRaw(svg_text),file = expected)
# rstudioapi::viewer(expected)
#
# # I expect the same image to be scaled to 50x50
# # instead I see truncation
# observed = tempfile(fileext = ".observed.pdf")
# rsvg::rsvg_pdf(charToRaw(svg_text),file = observed,width = 50,height = 50)
# rstudioapi::viewer(observed)
#
# # rsvg to svg produces image that is not scaled but viewport is smaller.
# # Its not shown in this test but any boxes outside the
# observed2 = tempfile(fileext = ".observed.svg")
# rsvg::rsvg_svg(charToRaw(svg_text),file = observed2,width = 50,height = 50)
# rstudioapi::viewer(observed2)
#
#
# # rsvg to svg produces image that is not scaled but viewport is smaller.
# # Its not shown in this test but any boxes outside the
# observed3 = tempfile(fileext = ".observed.png")
# rsvg::rsvg_png(charToRaw(svg_text),file = observed3,width = 50,height = 50)
# rstudioapi::viewer(observed3)
#
# observed4 = tempfile(fileext = ".observed.ps")
# rsvg::rsvg_ps(charToRaw(svg_text),file = observed4,width = 50,height = 50)
# # rstudioapi::viewer(observed4)
#
# # Inspecting the SVG there is only one path element - the smaller box is no
# # longer there.
# readLines(observed2)
#
# # rsvg to bitmap works as expected
# bitmap = rsvg::rsvg(charToRaw(svg_text),width = 50,height = 50)
# im <- magick::image_read(bitmap)
# grid::grid.raster(im)
#
# # sessionInfo()
# # R version 4.2.2 Patched (2022-11-10 r83330)
# # Platform: x86_64-pc-linux-gnu (64-bit)
# # Running under: Ubuntu 22.04.1 LTS
# #
# # Matrix products: default
# # BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.10.0
# # LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.10.0
# #
# # locale:
# #   [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C               LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8     LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_GB.UTF-8    LC_PAPER=en_GB.UTF-8       LC_NAME=C                  LC_ADDRESS=C
# # [10] LC_TELEPHONE=C             LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C
# #
# # attached base packages:
# #   [1] stats     graphics  grDevices datasets  utils     methods   base
# #
# # other attached packages:
# #   [1] rsvg_2.4.0
#
#
# svg_text <- '<?xml version="1.0" encoding="UTF-8" standalone="no"?>
# <svg
#    width="100"
#    height="100"
#    viewBox="0 0 100 100"
#    version="1.1"
#    xmlns="http://www.w3.org/2000/svg"
#    xmlns:svg="http://www.w3.org/2000/svg">
#    <g transform="scale(2 2)">
#     <rect
#        style="fill:#b3b3b3;stroke:#000000;"
#        width="90"
#        height="90"
#        x="5"
#        y="5" />
#     <rect
#        style="fill:#b3b3b3;stroke:#000000;"
#        width="20"
#        height="20"
#        x="60"
#        y="60" />
#   </g>
# </svg>
# '
# observed = tempfile(fileext = ".observed.pdf")
# rsvg::rsvg_pdf(charToRaw(svg_text),file = observed,width = 200,height = 200)
# rstudioapi::viewer(observed)
#
# observed3 = tempfile(fileext = ".observed.png")
# rsvg::rsvg_png(charToRaw(svg_text),file = observed3,width = 200,height = 200)
# rstudioapi::viewer(observed3)
#
# observed2 = tempfile(fileext = ".observed.svg")
# rsvg::rsvg_svg(charToRaw(svg_text),file = observed2,width = 200,height = 200)
# rstudioapi::viewer(observed2)
