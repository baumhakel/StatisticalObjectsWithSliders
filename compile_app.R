shinylive::export(
  appdir = ".",
  destdir = "site",
  pkgs = c("shiny", "ggplot2", "bslib", "munsell", "markdown") #  "PearsonDS", 
)

# and test
httpuv::runStaticServer("site", port = 8080)
