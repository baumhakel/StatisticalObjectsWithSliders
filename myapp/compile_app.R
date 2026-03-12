# manually set wd to deploy correctly to github
setwd("C:/Users/baujuc12/Nextcloud/lvs_betreuung/s26/statistik_fuer_info_vu/App_new/StatisticalObjectsWithSliders")
shinylive::export(
  appdir = "myapp",
  destdir = "docs",
  pkgs = c("shiny", "ggplot2", "bslib", "munsell", "markdown"),
  template_params = list(title = "SOS")
)

# and test
httpuv::runStaticServer("docs", port = 8080)

