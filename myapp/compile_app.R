# manually set wd to deploy correctly to github
setwd("C:/Users/baujuc12/Nextcloud/lvs_betreuung/s26/statistik_fuer_info_vu/App_new/StatisticalObjectsWithSliders")
shinylive::export(
  appdir = "myapp",
  destdir = "site",
  pkgs = c("shiny", "ggplot2", "bslib", "munsell", "commonmark", "gld", "shinylive", "Markdown"),
  template_params = list(title = "SOS",
                         base_url = "/StatisticalObjectsWithSliders/")
)

# and test
httpuv::runStaticServer("site", port = 8080)

