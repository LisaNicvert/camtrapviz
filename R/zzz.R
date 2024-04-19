# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2024-04-15
#
# Script Description: zzz srcipt holds the actions performed at package loading by default (https://r-pkgs.org/code.html#sec-code-onLoad-onAttach)

.onAttach <- function(libname, pkgname) {
  # https://stackoverflow.com/questions/38791613/including-an-image-in-a-shiny-app-package
  shiny::addResourcePath('www', system.file("app/www", package = "camtrapviz"))
}