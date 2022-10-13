if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny)
runApp(
  shinyAppDir("../chip",options=list(launch.browser=TRUE))
)