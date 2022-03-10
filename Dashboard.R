library(fpp3)
library(readr)
library(shinyWidgets)
library(shiny)
library(shinydashboard)


shinyApp(
  ui = dashboardPage(
    dashboardHeader(
      title = "Stock App (Group 16)",
      titleWidth = 350
    ),
    dashboardSidebar(
      width = 350,
      sidebarMenu(
        menuItem("Feature 1"),
        menuItem("Feature 2"),
        menuItem("Feature 3"),
        menuItem("Feature 4")
      )
    ),
    dashboardBody()
  ),
  server = function(input, output) { }
)