library(shinythemes);

shinyUI(
    fluidPage(theme = shinytheme("cosmo"),
              includeScript('www/tools.js'),
              ##css
              tags$head(tags$title("WECO"),
                        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
                        tags$link(rel = "stylesheet", type = "text/css", href = "//fonts.googleapis.com/css?family=Oswald"),
                        tags$link(rel = "stylesheet", type = "text/css", href = "//fonts.googleapis.com/css?family=Lora")
                        ),

              ##title box
              withTags({
                  div(class="cheader", "Western Electric Company Rules (WECO) for Shewhart Control Chart")
              }),

              ##main page
              uiOutput("mainpage"),

              ##foot
              withTags({
                  div(class="cfooter", "WECO RULES")
              })
              )
)
