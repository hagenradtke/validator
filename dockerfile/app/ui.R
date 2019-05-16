library(shiny)

# Define an empty UI that can be filled later
shinyUI(fluidPage(uiOutput("selected_page"),
                  tags$script('
                    $(document).on("keydown", function (e) {
                                                Shiny.onInputChange("last_key_pressed", e.keyCode);
                                              });
                  ')))