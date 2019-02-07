output$selected_page <- renderUI(fluidRow(column(12,

  ###############
  # HEADER LINE #
  ###############
  fluidRow(column(12,style = "background-color:#dddddd;",

    # Application title
        headerPanel("Interactive (multi-)model validation app")
  )),

  fluidRow(column(12,
            textOutput("password_question"),
            passwordInput("password_input",label="Enter password:"),
            actionButton("password_input_button", "OK")
          ))
)))
