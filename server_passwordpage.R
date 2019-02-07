  observers <- list()

  correct_password <- "BaSeMoVa"
 
  output$password_question <- renderText("To access the model validator app, you need to get a password by contacting someone@io-warnemuende.de")
  
  observers$button <- observeEvent(input$password_input_button, 
               {
                 if (input$password_input==correct_password) 
                 {
                   go_to_page("mainpage", observers)
                 } 
                   
               })
  observers$key <- observeEvent(input$last_key_pressed, 
               {
                 if (input$last_key_pressed==13) # catch Enter key
                 {  
                   if (input$password_input==correct_password) 
                   {
                     go_to_page("mainpage", observers)
                   } 
                 }
               })
