#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
    dat <- reactive({
      smst_data_good %>% filter(ID == input$select_ID)
    })
  
    output$smstPlot <- renderPlot({
      
        # draw the histogram with the specified number of bins
   
        ggplot() + 
          geom_bar(data = smst_data_good, aes(x=response, y=percentage), alpha = .3, stat="summary") +
          geom_col(data = dat(), aes(x=response, y=percentage), alpha = 8, stat = "identity", fill='pink', color='red') +
          custom_theme + 
          labs(x = "Response made", y = "Percentage of responses", title = "Good or not-good responses on ENC phase ") +
          scale_y_continuous(breaks = seq(0,1, by =0.2), labels=percent) +
          facet_wrap(~stimtype) +
          coord_cartesian(ylim = c(0, 1))

    })

})
