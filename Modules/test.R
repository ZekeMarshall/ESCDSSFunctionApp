library(shiny)
library(DT)

testUI <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    
    sidebarLayout(
      
      mainPanel(
        DT::DTOutput(outputId = ns('table'))
      ),
      
      sidebarPanel(
        numericInput(inputId = ns("x_val"), label = "X Value", value = 1),
        numericInput(inputId = ns("y_val"), label = "Y Value", value = 1),
        # Row selection
        numericInput(inputId = ns("row.selection"), label = "Select row to be 
                     deleted", min = 1, max = 100, value = ""),
        # Add button
        actionButton(inputId = ns("add.button"), label = "Add", icon = icon("plus")), 
        # Delete button 
        actionButton(inputId = ns("delete.button"), label = "Delete", icon = icon("minus"))
        
      )
    )
  )

}

test = function(input, output, session, 
                x_val,
                y_val,
                row.selection,
                add.button,
                delete.button) {
  
  init_df <- data.frame(x = 2, y = 2)
  values <- reactiveValues()
  values$df <- init_df
  
  observeEvent(input$add.button,{
    
    # cat("addEntry\n")
    newRow <- data.frame(input$x_val, input$y_val)
    colnames(newRow) <- colnames(values$df)
    values$df <- rbind(values$df, newRow)
    
  })
  
  observeEvent(input$delete.button,{
    
    cat("deleteEntry\n")
    if(is.na(input$row.selection)){
      values$df <- values$df[-nrow(values$df), ]
    } else {
      values$df <- values$df[-input$row.selection, ]
    }
    
  })  

  output$table <- DT::renderDT({values$df})
  
}


