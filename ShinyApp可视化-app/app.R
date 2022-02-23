if(!require("shiny")) install.packages(shiny);library(shiny)
if(!require("palmerpenguins")) install.packages("palmerpenguins");library(palmerpenguins)
if(!require("tidyverse")) install.packages("tidyverse");library(tidyverse)
#remove NA values in penguins data
data=na.omit(penguins)%>%
  mutate_if(is.character,as.factor)
data$year=factor(data$year)

# Define UI for app with 4 user inputs
ui <- fluidPage(
  
  # App title ----
  titlePanel("Task 1"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input 1: Select numeric variables
      selectInput(inputId = "y_axis",
                  label ="Choose a numeric variable:",
                  list("flipper length (millimeters)"="flipper_length_mm",
                       "bill depth (millimeters)"="bill_depth_mm",
                    "bill length (millimeters)"="bill_length_mm",
                   "body mass (grams)"="body_mass_g")),
      # Input 2: Select categorical variables
      selectInput(inputId = "x_axis",
                  label="Choose a categorical varible:",
                  list("sex","species",'year')),
      # Input 3: Checkbox to separate observations by island levels
      checkboxInput(inputId = "by_island",label = "Show different island levels",value = F),
      # Input 4: Plot by species or together
      selectInput(inputId = "separate",
                  label="View species",
                  list("Together","By species")),
      ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: 
      plotOutput(outputId = "boxplot")
    )
  )
)
server=function(input,output){
  output$boxplot=renderPlot({
    if(input$separate=="Together"){
      data%>%
      ggplot(aes_string(x=input$x_axis,y=input$y_axis,
                        fill=ifelse(input$by_island,"island","NULL")))+
      geom_boxplot()}
    else if(input$separate=="By species"){
      data%>%
        ggplot(aes_string(x=input$x_axis,y=input$y_axis,
                          fill=ifelse(input$by_island,"island","NULL")))+
        geom_boxplot()+
        facet_wrap(~species)
    }
    
  
  })
}
shinyApp(ui,server)


