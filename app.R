#Practicing building Shiny apps
#tutorial: https://vimeo.com/356696243

#NOTE: YOU CANNOT HAVE ANYTHING BELOW "shinyApp(ui, server)"
#OTHERWISE YOU WILL NOT BE ABLE TO RUN THE APP WITH 
# runApp() IN THE CONSOLE (MAKE SURE IT'S SAVED WITH NOTHING BELOW)

#step 1 - create R script in project called app.R

n <- peng %>% 
  filter(species == "Adelie") %>% 
  dplyr::summarize(Mean = mean(bill_length_mm, na.rm=TRUE))

# shiny widget gallery
# https://shiny.rstudio.com/gallery/widget-gallery.html

# shiny theme options
# https://rstudio.github.io/shinythemes/

# ui <- fluidPage for one page
# don't use pound signs to indicate header, use h1, h2, h3, p, etc.

#create app with palmer penguins dataset

library(shiny)
library(palmerpenguins)
library(tidyverse)
library(shinythemes)

peng <- palmerpenguins::penguins


ui <- navbarPage("Haley's navigation bar!",
                 theme = shinytheme("united"),
                 tabPanel("First tab",
                          h1("Some giant text"),
                          p("Here is some regular text..."),
                          plotOutput(outputId = "penguin_plot1")),
                 tabPanel("Second tab",
                          sidebarLayout(
                            sidebarPanel("Put widgets here",
                                         radioButtons(inputId = "species",
                                                      label = "Choose a species:",
                                                      choices = c("Adelie", "Gentoo", "Awesome Chinstrap" = "Chinstrap")),
                                         selectInput(inputId = "pt_color", 
                                                     label = "Select a point color",
                                                     choice = c("Favorite RED!" = "red",
                                                                "pretty purple" = "purple",
                                                                "ORAANNNGGEEE!" = "orange"))),
                            mainPanel("Scatterplot of flipper length vs body mass", 
                                      plotOutput(outputId = "penguin_plot2"))
                          )),
                 tabPanel("Third tab",
                          sidebarLayout(
                            sidebarPanel("More text for sidebars",
                            checkboxGroupInput(inputId = "Island",
                                               label = "Choose some islands",
                                               choices = c(levels(peng$island)))),
                            mainPanel("Main panel text",
                                      plotOutput(outputId = "penguin_plot3"))
                          ))
                 )


server <- function(input, output) {

  output$penguin_plot1 <- renderPlot({
    ggplot(data = peng, aes(x = bill_length_mm, y = body_mass_g)) +
      geom_point(aes(color = species))
  })
  
  # Create a reactive df
  penguin_select <- reactive({
    peng %>% 
      filter(species == input$species)
  })
  # a plot based on reactive df (what the user chooses)
  output$penguin_plot2 <- renderPlot({
    ggplot(data = penguin_select(), aes(x = flipper_length_mm, y = body_mass_g)) +
      geom_point(color = input$pt_color)
  })
  
  #next plot for tab 3
  island_select <- reactive({
    peng %>% 
      filter(island %in% input$Island)
  })
  output$penguin_plot3 <- renderPlot({
    ggplot(data = island_select(), aes(x = island, y = body_mass_g)) +
      geom_violin(aes(fill = island))
  })
}

shinyApp(ui, server)


