#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(caret)
library(mltools)
library(data.table)
library(ggplot2)
library(reshape2)
library(plyr)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(plotly)
library(heatmaply)
library(dplyr)
library(tidyr)
library(rsconnect)


# We read the CSV
df <- read_csv("data/mxmh_survey_results.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("Music x Mental Health",
             id="tabs",

    tabPanel("Correlations",
             pageWithSidebar(
               headerPanel("Relationships between two variables"),
               tags$div(style = "margin-top: 20px", sidebarPanel(
                 tags$h3("Correlation between numeric variables"),
                 br(),
                 tags$h4("Choose the variables you wish to study"),
                 br(),
                 # Input for selecting the two variables to compare
                 selectInput(inputId = "var1", label = "Variable 1", choices = names(df)[sapply(df, is.numeric)]),
                 selectInput(inputId = "var2", label = "Variable 2", choices = names(df)[sapply(df, is.numeric)]),
                 actionButton("button1", "Submit")
               )),
               mainPanel(
                 # Output for displaying the correlation between the selected variables
                 tags$div(style = "margin-top: 20px", textOutput("correlation")),
                 tags$div(style = "margin-top: 20px", plotlyOutput("corplot")),
               )
             )
    ),
    
    tabPanel("Bar Chart",
             pageWithSidebar(
               headerPanel("Relationships between Mental Illnesses and subject\'s favorite music genre"),
               tags$div(style = "margin-top: 20px", sidebarPanel(
                 checkboxGroupInput("varsBarChart", "Mental Illnesses to be seen in the bar chart:",
                                    c("Anxiety" = "Anxiety", "Depression" = "Depression", "Insomnia" = "Insomnia", "OCD" = "OCD"),
                                    selected = c("Anxiety")))
               ),
               mainPanel(
                 # Output for displaying the bar chart of the selected variables
                 plotOutput("plotBarChart")
               )
             )
    ),
    
    tabPanel("Line Chart",
             pageWithSidebar(
               headerPanel("Evolution of Mental Illnesses based on subject\'s age"),
               tags$div(style = "margin-top: 20px", sidebarPanel(
                 checkboxGroupInput("lineChartOption", "Method to summarise the data:",
                                    c("Sum" = "Sum", "Mean" = "Mean"),
                                    selected = c("Sum", "Mean")),
                 checkboxGroupInput("varsLineChart", "Mental Illnesses to include in the evolution line chart:",
                                    c("Anxiety" = "Anxiety", "Depression" = "Depression", "Insomnia" = "Insomnia", "OCD" = "OCD"),
                                    selected = c("Anxiety")))
               ),
               mainPanel(
                 # Output for displaying the line chart of the selected variables
                 plotOutput("plotLineChart")
               )
             )
    ),
    
    tabPanel("Heat Map",
             pageWithSidebar(
               headerPanel("Distribution of Mental Illnesses in subjects"),
               tags$div(style = "margin-top: 20px", sidebarPanel(
                 checkboxInput("check", "Create Heatmap with Favorite Genre and different Mental Illnesses"),
                 conditionalPanel(
                   condition = "input.check == false",
                   # Input for selecting the two variables to compare
                   selectInput(inputId = "var3", label = "Categorical Variable", choices = names(df)[!sapply(df, is.numeric) & !(names(df) %in% "Timestamp")]),
                   selectInput(inputId = "var4", label = "Variable to aggregate 1", choices = names(df)[sapply(df, is.numeric)]),
                   selectInput(inputId = "var5", label = "Variable to aggregate 2", choices = names(df)[sapply(df, is.numeric)]),
                   selectInput(inputId = "var6", label = "Variable to aggregate 3", choices = names(df)[sapply(df, is.numeric)]),
                   selectInput(inputId = "var7", label = "Variable to aggregate 4", choices = names(df)[sapply(df, is.numeric)]),
                 ),
                 actionButton("button3", "Submit")
               )),
               mainPanel(
                 # Output for displaying the heatmap of the selected aggregated variables
                 tags$div(style = "width:100%; height:100%;", plotlyOutput("heatmap"))
               )
             )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  # Remove empty rows from the data frame
  print("Removing empty rows")
  df <- df[rowSums(is.na(df)) == 0,]

  observeEvent(input$button1, {
    
    col1 <- df[, input$var1]
    col2 <- df[, input$var2]

    print(is.numeric(as.data.frame(col1)[[1]]))
    print(is.numeric(as.data.frame(col2)[[1]]))
    print(class(as.data.frame(col1)[[1]]))
    print(class(as.data.frame(col2)[[1]]))

    if(!is.numeric(as.data.frame(col1)[[1]]) && is.numeric(as.data.frame(col2)[[1]])){
      print("Boxplot of col2 grouped by col1")
      output$corplot <- renderPlotly({
        boxplot(unlist(col2) ~ unlist(col1), xlab = input$var1, ylab = input$var2, main = paste("Boxplot of", input$var2, "grouped by", input$var1))
      })
    }

    if(!is.numeric(as.data.frame(col2)[[1]]) && is.numeric(as.data.frame(col1)[[1]])){
      print("Boxplot of col1 grouped by col2")
      output$corplot <- renderPlotly({
        boxplot(unlist(col1) ~ unlist(col2), xlab = input$var2, ylab = input$var1, main = paste("Boxplot of", input$var1, "grouped by", input$var2))
      })
    }

    if(is.numeric(as.data.frame(col1)[[1]]) && is.numeric(as.data.frame(col2)[[1]])){
      # Calculate the correlation between the selected variables
      print("Calculating correlation")
      cor_val <- cor(col1, col2)
      print(cor_val)
      print("Correlation between col1 and col2")
      output$correlation <- renderText({
        # Return the value as a character string
        return(paste("Correlation between", input$var1, "and", input$var2, ":", as.character(cor_val)))
      })
      output$corplot <- renderPlotly({
        print("Scatter plot of col1 vs col2")
        ggplot(data = df, aes(x = unlist(col2), y = unlist(col1))) +
          geom_point() +
          xlab(input$var2) +
          ylab(input$var1)
      })
    }
    
  })
  
  
  observeEvent(input$varsBarChart, {
    
    df_sum <- df %>%
      group_by(`Fav genre`) %>%
      summarise(Anxiety = sum(Anxiety),
                Depression = sum(Depression),
                Insomnia = sum(Insomnia),
                OCD = sum(OCD))

    filtered_data <- reactive({
      df_sum[, c("Fav genre", input$varsBarChart)]
    })

    filtered_data_long <- filtered_data() %>%
      select(`Fav genre`, input$varsBarChart) %>%
      gather(key = "Legend", value = "Computed Illness Degree", -`Fav genre`)
    
    filtered_data_ord <- filtered_data_long %>%
      arrange(desc(`Computed Illness Degree`))
    
    output$plotBarChart <- renderPlot({
      ggplot(filtered_data_ord, aes(x = reorder(`Fav genre`, -`Computed Illness Degree`), y = `Computed Illness Degree`, fill = Legend)) +
        geom_bar(stat = "identity", position = "stack") +
        xlab("Fav genre")
    })
    
  })
  
  
  observeEvent(input$varsLineChart, {
    observeEvent(input$lineChartOption, {
      
      if("Sum" %in% input$lineChartOption & "Mean" %in% input$lineChartOption){
        print("Both checks pressed")
        
        df_sum <- df %>%
          group_by(Age) %>%
          summarise(Anxiety = sum(Anxiety),
                    Depression = sum(Depression),
                    Insomnia = sum(Insomnia),
                    OCD = sum(OCD))
        
        df_mean <- df %>%
          group_by(Age) %>%
          summarise(Anxiety = mean(Anxiety),
                    Depression = mean(Depression),
                    Insomnia = mean(Insomnia),
                    OCD = mean(OCD))
        
        filtered_data_sum <- reactive({
          df_sum[, c("Age", input$varsLineChart)]
        })
        
        filtered_data_mean <- reactive({
          df_mean[, c("Age", input$varsLineChart)]
        })
        
        filtered_data_long_mean <- filtered_data_mean() %>% 
          select(Age, input$varsLineChart) %>% 
          gather(key = "Legend", value = "Computed Illness Degree", -Age)
        
        filtered_data_long_sum <- filtered_data_sum() %>% 
          select(Age, input$varsLineChart) %>% 
          gather(key = "Legend", value = "Computed Illness Degree", -Age)
        
        output$plotLineChart <- renderPlot({
          plots <- list()
          
          plots[[1]] <- ggplot(filtered_data_long_sum, aes(x = Age, y = `Computed Illness Degree`, color = Legend)) +
            geom_line(size = 1) + geom_point(size = 2) + stat_summary(fun.y = mean, geom = "line", aes(group = 1)) +
            labs(title = "Sum method")
          
          plots[[2]] <- ggplot(filtered_data_long_mean, aes(x = Age, y = `Computed Illness Degree`, color = Legend)) +
            geom_line(size = 1) + geom_point(size = 2) + stat_summary(fun.y = mean, geom = "line", aes(group = 1)) +
            labs(title = "Mean method")
          
          gridExtra::grid.arrange(grobs = plots)
        })
      }
      else if("Sum" %in% input$lineChartOption | "Mean" %in% input$lineChartOption){
        print("One check pressed")
        if("Sum" %in% input$lineChartOption){
          print("Sum check pressed")
          df_filtered <- df %>%
            group_by(Age) %>%
            summarise(Anxiety = sum(Anxiety),
                      Depression = sum(Depression),
                      Insomnia = sum(Insomnia),
                      OCD = sum(OCD))
        }
        if("Mean" %in% input$lineChartOption){
          print("Mean check pressed")
          df_filtered <- df %>%
            group_by(Age) %>%
            summarise(Anxiety = mean(Anxiety),
                      Depression = mean(Depression),
                      Insomnia = mean(Insomnia),
                      OCD = mean(OCD))
        }
        
        filtered_data <- reactive({
          df_filtered[, c("Age", input$varsLineChart)]
        })
        
        filtered_data_long <- filtered_data() %>% 
          select(Age, input$varsLineChart) %>% 
          gather(key = "Legend", value = "Computed Illness Degree", -Age)
        
        output$plotLineChart <- renderPlot({
          ggplot(filtered_data_long, aes(x = Age, y = `Computed Illness Degree`, color = Legend)) +
            geom_line(size = 1) + geom_point(size = 2) + stat_summary(fun.y = mean, geom = "line", aes(group = 1))
        })
      }
      
    })
  })
  
  
  observeEvent(input$button3, {
    
    if(isTRUE(input$check)){
      df2 <- df %>% select("Fav genre", "Anxiety", "Depression", "Insomnia", "OCD")
      print("Aggregating variables")
      df_agg <- aggregate(cbind(Anxiety, Depression, Insomnia, OCD) ~ `Fav genre`, df2, sum)
      print("Heatmap")
      output$heatmap <- renderPlotly({
        heatmaply(df_agg)
      })
    }
    else{
      df2 <- df %>% select(input$var3, input$var4, input$var5, input$var6, input$var7)
      col_labels <- c(input$var4, input$var5, input$var6, input$var7)
      print(input$var3)
      print(input$var4)
      print("Aggregating selected variables")
      df_agg <- aggregate(cbind(unlist(df2[,input$var4]), unlist(df2[,input$var5]), unlist(df2[,input$var6]), unlist(df2[,input$var7])) ~ unlist(df2[,input$var3]), df2, sum)
      colnames(df_agg)[1] <- input$var3
      print("Heatmap")
      output$heatmap <- renderPlotly({
        heatmaply(df_agg, labCol = col_labels)
      })
    }
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)