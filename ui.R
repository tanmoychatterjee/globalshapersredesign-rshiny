#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyTree)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(gapminder)
library(tidyverse)
library(scales)
library(leaflet)
library(hash)


#initial data exploration


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel(
    fluidRow(
      column(9, "Global Shapers Annual Survey 2016"), 
      column(3, img(height = 105, width = 300, src = "logo.png"))
    )),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 3,
       h2('Filter Criteria'),
       fluidRow(
         #gender
         column(5,checkboxGroupInput(inputId = "gender", label = "Gender:",
                            choices = c("Male" = "Male",
                                        "Female" = "Female",
                                        "Other" = "Other"),
                            selected = c("Male","Female","Other"))),
         # age
         column(5,checkboxGroupInput(inputId = "age", label = "Age Group:",
                            choices = c("18-21" = "18-21",
                                        "22-26" = "22-26",
                                        "27-30" = "27-30",
                                        "31-35" = "31-35"),
                            selected = c("18-21","22-26","27-30", "31-35")))
       ),
       h3('Regions / Sub-regions:'),
       #region - subregion
       shinyTree("regionsubregion", checkbox = TRUE)
    ),
    
        
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Participation Statistics",
                 fluidRow(leafletOutput('myMap')),
                 fluidRow(
                   column(5,plotOutput(outputId = "agedist")),
                   column(6,valueBoxOutput("totalparticipants"),
                   valueBoxOutput("totalcompleted"),
                   valueBoxOutput("numofcountries"),
                   valueBoxOutput("totalmaleparticipants"),
                   valueBoxOutput("totalfemaleparticipants"),
                   valueBoxOutput("totalotherparticipants"),
                   valueBoxOutput("selectedcountries"),
                   #includeHTML("surveymethodology.html"),
                   shinythemes::themeSelector()
                   )
                  
                   
                 )),
        tabPanel("Economy & Global Outlook",
                 tabsetPanel(
                   tabPanel("Who am I, What are the Issues?",
                            fluidRow(
                              column(6,plotOutput(outputId = "identity"))
                            ),
                            fluidRow(
                              column(6,plotOutput(outputId = "issues"))
                            )
                            ),
                   tabPanel("Basic Needs - TBD")
                 )), 
        tabPanel("Technology & Innovation",
                 tabsetPanel(
                   tabPanel("Impact on Life - TBD"),
                   tabPanel("Internet & Latest Technologies",
                            fluidRow(
                              column(12,plotOutput(outputId = "internetdevice"))
                            ),
                            fluidRow(
                              column(12,radioButtons("rb", "Social Media & Productivity:",
                                           choiceNames = list(
                                             HTML("<p style='color:green;'>Improves my productivity</p>"),
                                             HTML("<p style='color:orange;'>Does not affect my productivity</p>"),
                                             HTML("<p style='color:red;'>Hinders my productivity</p>")
                                           ),
                                           choiceValues = list(
                                             "Improves my productivity", "Does not affect my productivity", "Hinders my productivity"
                                           ),inline = TRUE),
                              plotOutput(outputId = "socialmedia2"),
                              plotOutput(outputId = "socialmedia"))
                              ))
                 )),
        tabPanel("Values & Society",
                 tabsetPanel(
                   tabPanel("Views about Women",
                            column(12,plotOutput(outputId = "viewsaboutwomen"))
                              
                            )
                   )
                 ),
        tabPanel("Governance & Civic Engagement - TBD")
    )
  )
)
))

