#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(visNetwork)
library(DT)
library(shinythemes)
library(shinyBS)

spells_comb <- readRDS("spells_comb2.rds")
spells2 <- sort(spells_comb$Spell)
spells2 <- unique(spells2)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Discover Harry Potter", theme = shinytheme("flatly"),
                  
  
  tabPanel("Dumbledore's Army Network",
           bsModal("id1", "Welcome to the Wizarding World of Harry Potter", "open", size = "large",
                   fluidRow(column(12, align = "center", img(src = "Marauder.png", height = "600px"))),
                   fluidRow(column(12, align = "center", actionButton("no_good", "I solemnly swear that I’m up to no good"))),
                   tags$head(tags$style("#id1 .modal-footer{ display:none}"))
           ),
           sidebarPanel(
             h4("Choose how to visualize network:"),
             sliderInput("network_strength", "Strength of tie cutoff:",
                         min = 1, max = 5, value = 4, ticks = TRUE, step = 1
             ),
             HTML(
               "<br>
               <br>"
             ),
             selectInput("node_sizer", "Size nodes by measure:", choices = c("None", "Strength","Degree",
                                                                             "Closeness", "Betweenness",
                                                                             "Eigenvector")),
             HTML("<br>
                  <br>"),
             textOutput("network_def_da"),
             HTML("<br>"),
             h4("Network Topography Measures"),
             textOutput("node_count"),
             textOutput("edge_count"),
             HTML("<br>"),
             textOutput("avg_deg"),
             textOutput("avg_path_length"),
             HTML("<br>"),
             textOutput("node_centz")
           ),
           mainPanel(
             tabsetPanel(type = "tabs",
                         tabPanel("Network",
                                  visNetworkOutput("da_net", height = "600px")),
                         tabPanel("Table",
                                  DT::dataTableOutput("da_table")
                                  )
             )
                         
             
             )
           ),
  tabPanel("Deatheaters' Network",
           sidebarPanel(
             h4("Choose how to visualize network:"),
             sliderInput("network_strength_death", "Strength of tie cutoff:",
                         min = 1, max = 5, value = 4, ticks = TRUE, step = 1
             ),
             HTML(
               "<br>
               <br>"
             ),
             selectInput("node_sizer_death", "Size nodes by measure:", choices = c("None", "Strength", "Degree",
                                                                             "Closeness", "Betweenness",
                                                                             "Eigenvector")),
             HTML("<br>
                  <br>"),
             textOutput("network_def_death"),
             HTML("<br>"),
             h4("Network Topography Measures"),
             textOutput("node_count_death"),
             textOutput("edge_count_death"),
             HTML("<br>"),
             textOutput("avg_deg_death"),
             textOutput("avg_path_length_death"),
             HTML("<br>"),
             textOutput("node_centz_death")
             ),
           mainPanel(
             tabsetPanel(type = "tabs",
                         tabPanel("Network",
                                  visNetworkOutput("death_net", height = "600px")),
                         tabPanel("Table",
                                  DT::dataTableOutput("death_table")
                         )
             )
             
             
           )
           ),
  tabPanel("Geospatial Events",
           mainPanel(
             leafletOutput("da_geo", height = "700px"))
  )
                  
))
