
shinyUI(
  navbarPage("Discover Harry Potter",
             theme = shinytheme("flatly"),
             # Start - Dumbledore's Army Network -------------------------------
             tabPanel("Dumbledore's Army Network",
                      bsModal("id1",
                              "Welcome to the Wizarding World of Harry Potter",
                              "open",
                              size = "large",
                              fluidRow(
                                column(12,
                                       align = "center",
                                       img(src    = "Marauder.png",
                                           height = "600px"
                                       )
                                )
                              ),
                              tags$br(),
                              fluidRow(
                                column(
                                  12,
                                  align = "center",
                                  actionButton("no_good",
                                               "I solemnly swear that I’m up to no good"
                                  )
                                )
                              ),
                              tags$head(
                                tags$style("#id1 .modal-footer{ display:none}")
                              )
                      ),
                      sidebarPanel(
                        tags$h4("Choose how to visualize network:"),
                        sliderInput(inputId = "network_strength",
                                    label   = "Strength of tie cutoff:",
                                    min     = 1,
                                    max     = 5,
                                    value   = 4,
                                    ticks   = TRUE,
                                    step    = 1
                        ),
                        tags$br(),
                        tags$br(),
                        selectInput(inputId = "node_sizer",
                                    label   = "Size nodes by measure:",
                                    choices = c("None",
                                                "Wizard Strength",
                                                "Degree",
                                                "Closeness",
                                                "Betweenness",
                                                "Eigenvector")
                        ),
                        tags$br(),
                        tags$br(),
                        textOutput("network_def_da"),
                        tags$br(),
                        tags$h4("Network Topography Measures"),
                        textOutput("node_count"),
                        textOutput("edge_count"),
                        tags$br(),
                        textOutput("avg_deg"),
                        textOutput("avg_path_length"),
                        tags$br(),
                        textOutput("node_centz")
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Network",
                                             visNetworkOutput("da_net",
                                                              height = "600px"
                                             )
                                    ),
                                    tabPanel("Table",
                                             DT::dataTableOutput("da_table")
                                    )
                        )
                      )
             ),
             # Start - Death Eater's Network  -----------------------------------
             tabPanel("Death Eaters' Network",
                      sidebarPanel(
                        tags$h4("Choose whether to include Severus Snape"),
                        radioButtons(inputId = "inc_snape",
                                     label = "Include Severus Snape?",
                                     choices = c("Yes" = "y",
                                                 "No" = "n"),
                                     ),
                        tags$h4("Choose how to visualize network:"),
                        sliderInput(inputId = "network_strength_death",
                                    label   = "Strength of tie cutoff:",
                                    min     = 1,
                                    max     = 5,
                                    value   = 4,
                                    ticks   = TRUE,
                                    step    = 1
                        ),
                        tags$br(),
                        tags$br(),
                        selectInput(inputId = "node_sizer_death",
                                    label   = "Size nodes by measure:",
                                    choices = c("None",
                                                "Wizard Strength",
                                                "Degree",
                                                "Closeness",
                                                "Betweenness",
                                                "Eigenvector")),
                        tags$br(),
                        tags$br(),
                        textOutput("network_def_death"),
                        tags$br(),
                        tags$h4("Network Topography Measures"),
                        textOutput("node_count_death"),
                        textOutput("edge_count_death"),
                        tags$br(),
                        textOutput("avg_deg_death"),
                        textOutput("avg_path_length_death"),
                        tags$br(),
                        textOutput("node_centz_death")
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Network",
                                             visNetworkOutput("death_net",
                                                              height = "600px"
                                             )
                                    ),
                                    tabPanel("Table",
                                             DT::dataTableOutput("death_table")
                                    )
                        )
                      )
             ),
             # Start - Geospatial Events ---------------------------------------
             tabPanel("Geospatial",
                      mainPanel(
                        leafletOutput("da_geo",
                                      height = "800px"),
                        width = 12
                        )
                      )
             )
  )