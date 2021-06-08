library(shinydashboard)

nyc_prop_for_EDA <- read.csv(file = "./nyc_prop_for_EDA.csv")

dashboardPage(
    dashboardHeader(title = "NYC Profitable Property Analysis"),
    dashboardSidebar(
        sidebarUserPanel(
            "Nixon Lim"
        ), 
        sidebarMenu(
            menuItem("Introduction", tabName = "intro"), 
            menuItem("Data Cleaning", tabName = "data_cleaning"), 
            menuItem("Basic Analysis", tabName = "basic_analysis"), 
            menuItem("Numerical Plots", tabName = "numericalplots", icon = icon("map")),
            menuItem("Categorical Data", tabName = "categoricaldata", icon = icon("database")), 
            menuItem("Conlclusion", tabName = "conclusion"), 
            menuItem("Future Work", tabName = "futurework")
        )
        
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "intro", 
                verbatimTextOutput("introtext")
                ),
            tabItem(
                tabName = "data_cleaning", 
                verbatimTextOutput("cleaningtext")
            ),
            tabItem(
                tabName = "basic_analysis", 
                verbatimTextOutput("basictext")
            ),
            tabItem(
                tabName = "numericalplots",
                selectizeInput(
                    inputId = "numvariable",
                    label = "Good Price vs Variable",
                    choices = names(nyc_prop_for_EDA %>% select(., contains("ratio_")))), 
                fluidRow(
                    column(6, plotOutput("boxplot")),
                    column(6, plotOutput("density"))
                    )
            ), 
            tabItem(
                tabName = "categoricaldata",
                # selectizeInput(
                #     inputId = "catvariable",
                #     label = "Good Price vs Variable",
                #     choices = names(nyc_prop_for_EDA %>%
                #         select(., borough, neighborhood, building_class_category, zip_code))),
                tabsetPanel(
                    tabPanel(
                        "Plots",
                        plotOutput("neighborhood_p"), 
                        plotOutput("building_class_at_present_p"),
                        plotOutput("zip_code_p")
                    ),
                    tabPanel(
                        "Tables",
                        tableOutput("neighborhood_t"), 
                        tableOutput("building_class_at_present_t"),
                        tableOutput("zip_code_t")
                    )
                )

            ), 
            tabItem(
                tabName = "conclusion", 
                verbatimTextOutput("conclu")
            ), 
            tabItem(
                tabName = "futurework", 
                verbatimTextOutput("futwork")
            )
        )
    )
)

# shinyUI(dashboardPage(
#     dashboardHeader(),
# 
#     dashboardSidebar(
#             selectizeInput(
#                 inputId = "variable",
#                 label = "Variable",
#                 choices = names(nyc_prop_for_EDA %>% select(., contains("ratio_"))))
#             ),
#         
#     dashboardBody(
#         tabsetPanel(
            # "Numerical Plots",
            # fluidRow(
            #     column(5, plotOutput("boxplot")),
            #     column(7, plotOutput("density"))
#             #     # textOutput("text"),
#             #     # dataTableOutput("table")
#             )
#         )
#     )
# ))

# flights <- read.csv(file = "./flights14.csv")
# fluidPage(
#     titlePanel("NYC Flights 2014"),
#     sidebarLayout(
#         sidebarPanel(
#             selectizeInput(inputId = "origin",
#                            label = "Departure airport",
#                            choices = unique(flights$origin)),
#             selectizeInput(inputId = "dest",
#                            label = "Arrival airport",
#                            choices = unique(flights$dest))
#         ),
#         mainPanel(plotOutput("count"))
#     )
# )



