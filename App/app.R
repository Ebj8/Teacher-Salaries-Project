#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(sf)
library(USAboundaries)
library(shiny)
library(tidyverse)
library(scales)
library(shinydashboard)
library(plotly)
library(leaflet)
library(maps)
library(shinyWidgets)


# Loading Data Sets -------------------------------------------------------



teacher_dataset <- read_csv("teacher_dataset.csv") %>%
    mutate(region_color = case_when(
        Region == "East North Central" ~ "#118DFF",
        Region == "East South Central" ~ "#750985",
        Region == "Middle Atlantic" ~ "#C83D95",
        Region == "Mountain" ~ "#FF985E",
        Region == "New England" ~ "#1DD5EE",
        Region == "Pacific" ~ "#217C60",
        Region == "South Atlantic" ~ "#42F7C0",
        Region == "West North Central" ~ "#F8727D",
        Region == "West South Central" ~ "#3049AD"
    ),
    common_core_color = case_when(
        adopted_common_core == TRUE ~ "#42F7C0",
        adopted_common_core == FALSE ~ "#F64F5C"
    ))

states_geometry <- us_states() %>%
    select(6, 13) %>%
    rename(State = name) %>%
    filter(State != "Puerto Rico")

teacher_dataset2 <- left_join(teacher_dataset, states_geometry)

teacher_dataset2 <- sf::st_as_sf(teacher_dataset2)

alaska <- teacher_dataset2 %>%
    filter(State == "Alaska")
    

climate_data <- read_csv("Climate_data.csv") %>%
    select(2:10)

# Region data for region map
region <- teacher_dataset %>%
    group_by(Region, region_color) %>%
    summarise()

# Linear Regression for starting salary vs cost of living
mylm <- lm(starting_salary ~ Rank, data = teacher_dataset)


# Begin UI ----------------------------------------------------------------

ui <- dashboardPage(
    dashboardHeader(title = "Where to Teach"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Cost of Living vs. Salary", tabName = "costVSsalary", icon = icon("dollar-sign")),
            menuItem("Climate Patterns", tabName = "climatePatterns", icon = icon("sun")),
            menuItem("Crime Levels", tabName = "crimeLevels", icon = icon("angry"))
        )
    ),
    dashboardBody(
        tabItems(
            
            # Home Page ---------------------------------------------------------------
            
            tabItem(tabName = "home",
                    h1("Where to Live as a Teacher in the USA"),
                    h3("Introduction"),
                    box(solidHeader = TRUE, status = "primary",
                        "One of the biggest benefits of being a teacher in the US is that there are 
                    jobs everywhere. At the same time this presents a challenge; ",
                        strong("where should I choose to teach?"), " This app is designed to help prospective teachers make an 
                    educated decision as to where they should apply to teach after graduation. 
                    The app takes in your preferences and presents you with states to teach in 
                    that fit your criteria. A brief disclaimer first though. The data in this app
                    currently exists only at the state level. Many of these statistics will vary 
                    between different areas of a state. For example, the cost of living in the
                    state of Washington might be skewed by the Seattle area where it is incredibly
                    expensive to live when compared to Eastern Washington areas like Spokane. This
                    app is only designed to help users make an educated decision, but not ultimately
                    make the final decision for the user.",
                        width = 12),
                    h3("How to Use this App"),
                    box(solidHeader = TRUE, status = "primary",
                        "This app contains data from all 50 states plus the District of Columbia. You are
                    welcome to try to compare all fifty states against one another, but will likely 
                    find it hard to track so many states at once. Use this
                    page to ", strong("select your preferences"), "and filter out some states you definitely don't 
                    want to work in. Go through each section and declare some
                    preferences. These ", strong("filters will alter the rest of this app"), ", but they can be changed later
                    if you would like. Use the table below to see how many states are left after
                    filtering out some preferences.",
                        width = 12),
                    fluidRow(
                        box(title = "State", solidHeader = TRUE, status = "primary",
                            box(solidHeader = TRUE,
                                pickerInput("stateInputHome", "State",
                                            choices = teacher_dataset$State, multiple = TRUE,
                                            selected = teacher_dataset$State,
                                            options = list(`actions-box` = TRUE, liveSearch = TRUE)),
                                width = 2),
                            box(solidHeader = TRUE,
                                tableOutput("homeTable"),
                                width = 8),
                            valueBoxOutput("countOfStatesBox", 
                                           width = 2),
                            width = 12
                        )
                    ), br(),
                    #Region
                    fluidRow(
                        box(title = "Region", solidHeader = TRUE, status = "primary",
                            box(
                                "This slicer helps you filter out states that you don't want to live in due to",
                                strong("geographic restricitons."), "The most common situation is that you want to filter 
                    out states that are far from the rest of your family. For example, if your 
                    family lived in Utah and you wanted to only teach somewhere close to home, 
                    you would select the Mountain region. Regions are decided based off of how 
                    the", 
                                a(href = "https://github.com/cphalpert/census-regions/blob/master/us%20census%20bureau%20regions%20and%20divisions.csv",
                                  "US census categorizes"),
                                "states into regions.",
                                width = 3, solidHeader = TRUE, status = "primary"),
                            box(leafletOutput("regionMap"), width = 6, solidHeader = TRUE),
                            box(pickerInput("regionInputHome", "Region",
                                            choices = region$Region, multiple = TRUE,
                                            selected = region$Region,
                                            options = list(`actions-box` = TRUE)),
                                width = 3, solidHeader = TRUE),
                            width = 12
                        )
                    ), br(),
                    
                    # Common Core
                    fluidRow(
                        box(title = "Common Core", solidHeader = TRUE, status = "primary",
                            box("Many people have opinions on the common core. Some like it,
                                others don't. This slicer allows you to filter by states that
                                have either", strong("accepted or rejected"), "the common core.
                                For example, if you really disliked the common core, then you
                                could select \"FALSE\" to see only states where the common
                                core hasn't been adopted. This data comes from the",
                                a(href = "http://www.corestandards.org/standards-in-your-state/",
                                  "common core program's website."),
                                width = 3, solidHeader = TRUE, status = "primary"),
                            box(leafletOutput("commonCoreMap"), width = 6, solidHeader = TRUE),
                            box(pickerInput("commonCoreInputHome", "Adopted Common Core",
                                            choices = c(TRUE, FALSE),
                                            multiple = TRUE,
                                            selected = c(TRUE, FALSE),
                                            options = list(`actions-box` = TRUE)),
                                width = 3, solidHeader = TRUE),
                            width = 12
                        )
                    ),
                    
                    # Salary
                    h2("Salary"),
                    
                    fluidRow(
                        box("For most people, ", strong("salary"), "is the leading factor when determining where to
                        teach. Regardless of cost of living, many simply don't want to bother working
                        for a salary that is", strong("too little"), ". In the odd case that you also
                        don't want to get paid too much, we have included the option to filter by 
                        salary in either direction. This salary data comes from ",
                            a(href = "https://nces.ed.gov/programs/digest/d17/tables/dt17_211.60.asp",
                              "the National Center for Education Statistics"), " and ",
                            a(href = "http://www.nea.org/home/2017-2018-average-starting-teacher-salary.html",
                              "the National Education Association"), 
                            ". The average salary data is from the 2016-2017 school year and the average
                        starting salary data comes from the 2017-2018 school year.",
                            width = 3, solidHeader = TRUE, status = "primary"),
                        box(title = "Salary Preferences", solidHeader = TRUE, status = "primary",
                            box(solidHeader = TRUE,
                                sliderInput("startingSalaryInputHome", "Average Starting Salary",
                                            min = 31417, max = 55210,
                                            value = c(31417, 55210), pre = "$"),
                                width = 6),
                            box(solidHeader = TRUE,
                                sliderInput("averageSalaryInputHome", "Average Salary",
                                            min = 42667, max = 79638,
                                            value = c(42667, 79638), pre = "$"),
                                width = 6),
                            width = 9)
                    ),
                    # Climate
                    h2("Climate"),
                    fluidRow(
                        
                        box(title = "Climate Preferences", solidHeader = TRUE, status = "primary",
                            box(solidHeader = TRUE,
                                sliderInput("highTempInputHome", "Max High Temperature in a Month",
                                            min = 65, max = 106,
                                            value = c(65, 106)),
                                width = 3),
                            box(solidHeader = TRUE,
                                sliderInput("lowTempInputHome", "Min Low Temperature in a Month",
                                            min = 0, max = 66,
                                            value = c(0, 66)),
                                width = 3),
                            box(solidHeader = TRUE,
                                sliderInput("maxPrecipitationInputHome", "Max Precipitation in a Month",
                                            min = 1.06, max = 11.46,
                                            value = c(1.06, 11.46)),
                                width = 3),
                            box(solidHeader = TRUE,
                                sliderInput("minPrecipitationInputHome", "Min Precipitation in a Month",
                                            min = 0.04, max = 5.79,
                                            value = c(0.04, 5.79)),
                                width = 3),
                            width = 9),
                        box("Climate is an important part of where people live. Some prefer it ", 
                            strong("hot"), " while others prefer it ", strong("cold"), ". Additionally,
                                you can set ", strong("precipitation"), " preferences based off of
                                precipitation in inches. Use these
                                filters to narrow your potential states according to the climate you want to
                                live in. This data comes from ",
                            a(href = "https://www.usclimatedata.com/climate/united-states/us",
                              "usclimatedata.com"), ".",
                            width = 3, solidHeader = TRUE, status = "primary"
                        )
                    ),
                    # Crime
                    h2("Crime"),
                    fluidRow(
                        box(
                            "This last section is for filtering out states with too much ",
                            strong("crime"), ". Or you can filter down to just states with 
                                a lot of crime if you like to live dangerously. Although you may
                                not need it, this app gives you the ability to filter by many 
                                different types of crime as reported by the ",
                            a(href = "https://crime-data-explorer.fr.cloud.gov/downloads-and-docs",
                              "FBI"), " such as burglaries, motor vehicle thefts, and homicides.
                            The data shows reports of this crime per 100,000 people in the state.",
                            solidHeader = TRUE, width = 3, status = "primary"
                        ),
                        box(title = "Crime Preferences", solidHeader = TRUE, status = "primary",
                            box(solidHeader = TRUE,
                                sliderInput("totalCrimeInputHome", "Total Crimes per 100k",
                                            min = 3161.00, max = 10577.67,
                                            value = c(3161.00, 10577.67)),
                                sliderInput("larcenyInputHome", "Larcenies per 100k",
                                            min = 1077.95, max = 3650.44,
                                            value = c(1077.95, 3650.44)),
                                sliderInput("homicideInputHome", "Homicides per 100k",
                                            min = 1.03, max = 16.73,
                                            value = c(1.03, 16.73)),
                                width = 3),
                            box(solidHeader = TRUE,
                                sliderInput("propertyCrimeInputHome", "Property Crimes per 100k",
                                            min = 1381.81, max = 4283.89,
                                            value = c(1381.81, 4283.89)),
                                sliderInput("motorTheftInputHome", "Motor Vehicle Thefts per 100k",
                                            min = 31.10, max = 575.57,
                                            value = c(31.10, 575.57)),
                                sliderInput("robberyInputHome", "Robberies per 100k",
                                            min = 11.41, max = 377.98,
                                            value = c(11.41, 377.98)),
                                
                                
                                width = 3),
                            box(solidHeader = TRUE,
                                sliderInput("violentCrimeInputHome", "Violent Crimes per 100k",
                                            min = 121.04, max = 1004.94,
                                            value = c(121.04, 1004.94)),
                                sliderInput("burglaryInputHome", "Burglaries per 100k",
                                            min = 176.33, max = 858.08,
                                            value = c(176.33, 858.08)),
                                width = 3),
                            box(solidHeader = TRUE,
                                sliderInput("rapeInputHome", "Rapes per 100k",
                                            min = 16.71, max = 116.66,
                                            value = c(16.71, 116.66)),
                                sliderInput("aggravatedAssaultInputHome", "Aggravated Assaults per 100k",
                                            min = 65.26, max = 575.44,
                                            value = c(65.26, 575.44)),
                                width = 3),
                            width = 9
                            
                        )
                    )
            ),
            
            
            # Cost vs Salary Page -----------------------------------------------------
            
            tabItem(tabName = "costVSsalary",
                    fluidRow(
                        column(width = 2,
                               box(pickerInput("stateInputCost", "State",
                                               choices = teacher_dataset$State, multiple = TRUE,
                                               selected = teacher_dataset$State,
                                               options = list(`actions-box` = TRUE)),
                                   sliderInput("startingSalaryInput", "Average Starting Salary",
                                               min = 31418, max = 55209,
                                               value = c(31418, 55209), pre = "$"),
                                   sliderInput("costOfLivingInput", "Cost of Living Index",
                                               min = 86.10, max = 192.90,
                                               value = c(86.10, 192.90)),
                                   width = NULL, solidHeader = TRUE, status = "primary",
                                   title = "Filters")
                        ),
                        column(width = 8,
                               h1("Cost of Living vs. Salary"), br(), br(),
                               box(
                                   "The most important factor in determining where to teach is salary.
                                   Salary should always be observed against the cost of living however. 
                                   If I get paid $100,000 a month, but rent is $97,000 a month, I am no
                                   better off than if I got paid $5,000 a month and rent was $2,000 a 
                                   month. Use this page to make an educated decision of salary vs. cost
                                   of living. Note that the cost of living index has no dollar value
                                   assigned to it, so it may be hard to compare salary to cost of living
                                   directly. The salary data come from ",
                                   a(href ="http://www.nea.org/home/2017-2018-average-starting-teacher-salary.html",
                                     "the National Education Association"), " and ",
                                   a(href = "https://nces.ed.gov/programs/digest/d17/tables/dt17_211.60.asp",
                                     "the National Center for Education Statistics"), ". The index data
                                   comes from ", a(href = "https://www.missourieconomy.org/indicators/cost_of_living/",
                                                   "the Missouri Economic Research and Information Center"),
                                   ". The starting salary data comes from the 2017-2018 school year, the average salary
                                   data comes from the 2016-2017 school year, and the index data comes from the
                                   first quarter of 2019.",
                                   width = NULL, solidHeader = TRUE, status = "primary"
                               ), br(), br(),
                               
                               fluidRow(
                                   box(
                                       "This chart helps you get a general idea of which states pay the
                                       most with respect to the cost of living within that state. As 
                                       mentioned above though, there is no one to one connection between 
                                       salary and cost of living. This trend line does not necessarilly mean that 
                                       your spending power is the same in one state as it is in another state 
                                       if they both are on the line. It is just there to give a frame of reference as to 
                                       which states are better to live in compared to others.",
                                       solidHeader = TRUE, width = 3, status = "primary"
                                   ),
                                   box(solidHeader = TRUE, status = "primary",
                                       width = 9, 
                                       title = "Average Starting Salary vs. Cost of Living by State",
                                       plotlyOutput("startingVSCostOfLiving"))
                                   ), br(), br(),
                               
                               fluidRow(
                                   box(solidHeader = TRUE, width = 9,
                                   box(solidHeader = TRUE, status = "primary",
                                       width = 12,
                                       title = "Average Starting Salary by State",
                                       plotlyOutput("startingSalaryPlot")),
                                   box(solidHeader = TRUE, status = "primary",
                                       width = 12,
                                       title = "Cost of Living Index by State",
                                       plotlyOutput("costOfLivingPlot"))
                                   ),
                                   box(
                                       "These two plots are meant to go together. The best states, in terms
                                       of spending power, are those that are more highly rated in salary than
                                       they are in the cost of living index. Basically, if a country is higher
                                       up on the \"Average Starting Salary by State\" graph, then you want it
                                       to show up lower on the \"Cost of Living Index by State\" plot.",
                                       solidHeader = TRUE, status = "primary", width = 3
                                   )
                               )
                        )
                    )
            ),
            
            
            # Climate Patterns Page ---------------------------------------------------
            
            tabItem(tabName = "climatePatterns",
                    fluidRow(
                        column(width = 2,
                               box(title = "Filters", solidHeader = TRUE, status = "primary",
                                   pickerInput("stateInputClimate", "State",
                                               choices = teacher_dataset$State, multiple = TRUE,
                                               selected = teacher_dataset$State,
                                               options = list(`actions-box` = TRUE)),
                                   sliderInput("maxTempInput", "Max. Avg. High Temperature",
                                               min = 64, max = 107,
                                               value = c(64, 107)),
                                   sliderInput("minTempInput", "Min. Avg. Low Temperature",
                                               min = 0, max = 66,
                                               value = c(0, 66)),
                                   sliderInput("maxPrecipitationInput", "Max. Avg. Precipitation",
                                               min = 1.05, max = 11.47,
                                               value = c(1.05, 11.47)),
                                   sliderInput("minPrecipitationInput", "Min. Avg. Precipitation",
                                               min = 0.00, max = 5.80,
                                               value = c(0.00, 5.80)),
                                   width = NULL
                                   
                               )
                        ),
                        column(width = 9,
                               h1("Climate Patterns"), br(), br(),
                               box(solidHeader = TRUE, status = "primary",
                                   "This page is designed to help you get an understanding of the climate
                            in the states that you are investigating. The data, taken from ", 
                            a(href = "https://www.usclimatedata.com/climate/united-states/us",
                            "usclimatedata.com"), ", represents the average 
                            of the reported high temperatures in each state each month. However, the data 
                            does not average out the temperatures from all over the state, but rather 
                            takes the weather data for the capital of each state. Note that not all
                            states had precipitation or hours of sunshine data.",
                                   width = NULL
                               ), br(), br(),
                            
                            
                               box(width = NULL, solidHeader = TRUE,
                                   box(title = "Avg. High Temp. per Month by State",
                                       solidHeader = TRUE, status = "primary",
                                       plotlyOutput("highTempPlot"),
                                       width = 6
                                   ),
                                   box(title = "Avg. Low Temp. per Month by State",
                                       solidHeader = TRUE, status = "primary",
                                       plotlyOutput("lowTempPlot"),
                                       width = 6
                                       
                                   )
                               ), br(), br(),
                            
                            
                               box(title = "Average Precipitation by Month and State",
                                   solidHeader =  TRUE, status = "primary",
                                   plotlyOutput("avgPrecipitationPlot"),
                                   width = NULL
                                   
                               ), br(), br(),
                            
                            
                               box(
                                   title = "Count of Days with Precipitation by Month and State",
                                   solidHeader = TRUE, status = "primary",
                                   plotlyOutput("countDaysPrecipitationPlot"),
                                   width = NULL,
                                   box(solidHeader = TRUE, width = 12,
                                       strong("Note:"), " Data is not available for all states"
                                       )
                               ), br(), br(),
                            
                            
                               box(
                                   title = "Total Hours of Sunshine Each Month by State",
                                   solidHeader = TRUE, status = "primary",
                                   plotlyOutput("hoursSunshinePlot"),
                                   width = NULL,
                                   box(solidHeader = TRUE, width = 12,
                                       strong("Note:"), " Data is not available for all states"
                                   )
                               )
                        )
                    )),
            
            
            # Crime Levels Page -------------------------------------------------------
            
            tabItem(tabName = "crimeLevels",
                    fluidRow(
                        column(width = 2,
                               box(title = "Filters", solidHeader = TRUE, status = "primary",
                                   pickerInput("stateInputCrime", "State",
                                               choices = teacher_dataset$State, multiple = TRUE,
                                               selected = teacher_dataset$State,
                                               options = list(`actions-box` = TRUE)),
                                   sliderInput("totalCrimeInput", "Total Crimes per 100k",
                                               min = 3161.00, max = 10577.68,
                                               value = c(3161.00, 10577.68)),
                                   sliderInput("larcenyInput", "Larcenies per 100k",
                                               min = 1077.95, max = 3650.44,
                                               value = c(1077.95, 3650.44)),
                                   sliderInput("homicideInput", "Homicides per 100k",
                                               min = 1.03, max = 16.73,
                                               value = c(1.03, 16.73)),
                                   sliderInput("propertyCrimeInput", "Property Crimes per 100k",
                                               min = 1381.81, max = 4283.89,
                                               value = c(1381.81, 4283.89)),
                                   sliderInput("motorTheftInput", "Motor Vehicle Thefts per 100k",
                                               min = 31.10, max = 575.57,
                                               value = c(31.10, 575.57)),
                                   sliderInput("robberyInput", "Robberies per 100k",
                                               min = 11.41, max = 377.98,
                                               value = c(11.41, 377.98)),
                                   sliderInput("violentCrimeInput", "Violent Crimes per 100k",
                                               min = 121.04, max = 1004.94,
                                               value = c(121.04, 1004.94)),
                                   sliderInput("burglaryInput", "Burglaries per 100k",
                                               min = 176.33, max = 858.08,
                                               value = c(176.33, 858.08)),
                                   sliderInput("rapeInput", "Rapes per 100k",
                                               min = 16.71, max = 116.66,
                                               value = c(16.71, 116.66)),
                                   sliderInput("aggravatedAssaultInput", "Aggravated Assaults per 100k",
                                               min = 65.26, max = 575.44,
                                               value = c(65.26, 575.44)),
                                   width = NULL
                               )
                        ),
                        column(width = 9,
                               h1("Crime Levels"), br(), br(),
                               box(solidHeader = TRUE, width = NULL, status = "primary",
                                   "This page helps you determine the levels of crime within the states
                        you are investigating. All the data was taken from official reports 
                        on the ", a(href = "https://crime-data-explorer.fr.cloud.gov/downloads-and-docs",
                        "FBI website"), ". The data is taken from 2017. It should be noted 
                        that all of the metrics on this page are measured as instances of 
                        the crime per 100,000 people in the state. This page also suffers 
                        from a similar problem mentioned on the home page; ", 
                                   strong("states with smaller populations are subject to more extreme metrics"),
                                   ". Maybe 
                        Rhode Island has one town that is incredibly dangerous that is 
                        pulling the total number of crimes up significantly in the state 
                        and when it is presented as crimes per 100,000 people, it seems 
                        like a big number. As always, this page should help you get a 
                        general feel for how dangerous a state is, but is not a perfect 
                        representation of it."
                               ), br(), br(),
                        
                        
                               box(title = "Population by State",
                                   solidHeader = TRUE, status = "primary",
                                   plotlyOutput("populationPlot"),
                                   width = NULL
                                   
                               ), br(), br(),
                        
                        
                               box(title = "Total Crimes per 100k by State",
                                   solidHeader = TRUE, status = "primary",
                                   plotlyOutput("totalCrimePlot"),
                                   width = NULL
                                   
                               ), br(), br(),
                        
                        
                               box(title = "Aggravated Assaults per 100k by State",
                                   solidHeader = TRUE, status = "primary",
                                   plotlyOutput("aggravatedAssaultPlot"),
                                   width = NULL
                                   
                               ), br(), br(),
                        
                        
                               box(title = "Burglaries per 100k by State",
                                   solidHeader = TRUE, status = "primary",
                                   plotlyOutput("burglaryPlot"),
                                   width = NULL
                                   
                               ), br(), br(),
                        
                        
                               box(title = "Homicides per 100k by State",
                                   solidHeader = TRUE, status = "primary",
                                   plotlyOutput("homicidePlot"),
                                   width = NULL
                                   
                               ), br(), br(),
                        
                        
                               box(title = "Larcenies per 100k by State",
                                   solidHeader = TRUE, status = "primary",
                                   plotlyOutput("larcenyPlot"),
                                   width = NULL
                                   
                               ), br(), br(),
                        
                        
                               box(title = "Motor Vehicle Thefts per 100k by State",
                                   solidHeader = TRUE, status = "primary",
                                   plotlyOutput("motorTheftPlot"),
                                   width = NULL
                                   
                               ), br(), br(),
                        
                        
                               box(title = "Property Crimes per 100k by State",
                                   solidHeader = TRUE, status = "primary",
                                   plotlyOutput("propertyCrimePlot"),
                                   width = NULL
                                   
                               ), br(), br(),
                        
                        
                               box(title = "Rapes per 100k by State",
                                   solidHeader = TRUE, status = "primary",
                                   plotlyOutput("rapePlot"),
                                   width = NULL
                                   
                               ), br(), br(),
                        
                        
                               box(title = "Robberies per 100k by State",
                                   solidHeader = TRUE, status = "primary",
                                   plotlyOutput("robberyPlot"),
                                   width = NULL
                                   
                               ), br(), br(),
                        
                        
                               box(title = "Violent Crimes per 100k by State",
                                   solidHeader = TRUE, status = "primary",
                                   plotlyOutput("violentCrimePlot"),
                                   width = NULL
                                   
                               )
                        )
                    )
            )
        )
    )
)



# Begin Server ------------------------------------------------------------

server <- function(input, output) {
    
    # The dataset we will use throughout the report
    Filtered <- reactive({
        teacher_dataset %>%
            filter(
                starting_salary >= input$startingSalaryInput[1],
                starting_salary <= input$startingSalaryInput[2],
                State %in% input$stateInputCost,
                State %in% input$stateInputHome,
                Rank >= input$costOfLivingInput[1],
                Rank <= input$costOfLivingInput[2],
                adopted_common_core %in% input$commonCoreInputHome,
                Region %in% input$regionInputHome,
                avg_salary >= input$averageSalaryInputHome[1],
                avg_salary <= input$averageSalaryInputHome[2],
                max_high >= input$highTempInputHome[1],
                max_high <= input$highTempInputHome[2],
                min_low >= input$lowTempInputHome[1],
                min_low <= input$lowTempInputHome[2],
                max_precipitation >= input$maxPrecipitationInputHome[1],
                max_precipitation <= input$maxPrecipitationInputHome[2],
                min_precipitation >= input$minPrecipitationInputHome[1],
                min_precipitation <= input$minPrecipitationInputHome[2],
                total >= input$totalCrimeInputHome[1],
                total <= input$totalCrimeInputHome[2],
                property_crime >= input$propertyCrimeInputHome[1],
                property_crime <= input$propertyCrimeInputHome[2],
                violent_crime >= input$violentCrimeInputHome[1],
                violent_crime <= input$violentCrimeInputHome[2],
                rape_revised >= input$rapeInputHome[1],
                rape_revised <= input$rapeInputHome[2],
                larceny >= input$larcenyInputHome[1],
                larceny <= input$larcenyInputHome[2],
                motor_vehicle_theft >= input$motorTheftInputHome[1],
                motor_vehicle_theft <= input$motorTheftInputHome[2],
                burglary >= input$burglaryInputHome[1],
                burglary <= input$burglaryInputHome[2],
                aggravated_assault >= input$aggravatedAssaultInputHome[1],
                aggravated_assault <= input$aggravatedAssaultInputHome[2],
                homicide >= input$homicideInputHome[1],
                homicide <= input$homicideInputHome[2],
                robbery >= input$robberyInputHome[1],
                robbery <= input$robberyInputHome[2],
                max_high >= input$maxTempInput[1],
                max_high <= input$maxTempInput[2],
                min_low >= input$minTempInput[1],
                min_low <= input$minTempInput[2],
                max_precipitation >= input$maxPrecipitationInput[1],
                max_precipitation <= input$maxPrecipitationInput[2],
                min_precipitation >= input$minPrecipitationInput[1],
                min_precipitation <= input$minPrecipitationInput[2],
                total >= input$totalCrimeInput[1],
                total <= input$totalCrimeInput[2],
                larceny >= input$larcenyInput[1],
                larceny <= input$larcenyInput[2],
                homicide >= input$homicideInput[1],
                homicide <= input$homicideInput[2],
                property_crime >= input$propertyCrimeInput[1],
                property_crime <= input$propertyCrimeInput[2],
                motor_vehicle_theft >= input$motorTheftInput[1],
                motor_vehicle_theft <= input$motorTheftInput[2],
                robbery >= input$robberyInput[1],
                robbery <= input$robberyInput[2],
                violent_crime >= input$violentCrimeInput[1],
                violent_crime <= input$violentCrimeInput[2],
                burglary >= input$burglaryInput[1],
                burglary <= input$burglaryInput[2],
                rape_revised >= input$rapeInput[1],
                rape_revised <= input$rapeInput[2],
                aggravated_assault >= input$aggravatedAssaultInput[1],
                aggravated_assault <= input$aggravatedAssaultInput[2]
                
                
            )
    })
    
    # Table Data for Home Page
    dataFrame <- reactive({
        Filtered() %>%
            select(1, 12, 13, 35, 36, 37, 4, 5) %>%
            slice(1:10) %>%
            rename(`Adopted Common Core` = adopted_common_core,
                   `Cost of Living Index` = Rank,
                   `Total Crime per 100k` = total,
                   `Avg. Starting Salary` = starting_salary,
                   `Max. High Temp.` = max_high,
                   `Min. Low Temp.` = min_low)
    })
    
    # Map Data
    map_data <- reactive({
        teacher_dataset2 %>%
            filter(
                starting_salary >= input$startingSalaryInput[1],
                starting_salary <= input$startingSalaryInput[2],
                State %in% input$stateInputCost,
                State %in% input$stateInputHome,
                Rank >= input$costOfLivingInput[1],
                Rank <= input$costOfLivingInput[2],
                adopted_common_core %in% input$commonCoreInputHome,
                Region %in% input$regionInputHome,
                avg_salary >= input$averageSalaryInputHome[1],
                avg_salary <= input$averageSalaryInputHome[2],
                max_high >= input$highTempInputHome[1],
                max_high <= input$highTempInputHome[2],
                min_low >= input$lowTempInputHome[1],
                min_low <= input$lowTempInputHome[2],
                max_precipitation >= input$maxPrecipitationInputHome[1],
                max_precipitation <= input$maxPrecipitationInputHome[2],
                min_precipitation >= input$minPrecipitationInputHome[1],
                min_precipitation <= input$minPrecipitationInputHome[2],
                total >= input$totalCrimeInputHome[1],
                total <= input$totalCrimeInputHome[2],
                property_crime >= input$propertyCrimeInputHome[1],
                property_crime <= input$propertyCrimeInputHome[2],
                violent_crime >= input$violentCrimeInputHome[1],
                violent_crime <= input$violentCrimeInputHome[2],
                rape_revised >= input$rapeInputHome[1],
                rape_revised <= input$rapeInputHome[2],
                larceny >= input$larcenyInputHome[1],
                larceny <= input$larcenyInputHome[2],
                motor_vehicle_theft >= input$motorTheftInputHome[1],
                motor_vehicle_theft <= input$motorTheftInputHome[2],
                burglary >= input$burglaryInputHome[1],
                burglary <= input$burglaryInputHome[2],
                aggravated_assault >= input$aggravatedAssaultInputHome[1],
                aggravated_assault <= input$aggravatedAssaultInputHome[2],
                homicide >= input$homicideInputHome[1],
                homicide <= input$homicideInputHome[2],
                robbery >= input$robberyInputHome[1],
                robbery <= input$robberyInputHome[2],
                max_high >= input$maxTempInput[1],
                max_high <= input$maxTempInput[2],
                min_low >= input$minTempInput[1],
                min_low <= input$minTempInput[2],
                max_precipitation >= input$maxPrecipitationInput[1],
                max_precipitation <= input$maxPrecipitationInput[2],
                min_precipitation >= input$minPrecipitationInput[1],
                min_precipitation <= input$minPrecipitationInput[2],
                total >= input$totalCrimeInput[1],
                total <= input$totalCrimeInput[2],
                larceny >= input$larcenyInput[1],
                larceny <= input$larcenyInput[2],
                homicide >= input$homicideInput[1],
                homicide <= input$homicideInput[2],
                property_crime >= input$propertyCrimeInput[1],
                property_crime <= input$propertyCrimeInput[2],
                motor_vehicle_theft >= input$motorTheftInput[1],
                motor_vehicle_theft <= input$motorTheftInput[2],
                robbery >= input$robberyInput[1],
                robbery <= input$robberyInput[2],
                violent_crime >= input$violentCrimeInput[1],
                violent_crime <= input$violentCrimeInput[2],
                burglary >= input$burglaryInput[1],
                burglary <= input$burglaryInput[2],
                rape_revised >= input$rapeInput[1],
                rape_revised <= input$rapeInput[2],
                aggravated_assault >= input$aggravatedAssaultInput[1],
                aggravated_assault <= input$aggravatedAssaultInput[2]
            )
    })
    
    # Climate Data (Month Level)
    Climate_Data <- reactive({
        left_join(climate_data, teacher_dataset) %>%
            filter(
                starting_salary >= input$startingSalaryInput[1],
                starting_salary <= input$startingSalaryInput[2],
                State %in% input$stateInputCost,
                State %in% input$stateInputHome,
                Rank >= input$costOfLivingInput[1],
                Rank <= input$costOfLivingInput[2],
                adopted_common_core %in% input$commonCoreInputHome,
                Region %in% input$regionInputHome,
                avg_salary >= input$averageSalaryInputHome[1],
                avg_salary <= input$averageSalaryInputHome[2],
                max_high >= input$highTempInputHome[1],
                max_high <= input$highTempInputHome[2],
                min_low >= input$lowTempInputHome[1],
                min_low <= input$lowTempInputHome[2],
                max_precipitation >= input$maxPrecipitationInputHome[1],
                max_precipitation <= input$maxPrecipitationInputHome[2],
                min_precipitation >= input$minPrecipitationInputHome[1],
                min_precipitation <= input$minPrecipitationInputHome[2],
                total >= input$totalCrimeInputHome[1],
                total <= input$totalCrimeInputHome[2],
                property_crime >= input$propertyCrimeInputHome[1],
                property_crime <= input$propertyCrimeInputHome[2],
                violent_crime >= input$violentCrimeInputHome[1],
                violent_crime <= input$violentCrimeInputHome[2],
                rape_revised >= input$rapeInputHome[1],
                rape_revised <= input$rapeInputHome[2],
                larceny >= input$larcenyInputHome[1],
                larceny <= input$larcenyInputHome[2],
                motor_vehicle_theft >= input$motorTheftInputHome[1],
                motor_vehicle_theft <= input$motorTheftInputHome[2],
                burglary >= input$burglaryInputHome[1],
                burglary <= input$burglaryInputHome[2],
                aggravated_assault >= input$aggravatedAssaultInputHome[1],
                aggravated_assault <= input$aggravatedAssaultInputHome[2],
                homicide >= input$homicideInputHome[1],
                homicide <= input$homicideInputHome[2],
                robbery >= input$robberyInputHome[1],
                robbery <= input$robberyInputHome[2],
                max_high >= input$maxTempInput[1],
                max_high <= input$maxTempInput[2],
                min_low >= input$minTempInput[1],
                min_low <= input$minTempInput[2],
                max_precipitation >= input$maxPrecipitationInput[1],
                max_precipitation <= input$maxPrecipitationInput[2],
                min_precipitation >= input$minPrecipitationInput[1],
                min_precipitation <= input$minPrecipitationInput[2],
                total >= input$totalCrimeInput[1],
                total <= input$totalCrimeInput[2],
                larceny >= input$larcenyInput[1],
                larceny <= input$larcenyInput[2],
                homicide >= input$homicideInput[1],
                homicide <= input$homicideInput[2],
                property_crime >= input$propertyCrimeInput[1],
                property_crime <= input$propertyCrimeInput[2],
                motor_vehicle_theft >= input$motorTheftInput[1],
                motor_vehicle_theft <= input$motorTheftInput[2],
                robbery >= input$robberyInput[1],
                robbery <= input$robberyInput[2],
                violent_crime >= input$violentCrimeInput[1],
                violent_crime <= input$violentCrimeInput[2],
                burglary >= input$burglaryInput[1],
                burglary <= input$burglaryInput[2],
                rape_revised >= input$rapeInput[1],
                rape_revised <= input$rapeInput[2],
                aggravated_assault >= input$aggravatedAssaultInput[1],
                aggravated_assault <= input$aggravatedAssaultInput[2]
            )
    })
    
    # Table on Home Page
    output$homeTable <- renderTable(
        dataFrame()
    )
    
    # Number of States Selected Value Box
    output$countOfStatesBox <- renderValueBox({
        valueBox(
            value = nrow(Filtered()), 
            subtitle = if(nrow(Filtered()) == 1) {
                "State Selected"
            } else {
                "States Selected"
            }, 
            color = "light-blue",
            icon = icon("flag"))
    })
    
    # Map of Regions on Home Page
    output$regionMap <- renderLeaflet({
        leaflet(data = map_data()) %>%
            addTiles() %>%
            addPolygons(color = map_data()$region_color, fillColor = map_data()$region_color) %>%
            addLegend(values = region$region_color,
                      colors = region$region_color,
                      labels = region$Region,
                      position = "topright")
    })
    
    # Map of Common Core on Home Page
    output$commonCoreMap <- renderLeaflet({
        leaflet(data = map_data()) %>%
            addTiles() %>%
            addPolygons(color = map_data()$common_core_color, fillColor = map_data()$common_core_color) %>%
            addLegend(values = c("#F64F5C", "#42F7C0"),
                      colors = c("#F64F5C", "#42F7C0"),
                      labels = c("False", "True"),
                      position = "topright")
    })
    
    # Starting Salary vs Cost of Living
    output$startingVSCostOfLiving <- renderPlotly({
        ggplotly(ggplot(Filtered()) +
                     geom_point(aes(x = Rank, y = starting_salary, text = State), color = "#70BBFF") +
                     geom_abline(aes(intercept =21478.23, slope = 166.58)) +
                     theme_classic() +
                     labs(x = "Cost of Living Index", y = "Average Starting Salary",
                          title = "") +
                     xlim(85, 200) +
                     ylim(30000, 56000) +
                     scale_y_continuous(labels = scales::dollar),
                 tooltip = c("text", "y", "x")
        ) 
        
    })
    
    # Starting Salary Plot
    output$startingSalaryPlot <- renderPlotly({
        ggplotly(ggplot(Filtered()) +
                     geom_col(aes(reorder(State, -starting_salary), starting_salary), fill = "#70BBFF") +
                     theme_classic() +
                     labs(x = "", y = "", title = "") +
                     scale_y_continuous(labels = scales::dollar, breaks = c(0, 20000, 40000, 60000), limits = c(0, 60000)) +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
        )
    })
    
    # Cost of Living Plot
    output$costOfLivingPlot <- renderPlotly({
        ggplotly(ggplot(Filtered()) +
                     geom_col(aes(reorder(State, -Rank), Rank), fill = "#70BBFF") +
                     theme_classic() +
                     labs(x = "", y = "", title = "") +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
        )
    })
    
    #Avg High Temp per Month by State
    output$highTempPlot <- renderPlotly({
        ggplotly(ggplot(Climate_Data()) +
                     geom_point(aes(reorder(Month, Month_Order), Avg_High, color = State)) +
                     geom_line(mapping = aes(reorder(Month, Month_Order), Avg_High, color = State, group = State))+
                     theme_classic() +
                     labs(x = "", y = "Avg. High Temp. in Fahrenheit") +
                     ylim(0, 106)
        )
    })
    
    # Avg Low Temp per Month by State
    output$lowTempPlot <- renderPlotly({
        ggplotly(ggplot(Climate_Data()) +
                     geom_point(aes(reorder(Month, Month_Order), Avg_Low, color = State)) +
                     geom_line(mapping = aes(reorder(Month, Month_Order), Avg_Low, color = State, group = State))+
                     theme_classic() +
                     labs(x = "", y = "Avg. High Temp. in Fahrenheit") +
                     ylim(0, 106)
        )
    })
    
    # Avg Precipitation Plot
    output$avgPrecipitationPlot <- renderPlotly({
        ggplotly(ggplot(Climate_Data()) +
                     geom_col(aes(reorder(Month, Month_Order), Avg_Precipitation, fill = State), position = "dodge") +
                     theme_classic() +
                     labs(x = "", y = "Avg. Precipitation in Inches")
        )
    })
    
    # Count of Days with Precipitation by Month and State
    output$countDaysPrecipitationPlot <- renderPlotly({
        ggplotly(ggplot(Climate_Data()) +
                     geom_col(aes(reorder(Month, Month_Order), Days_with_Precipitation, fill = State), position = "dodge") +
                     theme_classic() +
                     labs(x = "", y = "")
        )
    })
    
    # Total Hours of Sunshine Each Month by State
    output$hoursSunshinePlot <- renderPlotly({
        ggplotly(ggplot(Climate_Data()) +
                     geom_col(aes(reorder(Month, Month_Order), Hours_Sunshine, fill = State), position = "dodge") +
                     theme_classic() +
                     labs(x = "", y = "")
        )
    })
    
    # Population by State
    output$populationPlot <-renderPlotly({
        ggplotly(ggplot(Filtered()) +
                     geom_col(aes(reorder(State, -population), population/1000000), fill = "#70BBFF") +
                     theme_classic() +
                     labs(x = "", y = "") +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
                 
        )
    })
    
    # Total Crime Plot
    output$totalCrimePlot <-renderPlotly({
        ggplotly(ggplot(Filtered()) +
                     geom_col(aes(reorder(State, -total), total), fill = "#70BBFF") +
                     theme_classic() +
                     labs(x = "", y = "") +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
                 
        )
    })
    
    # Aggravated Assault Plot
    output$aggravatedAssaultPlot <-renderPlotly({
        ggplotly(ggplot(Filtered()) +
                     geom_col(aes(reorder(State, -aggravated_assault), aggravated_assault), fill = "#70BBFF") +
                     theme_classic() +
                     labs(x = "", y = "") +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
                 
        )
    })
    
    # Burglary Plot
    output$burglaryPlot <-renderPlotly({
        ggplotly(ggplot(Filtered()) +
                     geom_col(aes(reorder(State, -burglary), burglary), fill = "#70BBFF") +
                     theme_classic() +
                     labs(x = "", y = "") +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
                 
        )
    })
    
    # Homicide Plot
    output$homicidePlot <-renderPlotly({
        ggplotly(ggplot(Filtered()) +
                     geom_col(aes(reorder(State, -homicide), homicide), fill = "#70BBFF") +
                     theme_classic() +
                     labs(x = "", y = "") +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
                 
        )
    })
    
    # Larceny Plot
    output$larcenyPlot <-renderPlotly({
        ggplotly(ggplot(Filtered()) +
                     geom_col(aes(reorder(State, -larceny), larceny), fill = "#70BBFF") +
                     theme_classic() +
                     labs(x = "", y = "") +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
                 
        )
    })
    
    # Motor Theft Plot
    output$motorTheftPlot <-renderPlotly({
        ggplotly(ggplot(Filtered()) +
                     geom_col(aes(reorder(State, -motor_vehicle_theft), motor_vehicle_theft), fill = "#70BBFF") +
                     theme_classic() +
                     labs(x = "", y = "") +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
                 
        )
    })
    
    # Property Crime Plot
    output$propertyCrimePlot <-renderPlotly({
        ggplotly(ggplot(Filtered()) +
                     geom_col(aes(reorder(State, -property_crime), property_crime), fill = "#70BBFF") +
                     theme_classic() +
                     labs(x = "", y = "") +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
                 
        )
    })
    
    # Rape Plot
    output$rapePlot <-renderPlotly({
        ggplotly(ggplot(Filtered()) +
                     geom_col(aes(reorder(State, -rape_revised), rape_revised), fill = "#70BBFF") +
                     theme_classic() +
                     labs(x = "", y = "") +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
                 
        )
    })
    
    # Robbery Plot
    output$robberyPlot <-renderPlotly({
        ggplotly(ggplot(Filtered()) +
                     geom_col(aes(reorder(State, -robbery), robbery), fill = "#70BBFF") +
                     theme_classic() +
                     labs(x = "", y = "") +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
                 
        )
    })
    
    # Violent Crime Plot
    output$violentCrimePlot <-renderPlotly({
        ggplotly(ggplot(Filtered()) +
                     geom_col(aes(reorder(State, -violent_crime), violent_crime), fill = "#70BBFF") +
                     theme_classic() +
                     labs(x = "", y = "") +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
                 
        )
    })
    
}
shinyApp(ui = ui, server = server)