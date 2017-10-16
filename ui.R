library(shiny)
library(shinydashboard)
library(highcharter)


#---Header---#
header <- dashboardHeader(title = "Clustering App")


#---Sidebar---#
sidebar <- dashboardSidebar(collapsed = FALSE,
                            sidebarMenu(
                                # sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                                #                   label = "Search..."),
                                # sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                                #                   label = "Search..."),
                                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                                menuItem("About", tabName = "about", icon = icon("info")),
                                menuItem("Charts", tabName = "charts", icon = icon("bar-chart")),
                                menuItem("Dataset", tabName = "view", icon = icon("table")),
                                menuItem("Clusters", tabName = "clusters", icon = icon("object-group")),
                                menuItem("Reports", tabName = "reports", icon = icon("file")),
                                h5("Dataset"),
                                downloadButton("downloadData", "Download iris.csv", class = NULL),
                                br(),
                                radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                                             inline = TRUE),
                                downloadButton("downloadReport", "Generate report"),
                                img(src = "iris.jpg", width = 200, alt = "logo.jpg")
  ))


#---Body---#
body <- dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
        tabItem("dashboard", fluidRow(
            valueBoxOutput("rate"),
            valueBoxOutput("count"),
            valueBoxOutput("users"),
            box(
                title = "Overview", solidHeader = TRUE,
                width = 8,
                status = "primary",
                plotOutput("overview")
            ),
            box(
                title = "Inputs", status = "primary", width = 4,
                selectInput("dataset", "Choose a dataset (or a subset) :", 
                            choices = c("all iris data", "setosa", "versicolor", "virginica")),
                selectInput("Xvar", "X variable", 
                            choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")),
                selectInput("Yvar", "Y variable", 
                            choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), selected = "Sepal.Width") 
            ),
            box(
                title = "Boxplot", width = 8, solidHeader = TRUE,
                status = "primary",
                plotOutput("boxPlot")
            ),
            box(
                title = "Pie Chart", width = 4, solidHeader = FALSE,
                status = "primary",
                htmlOutput("pie")
            ),
            box(
                title = "Violin plot", width = 6, solidHeader = TRUE,
                status = "primary",
                plotOutput("violin")
            ),
            box(
                title = "Barplot", width = 6, solidHeader = TRUE,
                status = "primary",
                highchartOutput("bar")
            ),
            box(
                title = "Andrews Curves", width = 6, solidHeader = TRUE,
                status = "primary",
                plotOutput("line")
            ),
            box(
              title = "Density plot", width = 6, solidHeader = TRUE,
              status = "primary",
              plotOutput("dens")
            )
        )), 
        tabItem("about", fluidRow(
            box(
                title = "Iris Flower Dataset", width = 12, solidHeader = TRUE,
                textOutput("text"),
                img(src = "iris_types.jpg", width = 300, height = 100, alt = "iris types")
            )
        )),
        tabItem("charts", fluidRow(
            box(
                title = "Sankey", solidHeader = TRUE, width = 4,
                status = "primary",
                htmlOutput("sankey")
            ),
            box(
                title = "Bubble", solidHeader = TRUE, width = 8,
                status = "primary",
                htmlOutput("bubble")
            ),
            box(
                title = "Scatter", solidHeader = TRUE, width = 12,
                status = "primary",
                htmlOutput("scatter")
            )
        )),
        tabItem("view", fluidRow(
            box(
                title = "Dataset", solidHeader = TRUE, width = 5,
                status = "primary",
                dataTableOutput("view")
            ),
            box(
                title = "Summary Statistics", solidHeader = TRUE, width = 7,
                status = "primary",
                verbatimTextOutput("summary")
            )
        )),
        tabItem("reports", fluidRow(
            box(
                title = "Saratoga Report", solidHeader = TRUE, width = 12,
                status = "primary",
                htmlOutput("inc")
            )
        )),
        tabItem("clusters",  fluidRow(
            box(
                title = "K-means", solidHeader = TRUE, width = 8,
                textOutput("NbClust"), status = "primary",
                plotOutput("kmeansPlot")
            ),
            box(
                title = "Inputs", status = "primary", width = 4,
                numericInput("clusters", "Cluster count", 3, min = 1, max = 9)
            ),
            box(
                title = "Dbscan", width = 8, solidHeader = TRUE,
                textOutput("dbscan_Param"), status = "primary",
                plotOutput("dbscanPlot")
            ),
            box(
                title = "Inputs", width = 4, status = "primary",
                sliderInput("eps", "Radius of neighborhood of each point", min = 0.0, max = 1.0, value = 0.2),
                sliderInput("minPoints", "Number of neighbors within the eps radius", min = 0, max = 10, value = 3)
            ),
            box(
                title = "Decision Tree", solidHeader = TRUE, width = 12,
                status = "primary",
                plotOutput("treePlot")
            )
        ))
    )
)


#---Construct Page---#
dashboardPage(header, sidebar, body)