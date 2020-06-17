library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyDND)
library(plotly)
library(shinycssloaders)

header = dashboardHeader(title = "NonHomogeneous Poisson Process", titleWidth = 350,
                         tags$li(class = "dropdown",
                                 tags$a(href = "https://shinyapps.science.psu.edu/",
                                        icon("home", lib = "font-awesome"))),
                         tags$li(class = "dropdown",
                                 actionLink("info", icon("info"), class = "myClass"))
                         
                         )

sidebar = dashboardSidebar(
  sidebarMenu(id = 'tabs', 
              menuItem('Prerequisites', tabName = 'prerequisite', icon = icon('book')),
              menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
              menuItem('Explore', tabName = "exp", icon = icon('wpexplorer'))
              )
)

body = dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
  ),
  
  useShinyjs(),
  
  tabItems(
    tabItem(
      tabName = 'prerequisite', withMathJax(),
      
      h3(strong('Background: NonHomogeneous Poisson Process')),br(),
      h4("If a point process, {N*(t);t\\(\\geqslant\\)0}, with independent increments and with N(t) being Poisson for all t with an
          average of m(t) = \\(\\int_0^t \\lambda(\\tau) \\, d \\tau\\) where \\(\\lambda(\\tau)\\) is a function
          that represents the rate (or intensity) at time \\(\\tau\\), then this point process is called a non-homogeneous Poisson process."),
      br(),
      
      h4(strong("Properties: ")),
      
      h4(tags$li("N(0)=0")),
      
      h4(tags$li("The expected value of N*(t+s)-N*(t) is m(t+s)-m(t)" )),
     
      
      h4(tags$li("If N*(t) follows a non-homogeneous Poisson process then it can be considered as a homogeneous process with a rescaling of time. Thus, N*(t) = N(m(t))
                 where {N(t);t\\(\\geqslant\\)0} is a homogeneous Poisson process with rate 1.")),

      tags$img(src = 'Homo3.png', width = "537.6px", height = "144px"),
      
      br(),
      div(style = "text-align: center",bsButton("goover", "Go to the overview", icon("bolt"), size = "medium"))
      
    ),
    
    tabItem(tabName = "overview",
            tags$a(href='http://stat.psu.edu/', tags$img(src='PS-HOR-RGB-2C.png', align = "left", width = 180)),
            br(),br(),br(),
            
            h3(tags$b("About:")),
            
            withMathJax(),
            h4("In this app you will explore the relationship between N(t) and \\(t_{k}\\) through the simulation
               of data"),
            
            br(),
            h3(tags$b("Instructions:")),
            
            h4(
              tags$li("By sliding the bar for the number of events and the interarrival rate, you will see how the Poisson 
                      process will be changed."),
              tags$li("You can view four graphs:"),
              h4(HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"1. Intensity function: \\(\\lambda\\) against Time"),
              h4(HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"2. Number of Events vs. Time"),
              h4(HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"3. Residuals vs. Time"),
              h4(HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"4. Interarrival Time Distribution: Density vs. Interarrival Time")  ),
            div(style = "text-align: center", bsButton(inputId = "bsButton1", label = "G O !",icon = icon('bolt'), size = 'median')),
            br(),
            
            h3(tags$b("Acknowledgements:"),
               h4("This app was coded and developed by Shubo Sun and Shunqi Zhang."),
               h4("Special thanks to Dr. Pearl for giving useful and supportive suggestions throughout the program."))
            ),
    
    ########################### Exploration activity ################################
    tabItem(tabName = "exp",
            fluidPage(
              titlePanel("Simluation plot for Non-Homogeneous Poisson Process"),
              sidebarLayout(
                sidebarPanel(
                  #Let them choose a preloaded dataset or input their own
                  #If it is preloaded output some information about the dataset
                  h3(strong("Design: ")),
                  checkboxInput("designcheckbox","Show design info:", TRUE),
                  uiOutput("design")%>% withSpinner(color="#0dc5c1"),
                  br(),
                  selectInput(inputId ="lambdatype",withMathJax("Select which type of intensity function you would like to choose."),choices = c(Linear= "linear", Exponential='exponential',Inverse='inverse',Constant='constant')),
                  conditionalPanel("input.lambdatype == 'linear'",
                                   sliderInput("slope",
                                               "Slope",
                                               min = 1/10,
                                               max = 1,
                                               value = 1/3,
                                               step = 0.02
                                   )),
                  conditionalPanel("input.lambdatype == 'exponential'",
                                   sliderInput("growth",
                                               "Growth rate",
                                               min = 0.1,
                                               max = 1,
                                               value = 0.3,
                                               step = 0.05
                                   )),
                  conditionalPanel("input.lambdatype == 'inverse'",
                                   sliderInput("coefficient",
                                               "Coefficient",
                                               min = 0.001,
                                               max = 0.05,
                                               value = 0.002,
                                               step = 0.002
                                   )),
                  conditionalPanel("input.lambdatype == 'constant'",
                                   sliderInput("constant",
                                               "Value",
                                               min = 1,
                                               max = 10,
                                               value = 3,
                                               step = 0.2
                                   )),
                  
                  sliderInput("nevent", "# of events up to t",
                              min = 1, max = 150, value = 100, step = 1),
                  sliderInput("path", "# of residual paths",
                              min = 1, max = 5, value = 1, step = 1)
                ),
            
              mainPanel(
                     h4("Challenge: How does the rate and number of events influence the result respectively?"),
                     bsPopover("plot1","Sample NonHomogeneous Poisson Plot","The lambda function you have chosen is ",
                               trigger="hover",placement="top"),br(),
                     plotOutput("plot1",height = "300px")%>% withSpinner(color="#0dc5c1"),
                     
                     br(),br(),
                     bsPopover("plot2","Sample NonHomogeneous Poisson Plot","This is a Nonhomogeneous Poisson Plot with constant rate, you could use the slider to change the number of events.This distance on the graph is N(t+s)-N(s), which is a Poisson distribution with parameter m(t+s)-m(s). m(t+s) can be calculated by integrating our lambda function from 0 to t+s, while m(s) is integrating lambda function from 0 to s.",
                               trigger="hover",placement="top"),br(),
                     plotOutput("plot2",height = "300px")%>% withSpinner(color="#0dc5c1"),
                     h4("Hint: Move your mouse on the graph to see more explanation."),
                     
                     br(),br(),
                     bsPopover("plot3","Residuals Plot","This is a Nonhomogeneous Poisson Plot with constant rate, you could use the slider to change the number of events.",
                               trigger="hover",placement="top"),br(),
                     plotOutput("plot3",height = "300px")%>% withSpinner(color="#0dc5c1"),
                     
                     br(),br(),
                     # bsPopover("plot4","Interarrival Time Distribution","This plot shows the distribution of interarrival times. From the plot it is
                     #                easy to see that interarrival times roughly follow exponential distribution.",
                     #           trigger="hover",placement="top"),br(),
                     # plotOutput("plot4",height = "500px")%>% withSpinner(color="#0dc5c1")
                     plotOutput("dis")%>% withSpinner(color="#0dc5c1")
                    # checkboxInput("dis2","Distribution of Path 2", TRUE),
                    # plotOutput("dis2"),
                    # checkboxInput("dis3","Distribution of Path 3", TRUE),
                    # plotOutput("dis3"),
                    # checkboxInput("dis4","Distribution of Path 4", TRUE),
                    # plotOutput("dis4"),
                    # checkboxInput("dis5","Distribution of Path 5", TRUE),
                    # plotOutput("dis5")
                     
                     
                     
                     
                     
                     
                     
                     )
              )
            )
    )
  )
)
shinyUI(dashboardPage(header, sidebar, body))