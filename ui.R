library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(dplyr)
library(shinycssloaders)

header = dashboardHeader(title = "Nonhomogeneous Pois.", titleWidth = 250,
                         tags$li(class = "dropdown",
                                 tags$a(href = "https://shinyapps.science.psu.edu/",
                                        icon("home", lib = "font-awesome"))),
                         tags$li(class = "dropdown", actionLink("info", icon("info")))
                         
                         )

sidebar = dashboardSidebar(
  width=250,
  sidebarMenu(id = 'tabs', 
              menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
              menuItem('Prerequisites', tabName = 'prerequisite', icon = icon('book')),
              menuItem('Explore', tabName = "exp", icon = icon('wpexplorer')),
              menuItem("Game", tabName = "Game", icon = icon("gamepad")),
              menuItem("References", tabName = "References", icon = icon("leanpub"))
              ),
  tags$div(
    class = "sidebar-logo",
    boastUtils::psu_eberly_logo("reversed")
  ))

body = dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css",
              href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
  ),
  useShinyjs(),
  
  tabItems(
    tabItem(tabName = "overview",
            h1("Nonhomogeneous Poisson Process"),
            
            withMathJax(),
            p("In this app you will explore the relationship between N(t) and \\(t_{k}\\) through the simulation
               of data"),
            
            br(),
            h2("Instructions:"),
            
            tags$ul(
              tags$li("By sliding the bar for the number of events and the interarrival rate, you will see how the Poisson 
                      process will be changed."),
              tags$li("You can view four graphs:"),
              tags$ol(
              tags$li("Intensity function: \\(\\lambda\\) against Time"),
              tags$li("Number of Events vs. Time"),
              tags$li("Residuals vs. Time"),
              tags$li("Interarrival Time Distribution: Density vs. Interarrival Time")  )),
            div(style = "text-align: center", bsButton(inputId = "bsButton1", label = "GO!",icon = icon('bolt'), size = 'large')),
            br(),
            
            h2("Acknowledgements:"),
               p("This app was coded and developed by Shubo Sun and Shunqi Zhang and was reformatted by Leah Hunt."),
               p("Special thanks to Dr. Pearl for giving useful and supportive suggestions throughout the program."),
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 7/15/2020 by LMH.")
            ),
    
    tabItem(
      tabName = 'prerequisite', withMathJax(),
      
      h2('Background: Nonhomogeneous Poisson Process'),br(),
      p("If a point process, {N*(t);t\\(\\geqslant\\)0}, with independent increments and with N(t) being Poisson for all t with an
          average of m(t) = \\(\\int_0^t \\lambda(\\tau) \\, d \\tau\\) where \\(\\lambda(\\tau)\\) is a function
          that represents the rate (or intensity) at time \\(\\tau\\), then this point process is called a Nonhomogeneous Poisson process."),
      br(),
      
      h2("Properties: "),
      tags$ul(
      tags$li("N(0)=0"),
      
      tags$li("The expected value of N*(t+s)-N*(t) is m(t+s)-m(t)" ),
      
      
      tags$li("If N*(t) follows a Nonhomogeneous Poisson process then it can be considered as a homogeneous process with a rescaling of time. Thus, N*(t) = N(m(t))
                 where {N(t);t\\(\\geqslant\\)0} is a homogeneous Poisson process with rate 1.")),
      
      tags$img(src = 'Homo3.png', width = "537.6px", height = "144px"),
      
      br(),
      div(style = "text-align: center",bsButton("goover", "GO!", icon("bolt"), size = "large"))
      
    ),
    
    ########################### Exploration activity ################################
    tabItem(tabName = "exp",
            fluidPage(
              titlePanel("Simluation plot for Non-homogeneous Poisson Process"),
              #p("Challenge: How does the rate and number of events influence the result respectively?"),
              #p("Hint: Move your mouse on the graph to see more explanation."),
              p("Explore the non-homogeneous poisson process by choosing an intensity function for the process
                then manipulating the parameters of the function as well as the number of events and number
                of paths. How does the rate and number of events influence the result respectively? Observe the changes
                on the various graphs."),
              sidebarLayout(
                sidebarPanel(
                  "Design: ",
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
                  
                  sliderInput("nevent", "Number of events up to t",
                              min = 1, max = 150, value = 100, step = 1),
                  sliderInput("path", "Number of residual paths",
                              min = 1, max = 5, value = 1, step = 1)
                ),
            
              mainPanel(
                     p("Note: Individual paths are coded by color."),
                     # uiOutput("popover"),
                    # bsPopover("plot1","Sample Nonhomogeneous Poisson Plot","The lambda function you have chosen is ",
                    #           trigger="hover",placement="top"),
                     br(),
                     plotOutput("plot1",height = "300px")%>% withSpinner(color="#0dc5c1"),
                     
                     br(),
                     # bsPopover("plot2","Sample Nonhomogeneous Poisson Plot","This is a Nonhomogeneous Poisson Plot with constant rate, you could use the slider to change the number of events.This distance on the graph is N(t+s)-N(s), which is a Poisson distribution with parameter m(t+s)-m(s). m(t+s) can be calculated by integrating our lambda function from 0 to t+s, while m(s) is integrating lambda function from 0 to s.",
                     #           trigger="hover",placement="top"),br(),
                     plotOutput("plot2",height = "300px")%>% withSpinner(color="#0dc5c1"),
                     textOutput("plot2Text"),
                     br(),
                     # bsPopover("plot3","Residuals Plot","This is a Nonhomogeneous Poisson Plot with constant rate, you could use the slider to change the number of events.",
                     #           trigger="hover",placement="top"),br(),
                     plotOutput("plot3",height = "300px")%>% withSpinner(color="#0dc5c1"),
                     
                     br(),
                     # bsPopover("plot4","Interarrival Time Distribution","This plot shows the distribution of interarrival times. From the plot it is
                     #                easy to see that interarrival times roughly follow exponential distribution.",
                     #           trigger="hover",placement="top"),br(),
                     # plotOutput("plot4",height = "500px")%>% withSpinner(color="#0dc5c1")
                    # bsPopover("plot4","Interarrival Time Distribution","This plot shows the distribution of interarrival times. From the plot it is easy to see that interarrival times roughly follow exponential distribution.",
                              # trigger="hover",placement="top"),br(),
                    plotOutput("plot4",height = "300px")%>% withSpinner(color="#0dc5c1"),
                     br(),
                     textOutput("feedback"),
                     tags$head(tags$style("#feedback{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                     )
                     )
                    
                     )
              )
            )
            ),
    
            # Game tab
            tabItem(
              tabName="Game",
              tabsetPanel(id = "game",
                          tabPanel(title = "Instructions", value = "instr",
                                   fluidPage(theme = 'Muted',
                                             titlePanel('Instructions'),
                                             p("Use the drop down to indicate which lambda
                                               function the graph shows. You can play either 
                                               in practice mode, which is untimed, or in game 
                                               mode, which is timed."),
                                             br(),
                                             div(style = "text-align: center",
                                                 bsButton(inputId = "bsButton4",label = "GO!", icon('bolt'),  size = "median"))
                                   )
                          ),
                          
                          tabPanel(title = "Practice Mode", value = "fib",
                            
                                   fluidRow(
                                     column(actionButton("resetPractice", "Reset"),
                                      
                                       p("Select which intensity function matches the graph."), width=8),
                                     column(width=4,
                                            textOutput("score"))
                                   ),
                                   
                                   plotOutput("gamePlot2"),
                                  
                                   fluidRow(
                                         selectInput('challengeChoice', label = '',
                                                     choices = c("constant", "linear", "exponential", "inverse"),
                                                     selectize=TRUE
                                         )),
                                   
                                   conditionalPanel('input.submitX != 0',
                                                    div(style = "display:inline-block", htmlOutput('challengeFeedback')),
                                                    div(style = "display:inline-block", htmlOutput('textFeedback'))),
                                   
                                   br(),
                                   
                                   div(style = "text-align: center",
                                       bsButton(inputId = 'submitX', label = 'Check Answer',size = 'median'),
                                       bsButton(inputId = 'nextX', label = 'Next',size = 'median'))
                          ),
                          tabPanel(title = "Timed Mode", value = "time",
                                   fluidRow(
                                     column(actionButton("startTimedGame", "Start Game"),
                                            actionButton("resetTimedGame", "Reset Game"),
                                            conditionalPanel(condition="output.showGame", p("Select which intensity function matches the graph.")), 
                                            width=8),
                                     column(width=4,
                                            textOutput("scoreT"),
                                            textOutput("countdown"))
                                   ),
                                   conditionalPanel(
                                     condition="output.showGame",
                                     
                                   plotOutput("plot2T"),
                                   
                                   fluidRow(
                                     selectInput('challengeChoiceT', label = 'Intensity Function',
                                                 choices = c("constant", "linear", "exponential", "inverse"),
                                                 selectize=TRUE
                                     ),
                                      conditionalPanel('input.submitT != 0',
                                                       div(style = "display:inline-block", htmlOutput('challengeFeedbackT')),
                                                       div(style = "display:inline-block", htmlOutput('textFeedbackT')))
                                   ),

                                   br(),
                                   
                                   div(style = "text-align: center",
                                       bsButton(inputId = 'submitT', label = 'Check Answer',size = 'median'),
                                       bsButton(inputId = 'nextT', label = 'Next',size = 'median'))
                          ))
                          
              )
            ),
            # References tab
            tabItem(
              tabName = "References",
              withMathJax(),
              h2("References"),
              p(
                class = "hangingindent",
                "Attali, D. (2020), shinyjs: Easily Improve the User Experience of Your Shiny
  Apps in Seconds, R package. Available from
  https://CRAN.R-project.org/package=shinyjs"
              ),
              p(
                class = "hangingindent",
                "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny, R package. Available
            from https://CRAN.R-project.org/package=shinyBS"
              ),
              p(
                class = "hangingindent",
                "Carey, R. (2019), boastUtils: BOAST Utilities, R Package. Available from
          https://github.com/EducationShinyAppTeam/boastUtils"
              ),
              p(
                class = "hangingindent",
                "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create dashboards with 'Shiny', R
    Package. Available from https://CRAN.R-project.org/package=shinydashboard"
              ),
              p(
                class = "hangingindent",
                "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2019),  shiny: Web 
    application framework for R, R Package. Available from 
    https://CRAN.R-project.org/package=shiny"
              ),
              
              p(
              class = "hangingindent",
              "Perrier, V., Meyer, F., and Granjon, D. (2020), shinyWidgets: Custom Inputs
  Widgets for Shiny, R package. Available from
  https://CRAN.R-project.org/package=shinyWidgets"
            ),
              p(
                class = "hangingindent",
                "Sali, A. and Attali, D. (2020), shinycssloaders: Add CSS Loading Animations
  to 'shiny' Outputs, R package. Available from
  https://CRAN.R-project.org/package=shinycssloaders"
              ),
    p(
      class = "hangingindent",
      "Wickham, H., François, R., Henry L., and Müller, K. (2020), dplyr: A Grammar of Data
  Manipulation, R package. Available from https://CRAN.R-project.org/package=dplyr"
    ),
    p(
      class = "hangingindent",
      "Wickham, H. (2016), ggplot2: Elegant graphics for data analysis, R Package, New York:
    Springer-Verlag. Available from https://ggplot2.tidyverse.org"
    )
            )
    )
  
)
shinyUI(dashboardPage(header, sidebar, body))