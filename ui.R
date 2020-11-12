library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(dplyr)
library(shinycssloaders)
library(boastUtils)

# App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Nonhomogeneous Poisson"
APP_DESCP  <<- "This app explores the nonhomogeneous poisson process through simulation."

# End App Meta Data------------------------------------------------------------

dashboardPage(
  # Header ----
  dashboardHeader(
    title = "Nonhomogeneous Pois.",
    titleWidth = 250,
    tags$li(class = "dropdown", actionLink("info", icon("info"))),
    tags$li(
      class = "dropdown",
      tags$a(target = "_blank", icon("comments"),
             href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Nonhomogeneous_Poisson_Process"
      )
    ),
    tags$li(class = "dropdown",
            tags$a(href = "https://shinyapps.science.psu.edu/",
                   icon("home")
            )
    )
  ),
  # Sidebar ----
  dashboardSidebar(
    width=250,
    sidebarMenu(
      id = 'pages',
      menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
      menuItem('Prerequisites', tabName = 'prerequisite', icon = icon('book')),
      menuItem('Explore', tabName = "exp", icon = icon('wpexplorer')),
      menuItem("Game", tabName = "Game", icon = icon("gamepad")),
      menuItem("References", tabName = "References", icon = icon("leanpub"))
    ),
    tags$div(
      class = "sidebar-logo",
      boastUtils::psu_eberly_logo("reversed")
    )
  ),
  # Body ----
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css",
                href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
    ),
    useShinyjs(),
    tabItems(
      tabItem(
        ## Overview ----
        tabName = "overview",
        h1("Nonhomogeneous Poisson Process"),
        withMathJax(),
        p("In this app you will explore the relationship between N(t)
          and \\(t_{k}\\) through the simulation of data"),
        br(),
        h2("Instructions:"),
        tags$ul(
          tags$li("By sliding the bar for the number of events and the
                  interarrival rate, you will see how the Poisson process
                  will be changed."),
          tags$li("You can view four graphs:"),
          tags$ol(
            tags$li("Intensity function: \\(\\lambda\\) against Time"),
            tags$li("Number of Events vs. Time"),
            tags$li("Residuals vs. Time"),
            tags$li("Interarrival Time Distribution: Density vs.
                    Interarrival Time")
          )
        ),
        div(
          style = "text-align: center;",
          bsButton(
            inputId = "bsButton1",
            label = "GO!",
            icon = icon('bolt'),
            size = 'large'
          )
        ),
        br(),
        h2("Acknowledgements:"),
        p("This app was coded and developed by Shubo Sun and Shunqi Zhang
          and was reformatted by Leah Hunt."),
        br(),
        br(),
        br(),
        div(class = "updated", "Last Update: 9/24/2020 by LMH.")
      ),
      ## Prereq's Page ----
      tabItem(
        tabName = 'prerequisite',
        withMathJax(),
        h2('Prerequisites'),br(),
        p("If a point process, \\(\\left\\{N^*(t);\\;t\\geq0\\right\\}\\), with
          independent increments and with \\(N(t)\\) being Poisson for all ",
          tags$em("t"), " with an average of \\(m(t) = \\int_0^t\\lambda(\\tau)\\,
          d\\tau\\) where \\(\\lambda(\\tau)\\) represents the rate (or intensity)
          at time \\(\\tau\\), then this point process is called a Nonhomogeneous
          Poisson process."),
        br(),
        h3("Properties"),
        tags$ul(
          tags$li("\\(N(0)=0\\)"),
          tags$li("The expected value of \\(N^*(t+s)-N^*(t)\\) is \\(m(t+s) - m(t)\\)"),
          tags$li("If \\(N^*(t)\\) follows a Nonhomogeneous Poisson process then
                  it can be considered as a homogeneous process with a rescaling
                  of time. Thus, \\(N^*(t)=N(m(t))\\) where
                  \\(\\left\\{N(t);\\;t\\geq0\\right\\}\\) is a homogeneous
                  Poisson process with rate 1."
          )
        ),
        tags$img(
          src = 'Homo3.png',
          alt = "This image illustrates interarrival times as the time between
            events. For example, the interarrival time between events 3 and 4
            would be the time of event 4 minus the time of event 3.",
          width = "100%"
        ),
        br(),
        div(
          style = "text-align: center;",
          bsButton(
            inputId = "goover",
            label = "GO!",
            icon = icon("bolt"),
            size = "large"
          )
        )
      ),
      ########################### Exploration activity #########################
      tabItem(
        tabName = "exp",
        fluidPage(
          titlePanel("Simulation Plot for Nonhomogeneous Poisson Process"),
          p("Explore the nonhomogeneous poisson process by choosing an
            intensity function for the process then manipulating the parameters
            of the function as well as the number of events and number of paths.
            How does the rate and number of events influence the result
            respectively? Observe the changes on the various graphs."),
          uiOutput("design"),
          sidebarLayout(
            sidebarPanel(
              # "Design: ",
              # checkboxInput("designcheckbox","Show design info:", TRUE),
              # uiOutput("design"),
              # br(),
              selectInput(
                inputId ="lambdatype",
                label = withMathJax("Select which type of intensity function you
                                    would like to choose."),
                choices = c(
                  Linear= "linear",
                  Exponential='exponential',
                  Inverse='inverse',
                  Constant='constant'
                )
              ),
              conditionalPanel(
                condition = "input.lambdatype == 'linear'",
                sliderInput(
                  inputId = "slope",
                  "Slope",
                  min = 0.1,
                  max = 1,
                  value = 1/3,
                  step = 0.02
                )
              ),
              conditionalPanel(
                condition = "input.lambdatype == 'exponential'",
                sliderInput(
                  inputId = "growth",
                  label = "Growth rate",
                  min = 0.1,
                  max = 1,
                  value = 0.3,
                  step = 0.05
                )
              ),
              conditionalPanel(
                condition = "input.lambdatype == 'inverse'",
                sliderInput(
                  inputId = "coefficient",
                  label = "Coefficient",
                  min = 0.001,
                  max = 0.05,
                  value = 0.002,
                  step = 0.002
                )
              ),
              conditionalPanel(
                condition = "input.lambdatype == 'constant'",
                sliderInput(
                  inputId = "constant",
                  label = "Value",
                  min = 1,
                  max = 10,
                  value = 3,
                  step = 0.2
                )
              ),
              sliderInput(
                inputId = "nevent",
                label = "Number of events up to t",
                min = 1,
                max = 150,
                value = 100,
                step = 1
              ),
              textOutput("feedback"),
              sliderInput(
                inputId = "path",
                label = "Number of residual paths",
                min = 1,
                max = 5,
                value = 1,
                step = 1
              )
            ),
            mainPanel(
              p("Note: Individual paths are coded by color."),
              br(),
              plotOutput("plot1", height = "300px") %>%
                withSpinner(color="#0dc5c1"),
              htmlOutput("plot1Alt"),
              br(),
              plotOutput("plot2", height = "300px") %>%
                withSpinner(color="#0dc5c1"),
              tags$script(HTML(
                "$(document).ready(function() {
                document.getElementById('plot2').setAttribute('aria-labelledby',
                'plot2Text')
                })"
              )),
              uiOutput("plot2Text"),
              br(),
              plotOutput("plot3", height = "300px") %>%
                withSpinner(color="#0dc5c1"),
              tags$script(HTML(
                "$(document).ready(function() {
                document.getElementById('plot3').setAttribute('aria-label',
                `This plot shows the residuals of the trial plotted over time.`)
                })"
              )),
              br(),
              plotOutput("plot4", height = "300px") %>%
                withSpinner(color="#0dc5c1"),
              tags$script(HTML(
                "$(document).ready(function() {
                document.getElementById('plot4').setAttribute('aria-label',
                `This plot shows an approximation of the density of the
                times between arrivals of the current run.`)
                })"
              ))
            )
          )
        )
      ),
      # Game tab ----
      tabItem(
        tabName="Game",
        tabsetPanel(
          id = "game",
          {tabPanel(
            title = "Instructions",
            value = "instr",
            br(),
            h2("Instructions"),
            p(
              "Use the drop down to indicate which lambda function the graph
              shows. You can play either in practice mode, which is untimed, or
              in game mode, which is timed."
            ),
            p(
              "For practice mode, for each plot, use the drop down to select
              which lambda function was used to generate the path shown. You
              get 1 point for each correct answer with a .5 point penalty on
              your problem score for every incorrect guess."
            ),
            p(
              "For the timed game, to begin the game, click the Start Game
              button. Then, for each plot, use the drop down to select which
              lambda function was used to generate the path shown. You get 1
              point for each correct answer with a .5 point penalty on your
              problem score for every incorrect guess. You will have 60 seconds
              to try to get as many points as possible."
            ),
            br(),
            div(
              style = "text-align: center;",
              bsButton(
                inputId = "bsButton4",
                label = "GO!",
                icon = icon('bolt'),
                size = "large"
              )
            )
          )},
          {tabPanel(
            title = "Practice Mode",
            value = "fib",
            br(),
            fluidRow(
              column(
                width = 8,
                p("Select which intensity function matches the graph.")
              ),
              column(
                width = 2,
                textOutput("score")
              ),
              column(
                width = 2,
                bsButton(
                  inputId = "resetPractice",
                  label = "Reset",
                  size = "large"
                )
              )
            ),
            fluidRow(
              column(
                width = 3,
                selectInput(
                  inputId = 'challengeChoice',
                  label = 'Intensity Function',
                  choices = c(
                    "Select Answer",
                    "constant",
                    "linear",
                    "exponential",
                    "inverse"
                  ),
                  selectize = TRUE
                )
              ),
              column(
                width = 1,
                br(),
                htmlOutput('challengeFeedback')
              ),
              column(
                width = 2,
                br(),
                htmlOutput('textFeedback')
              ),
              column(
                width = 3,
                br(),
                bsButton(
                  inputId = 'submitX',
                  label = 'Check Answer',
                  size = "large"
                )
              ),
              column(
                width = 2,
                offset = 1,
                br(),
                bsButton(
                  inputId = 'nextX',
                  label = 'Next',
                  size = "large"
                )
              )
            ),
            plotOutput("gamePlot2"),
            htmlOutput("gamePracticePlotAlt")
          )},
          {tabPanel(
            title = "Timed Mode",
            value = "time",
            br(),
            fluidRow(
              column(
                width = 2,
                offset = 1,
                bsButton(
                  inputId = "startTimedGame",
                  label = "Start Game",
                  size = "large"
                )
              ),
              column(
                width = 3,
                textOutput("scoreT"),
                textOutput("countdown")
              ),
              column(
                width = 2,
                bsButton(
                  inputId = "stopTimedGame",
                  label = "Pause Game",
                  disabled = TRUE,
                  size = "large"
                )
              ),
              column(
                width = 2,
                bsButton(
                  inputId = "resetTimedGame",
                  label = "Reset Game",
                  size = "large"
                )
              )
            ),
            conditionalPanel(
              condition = "!output.showGame",
              br(),
              textOutput("pauseMessage")
            ),
            conditionalPanel(
              condition = "output.showGame",
              p("Select which intensity function matches the graph."),
              fluidRow(
                column(
                  width = 3,
                  selectInput(
                    inputId = 'challengeChoiceT',
                    label = 'Intensity Function',
                    choices = c(
                      "Select Answer",
                      "constant",
                      "linear",
                      "exponential",
                      "inverse"
                    ),
                    selectize = TRUE
                  )
                ),
                column(
                  width = 1,
                  br(),
                  htmlOutput('challengeFeedbackT')
                ),
                column(
                  width = 2,
                  br(),
                  htmlOutput('textFeedbackT')
                ),
                column(
                  width = 3,
                  br(),
                  bsButton(
                    inputId = 'submitT',
                    label = 'Check Answer',
                    size = "large"
                  )
                ),
                column(
                  width = 2,
                  offset = 1,
                  br(),
                  bsButton(
                    inputId = 'nextT',
                    label = 'Next',
                    size = "large"
                  )
                )
              ),
              plotOutput("plot2T"),
              htmlOutput("gameTimedPlotAlt")
            )
          )}
        )
      ),
      # References tab ----
      tabItem(
        tabName = "References",
        withMathJax(),
        h2("References"),
        p(
          class = "hangingindent",
          "Attali, D. (2020), shinyjs: Easily Improve the User Experience of Your
          Shiny Apps in Seconds, R package. Available from
          https://CRAN.R-project.org/package=shinyjs"),
        p(
          class = "hangingindent",
          "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny,
          R package. Available from https://CRAN.R-project.org/package=shinyBS"),
        p(
          class = "hangingindent",
          "Carey, R. (2019), boastUtils: BOAST Utilities, R Package. Available from
          https://github.com/EducationShinyAppTeam/boastUtils"),
        p(
          class = "hangingindent",
          "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create
          dashboards with 'Shiny', R Package. Available from
          https://CRAN.R-project.org/package=shinydashboard"),
        p(
          class = "hangingindent",
          "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2019),
          shiny: Web application framework for R, R Package. Available from
          https://CRAN.R-project.org/package=shiny"),
        p(
          class = "hangingindent",
          "Perrier, V., Meyer, F., and Granjon, D. (2020), shinyWidgets: Custom
          Inputs Widgets for Shiny, R package. Available from
          https://CRAN.R-project.org/package=shinyWidgets"),
        p(
          class = "hangingindent",
          "Sali, A. and Attali, D. (2020), shinycssloaders: Add CSS Loading
          Animations to 'shiny' Outputs, R package. Available from
          https://CRAN.R-project.org/package=shinycssloaders"),
        p(
          class = "hangingindent",
          "Wickham, H., François, R., Henry L., and Müller, K. (2020), dplyr: A
          Grammar of Data Manipulation, R package. Available from
          https://CRAN.R-project.org/package=dplyr"),
        p(
          class = "hangingindent",
          "Wickham, H. (2016), ggplot2: Elegant graphics for data analysis,
          R Package, New York: Springer-Verlag. Available from
          https://ggplot2.tidyverse.org")
      )
    )
  )
)