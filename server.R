library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyWidgets)

nevent <- 50

shinyServer(function(input, output, session) {
  # Reactive Values to score game information
  score <- reactiveValues(val=0, prob=1, valT=0, probT=1)
  timer <- reactiveValues(run=FALSE, value=90, paused=FALSE)
  params <- reactiveValues(constant=1, lambdatype="constant",
                           slope=1, growth=1, coefficient=.05)

  # Explore Button
  observeEvent(input$goover, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "exp"
    )
  })

  # Go to Prereqs button
  observeEvent(input$bsButton1, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "exp"
    )
  })

  # Info Button
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "This app explores the Non-homogeneous Poisson Process. After
      reviewing the Prerequisites page, go to the Explore page to explore how
      the rate and number of events influence the behavior of the process for
      various intensity functions. The first graph shows the intensity. The
      second graph graphs the number of events. The third graph shows the
      residuals. The fourth graph shows the estimated density function of the
      interarrival times.",
      type = "info"
    )
  })

  # Description (if checkbox is checked)
  output$design <- renderUI({
    p(withMathJax("This plot was generated from an exponential prior
      distribution. X-axis represents t and Y-axis represents N(t). In the plot,
      you can see that the rate \\(\\lambda\\) multiplied by time (t) roughly
      equals to the number of events up to t (N(t))."))
  })

  # Go button for game
  observeEvent(input$bsButton4, {
    updateTabItems(
      session = session,
      inputId = "game",
      selected = "fib"
      )
  })

  # Simulates data for process (so same data can be used for all plots)
  data <- reactive({
    # linear lambda function: t/3
    if (input$lambdatype=='linear'){
      timeFun <- function(y, time){
        (sqrt((2/input$slope)*(y+(input$slope/2)*time^2)))}}

    # exponential lambda function: exp{3t}
    else if (input$lambdatype=='exponential'){
      timeFun <- function(y, time){(
        1/input$growth)*log(input$growth*y+exp(input$growth*time))}}

    # inverse lambda function: 1/1+3t
    else if (input$lambdatype=='inverse'){
      timeFun <- function(y, time){
        (((1+input$coefficient*time)*
            exp(input$coefficient*y)-1)/input$coefficient)}}

    # constant lambda function: 3
    else if (input$lambdatype=='constant'){
      timeFun <- function(y, time)((y/input$constant)+time)}
    else{
      timeFun <- function(y, time)(0)
    }

    # Set up matrices to hold all simulated values
    xValue <- matrix(0, nrow = input$path, ncol = input$nevent)
    yValue <- matrix(0, nrow = input$path, ncol = input$nevent)
    resiValue <- matrix(0, nrow = input$path, ncol = input$nevent)

    # Run simulation for each path
    for (j in 1:input$path){
      Y <- rexp(input$nevent,1)
      newtime <- 0
      x <- NULL
      i <- 1
      # Create data by moving through time
      while (i<(input$nevent+1)){
        time <- newtime
        x <- append(x,timeFun(Y[i], time))
        newtime <- timeFun(Y[i], time)
        i <- i+1
      }
      m <- x
      h <- 1:input$nevent
      int_lambda <- NULL
      # Take integrals of intensity function
      for (k in m){
        int_lambda <- append(int_lambda,
                             integrate(intensity(), lower = 0, upper = k)$value)
      }
      resi <- (h-int_lambda)
      xValue[j,] <- m
      yValue[j,] <- h
      resiValue[j,] <- resi
    }
    # returns a data frame with the three values to be used in the various plots
    list(xValue=xValue, yValue=yValue, resiValue=resiValue)
  })

  # Gives the intensity function
  intensity <- reactive({
    ## linear lambda function: t/3
    if (input$lambdatype=='linear'){
      intensity <- function(t) ((input$slope) * t)}

    ## exponential lambda function: exp{3t}
    else if (input$lambdatype=='exponential'){
      intensity <- function(t)(exp(input$growth*t))}

    ## inverse lambda function: 1/1+3t
    else if (input$lambdatype=='inverse'){
      intensity <- function(t) (1/(1+input$coefficient*t))}

    ## constant lambda function: 3
    else if (input$lambdatype=='constant'){
      intensity <- function(t)(input$constant*t^0)}
    intensity
  })

  # Plot intensity function
  output$plot1 <- renderPlot({
    plot <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
      stat_function(fun=intensity(), size=1) +
      xlab("Time (t)") +
      ylab("Lambda(t)") +
      ggtitle("Intensity Function") +
      scale_x_continuous(limits = c(0, max(data()$xValue)*1.1),
                         expand = expansion(mult = 0, add = c(0,0.05))) +
      theme_bw() +
      theme(
        axis.text = element_text(size=18),
        plot.title = element_text(size=18),
        axis.title = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    if(input$lambdatype=="constant"){
      plot <- plot + ylim(c(input$constant-1, input$constant+1))
    }
    else{
      plot <- plot +
        scale_y_continuous(expand = expansion(mult = 0, add = c(0,0.05)))
    }
    plot
  })

  # Plot events
  output$plot2 <- renderPlot({
    # Set up data
    xValue <- data()$xValue
    yValue <- data()$yValue
    point <- ceiling(input$nevent/3)
    x <- NULL
    y <- NULL
    grp <- NULL
    for(i in 1:input$path){
      x <- c(x, xValue[i,])
      y <- c(y, yValue[i,])
      grp <- c(grp, rep(i, input$nevent))
    }
    data <- data.frame(xValue=x, yValue=y, group=grp)

    # Plot each path
    plot <- ggplot(aes(x=xValue,
                       y=yValue,
                       group=as.factor(group),
                       color=as.factor(group)),
                   data=data) +
      geom_point(size=2) +
      ggtitle("Number of Events vs. Time")+
      xlab("Time (t)") +
      ylab("Number of events") +
      theme_bw() +
      theme(
        axis.text = element_text(size=18),
        plot.title = element_text(size=18),
        axis.title = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size=14),
        legend.position="none"
      ) +
      scale_y_continuous(limits = c(0, max(yValue)*1.1),
                         expand = expansion(mult = 0, add = c(0,0.05))) +
      scale_x_continuous(limits = c(0, max(xValue)*1.1),
                         expand = expansion(mult = 0, add = c(0,0.05))) +
      scale_color_manual(values = boastPalette[c(1:4, 6)])
    # Only include the s and s+t if there are at least 2 events
    if(input$nevent>1){
      plot <- plot+
        geom_path()+
        geom_segment(aes(x=xValue[point],
                         y=point,
                         xend=xValue[point*2],
                         yend=point), color="black",
                     linetype=2)+
        geom_segment(aes(x=xValue[point],
                         y=point/2,
                         xend=xValue[point],
                         yend= point),
                     color="black",
                     linetype=2)+
        geom_segment(aes(x=xValue[point*2],
                         y=point/2,
                         xend=xValue[point*2],
                         yend=point*2),
                     color="black",
                     linetype=2)+
        geom_segment(aes(x=xValue[point*2],
                         y=point,
                         xend=xValue[point*2]+max(xValue)*.1,
                         yend=point),
                     color="black",
                     size=.75)+
        geom_segment(aes(x=xValue[point*2],
                         y=point*2,
                         xend=xValue[point*2]+max(xValue)*.1,
                         yend=point*2),
                     color="black",
                     size=.75)+
        geom_segment(aes(x=xValue[point*2]+max(xValue)*.05,
                         y=point,
                         xend=xValue[point*2]+max(xValue)*.05,
                         yend= point*2),
                     arrow=arrow(length=unit(.4,"cm")),
                     color="black",
                     size=.75)+
        geom_segment(aes(x=xValue[point*2]+max(xValue)*.05,
                         y=point*2, xend=xValue[point*2]+max(xValue)*.05,
                         yend=point),
                     arrow=arrow(length=unit(.4,"cm")),
                     color="black",
                     size=.75)+
        geom_text(aes(xValue[point],
                      y = point/3,
                      label = 's'),
                  color="black")+
        geom_text(aes(xValue[point*2]+.01*max(xValue),
                      y = point/3,
                      label = 's+t'),
                  color="black")
    }
    plot
  })

  # Text to go with second plot
  output$plot2Text <- renderUI({
    p(withMathJax("
           The above Number of Events vs. Time plot is for a Nonhomogeneous Poisson
           Plot. In the plot, you can see that the rate \\(\\lambda\\) multiplied
           by time (t) roughly equals to the number of events up to t (N(t)). The
           distance between points on the graph is N(t+s)-N(s), which is a Poisson
           distribution with parameter m(t+s)-m(s). m(t+s) can be calculated by
           integrating our lambda function from 0 to t+s, while m(s) is integrating
           the lambda function from 0 to s."))
  })

  # Residual Plot
  output$plot3 <- renderPlot({
    # Get data values
    xValue <- data()$xValue
    resiValue <- data()$resiValue
    x <- NULL
    y <- NULL
    grp <- NULL
    for(i in 1:input$path){
      x <- c(x, xValue[i,])
      y <- c(y, resiValue[i,])
      grp <- c(grp, rep(i, input$nevent))
    }
    data <- data.frame(xValue=x, yValue=y, group=grp)

    # Plot each path
    plot <- ggplot(aes(x=xValue,
                       y=yValue,
                       group=as.factor(group),
                       color=as.factor(group)),
                   data=data) +
      geom_point(size=2) +
      geom_hline(aes(yintercept=0), linetype="dashed", size=1) +
      ggtitle("Residuals vs. Time") +
      xlab("Time (t)") +
      ylab("Residual{N(t)-E(N(t))}") +
      theme_bw()+
      theme(
        axis.text = element_text(size=18),
        plot.title = element_text(size=18),
        axis.title = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none") +
      scale_color_manual(values = boastPalette[c(1:4, 6)])+
      scale_x_continuous(limits = c(0, max(xValue)*1.1),
                         expand = expansion(mult = 0, add = c(0,0.05)))

    if(input$nevent>1){
      plot <- plot + geom_path()
    }
    plot
  })

  # Density Plot
  output$plot4 <- renderPlot({
    # Set up data frames
    arr <- cbind(matrix(0,nrow=input$path,ncol=1),data()$xValue)
    interArr <- data.frame()
    Int <- data.frame()
    Group <- data.frame()

    # Fill in intervals and path number data frame for plotting
    for (i in 1:input$path){
      for (j in 1:input$nevent){
        Int[j + input$nevent*(i - 1), 1] <- arr[i,j+1] - arr[i,j]
        Group[j + input$nevent*(i - 1), 1] <- i
      }
      interArr <- cbind(Int,Group)
    }
    names(interArr) <- c("Int","Group")

    # Create actual plot
    plot <- ggplot(interArr,
                   aes(
                     x = Int,
                     group = Group,
                     color = as.factor(Group),
                     adjust = 2
                   )) +
      ggtitle("Estimated Interarrival Time Distribution") +
      xlab("Interarrival Time") +
      ylab("Estimated Density") +
      labs(color = "Path") +
      theme(
        axis.text = element_text(size=18),
        plot.title = element_text(size=18),
        axis.title = element_text(size=18),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none"
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05), add = 0)) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.05), add = 0)) +
      scale_color_manual(values = boastPalette[c(1:4, 6)])
    if(input$nevent>1){
      plot <- plot + stat_density(geom = "line", size = 1,position='identity')
    }
    plot
  })

  # Warning message for single sample interarrival times
  output$feedback = renderPrint({
    if (input$nevent==1) {cat("Warning: You need more than one event in order to
                              estimate the density of interarrival times.")}
  })

  dataGame <- reactive({
    # linear lambda function: t/3
    if (params$lambdatype=='linear'){
      timeFun <- function(y, time){
        (sqrt((2/params$slope)*(y+(params$slope/2)*time^2)))}}

    # exponential lambda function: exp{3t}
    else if (params$lambdatype=='exponential'){
      timeFun <- function(y, time){
        (1/params$growth)*log(params$growth*y+exp(params$growth*time))}}

    # inverse lambda function: 1/1+3t
    else if (params$lambdatype=='inverse'){
      timeFun <- function(y, time){
        ((1+params$coefficient*time)*exp(params$coefficient*y)-1)/
          params$coefficient}}

    # constant lambda function: 3
    else if (params$lambdatype=='constant'){
      timeFun <- function(y, time)((y/params$constant)+time)}

    # Set up matrices to hold all simulated values
    xValue <- matrix(0, nrow = 1, ncol = nevent)
    yValue <- matrix(0, nrow = 1, ncol = nevent)
    resiValue <- matrix(0, nrow = 1, ncol = nevent)

    # Run simulation for each path
    Y <- rexp(nevent,1)
    newtime <- 0
    x <- NULL
    i <- 1
    # Create data by moving through time
    while (i<(nevent+1)){
      time <- newtime
      x <- append(x,timeFun(Y[i], time))
      newtime <- timeFun(Y[i], time)
      i <- i+1
    }
    m <- x
    h <- 1:nevent
    int_lambda <- NULL
    # Take integrals of intensity function
    for (k in m){
      int_lambda <- append(int_lambda,
                           integrate(intensityGame(), lower = 0, upper = k)$value)
    }
    resi <- (h-int_lambda)
    xValue[1,] <- m
    yValue[1,] <- h
    resiValue[1,] <- resi
    # returns a data frame with the three values to be used in the various plots
    list(xValue=xValue, yValue=yValue, resiValue=resiValue)
  })

  # Gives the intensity function
  intensityGame <- reactive({
    ## linear lambda function: t/3
    if (params$lambdatype=='linear'){
      intensity <- function(t) ((params$slope) * t)}

    ## exponential lambda function: exp{3t}
    else if (params$lambdatype=='exponential'){
      intensity <- function(t)(exp(params$growth*t))}

    ## inverse lambda function: 1/1+3t
    else if (params$lambdatype=='inverse'){
      intensity <- function(t) (1/(1+params$coefficient*t))}

    ## constant lambda function: 3
    else if (params$lambdatype=='constant'){
      intensity <- function(t)(params$constant*t^0)}
    intensity
  })

  # Shows game scores for practice and Timed mode
  output$score <- renderText({paste("Score:", score$val)})
  output$scoreT <- renderText({paste("Score:", score$valT)})

  # Creates the plots for the game (practice and timed mode)
  output$gamePlot2 <- renderPlot({makePlot()})
  output$plot2T <- renderPlot({makePlot()})

  # Makes actual plot for game section
  makePlot <- reactive({
    # Set up data
    xValue <- dataGame()$xValue
    yValue <- dataGame()$yValue
    point <- ceiling(nevent/3)
    x <- NULL
    y <- NULL
    grp <- NULL
    x <- c(x, xValue[1,])
    y <- c(y, yValue[1,])
    data <- data.frame(xValue=x, yValue=y)
    # Plot each path
    plot <- ggplot(aes(x=xValue, y=yValue), data=data) +
      geom_path()+
      geom_point(size=2) +
      ggtitle("Number of Events vs. Time")+
      xlab("Time (t)") +
      ylab("Number of events") +
      theme_bw()+
      theme(
        axis.text = element_text(size=18),
        plot.title = element_text(size=18),
        axis.title = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none"
      ) +
      scale_y_continuous(limits = c(0, max(yValue)*1.1),
                         expand = expansion(mult = 0, add = c(0,0.05))) +
      scale_x_continuous(limits = c(0, max(xValue)*1.1),
                         expand = expansion(mult = 0, add = c(0,0.05)))

    plot
  })

  # Updates that occur hitting next button in practice mode
  observeEvent(input$nextX, {
    params$lambdatype <- sample(c("constant", "exponential", "linear", "inverse"),
                                size = 1,
                                replace=TRUE)
    if(params$lambdatype=="constant"){
      params$constant <- runif(1,1,10)
    }
    else if(params$lambdatype=="exponential"){
      params$growth <- runif(1,.1,1)
    }
    else if(params$lambdatype=="linear"){
      params$slope <- runif(1,.1,1)
    }
    else{
      params$coefficient <- runif(1,.01,.05)
    }
    score$prob <- 1
    shinyjs::hideElement("challengeFeedback")
    shinyjs::hideElement('textFeedback')
    shinyjs::enable("submitX")
    updateSelectInput(session, "challengeChoice", selected="Select Answer")
  })

  # Updates that occur hitting next button in timed mode
  observeEvent(input$nextT, {
    params$lambdatype <- sample(c("constant", "exponential", "linear", "inverse"),
                                size = 1,
                                replace=TRUE)
    if(params$lambdatype=="constant"){
      params$constant <- runif(1,1,10)
    }
    else if(params$lambdatype=="exponential"){
      params$growth <- runif(1,.1,1)
    }
    else if(params$lambdatype=="linear"){
      params$slope <- runif(1,.1,1)
    }
    else{
      params$coefficient <- runif(1,.01,.05)
    }
    score$probT <- 1
    shinyjs::hideElement("challengeFeedbackT")
    shinyjs::hideElement('textFeedbackT')
    shinyjs::enable("submitT")
    shinyjs::disable("nextT")
    updateSelectInput(session, "challengeChoiceT", selected="Select Answer")
  })

  # Updates that occur hitting submit button in practice mode
  observeEvent(input$submitX, {
    # If correct
    if (input$challengeChoice == params$lambdatype) {
      output$challengeFeedback <- renderUI ({
        img(src = 'check.PNG',
            alt = "Correct Answer",
            height = 30,
            width = 30)
      })
      output$textFeedback <- renderUI ({ #UI
        'Congratulations!'
      })
      score$val <- score$val+score$prob
      score$prob <- 1
      shinyjs::disable("submitX")
    }
    # If incorrect
    else {
      output$challengeFeedback <- renderUI ({
        img(src = 'cross.PNG',
            alt = "Incorrect Answer",
            height = 30,
            width = 30)
      })
      output$textFeedback <- renderUI ({
        "Please try again"
      })
      score$prob <- score$prob-.5
    }
    shinyjs::showElement("challengeFeedback")
    shinyjs::showElement('textFeedback')
  })

  # Updates that occur hitting submit button in timed mode
  observeEvent(input$submitT, {
    # If correct, move to next problem
    if (input$challengeChoiceT == params$lambdatype) {
      output$challengeFeedbackT <- renderUI ({
        img(src = 'check.PNG',
            alt = "Correct Answer",
            height = 30,
            width = 30)
      })
      output$textFeedbackT <- renderUI ({ #UI
        'Congratulations!'
      })
      score$valT <- score$valT+score$probT
      score$probT <- 1
      shinyjs::disable("submitT")
      shinyjs::enable("nextT")
    }
    # If incorrect, give feedback
    else {
      output$challengeFeedbackT <- renderUI ({
        img(src = 'cross.png',
            alt = "Incorrect Answer",
            height = 30,
            width = 30)
      })
      output$textFeedbackT <- renderUI ({
        "Please try again"
      })
      score$probT <- score$probT-.5
    }
    shinyjs::showElement('textFeedbackT')
    shinyjs::showElement("challengeFeedbackT")
  })

  # Controls Timer
  observe({
    invalidateLater(1000)
    isolate(
      if(timer$run){
        # Pauses the game if the user switches off the game tab
        if(input$game != "time" || input$pages != "Game"){
          click("stopTimedGame")
        }
        timer$value <- timer$value-1
        # What to do when time runs out
        if(timer$value==-1){
          timer$run <- FALSE
          shinyjs::disable("submitT")
          shinyjs::disable("nextT")
          shinyjs::disable("startTimedGame")
          shinyjs::disable("stopTimedGame")
          shinyjs::enable("resetTimedGame")
          timer$paused <- FALSE
          sendSweetAlert(
            session = session,
            title = paste("Final Score: ", score$valT),
            text = ifelse(score$valT<5, "Try again for a better score.",
                          ifelse(score$valT<10, "Good work",
                                 "Great Job!"))
          )
        }}
    )
  })

  # Starts timed game
  observeEvent(input$startTimedGame, {
    timer$run <- TRUE
    score$val <- 0
    score$prob <- 1
    click('nextT')
    shinyjs::disable("startTimedGame")
    shinyjs::disable("resetTimedGame")
    shinyjs::enable("stopTimedGame")
  })

  # Displays the timer
  output$countdown <- renderText({
    paste("Time:", timer$value)})

  # Stop button for timed game
  observeEvent(input$stopTimedGame, {
    shinyjs::enable("submitT")
    shinyjs::enable("nextT")
    shinyjs::disable("stopTimedGame")
    shinyjs::enable("resetTimedGame")
    shinyjs::enable("startTimedGame")
    timer$run <- FALSE
    timer$paused <- TRUE
  })

  # Reset button for timed game
  observeEvent(input$resetTimedGame, {
    timer$value <- 90
    output$countdown <- renderText({paste("Time:", timer$value)})
    shinyjs::enable("submitT")
    shinyjs::enable("nextT")
    shinyjs::enable("startTimedGame")
    score$valT <- 0
    score$probT <- 1
    timer$paused <- FALSE
  })

  # Reset button for practice mode
  observeEvent(input$resetPractice,{
    score$val <- 0
    score$prob <- 1
    click("nextX")
  })

  output$pauseMessage <- renderText({
    if(timer$paused){
      "The game is currently paused."
    }
  })

  # Tells UI whether or not to show the timed game game portion
  output$showGame <- reactive({timer$run})
  outputOptions(output, "showGame", suspendWhenHidden=FALSE)


  # Alt text
  output$plot1Alt <- renderUI({
    text <- paste("`This plot shows a ", input$lambdatype, "function, which
                  represents that intensity function for the graph.`")
    tags$script(HTML(paste0("$(document).ready(function() {
                            document.getElementById('plot1').setAttribute('aria-label',",
                            text, ")})"))
    )
  })

  output$gamePracticePlotAlt <- renderUI({
    arrivalTimes <- toString(round(dataGame()$xValue, 2))
    tags$script(HTML(
      paste0("$(document).ready(function() {
            document.getElementById('gamePlot2').setAttribute('aria-label',
            `This plot shows the path taken by the run generated for
            the problem. For this problem, the arrival times are ",
             arrivalTimes, "`)})"
      )))
  })

  output$gameTimedPlotAlt <- renderUI({
    arrivalTimes <- toString(round(dataGame()$xValue, 2))
    tags$script(HTML(
      paste0("$(document).ready(function() {
            document.getElementById('plot2T').setAttribute('aria-label',
            `This plot shows the path taken by the run generated for
            the problem. For this problem, the arrival times are ",
             arrivalTimes, "`)})"
      )))
  })

})
