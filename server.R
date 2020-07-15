library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyWidgets)

# Reactive Values to score game information
score<-reactiveValues(val=0, prob=1, valT=0, probT=1)
timer<-reactiveValues(run=FALSE, value=60)
params<-reactiveValues(constant=1, lambdatype="constant", slope=1, growth=1, coefficient=.05)
nevent<-50

colors =  c("#0072B2","#D55E00","#009E73","#ce77a8","#E69F00") # Colors for plots

shinyServer(function(input, output, session) {
  # Explore Button
  observeEvent(input$goover, {
    updateTabItems(session, "tabs", "exp")
  })
  
  # Go to Prereqs button
  observeEvent(input$bsButton1, {
    updateTabItems(session, "tabs", "exp")
  })

  # Info Button
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "This app explores the Non-homogeneous Poisson Process. After reviewing the Prerequisites page, 
      go to the Explore page to explore how the rate and number of events influence the behavior of the process for various intensity functions.
      The first graph shows the intensity. The second graph graphs the number of events. The third graph
      shows the residuals. The fourth graph shows the estimated density function of the interarrival times.",
      type = "info"
    )
  })
  
  # Description (if checkbox is checked)
  output$design = renderUI({
    if(input$designcheckbox){
      p(withMathJax("This plot was generated from an exponential prior distribution. X-axis represents t and Y-axis 
         represents N(t). In the plot, you can see that the rate \\(\\lambda\\) multiplied by time (t) roughly 
         equals to the number of events up to t (N(t))."))
    }
  })
  
  # Go button for game
  observeEvent(input$bsButton4,{updateTabItems(session, "game", "fib")})
  
  # Simulates data for process (so same data can be used for all plots)
  data <- reactive({
    # linear lambda function: t/3
    if (input$lambdatype=='linear'){
      timeFun<-function(y, time)(sqrt((2/input$slope)*(y+(input$slope/2)*time^2)))}
    
    # exponential lambda function: exp{3t}
    else if (input$lambdatype=='exponential'){
      timeFun<-function(y, time)(1/input$growth)*log(input$growth*y+exp(input$growth*time))}
    
    # inverse lambda function: 1/1+3t
    else if (input$lambdatype=='inverse'){
      timeFun<-function(y, time)(((1+input$coefficient*time)*exp(input$coefficient*y)-1)/input$coefficient)}
    
    # constant lambda function: 3
    else if (input$lambdatype=='constant'){
      timeFun<-function(y, time)((y/input$constant)+time)}
    
    # Set up matrices to hold all simulated values
    x.value = matrix(0, nrow = input$path, ncol = input$nevent)
    y.value = matrix(0, nrow = input$path, ncol = input$nevent)
    resi.value = matrix(0, nrow = input$path, ncol = input$nevent)
    
    # Run simulation for each path
    for (j in 1:input$path){
      Y=rexp(input$nevent,1)
      newtime=0
      x=NULL
      i=1
      # Create data by moving through time
      while (i<(input$nevent+1)){
        time=newtime
        x<-append(x,timeFun(Y[i], time))
        newtime=timeFun(Y[i], time)
        i=i+1
      }
      m = x
      h = 1:input$nevent
      int_lambda<- NULL
      # Take integrals of intensity function
      for (k in m){
        int_lambda<-append(int_lambda,integrate(intensity(), lower = 0, upper = k)$value)
      }
      resi=(h-int_lambda)
      x.value[j,]<-m
      y.value[j,]<-h
      resi.value[j,]<-resi
    }
    # returns a data frame with the three values to be used in the various plots
    list(x.value=x.value, y.value=y.value, resi.value=resi.value)
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
  output$plot1<-renderPlot({
    plot<-ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + stat_function(fun=intensity(), size=1) + 
      xlab("Time (t)") +
      ylab("Lambda(t)") +
      ggtitle("Intensity Function") +
      scale_x_continuous(limits = c(0, max(data()$x.value)*1.1), expand = expansion(mult = 0, add = c(0,0.05))) +
      theme_bw() +
      theme(axis.text = element_text(size=18),
            plot.title = element_text(size=18, face="bold"),
            axis.title = element_text(size=18),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
            )
    if(input$lambdatype=="constant"){
      plot<- plot + ylim(c(input$constant-1, input$constant+1))
    }
    else{
      plot <- plot + 
        scale_y_continuous(expand = expansion(mult = 0, add = c(0,0.05)))
    }
    plot
    })
  
  # Plot events
  output$plot2<-renderPlot({
    # Set up data
    x.value<-data()$x.value
    y.value<-data()$y.value
    point <- ceiling(input$nevent/3)
    x<-NULL
    y <- NULL
    grp <- NULL
    for(i in 1:input$path){
      x<- c(x, x.value[i,])
      y<- c(y, y.value[i,])
      grp <- c(grp, rep(i, input$nevent))
    }
    data <- data.frame(x.value=x, y.value=y, group=grp)

    # Plot each path
    plot<-ggplot(aes(x=x.value, y=y.value, group=as.factor(group), color=as.factor(group)), data=data) +
      geom_path()+
      geom_point(size=2) +
      ggtitle("Number of Events vs. Time")+
      xlab("Time (t)") +
      ylab("Number of events") +
      theme_bw() +
      theme(
        axis.text = element_text(size=18),
        plot.title = element_text(size=18, face="bold"),
        axis.title = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size=14),
        legend.position="none"
      )+
      scale_y_continuous(limits = c(0, max(y.value)*1.1), expand = expansion(mult = 0, add = c(0,0.05))) +
      scale_x_continuous(limits = c(0, max(x.value)*1.1), expand = expansion(mult = 0, add = c(0,0.05))) +
      scale_color_manual(values = colors)
    # Only include the s and s+t if there are at least 2 events
    if(input$nevent>1){
      plot<-plot+
        geom_segment(aes(x=x.value[point], y=point, xend=x.value[point*2], yend=point), color="black", linetype=2)+
        geom_segment(aes(x=x.value[point], y=point/2,  xend=x.value[point], yend= point), color="black", linetype=2)+
        geom_segment(aes(x=x.value[point*2],  y=point/2,  xend=x.value[point*2], yend=point*2), color="black", linetype=2)+
        geom_segment(aes(x=x.value[point*2], y=point, xend=x.value[point*2]+max(x.value)*.1, yend=point), color="black", size=.75)+
        geom_segment(aes(x=x.value[point*2], y=point*2, xend=x.value[point*2]+max(x.value)*.1, yend=point*2), color="black", size=.75)+
        geom_segment(aes(x=x.value[point*2]+max(x.value)*.05,  y=point,   xend=x.value[point*2]+max(x.value)*.05,yend= point*2),arrow=arrow(length=unit(.4,"cm")), color="black", size=.75)+
        geom_segment(aes(x=x.value[point*2]+max(x.value)*.05,  y=point*2, xend=x.value[point*2]+max(x.value)*.05,  yend=point),arrow=arrow(length=unit(.4,"cm")), color="black", size=.75)+
        geom_text(aes(x.value[point], y = point/3, label = 's'),color="black")+
        geom_text(aes(x.value[point*2]+.01*max(x.value), y = point/3,label = 's+t'),color="black")
    }
    plot
  })
  
  # Text to go with second plot
  output$plot2Text <- renderText({paste("
           The above Number of Events vs. Time plot is for a Nonhomogeneous Poisson 
           Plot with", input$lambdatype,"rate. The distance between points on 
           the graph is N(t+s)-N(s), which is a Poisson distribution with 
           parameter m(t+s)-m(s). m(t+s) can be calculated by integrating our 
           lambda function from 0 to t+s, while m(s) is integrating the lambda 
           function from 0 to s.") })
  
  # Residual Plot
  output$plot3<-renderPlot({
    # Get data values
    x.value<-data()$x.value
    resi.value<-data()$resi.value
    x<-NULL
    y <- NULL
    grp <- NULL
    for(i in 1:input$path){
      x<- c(x, x.value[i,])
      y<- c(y, resi.value[i,])
      grp <- c(grp, rep(i, input$nevent))
    }
    data <- data.frame(x.value=x, y.value=y, group=grp)

    # Plot each path
    ggplot(aes(x=x.value, y=y.value, group=as.factor(group), color=as.factor(group)), data=data) +
      geom_path() +
      geom_point(size=2) +
      geom_hline(aes(yintercept=0), linetype="dashed", size=1)+
      ggtitle("Residuals vs. Time") +
      xlab("Time (t)") +
      ylab("Residual{N(t)-E(N(t))}") +
      theme_bw()+
      theme(
        axis.text = element_text(size=18),
        plot.title = element_text(size=18, face="bold"),
        axis.title = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none") +
      scale_color_manual(values = colors)+
      scale_x_continuous(limits = c(0, max(x.value)*1.1), expand = expansion(mult = 0, add = c(0,0.05))) 
  })
  
  # Density Plot
  output$plot4<-renderPlot({
    # Set up data frames
    arr = cbind(matrix(0,nrow=input$path,ncol=1),data()$x.value)
    inter.arr = data.frame()
    Int = data.frame()
    Group = data.frame()
    
    # Fill in intervals and path number data frame for plotting
    for (i in 1:input$path){
      for (j in 1:input$nevent){
        Int[j + input$nevent*(i - 1), 1] = arr[i,j+1] - arr[i,j]
        Group[j + input$nevent*(i - 1), 1] = i
      }
      inter.arr = cbind(Int,Group)
    }
    names(inter.arr) = c("Int","Group")
    
    # Create actual plot
    ggplot(inter.arr,
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
        plot.title = element_text(size=18, face="bold"),
        axis.title = element_text(size=18),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none"
      ) +
      stat_density(geom = "line", size = 1,position='identity')+
      scale_y_continuous(expand = expansion(mult = c(0, 0.05), add = 0)) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.05), add = 0)) +
      scale_color_manual(values = colors)
    
    })
  
  # Warning message for single sample interarrival times  
  output$feedback = renderPrint({
    if (input$nevent==1) {cat("Warning: You need more than one event in order to estimate the density of interarrival times.")}
  })
  
  dataGame <- reactive({
    # linear lambda function: t/3
    if (params$lambdatype=='linear'){
      timeFun<-function(y, time)(sqrt((2/params$slope)*(y+(params$slope/2)*time^2)))}

    # exponential lambda function: exp{3t}
    else if (params$lambdatype=='exponential'){
      timeFun<-function(y, time)(1/params$growth)*log(params$growth*y+exp(params$growth*time))}

    # inverse lambda function: 1/1+3t
    else if (params$lambdatype=='inverse'){
      timeFun<-function(y, time)(((1+params$coefficient*time)*exp(params$coefficient*y)-1)/params$coefficient)}

    # constant lambda function: 3
    else if (params$lambdatype=='constant'){
      timeFun<-function(y, time)((y/params$constant)+time)}

    # Set up matrices to hold all simulated values
    x.value = matrix(0, nrow = 1, ncol = nevent)
    y.value = matrix(0, nrow = 1, ncol = nevent)
    resi.value = matrix(0, nrow = 1, ncol = nevent)

    # Run simulation for each path
    Y=rexp(nevent,1)
    newtime=0
    x=NULL
    i=1
    # Create data by moving through time
    while (i<(nevent+1)){
      time=newtime
      x<-append(x,timeFun(Y[i], time))
      newtime=timeFun(Y[i], time)
      i=i+1
    }
    m = x
    h = 1:nevent
    int_lambda<- NULL
    # Take integrals of intensity function
    for (k in m){
      int_lambda<-append(int_lambda,integrate(intensityGame(), lower = 0, upper = k)$value)
    }
    resi=(h-int_lambda)
    x.value[1,]<-m
    y.value[1,]<-h
    resi.value[1,]<-resi
    # returns a data frame with the three values to be used in the various plots
    list(x.value=x.value, y.value=y.value, resi.value=resi.value)
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
  output$score<-renderText({paste("Current Score:", score$val)})
  output$scoreT<-renderText({paste("Current Score:", score$valT)})
  
  # Creates the plots for the game (practice and timed mode)
  output$gamePlot2<-renderPlot({makePlot()})
  output$plot2T<-renderPlot({makePlot()})
  
  # Makes actual plot for game section
  makePlot<-reactive({    
    # Set up data
    x.value<-dataGame()$x.value
    y.value<-dataGame()$y.value
    point <- ceiling(nevent/3)
    x<-NULL
    y <- NULL
    grp <- NULL
    x<- c(x, x.value[1,])
    y<- c(y, y.value[1,])
    data <- data.frame(x.value=x, y.value=y)
    # Plot each path
    plot<-ggplot(aes(x=x.value, y=y.value), data=data) +
      geom_path()+
      geom_point(size=2) +
      ggtitle("Number of Events vs. Time")+
      xlab("Time (t)") +
      ylab("Number of events") +
      theme_bw()+
      theme(
        axis.text = element_text(size=18),
        plot.title = element_text(size=18, face="bold"),
        axis.title = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none"
      )+
      scale_y_continuous(limits = c(0, max(y.value)*1.1), expand = expansion(mult = 0, add = c(0,0.05))) +
      scale_x_continuous(limits = c(0, max(x.value)*1.1), expand = expansion(mult = 0, add = c(0,0.05))) 
    
    plot
  })
  
  # Updates that occur hitting next button in practice mode
  observeEvent(input$nextX, {
    params$lambdatype<-sample(c("constant", "exponential", "linear", "inverse"), 1, replace=TRUE)
    if(params$lambdatype=="constant"){
      params$constant<-runif(1,1,10)   
    }
    else if(params$lambdatype=="exponential"){
      params$growth<-runif(1,.1,1) 
    }
    else if(params$lambdatype=="linear"){
      params$slope<-runif(1,.1,1) 
    }
    else{
      params$coefficient<-runif(1,.01,.05) 
    }
    score$prob<-1
    shinyjs::hideElement("challengeFeedback")
    shinyjs::hideElement('textFeedback')
    shinyjs::enable("submitX")
  })
  
  # Updates that occur hitting next button in timed mode
  observeEvent(input$nextT, {
    params$lambdatype<-sample(c("constant", "exponential", "linear", "inverse"), 1, replace=TRUE)
    if(params$lambdatype=="constant"){
      params$constant<-runif(1,1,10)   
    }
    else if(params$lambdatype=="exponential"){
      params$growth<-runif(1,.1,1) 
    }
    else if(params$lambdatype=="linear"){
      params$slope<-runif(1,.1,1) 
    }
    else{
      params$coefficient<-runif(1,.01,.05) 
    }
    score$probT<-1
    shinyjs::hideElement("challengeFeedbackT")
    shinyjs::hideElement('textFeedbackT')
    shinyjs::enable("submitT")
  })
  
  # Updates that occur hitting submit button in practice mode
  observeEvent(input$submitX, {
    # If correct
    if (input$challengeChoice == params$lambdatype) {
      output$challengeFeedback <- renderUI ({
        div(style = "text-align: center", img(src = 'check.png', height = 30, width = 30))
      })
      output$textFeedback <- renderUI ({ #UI
        div(style = "text-align: center", 'Congratulations!')
      })
      score$val<-score$val+score$prob
      score$prob<-1
      shinyjs::disable("submitX")
    }
    # If incorrect
    else {
      output$challengeFeedback <- renderUI ({
        div(style = "text-align: center", img(src = 'cross.png', height = 30, width = 30))
      })
      output$textFeedback <- renderUI ({
        div(style = "text-align: center", "Try Again.")
      })
      score$prob<-score$prob-.5
    }
    shinyjs::showElement("challengeFeedback")
    shinyjs::showElement('textFeedback')
  })
  
  # Updates that occur hitting submit button in timed mode
  observeEvent(input$submitT, {
    # If correct, move to next problem
    if (input$challengeChoiceT == params$lambdatype) {
      score$valT<-score$valT+score$probT
      score$probT<-1
      click("nextT")
    }
    # If incorrect, give feedback
    else {
      output$challengeFeedbackT <- renderUI ({
        div(style = "text-align: center", img(src = 'cross.png', height = 30, width = 30))
      })
      output$textFeedbackT <- renderUI ({
        div(style = "text-align: center", "Try Again.")
      })
      score$probT<-score$probT-.5
      shinyjs::showElement("challengeFeedbackT")
      shinyjs::showElement('textFeedbackT')
    }
  })
  
  # Controls Timer
  observe({
    invalidateLater(1000)
    isolate(
      if(timer$run){
        timer$value<-timer$value-1
        # What to do when time runs out
        if(timer$value==-1){
          timer$run<-FALSE
          shinyjs::disable("submitT")
          shinyjs::disable("nextT")
          shinyjs::disable("startTimedGame")
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
    timer$run<-TRUE
    score$val<-0
    score$prob<-1
    click('nextT')
  })
  
  # Displays the timer
  output$countdown<-renderText({paste("Time:", timer$value)})
  
  # Reset button for timed game
  observeEvent(input$resetTimedGame, {
    timer$value<-60
    shinyjs::enable("submitT")
    shinyjs::enable("nextT")
    shinyjs::enable("startTimedGame")
    score$valT<-0
    score$probT<-1
  })
  
  # Reset button for practice mode
  observeEvent(input$resetPractice,{
    score$val<-0
    score$prob<-1
    click("nextX")
  })
  
  # Tells UI whether or not to show the timed game game portion
  output$showGame<-reactive({timer$run})
  outputOptions(output, "showGame", suspendWhenHidden=FALSE)
})