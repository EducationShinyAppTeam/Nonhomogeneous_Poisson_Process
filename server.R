library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(plotly)
library(poisson)
library(ggplot2)
library(Runuran)
library(data.table)





colors =  c("#0072B2","#D55E00","#009E73","#ce77a8","#E69F00")
shinyServer(function(input, output, session) {
  #Go to overview Button
  observeEvent(input$goover, {
    updateTabItems(session, "tabs", "overview")
    })
    
    observeEvent(input$bsButton1, {
      updateTabItems(session, "tabs", "exp")
    })
  

  output$design = renderUI({
    if(input$designcheckbox){
      h4(withMathJax("This plot was generated from an exponential prior distribution. X-axis represents t and Y-axis 
         represents N(t). In the plot, you can see that the rate \\(\\lambda\\) multiplied by time (t) roughly 
         equals to # of events up to t (N(t))."))
    }
    })

  

  
  generatePlot <- function(lambdatype,variable){
    ## linear lambda function: t/3
    if (lambdatype=='linear'){
      
      intensity <- function(t) ((input$slope) * t)
      x.value = matrix(0, nrow = input$path, ncol = input$nevent)
      y.value = matrix(0, nrow = input$path, ncol = input$nevent)
      resi.value = matrix(0, nrow = input$path, ncol = input$nevent)
      for (j in 1:input$path){
        Y=rexp(input$nevent,1)
        newtime=0
        x=NULL
        i=1
        while (i<(input$nevent+1)){
          time=newtime
          x<-append(x,sqrt((2/input$slope)*(Y[i]+(input$slope/2)*time^2)))
          newtime=sqrt((2/input$slope)*(Y[i]+(input$slope/2)*time^2))
          i=i+1
        }
        m = x
        h = 1:input$nevent
        int_lambda<- NULL
        for (k in m){
          integrand <- function(t) ((input$slope) * t)
          int_lambda<-append(int_lambda,integrate(integrand, lower = 0, upper = k)$value)
        }
        resi=(h-int_lambda)
        for (a in 1:input$nevent){
          x.value[j,a] = m[a] 
          y.value[j,a] = h[a] 
          resi.value[j,a] = resi[a]
        }
      }
    
      if (variable==1){
      ## n constant, rate slider
        plot(intensity, from=0, to=20, xlab="Time", ylab="lambda(t)",main = "Intensity function")}
     
      ## rate constant, n slider
      if (variable==2){
      for (i in 1:input$path){
        plot(  x.value[1:i,], y.value[1:i,],
               xlab="Time", ylab="Number of Events", main = "Number of Events vs. Time",
               col = colors[1:i], pch=16)+
          segments(x.value[1,30], 30, x.value[1,60], 30,lty=2)+
          segments(x.value[1,30], 0,  x.value[1,30],  30,lty=2)+
          segments(x.value[1,60],  0,  x.value[1,60],  60,lty=2)+
          segments(x.value[1,60], 30, x.value[1,68], 30)+
          segments(x.value[1,60], 60, x.value[1,68], 60)+
          
          
          arrows( x.value[1,63],  30,   x.value[1,63], 60,
                  length = 0.1)+
          arrows( x.value[1,63],  60, x.value[1,63],  30,length = 0.1)+

        text(x.value[1,31], y = 2, labels = 's',color='blue')+
          text(x.value[1,63], y = 2, labels = 's+t',color='blue')
        

        for (k in 1:i){
          m = x.value[k,]
          y = y.value[k,]
          lines(m[order(m)],y[order(m)], pch=16, lwd = 1.5, col = colors[k])
        }
      }
      }
      
    
      if (variable==3){
        
        for (i in 1:input$path){
          plot( x.value[1:i,], resi.value[1:i,], 
               xlab="Time", ylab="Redisual{N(t)-E(N(t))}", main = "Residuals vs. Time", col = colors[1:i], pch=16)
          for (k in 1:i){
            m = x.value[k,]
            resi = resi.value[k,]
            lines(m[order(m)],resi[order(m)], pch=16, lwd = 1.5, col = colors[k])
          }
        
      }
      }
      
     
        arr = cbind(matrix(0,nrow=input$path,ncol=1),x.value)
        inter.arr = data.frame()
        Int = data.frame()
        Group = data.frame()

        for (i in 1:input$path){
          for (j in 1:input$nevent){
            Int[j + input$nevent*(i - 1), 1] = arr[i,j+1] - arr[i,j]
            Group[j + input$nevent*(i - 1), 1] = i
          }
          inter.arr = cbind(Int,Group)
        }

        names(inter.arr) = c("Int","Group")
        # inter.arr=data.table(inter.arr)
        
        output$dis = renderPlot({
        ggplot(inter.arr,
               aes(
                 x = Int,
                 group = Group,
                 color = as.factor(Group),
                 adjust = 2
               )) +
          ggtitle("Interarrival Time Distribution") +
          xlab("Interarrival Time") +
          ylab("Density") +
          labs(color = "Path") +
          theme(
            plot.title = element_text(hjust = 0.5,
                                      face = "bold",
                                      size = 14),
            panel.background = element_rect(fill = 'white', colour = 'black'),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          ) +
          stat_density(geom = "line", size = 1)+
          scale_y_continuous(expand = expand_scale(mult = c(0, 0.15), add = 0)) +
          scale_x_continuous(expand = expand_scale(mult = c(0, 0.05), add = 0)) +
          scale_color_manual(values = colors)})
       
        
      
    }
    
    
    
      
      ## exponential lambda function: exp{3t}
      if (lambdatype=='exponential'){
        intensity <- function(t)(exp(input$growth*t))
        x.value = matrix(0, nrow = input$path, ncol = input$nevent)
        y.value = matrix(0, nrow = input$path, ncol = input$nevent)
        resi.value = matrix(0, nrow = input$path, ncol = input$nevent)
        for (j in 1:input$path){
          Y=rexp(input$nevent,1)
          newtime=0
          x=NULL
          i=1
          while (i<(input$nevent+1)){
            time=newtime
            x<-append(x,(1/input$growth)*log(input$growth*Y[i]+exp(input$growth*time)))
            newtime=(1/input$growth)*log(input$growth*Y[i]+exp(input$growth*time))
            i=i+1
          }
          m = x
          h = 1:input$nevent
          int_lambda<- NULL
          for (k in m){
            integrand <- function(t) (exp((input$growth)*(t)))
            int_lambda<-append(int_lambda,integrate(integrand, lower = 0, upper = k)$value)
          }
          resi=(h-int_lambda)
          for (a in 1:input$nevent){
            x.value[j,a] = m[a] 
            y.value[j,a] = h[a] 
            resi.value[j,a] = resi[a]
          }
        }
         
        if (variable==1){
        ## n constant, rate slider
          plot(intensity, from=0, to=20, xlab="Time", ylab="lambda(t)",main = "Intensity function")}
        ## rate constant, n slider
        
        if (variable==2){
          for (i in 1:input$path){
            plot(  x.value[1:i,], y.value[1:i,],
                   xlab="Time", ylab="Number of Events", main = "Number of Events vs. Time",
                   col = colors[1:i], pch=16)+
              segments(x.value[1,30], 30, x.value[1,60], 30,lty=2)+
              segments(x.value[1,30], 0,  x.value[1,30],  30,lty=2)+
              segments(x.value[1,60],  0,  x.value[1,60],  60,lty=2)+
              segments(x.value[1,60], 30, x.value[1,68], 30)+
              segments(x.value[1,60], 60, x.value[1,68], 60)+
              
              
              arrows( x.value[1,63],  30,   x.value[1,63], 60,
                      length = 0.1)+
              arrows( x.value[1,63],  60, x.value[1,63],  30,length = 0.1)+
              text(x.value[1,31], y = 2, labels = 's',color='blue')+
              text(x.value[1,63], y = 2, labels = 's+t',color='blue')

            
            for (k in 1:i){
              m = x.value[k,]
              y = y.value[k,]
              lines( m[order(m)],y[order(m)], pch=16, lwd = 1.5, col = colors[k])
            }
          }
        }
        
        if (variable==3){
          
          for (i in 1:input$path){
            plot( x.value[1:i,],resi.value[1:i,],
                 xlab="Time", ylab="Residual->N(t)-E(N(t))", main = "Residuals vs. Time", col = colors[1:i], pch=16)
            for (k in 1:i){
            m = x.value[k,]
            resi = resi.value[k,]
            lines(m[order(m)], resi[order(m)], pch=16, col = colors[k])
            }
          }
        }
       
          arr = cbind(matrix(0,nrow=input$path,ncol=1),x.value)
          inter.arr = data.frame()
          Int = data.frame()
          Group = data.frame()
          
          for (i in 1:input$path){
            for (j in 1:input$nevent){
              Int[j + input$nevent*(i - 1), 1] = arr[i,j+1] - arr[i,j]
              Group[j + input$nevent*(i - 1), 1] = i
            }
            inter.arr = cbind(Int,Group)
          }
          
          names(inter.arr) = c("Int","Group")
          inter.arr=data.table(inter.arr)
          
          output$dis = renderPlot({
            ggplot(inter.arr,
                   aes(
                     x = Int,
                     group = Group,
                     color = as.factor(Group),
                     adjust = 2
                   )) +
              ggtitle("Interarrival Time Distribution") +
              xlab("Interarrival Time") +
              ylab("Density") +
              labs(color = "Path") +
              theme(
                plot.title = element_text(hjust = 0.5,
                                          face = "bold",
                                          size = 14),
                panel.background = element_rect(fill = 'white', colour = 'black'),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()
              ) +
              stat_density(geom = "line", size = 1)+
              scale_y_continuous(expand = expand_scale(mult = c(0, 0.15), add = 0)) +
              scale_x_continuous(expand = expand_scale(mult = c(0, 0.05), add = 0)) +
              scale_color_manual(values = colors)})
          

      }
       
      
      
      ## inverse lambda function: 1/1+3t
    if (lambdatype=='inverse'){
      intensity <- function(t) (1/(1+input$coefficient*t))
      x.value = matrix(0, nrow = input$path, ncol = input$nevent)
      y.value = matrix(0, nrow = input$path, ncol = input$nevent)
      resi.value = matrix(0, nrow = input$path, ncol = input$nevent)
      for (j in 1:input$path){
        Y=rexp(input$nevent,1)
        newtime=1
        x=NULL
        i=1
        while (i<(input$nevent+1)){
          time=newtime
          x<-append(x, ((1+input$coefficient*time)*exp(input$coefficient*Y[i])-1)/input$coefficient)
          newtime= ((1+input$coefficient*time)*exp(input$coefficient*Y[i])-1)/input$coefficient
          i=i+1
        }
        m = x
        h = 1:input$nevent
        int_lambda<- NULL
        for (k in m){
          integrand <- function(t) (1/(1+input$coefficient*t))
          int_lambda<-append(int_lambda,integrate(integrand, lower = 1, upper = k)$value)
        }
        resi=(h-int_lambda)
        for (a in 1:input$nevent){
          x.value[j,a] = m[a] 
          y.value[j,a] = h[a] 
          resi.value[j,a] = resi[a]
        }
      }
      
      
      
      if (variable==1){
        ## n constant, rate slider
        plot(intensity, from=0, to=20, xlab="Time", ylab="lambda(t)",main = "Intensity function")}
      ## rate constant, n slider
      
      if (variable==2){
        for (i in 1:input$path){
          plot(  x.value[1:i,], y.value[1:i,],
                 xlab="Time", ylab="Number of Events", main = "Number of Events vs. Time",
                 col = colors[1:i], pch=16)+
            segments(x.value[1,30], 30, x.value[1,60], 30,lty=2)+
            segments(x.value[1,30], 0,  x.value[1,30],  30,lty=2)+
            segments(x.value[1,60],  0,  x.value[1,60],  60,lty=2)+
            segments(x.value[1,60], 30, x.value[1,68], 30)+
            segments(x.value[1,60], 60, x.value[1,68], 60)+
            
            
            arrows( x.value[1,63],  30,   x.value[1,63], 60,
                    length = 0.1)+
            arrows( x.value[1,63],  60, x.value[1,63],  30,length = 0.1)+
            text(x.value[1,31], y = 2, labels = 's',color='blue')+
            text(x.value[1,63], y = 2, labels = 's+t',color='blue')
          
          
          for (k in 1:i){
            m = x.value[k,]
            y = y.value[k,]
            lines( m[order(m)],y[order(m)], pch=16, lwd = 1.5, col = colors[k])
          }
        }
      }
      
      if (variable==3){
        
        for (i in 1:input$path){
          plot(  x.value[1:i,],resi.value[1:i,],
                xlab="Time", ylab="N(t)-E(N(t))", main = "Residuals vs. Time", col = colors[1:i], pch=16)
          for (k in 1:i){
          m = x.value[k,]
          resi = resi.value[k,]
          lines(m[order(m)], resi[order(m)],xlim=range(m), ylim=range(resi), pch=16, col = colors[k])
          }
        }
      }
      arr = cbind(matrix(0,nrow=input$path,ncol=1),x.value)
      inter.arr = data.frame()
      Int = data.frame()
      Group = data.frame()
      
      for (i in 1:input$path){
        for (j in 1:input$nevent){
          Int[j + input$nevent*(i - 1), 1] = arr[i,j+1] - arr[i,j]
          Group[j + input$nevent*(i - 1), 1] = i
        }
        inter.arr = cbind(Int,Group)
      }
      
      names(inter.arr) = c("Int","Group")
      inter.arr=data.table(inter.arr)
      
      output$dis = renderPlot({
        ggplot(inter.arr,
               aes(
                 x = Int,
                 group = Group,
                 color = as.factor(Group),
                 adjust = 2
               )) +
          ggtitle("Interarrival Time Distribution") +
          xlab("Interarrival Time") +
          ylab("Density") +
          labs(color = "Path") +
          theme(
            plot.title = element_text(hjust = 0.5,
                                      face = "bold",
                                      size = 14),
            panel.background = element_rect(fill = 'white', colour = 'black'),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          ) +
          stat_density(geom = "line", size = 1)+
          scale_y_continuous(expand = expand_scale(mult = c(0, 0.15), add = 0)) +
          scale_x_continuous(expand = expand_scale(mult = c(0, 0.05), add = 0)) +
          scale_color_manual(values = colors)})
      
    }
      
      
      
    
      ## constant lambda function: 3
    if (lambdatype=='constant'){
      intensity <- function(t)(input$constant*t^0)
      x.value = matrix(0, nrow = input$path, ncol = input$nevent)
      y.value = matrix(0, nrow = input$path, ncol = input$nevent)
      resi.value = matrix(0, nrow = input$path, ncol = input$nevent)
      for (j in 1:input$path){
        Y=rexp(input$nevent,1)
        newtime=0
        x=NULL
        i=1
        while (i<(input$nevent+1)){
          time=newtime
          x<-append(x,(Y[i]/input$constant)+time)
          newtime=(Y[i]/input$constant)+time
          i=i+1
        }
        m = x
        h = 1:input$nevent
        int_lambda<- NULL
        for (k in m){
          integrand <- function(t) (input$constant*t^0)
          int_lambda<-append(int_lambda,integrate(integrand, lower = 0, upper = k)$value)
        }
        resi=(h-int_lambda)
        for (a in 1:input$nevent){
          x.value[j,a] = m[a] 
          y.value[j,a] = h[a] 
          resi.value[j,a] = resi[a]
        }
      }
      
      if (variable==1){
        ## n constant, rate slider
        plot(intensity, from=0, to=20, xlab="Time", ylab="lambda(t)",main = "Intensity function")}
               
      ## rate constant, n slider
      
      if (variable==2){
        for (i in 1:input$path){
          plot(  x.value[1:i,], y.value[1:i,],
                 xlab="Time", ylab="Number of Events", main = "Number of Events vs. Time",
                 col = colors[1:i], pch=16)+
            segments(x.value[1,30], 30, x.value[1,60], 30,lty=2)+
            segments(x.value[1,30], 0,  x.value[1,30],  30,lty=2)+
            segments(x.value[1,60],  0,  x.value[1,60],  60,lty=2)+
            segments(x.value[1,60], 30, x.value[1,68], 30)+
            segments(x.value[1,60], 60, x.value[1,68], 60)+
            
            
            arrows( x.value[1,63],  30,   x.value[1,63], 60,
                    length = 0.1)+
            arrows( x.value[1,63],  60, x.value[1,63],  30,length = 0.1)+
            text(x.value[1,31], y = 2, labels = 's',color='blue')+
            text(x.value[1,63], y = 2, labels = 's+t',color='blue')
          
          
          for (k in 1:i){
            m = x.value[k,]
            y = y.value[k,]
            lines( m[order(m)],y[order(m)], pch=16, lwd = 1.5, col = colors[k])
          }
        }
      }
      
      if (variable==3){
        
        for (i in 1:input$path){
          plot(  x.value[1:i,],resi.value[1:i,],
                 xlab="Time", ylab="N(t)-E(N(t))", main = "Residuals vs. Time", col = colors[1:i], pch=16)
          for (k in 1:i){
          m = x.value[k,]
          resi = resi.value[k,]
          lines(m[order(m)], resi[order(m)],xlim=range(m), ylim=range(resi), pch=16, col = colors[k])
          }
        }
      }
      arr = cbind(matrix(0,nrow=input$path,ncol=1),x.value)
      inter.arr = data.frame()
      Int = data.frame()
      Group = data.frame()
      
      for (i in 1:input$path){
        for (j in 1:input$nevent){
          Int[j + input$nevent*(i - 1), 1] = arr[i,j+1] - arr[i,j]
          Group[j + input$nevent*(i - 1), 1] = i
        }
        inter.arr = cbind(Int,Group)
      }
      
      names(inter.arr) = c("Int","Group")
      inter.arr=data.table(inter.arr)
      
      output$dis = renderPlot({
        ggplot(inter.arr,
               aes(
                 x = Int,
                 group = Group,
                 color = as.factor(Group),
                 adjust = 2
               )) +
          ggtitle("Interarrival Time Distribution") +
          xlab("Interarrival Time") +
          ylab("Density") +
          labs(color = "Path") +
          theme(
            plot.title = element_text(hjust = 0.5,
                                      face = "bold",
                                      size = 14),
            panel.background = element_rect(fill = 'white', colour = 'black'),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          ) +
          stat_density(geom = "line", size = 1)+
          scale_y_continuous(expand = expand_scale(mult = c(0, 0.15), add = 0)) +
          scale_x_continuous(expand = expand_scale(mult = c(0, 0.05), add = 0)) +
          scale_color_manual(values = colors)})
      

    }
    }
    
    
output$plot1<-renderPlot({generatePlot(input$lambdatype,1)})
output$plot2<-renderPlot({generatePlot(input$lambdatype,2)})
output$plot3<-renderPlot({generatePlot(input$lambdatype,3)})
output$plot4<-renderPlot({generatePlot(input$lambdatype,4)})
})