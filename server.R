# server.R
# ANOVA overview


# LOAD PACKAGES ###############################################################

library(shiny)
library(ggplot2)
library(plyr)
#library(dplyr)
library(DT)
#library(car)



# FUNCTIONS ###################################################################

gg_qq <- function(x, distribution = "norm", ..., line.estimate = NULL, conf = 0.95,
                  labels = names(x)){
  q.function <- eval(parse(text = paste0("q", distribution)))
  d.function <- eval(parse(text = paste0("d", distribution)))
  x <- na.omit(x)
  ord <- order(x)
  n <- length(x)
  P <- ppoints(length(x))
  df <- data.frame(ord.x = x[ord], z = q.function(P, ...))
  
  if(is.null(line.estimate)){
    Q.x <- quantile(df$ord.x, c(0.25, 0.75))
    Q.z <- q.function(c(0.25, 0.75), ...)
    b <- diff(Q.x)/diff(Q.z)
    coef <- c(Q.x[1] - b * Q.z[1], b)
  } else {
    coef <- coef(line.estimate(ord.x ~ z))
  }
  
  zz <- qnorm(1 - (1 - conf)/2)
  SE <- (coef[2]/d.function(df$z)) * sqrt(P * (1 - P)/n)
  fit.value <- coef[1] + coef[2] * df$z
  df$upper <- fit.value + zz * SE
  df$lower <- fit.value - zz * SE
  
  if(!is.null(labels)){ 
    df$label <- ifelse(df$ord.x > df$upper | df$ord.x < df$lower, labels[ord],"")
  }
  
  p <- ggplot(df, aes(x=z, y=ord.x)) +
    geom_point() + 
    geom_abline(intercept = coef[1], slope = coef[2]) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2) 
  if(!is.null(labels)) p <- p + geom_text( aes(label = label))
  print(p)
  coef
}



# SHINY SERVER ################################################################

shinyServer(function(input, output, session) { 
  
  datasetinit <- reactive({
    
    # TESTING
    # mu1 = 100; sd1 = 10
    # mu2 = 150; sd2 = 10
    # mu3 = 200; sd3 = 10
    # lenx = len1 = len2 = len3 = 100
    
    cssize = input$ssize
    mu1 = input$mu1; sd1 = input$sd1
    mu2 = input$mu2; sd2 = input$sd2
    mu3 = input$mu3; sd3 = input$sd3
    lenx = 100  # keep constant, used to create prob dens fn
    len1 = len2 = len3 = cssize
    
    
    x1_min = mu1 - 4*sd1
    x1_max = mu1 + 4*sd1
    x1 = seq(x1_min, x1_max, length.out = lenx)
    grp1 = rep('group 1', lenx)
    y1 = dnorm(x1, mu1, sd1)
    x1seq = 1:lenx

    
    x2_min = mu2 - 4*sd2
    x2_max = mu2 + 4*sd2
    x2 = seq(x2_min, x2_max, length.out = lenx)
    grp2 = rep('group 2', lenx)
    y2 = dnorm(x2, mu2, sd2)
    x2seq = 1:lenx
    
    
    x3_min = mu3 - 4*sd3
    x3_max = mu3 + 4*sd3
    x3 = seq(x3_min, x3_max, length.out = lenx) 
    grp3 = rep('group 3', lenx)
    y3 = dnorm(x3, mu3, sd3)
    x3seq = 1:lenx
    
    
    dat = data.frame(x = append(append(x1,x2),x3),
                     y = append(append(y1,y2),y3),
                     group = append(append(grp1,grp2),grp3),
                     xseq = append(append(x1seq,x2seq),x3seq))
    
    
    return(dat)
    
  })
  
  datasetanova <- reactive({
    
    dtemp1 = datasetinit()

    
    seqfm = 1
    seqto = 100  # matches lenx above
    
    tmpssize = input$ssize
    # tmpssize = 10
    
    seqby = round((seqto - seqfm)/tmpssize, 0)

    subset_seq = seq(from = seqfm, to = seqto, by = seqby)
    
    #dsmpl <- dplyr::filter(dtemp1, xseq %in% subset_seq)
    
    dsmpl <- dtemp1[dtemp1$xseq %in% subset_seq, ]
    
    return(dsmpl)
    
  })


  output$datatableanova <- renderDataTable({

    datasetanova()

    })
  
  
  output$datainitplot <- renderPlot({
    
    plotdat1 = datasetinit()
    
    plot1 = ggplot(data = plotdat1, aes(x = x, y = y, group = group, color = group)) +
      geom_line(size = 2)
    
    return(plot1)
    
  })
  
  
  output$dataanovaplot <- renderPlot({
    
    plotdat2 = datasetanova()
    
    plot2 = ggplot(data = plotdat2, aes(x = group, y = x, group = group, color = group)) +
      geom_boxplot() + coord_flip()
    
    return(plot2)
    
  })
  
  
  anovafit <- reactive({
    
    dat_aov <- datasetanova()
    
    fit1 <- aov(x ~ group, data = dat_aov)
    
    return(fit1)
    
  })
  
  output$tukeyfit <- renderPrint({
    
    tukeyout <- TukeyHSD(anovafit())
    
    return(tukeyout)
    
  })
  
  
  output$datainitsummary <- renderPrint({
    
    datasummary = ddply(datasetinit(), .(group), summarize,
                        meanX = mean(x),
                        stdevX = sd(x),
                        ssizeX = length(x))
    
    return(datasummary)
    
  })
  
  
  output$dataanovasummary <- renderPrint({
    
    datasummary = ddply(datasetanova(), .(group), summarize,
                        meanX = mean(x),
                        stdev = sd(x),
                        ssize = length(x))
    
    return(datasummary)
    
  })
  
  # output$glimpsedata <- renderPrint({
  # 
  #   glimpse(anovadata())
  # 
  # })
  
  output$anovaresults <- renderPrint({
    
    summary(anovafit())
    
  })
  

  datplusfit <- reactive({
    
    anovadat = datasetanova()
    
    fitresults = anovafit()
    
    anovadat$residuals = residuals(fitresults)
    anovadat$rstudent = rstudent(fitresults)
    
    return(anovadat)
    
  })
  
  # DIAGNOSTICS ---------------------------------------------------------------
  
  output$plotfitresids <- renderPlot({
    
    plotdat3 = datplusfit()
    
    plot3 = ggplot(data = plotdat3, aes(x = group, y = residuals, color = group)) +
      geom_point(size = 3) + 
      geom_hline(yintercept = 0, linetype = 2, color = 'red')
    
    return(plot3)
    
  })
  
  
  output$plotnormresids <- renderPlot({

    plotdat4 = datplusfit()

    plot4 = gg_qq(plotdat4$residuals)  # previously used qqPlot from car package

    return(plot4)

  })
  
  
  output$shapirowilk <- renderPrint({
    
    tempdatnorm = datplusfit()
    
    normtest = shapiro.test(tempdatnorm$residuals)
    
    return(normtest)
    
  })
  
  
}



)  # end shinyserver


# END CODE ####################################################################
