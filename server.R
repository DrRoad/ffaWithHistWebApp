# P:/HRRD/FRF_Group/FEH Local/StatModel/develop
library(shiny)
# gumbX <- function(x, emp=FALSE) {
#   if(emp) x <- (1:length(x)-0.44)/(length(x) + 1 - 0.88)
#   -log(-log(x))
# }

library(ismev)
library(lmom)
library(markdown)
# devtools::install_github("ilapros/ilaprosUtils")
# library(ilaprosUtils)


source("withHist.R")
source("ismevExtension.R")

# dat <- read.table("http://gist.githubusercontent.com/ilapros/44c09e95ab5be7591f74/raw/410b165001be83f5e4efddb7d078cf1780b48d69/amData",header = TRUE)
# dat <- read.csv("Data/amData_v3.3.4.csv", header = TRUE,stringsAsFactors = FALSE)
# dat <- dat[dat$Rejected == "False",-6]
load("Data/dat.RDa")

# 
# fitdat <- function(x,k,X0,dist){
#   if(h == "NULL" & dist == "glo") glo.fit(xdat=x)
#   if(h == "NULL" & dist == "glo") glo.hist.fit(xdat=x, k = k, h = h, X0 = as.numeric(X0), binomialcens = FALSE)
#   if(h == "NULL" & dist == "gev") gev.fit(xdat=x)
#   if(h == "NULL" & dist == "gev") gev.hist.fit(xdat=x, k = k, h = h, X0 = as.numeric(X0), binomialcens = FALSE)
# }
# 

  
  
# Define server logic for random distribution application
shinyServer(function(input, output) {
  # GiveInfo <- input$GiveInfo
  #observe({reactive({input$GiveInfo})})
  #Station <- reactive({Station})
  #st <- Station()
  #   output$summary <- renderPrint(selDat)
  
  
  
  st <- reactive({
    selDat <- dat[dat$Station %in% input$Station,c("Year","Flow")]
  })
  
  output$dataplot <- renderPlot({   
    selDat <- st()
    if(max(1,dim(selDat)[1]) < 2) {
      selDat <- data.frame(Flow = as.numeric(unlist(strsplit(input$addAMAX,","))));
      selDat$Year = seq(from = 1, to = length(selDat$Flow))}
      plot(selDat[,c("Year","Flow")], col = "grey20", type = "h")
  })
  
output$ffaplot <- renderPlot({  
    
    selDat <- st()
    if(max(1,dim(selDat)[1]) < 2) {selDat <- data.frame(Flow = as.numeric(unlist(strsplit(input$addAMAX,",")))); 
    selDat$Year = seq(from = 1, to = length(selDat$Flow))} 
    ff <- seq(0.25,0.995,length=300)
    
    sfit <- switch (input$dist,
                     "glo" = glo.fit(selDat$Flow,show=FALSE),
                     "gev" = ismev::gev.fit(selDat$Flow,show=FALSE))
    sRet <- retPlot(sfit, pch = 16, p = c(seq(0.005,0.99,l=50),seq(0.991, 0.999, l=200)))
    
    # estmle <- switch (input$dist,
    #   "glo" = glo.fit(selDat$Flow,show=FALSE),
    #   "gev" = ismev::gev.fit(selDat$Flow,show=FALSE))
    # 
    # 
    # fitret <- switch (input$dist,
    #                   "glo" = lmom::quaglo(ff,estmle$mle),
    #                   "gev" = lmom::quagev(ff,estmle$mle*c(1,1,-1)))
    # # print(estmle$mle)
    # estSE <- switch (input$dist,
    #                  "glo" = apply(t(glo.rl.gradient(a=estmle$mle, p=1-ff)), 1, q.form, m = estmle$cov),
    #                  "gev" = apply(t(gev.rl.gradient(a=estmle$mle, p=1-ff)), 1, q.form, m = estmle$cov))
    # 
    # plot(gumbX(selDat$Flow, emp=TRUE), sort(selDat$Flow), col = 0, pch = 16,type="n",
    #      xlab = paste(paste("Gumbel reduced variate","\n"),"-log(-log(1-1/T))"), 
    #      ylab = " ",bty="l", 
    #      xlim = gumbX(ff[c(1,length(ff))]), 
    #      ylim = range(c(fitret + 1.96 * sqrt(estSE),  fitret - 1.96 * sqrt(estSE))))
    # mtext(expression(paste("Peak flow ",(m^3/s))),2,line = 2)
    # lines(gumbX(ff), fitret, col = 2)
    # lines(gumbX(ff), fitret + 1.96 * sqrt(estSE), lty = 4, col = 2)
    # lines(gumbX(ff), fitret - 1.96 * sqrt(estSE), lty = 4, col = 2)
    # legend("topleft", col = c(2),legend="Systematic data", bty = "n", lty = 1)
    legend("topleft", col = c(1),legend=c("Flood freq. curve with systematic data only"), bty = "n", lty = c(1))
    
    if(input$hdata != "NULL"){
      hdat <- as.numeric(unlist(strsplit(input$hdata,",")))
      
      hfit <- switch (input$dist,
                        "glo" = 
                          glo.hist.fit(c(hdat,selDat$Flow),show=FALSE, 
                                       h=as.numeric(unlist(strsplit(input$h,","))),
                                       k = length(hdat),
                                       X0 = as.numeric(unlist(strsplit(input$X0,","))), 
                                       binomialcens = as.logical(input$binCens)),
                        "gev" = 
                          gev.hist.fit(c(hdat,selDat$Flow),show=FALSE, 
                                       h=as.numeric(unlist(strsplit(input$h,","))),
                                       k = length(hdat),
                                       X0 = as.numeric(unlist(strsplit(input$X0,","))), 
                                       binomialcens = as.logical(input$binCens)))
      
      hRet <- retPlot(hfit, col = 2, sign.alpha = 0, pch = 16, p = c(seq(0.005,0.99,l=50),seq(0.991, 0.999, l=100)))
      lines(log((hRet$p)/(1-hRet$p)), hRet$retLev - 1.96*hRet$se, col = 2, lty = 2)
      lines(log((hRet$p)/(1-hRet$p)), hRet$retLev + 1.96*hRet$se, col = 2, lty = 2)
      lines(log((sRet$p)/(1-sRet$p)), sRet$retLev, col = 1)
      lines(log((sRet$p)/(1-sRet$p)), sRet$retLev - 1.96*sRet$se, col = 1, lty = 2)
      lines(log((sRet$p)/(1-sRet$p)), sRet$retLev + 1.96*sRet$se, col = 1, lty = 2)
      
      
      #   
      #   gumbX(ff), fitret, col = 4)
      # lines(gumbX(ff), fitret + 1.96 * sqrt(estSE), lty = 4, col = 4)
      # lines(gumbX(ff), fitret - 1.96 * sqrt(estSE), lty = 4, col = 4)
      legend("topleft", col = c(1,2),legend=c("Flood freq. curve with systematic data only","Flood freq. curve with additional historical data"), bty = "n", lty = c(1,1))
      
    }
  })
  
output$varplot <- renderPlot({  
  
  selDat <- st()
  if(max(1,dim(selDat)[1]) < 2) {selDat <- data.frame(Flow = as.numeric(unlist(strsplit(input$addAMAX,",")))); 
  selDat$Year = seq(from = 1, to = length(selDat$Flow))} 
  ff <- seq(0.5,0.995,length=300)
  
  sfit <- switch (input$dist,
                  "glo" = glo.fit(selDat$Flow,show=FALSE),
                  "gev" = ismev::gev.fit(selDat$Flow,show=FALSE))
  sRet <- retPlot(sfit, pch = 16, p = c(seq(0.005,0.99,l=50),seq(0.991, 0.999, l=200)), plot.out = FALSE)
  # estmle <- switch (input$dist,
  #                   "glo" = glo.fit(selDat$Flow,show=FALSE),
  #                   "gev" = ismev::gev.fit(selDat$Flow,show=FALSE))
  # 
  # 
  # fitret <- switch (input$dist,
  #                   "glo" = lmom::quaglo(ff,estmle$mle),
  #                   "gev" = lmom::quagev(ff,estmle$mle*c(1,1,-1)))
  # # print(estmle$mle)
  # estSE <- switch (input$dist,
  #                  "glo" = apply(t(glo.rl.gradient(a=estmle$mle, p=1-ff)), 1, q.form, m = estmle$cov),
  #                  "gev" = apply(t(gev.rl.gradient(a=estmle$mle, p=1-ff)), 1, q.form, m = estmle$cov))
  
  plot(log((sRet$p)/(1-sRet$p)), 2*1.96*sRet$se/sRet$retLev, col = 1,
       ylab = "CI width over estimate",bty="l", type="l",
       xlab = paste(paste("Gumbel reduced variate","\n"),"-log(-log(1-1/T))"), lwd=2)
  
  # plot(gumbX(ff), 2*1.96*sqrt(estSE)/fitret, col = 2,
  #      ylab = "CI width over estimate",bty="l", type="l",
  #      xlab = paste(paste("Gumbel reduced variate","\n"),"-log(-log(1-1/T))"), lwd=2)
  # legend("topleft", col = c(2),legend="Systematic data", bty = "n", lty = 1)
  legend("topleft", col = c(1),legend=c("Systematic only"), bty = "n", lty = 1)
  if(input$hdata != "NULL"){
    hdat <- as.numeric(unlist(strsplit(input$hdata,",")))
    # estmle <- switch (input$dist,
    #                   "glo" = 
    #                     glo.hist.fit(c(hdat,selDat$Flow),show=FALSE, 
    #                                  h=as.numeric(unlist(strsplit(input$h,","))),
    #                                  k = length(hdat),
    #                                  X0 = as.numeric(unlist(strsplit(input$X0,","))), 
    #                                  binomialcens = as.logical(input$binCens)),
    #                   "gev" = 
    #                     gev.hist.fit(c(hdat,selDat$Flow),show=FALSE, 
    #                                  h=as.numeric(unlist(strsplit(input$h,","))),
    #                                  k = length(hdat),
    #                                  X0 = as.numeric(unlist(strsplit(input$X0,","))), 
    #                                  binomialcens = as.logical(input$binCens)))
    # fitret <- switch (input$dist,
    #                   "glo" = lmom::quaglo(ff,estmle$mle),
    #                   "gev" = lmom::quagev(ff,estmle$mle*c(1,1,-1)))
    # 
    # estSE <- switch (input$dist,
    #                  "glo" = apply(t(glo.rl.gradient(a=estmle$mle, p=1-ff)), 1, q.form, m = estmle$cov),
    #                  "gev" = apply(t(gev.rl.gradient(a=estmle$mle, p=1-ff)), 1, q.form, m = estmle$cov))
    # 
    #lines(gumbX(ff), 2*1.96*sqrt(estSE)/fitret, col = 4, lwd=2)
    
    hdat <- as.numeric(unlist(strsplit(input$hdata,",")))
    
    hfit <- switch (input$dist,
                    "glo" = 
                      glo.hist.fit(c(hdat,selDat$Flow),show=FALSE, 
                                   h=as.numeric(unlist(strsplit(input$h,","))),
                                   k = length(hdat),
                                   X0 = as.numeric(unlist(strsplit(input$X0,","))), 
                                   binomialcens = as.logical(input$binCens)),
                    "gev" = 
                      gev.hist.fit(c(hdat,selDat$Flow),show=FALSE, 
                                   h=as.numeric(unlist(strsplit(input$h,","))),
                                   k = length(hdat),
                                   X0 = as.numeric(unlist(strsplit(input$X0,","))), 
                                   binomialcens = as.logical(input$binCens)))
    
    hRet <- retPlot(hfit, col = 2, sign.alpha = 0, pch = 16, p = c(seq(0.005,0.99,l=50),seq(0.991, 0.999, l=100)), plot.out = FALSE)
    lines(log((hRet$p)/(1-hRet$p)), 2*1.96*hRet$se/hRet$retLev, col = 2, lwd=2)
    legend("topleft", col = c(0,2),legend=c(" ","Historical data"), bty = "n", lty = c(0,1))
  }
})





  output$summary <- renderPrint({
    
    selDat <- st()
    if(max(1,dim(selDat)[1]) < 2) {selDat <- data.frame(Flow = as.numeric(unlist(strsplit(input$addAMAX,",")))); 
    selDat$Year = seq(from = 1, to = length(selDat$Flow))} 
    ff <- seq(0.25,0.995,length=300)
    
    estmle <- switch (input$dist,
                      "glo" = glo.fit(selDat$Flow,show=FALSE)$mle,
                      "gev" = ismev::gev.fit(selDat$Flow,show=FALSE)$mle*c(1,1,-1))
    estlmom <- switch (input$dist,
                      "glo" = lmom::lmrglo(estmle),
                      "gev" = lmom::lmrgev(estmle))
    
    cat("Parameter estimates - systematic only\n")
    cat(estmle[1],"   ",estmle[2],"   ",estmle[3],"   \n")    
    cat("and corresponding L-moment\n")
    cat(estlmom[1],"   ",estlmom[2],"   ",estlmom[3],"   \n\n")
    
    if(input$hdata != "NULL"){
      hdat <- as.numeric(unlist(strsplit(input$hdata,",")))
      estmle <- switch (input$dist,
                        "glo" = 
                          glo.hist.fit(c(hdat,selDat$Flow),show=FALSE, 
                                       h=as.numeric(unlist(strsplit(input$h,","))),
                                       k = length(hdat),
                                       X0 = as.numeric(unlist(strsplit(input$X0,","))),
                                       binomialcens = as.logical(input$binCens))$mle,
                        "gev" = 
                          gev.hist.fit(c(hdat,selDat$Flow),show=FALSE, 
                                       h=as.numeric(unlist(strsplit(input$h,","))),
                                       k = length(hdat),
                                       X0 = as.numeric(unlist(strsplit(input$X0,","))), 
                                       binomialcens = as.logical(input$binCens))$mle*c(1,1,-1))
      estlmom <- switch (input$dist,
                         "glo" = lmom::lmrglo(estmle),
                         "gev" = lmom::lmrgev(estmle))
      cat("Parameter estimates - with Historical data\n")
      cat(estmle[1],"   ",estmle[2],"   ",estmle[3],"   \n")    
      cat("and corresponding L-moment\n")
      cat(estlmom[1],"   ",estlmom[2],"   ",estlmom[3],"   \n")
    }
    #print(lmom::samlmu(selDat$Flow))
  })
  
  # Generate a plot of the data. Also uses the inputs to build the 
  # plot label. Note that the dependencies on both the inputs and
  # the 'data' reactive expression are both tracked, and all expressions 
  # are called in the sequence implied by the dependency graph
  # output$plot <- renderPlot({
  #   plot(selDat[,c("Year","Flow")], col = "grey70", type = "h")
  #   # points(dat[dat$Component == comp,c("hour","value")], 
  #   #        col = ifelse(comp == "PM10",2,4), pch =16)
  # })
  # 
  # # Generate a summary of the data
  # output$summary <- renderPrint({
  #   # comp <- input$comp
  #   summary(selDat)
  # })
  
  # Generate an HTML table view of the data
#   output$table <- renderTable({
#     data.frame(x=data())
#   })
})
