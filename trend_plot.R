get_the_estimates <- function(dataset, components=c("longterm", "seasonal", "both"), continuous=FALSE) {
  if (continuous) {
    dataset$decadeday <- dataset$datetime/10
    # 10-daily averages
    dataset <- aggregate(cbind("value"=value,"datetime"=datetime)~floor(decadeday),data=dataset,FUN=mean)
  } else {
    dataset<-dataset[order(dataset$datetime),]
    # daily averages
    dataset <- aggregate(cbind("value"=value,"datetime"=datetime)~floor(datetime),data=dataset,FUN=mean)
    # alternatively, remove duplicates
    #dataset<-dataset[!duplicated(dataset$datetime),]
  } 
  # define time axis as POSIXct
  dataset$time <- as.POSIXct(as.Date(dataset$datetime,origin = "1899-12-30"))

  # get a relative time and a "season" between 0 and 1
  dataset$year <- strftime(dataset$time,"%Y")
  firstdayofyear <- as.POSIXct(paste0(strftime(dataset$time,"%Y"),"-01-01"))
  firstdayofnextyear <- as.POSIXct(paste0(as.numeric(strftime(dataset$time,"%Y"))+1,"-01-01"))
  dayofyear <- difftime(dataset$time,firstdayofyear,units = "days")
  daysofyear <- difftime(firstdayofnextyear,firstdayofyear,units = "days")
  dataset$season <- as.double(dayofyear)/as.double(daysofyear)
  dataset$reltime <- as.double(difftime(dataset$time,mean(dataset$time),units="days"))

  # STEP 3: DEFINE A GAMM MODEL AND FIT IT TO THE DATA
  # fit including autocorrelation
  ctrl <- list(maxIter=50, msMaxEval=500, niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B", returnObject=TRUE)
  if (continuous) {
    gamm_measured <- gamm(value ~ s(season, bs = "cc", k = 12) + s(reltime, k = 20),
                         data = dataset, correlation = corAR1(form = ~ 1|year),
                         control = ctrl, knots=list(season=c(0,1),reltime=NULL))
  } else {
    gamm_measured <- gamm(value ~ s(season, bs = "cc", k = 12) + s(reltime, k = 20),
                         data = dataset, correlation = corCAR1(form = ~ reltime|year, value=0.967, fixed=TRUE),
                         control = ctrl, knots=list(season=c(0,1),reltime=NULL))
  }

  # STEP 4: CALCULATE EITHER LONG-TERM OR SEASONAL COMPONENT
  if ((components=="longterm")|(components=="both")) {
    # calculate long-term estimate (potentially seasonally detrended)
    pred_time <- seq(min(dataset$time),to=max(dataset$time),by=10*24*3600)
    myyear <- strftime(pred_time,"%Y")
    firstdayofyear <- as.POSIXct(paste0(strftime(pred_time,"%Y"),"-01-01"))
    firstdayofnextyear <- as.POSIXct(paste0(as.numeric(strftime(pred_time,"%Y"))+1,"-01-01"))
    dayofyear <- difftime(pred_time,firstdayofyear,units = "days")
    daysofyear <- difftime(firstdayofnextyear,firstdayofyear,units = "days")
    season <- as.double(dayofyear)/as.double(daysofyear)
    reltime <- as.double(difftime(pred_time,mean(dataset$time),units="days"))
    pred_base=data.frame(season,reltime)
    if (components=="longterm") {
      terms <- predict.gam(gamm_measured$gam,pred_base,exclude="s(season)",se.fit=TRUE,unconditional=FALSE)
    }
    if (components=="both") {
      terms <- predict.gam(gamm_measured$gam,pred_base,se.fit=TRUE,unconditional=FALSE)
    }
  } else if (components=="seasonal") {
    # calculate seasonal climatology
    season <- seq(0,to=1,length.out=200)
    reltime <- season*0.0
    pred_time <- as.POSIXct(reltime,origin="1899-12-30")
    pred_base=data.frame(season,reltime)
    terms <- predict.gam(gamm_measured$gam,pred_base,exclude="s(reltime)",se.fit=TRUE,unconditional=FALSE)
  }
  estimated_mean <- terms$fit
  estimated_upper <- terms$fit+terms$se.fit*1.96
  estimated_lower <- terms$fit-terms$se.fit*1.96
  dayofyear <- as.Date("2001-01-01")+dataset$season*365
  dataset$dayofyear <- dayofyear
  estimate <- data.frame(pred_time,estimated_mean,estimated_upper,estimated_lower,season)
  estimate$dayofyear <- as.Date("2001-01-01")+estimate$season*365
  dataset$date <- as.Date(dataset$datetime, origin="1899-12-30" )
  estimate$date <- as.Date(estimate$pred_time)
  return(list(dataset=dataset,estimate=estimate))
}

trend_plot <- function(
                            plot_options,
                            measureddata, 
                            model1, 
                            model2=NULL, 
                            model3=NULL, 
                            model4=NULL,
                            start_date, 
                            end_date,
                            min_depth, 
                            max_depth, 
                            description, 
                            output=NULL,
                            width=NULL,
                            height=NULL,
                            dpi=NULL
                           ) {
  library("mgcv")
  # STEP 1: VERTICAL AVERAGING MODEL DATA
  model1ave <- colMeans(model1$data,na.rm=TRUE)
  if (!is.null(model2)) { model2ave <- colMeans(model2$data,na.rm=TRUE) }
  if (!is.null(model3)) { model3ave <- colMeans(model3$data,na.rm=TRUE) }
  if (!is.null(model4)) { model4ave <- colMeans(model4$data,na.rm=TRUE) }

  model1time <- model1$time_axis
  if (!is.null(model2)) { model2time <- model2$time_axis }
  if (!is.null(model3)) { model3time <- model3$time_axis }
  if (!is.null(model4)) { model4time <- model4$time_axis }


  model1 <- data.frame(datetime=as.double(difftime(model1time,as.Date("1899-12-30"),units="days")), value=model1ave)
  if (!is.null(model2)) { model2 <- data.frame(datetime=as.double(difftime(model2time,as.Date("1899-12-30"),units="days")), value=model2ave) }
  if (!is.null(model3)) { model3 <- data.frame(datetime=as.double(difftime(model3time,as.Date("1899-12-30"),units="days")), value=model3ave) }
  if (!is.null(model4)) { model4 <- data.frame(datetime=as.double(difftime(model4time,as.Date("1899-12-30"),units="days")), value=model4ave) }


  measurements <- get_the_estimates(dataset=measureddata,components=plot_options$components,continuous=FALSE)
  model1 <- get_the_estimates(dataset=model1,components=plot_options$components,continuous=TRUE)
  if (!is.null(model2)) { model2 <- get_the_estimates(dataset=model2,components=plot_options$components,continuous=TRUE) }
  if (!is.null(model3)) { model3 <- get_the_estimates(dataset=model3,components=plot_options$components,continuous=TRUE) }
  if (!is.null(model4)) { model4 <- get_the_estimates(dataset=model4,components=plot_options$components,continuous=TRUE) }
  totalmax = max(max(measurements$dataset$value),max(measurements$estimate$estimated_upper))
  totalmin = min(min(measurements$dataset$value),min(measurements$estimate$estimated_lower))

  totalmax = max(totalmax,max(model1$estimate$estimated_upper))
  totalmin = min(totalmin,min(model1$estimate$estimated_lower))
  if (!is.null(model2)) {
    totalmax = max(totalmax,max(model2$estimate$estimated_upper))
    totalmin = min(totalmin,min(model2$estimate$estimated_lower))
  }
  if (!is.null(model3)) {
    totalmax = max(totalmax,max(model3$estimate$estimated_upper))
    totalmin = min(totalmin,min(model3$estimate$estimated_lower))
  }
  if (!is.null(model4)) {
    totalmax = max(totalmax,max(model4$estimate$estimated_upper))
    totalmin = min(totalmin,min(model4$estimate$estimated_lower))
  }


  # STEP 3: DO THE PLOTTING
  # if output contains something, we will store a PNG file
  if (!is.null(output)) {
    # if no width/height/dpi are given, use standard values
    if (is.null(width))  {width  <- 6000}
    if (is.null(height)) {height <- 3000}
    if (is.null(dpi))    {dpi    <-  600}
    png(
      output,
      width     = width,
      height    = height,
      units     = "px",
      res       = dpi,
      pointsize = 12.0
    )
  }

  require(ggplot2)
  data2plot=data.frame(model1time,model1ave)
  data2plot$model1time <- as.Date( data2plot$model1time, origin="1899-12-30" )

  if ((plot_options$components=="longterm")|(plot_options$components=="both")) {
    myggplot=ggplot(data2plot,aes(model1time,model1ave)) +
              #geom_line(color="#0000FF")+
              theme_bw()+
              xlab(label="time") + ylab(label= description) +
              theme(panel.grid.major =   element_line(colour = "#CCCCCC",size=0.5),panel.grid.minor=element_line(colour = "#CCCCCC",size=0.1))+
              scale_x_date(limits=c(start_date,end_date))+
              scale_y_continuous(limits=c(totalmin,totalmax))
    myggplot = myggplot + geom_point(data=measurements$dataset,aes(date,value),shape=3,stroke=1.0)
    myggplot = myggplot + geom_line(data=measurements$estimate,aes(date,estimated_mean),color="#000000")
    myggplot = myggplot + geom_line(data=measurements$estimate,aes(date,estimated_lower),color="#808080")
    myggplot = myggplot + geom_line(data=measurements$estimate,aes(date,estimated_upper),color="#808080")

    myggplot = myggplot + geom_line(data=model1$estimate,aes(date,estimated_mean),color="#0000FF")
    myggplot = myggplot + geom_line(data=model1$estimate,aes(date,estimated_lower),color="#8080FF")
    myggplot = myggplot + geom_line(data=model1$estimate,aes(date,estimated_upper),color="#8080FF")
    if (!is.null(model2)) { 
      myggplot = myggplot + geom_line(data=model2$estimate,aes(date,estimated_mean),color="#FF0000")
      myggplot = myggplot + geom_line(data=model2$estimate,aes(date,estimated_lower),color="#FF8080")
      myggplot = myggplot + geom_line(data=model2$estimate,aes(date,estimated_upper),color="#FF8080")
    }
    if (!is.null(model3)) { 
      myggplot = myggplot + geom_line(data=model3$estimate,aes(date,estimated_mean),color="#008000")
      myggplot = myggplot + geom_line(data=model3$estimate,aes(date,estimated_lower),color="#80FF80")
      myggplot = myggplot + geom_line(data=model3$estimate,aes(date,estimated_upper),color="#80FF80")
    }
    if (!is.null(model4)) { 
      myggplot = myggplot + geom_line(data=model4$estimate,aes(date,estimated_mean),color="#FF00FF")
      myggplot = myggplot + geom_line(data=model4$estimate,aes(date,estimated_lower),color="#FF80FF")
      myggplot = myggplot + geom_line(data=model4$estimate,aes(date,estimated_upper),color="#FF80FF")
    }

  } else if (plot_options$components=="seasonal") {
    Sys.setlocale("LC_TIME", "en_GB.utf8")
    myggplot=ggplot(measurements$estimate,aes(dayofyear,estimated_mean)) +
              #geom_line(color="#0000FF")+
              theme_bw()+
              xlab(label="month") + ylab(label= description) +
              theme(panel.grid.major =   element_line(colour = "#CCCCCC",size=0.5),panel.grid.minor=element_line(colour = "#CCCCCC",size=0.1))+
              scale_x_date(limits=c(as.Date("2001-01-01"),as.Date("2002-01-01")),breaks=c(as.Date("2001-01-01"),as.Date("2001-04-01"),as.Date("2001-07-01"),as.Date("2001-10-01"),as.Date("2002-01-01")),date_minor_breaks="months",date_labels="%d %b")+
              scale_y_continuous(limits=c(totalmin,totalmax))
    myggplot = myggplot + geom_point(data=measurements$dataset,aes(dayofyear,value),shape=3,stroke=1.0)
    myggplot = myggplot + geom_line(data=measurements$estimate,aes(dayofyear,estimated_mean),color="#000000")
    myggplot = myggplot + geom_line(data=measurements$estimate,aes(dayofyear,estimated_lower),color="#808080")
    myggplot = myggplot + geom_line(data=measurements$estimate,aes(dayofyear,estimated_upper),color="#808080")

    myggplot = myggplot + geom_line(data=model1$estimate,aes(dayofyear,estimated_mean),color="#0000FF")
    myggplot = myggplot + geom_line(data=model1$estimate,aes(dayofyear,estimated_lower),color="#8080FF")
    myggplot = myggplot + geom_line(data=model1$estimate,aes(dayofyear,estimated_upper),color="#8080FF")
    if (!is.null(model2)) { 
      myggplot = myggplot + geom_line(data=model2$estimate,aes(dayofyear,estimated_mean),color="#FF0000")
      myggplot = myggplot + geom_line(data=model2$estimate,aes(dayofyear,estimated_lower),color="#FF8080")
      myggplot = myggplot + geom_line(data=model2$estimate,aes(dayofyear,estimated_upper),color="#FF8080")
    }
    if (!is.null(model3)) { 
      myggplot = myggplot + geom_line(data=model3$estimate,aes(dayofyear,estimated_mean),color="#008000")
      myggplot = myggplot + geom_line(data=model3$estimate,aes(dayofyear,estimated_lower),color="#80FF80")
      myggplot = myggplot + geom_line(data=model3$estimate,aes(dayofyear,estimated_upper),color="#80FF80")
    }
    if (!is.null(model4)) { 
      myggplot = myggplot + geom_line(data=model4$estimate,aes(dayofyear,estimated_mean),color="#FF00FF")
      myggplot = myggplot + geom_line(data=model4$estimate,aes(dayofyear,estimated_lower),color="#FF80FF")
      myggplot = myggplot + geom_line(data=model4$estimate,aes(dayofyear,estimated_upper),color="#FF80FF")
    }

  }

  print(myggplot)

  if (!is.null(output)) {
    dev.off()
  }
}