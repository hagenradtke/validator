timeseries_plot <- function(
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
  # STEP 1: VERTICAL AVERAGING MODEL DATA
  model1ave <- colMeans(model1$data,na.rm=TRUE)
  if (!is.null(model2)) { model2ave <- colMeans(model2$data,na.rm=TRUE) }
  if (!is.null(model3)) { model3ave <- colMeans(model3$data,na.rm=TRUE) }
  if (!is.null(model4)) { model4ave <- colMeans(model4$data,na.rm=TRUE) }

  model1time <- model1$time_axis
  if (!is.null(model2)) { model2time <- model2$time_axis }
  if (!is.null(model3)) { model3time <- model3$time_axis }
  if (!is.null(model4)) { model4time <- model4$time_axis }

  # STEP 2: FIND TOTAL RANGE FOR ALL VALUES
  model1max <- max(model1ave,na.rm=TRUE)
  if (is.null(model2)) {model2max <- model1max} else {model2max <- max(model2ave,na.rm=TRUE)}
  if (is.null(model3)) {model3max <- model1max} else {model3max <- max(model3ave,na.rm=TRUE)}
  if (is.null(model4)) {model4max <- model1max} else {model4max <- max(model4ave,na.rm=TRUE)}
  model1min <- min(model1ave,na.rm=TRUE)
  if (is.null(model2)) {model2min <- model1min} else {model2min <- min(model2ave,na.rm=TRUE)}
  if (is.null(model3)) {model3min <- model1min} else {model3min <- min(model3ave,na.rm=TRUE)}
  if (is.null(model4)) {model4min <- model1min} else {model4min <- min(model4ave,na.rm=TRUE)}
  measurementsmax=max(measureddata$value,na.rm=TRUE)
  measurementsmin=min(measureddata$value,na.rm=TRUE)
  totalmax=max(model1max,model2max,model3max,model4max)
  totalmin=min(model1min,model2min,model3min,model4min)
  if (is.finite(measurementsmax)) {totalmax=max(totalmax,measurementsmax)}
  if (is.finite(measurementsmin)) {totalmin=min(totalmin,measurementsmin)}
  if (totalmin <= 0.5*totalmax) {totalmin=min(0.0,totalmin)}

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
  myggplot=ggplot(data2plot,aes(model1time,model1ave)) +
              geom_line(color="#0000FF")+
              theme_bw()+
              xlab(label="time") + ylab(label=description) +
              theme(panel.grid.major =   element_line(colour = "#CCCCCC",size=0.5),panel.grid.minor=element_line(colour = "#CCCCCC",size=0.1))+
              scale_x_date(limits=c(start_date,end_date))+
              scale_y_continuous(limits=c(totalmin,totalmax))
  if (!is.null(model2)) {
    data2plot=data.frame(model2time,model2ave)
    data2plot$model2time <- as.Date( data2plot$model2time, origin="1899-12-30" )
    myggplot=myggplot+geom_line(data=data2plot,aes(model2time,model2ave),color="#FF0000")
  }
  if (!is.null(model3)) {
    data2plot=data.frame(model3time,model3ave)
    data2plot$model3time <- as.Date( data2plot$model3time, origin="1899-12-30" )
    myggplot=myggplot+geom_line(data=data2plot,aes(model3time,model3ave),color="#00FF00")
  }
  if (!is.null(model4)) {
    data2plot=data.frame(model4time,model4ave)
    data2plot$model4time <- as.Date( data2plot$model4time, origin="1899-12-30" )
    myggplot=myggplot+geom_line(data=data2plot,aes(model4time,model4ave),color="#FF00FF")
  }
  myggplot = myggplot + geom_point(data=measureddata,aes(date,value),shape=3,stroke=1.0)
  print(myggplot)

  if (!is.null(output)) {
    dev.off()
  }
}