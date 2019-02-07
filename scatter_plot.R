source("get_model_estimates.R")

scatter_plot <- function(
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
                         above_ground, 
                         description, 
                         output=NULL,
                         width=NULL,
                         height=NULL,
                         dpi=NULL
                        ) {

  # STEP 1: IF DEPTH IS MEASURED ABOVE BOTTOM, CONVERT TO "BELOW SURFACE"  

  if (above_ground > 0) { 
    # depth range is measured above the bottom, whose depth is given in "above_ground"
    # invert mindepth and maxdepth
    my_max_depth <- above_ground - min_depth
    my_min_depth <- max(0.0, above_ground - max_depth)
  } else {
    # depth is measured below surface
    my_max_depth <- max_depth
    my_min_depth <- min_depth
  }

  # STEP 2: GET MODEL VALUES AT THE SAME DEPTH/TIME AS MEASUREMENTS
  if (!is.null(model1)) {model1estimates <- get_model_estimates(measureddata, model1, above_ground)}
  if (!is.null(model2)) {model2estimates <- get_model_estimates(measureddata, model2, above_ground)}
  if (!is.null(model3)) {model3estimates <- get_model_estimates(measureddata, model3, above_ground)}
  if (!is.null(model4)) {model4estimates <- get_model_estimates(measureddata, model4, above_ground)}


  # STEP 3: FIND TOTAL RANGE FOR ALL VALUES

  # find range of values
  totalmax=max(model1$data,na.rm=TRUE)
  if (!is.null(model2)) {totalmax = max(totalmax,max(model2$data,na.rm=TRUE))}
  if (!is.null(model3)) {totalmax = max(totalmax,max(model3$data,na.rm=TRUE))}
  if (!is.null(model4)) {totalmax = max(totalmax,max(model4$data,na.rm=TRUE))}
  totalmin=min(model1$data,na.rm=TRUE)
  if (!is.null(model2)) {totalmin = min(totalmin,min(model2$data,na.rm=TRUE))}
  if (!is.null(model3)) {totalmin = min(totalmin,min(model3$data,na.rm=TRUE))}
  if (!is.null(model4)) {totalmin = min(totalmin,min(model4$data,na.rm=TRUE))}
  measurementsmax=max(measureddata$value,na.rm=TRUE)
  measurementsmin=min(measureddata$value,na.rm=TRUE)
  if (is.finite(measurementsmax)) {totalmax=max(totalmax,measurementsmax)}
  if (is.finite(measurementsmin)) {totalmin=min(totalmin,measurementsmin)}
#  if (totalmin <= 0.5*totalmax) {totalmin=min(0.0,totalmin)}

  # STEP 4: DO THE PLOTTING
  # if output contains something, we will store a PNG file
  if (!is.null(output)) {
    # if no width/height/dpi are given, use standard values
    if (is.null(width))  {width  <- 3000}
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
  require("ggplot2")

  data1<-data.frame(obs=measureddata$value,model=model1estimates)
  data1<-data1[is.finite(data1$model),]
  MyMod <- lm(data1$model~data1$obs)
  MyPreds1 <- data.frame(predict(MyMod, interval="confidence", level = 0.95))
  if (!is.null(model2)) {
    data2<-data.frame(obs=measureddata$value,model=model2estimates)
    data2<-data2[is.finite(data2$model),]
    MyMod <- lm(data2$model~data2$obs)
    MyPreds2 <- data.frame(predict(MyMod, interval="confidence", level = 0.95))
  }
  if (!is.null(model3)) {
    data3<-data.frame(obs=measureddata$value,model=model3estimates)
    data3<-data3[is.finite(data3$model),]
    MyMod <- lm(data3$model~data3$obs)
    MyPreds3 <- data.frame(predict(MyMod, interval="confidence", level = 0.95))
  }
  if (!is.null(model4)) {
    data4<-data.frame(obs=measureddata$value,model=model4estimates)
    data4<-data4[is.finite(data4$model),]
    MyMod <- lm(data4$model~data4$obs)
    MyPreds4 <- data.frame(predict(MyMod, interval="confidence", level = 0.95))
  }

myplot = ggplot(data=data1, aes(x = data1$obs, y = data1$model)) 
myplot = myplot + scale_x_continuous(limits=c(totalmin,totalmax))
myplot = myplot + scale_y_continuous(limits=c(totalmin,totalmax))
myplot = myplot + xlab(label=paste0("observed ",description))
myplot = myplot + ylab(label=paste0("modeled ",description))

# first plot the absolute value line
myplot = myplot + geom_abline(intercept = 0,col="black") 

# second, plot the confidence intervals as a shaded box in the background
if (plot_options$show_confidence_interval == 1) {
  myplot = myplot + geom_ribbon(data=MyPreds1,aes(x=data1$obs,ymin=pmax(MyPreds1$lwr,totalmin), ymax=pmin(MyPreds1$upr,totalmax)), fill="blue", alpha=0.2)
  if (!is.null(model2)) {
    myplot = myplot + geom_ribbon(data=MyPreds2,aes(x=data2$obs,ymin=pmax(MyPreds2$lwr,totalmin), ymax=pmin(MyPreds2$upr,totalmax), y=pmax(MyPreds2$lwr,totalmin)), fill="red", alpha=0.2)
  }  
  if (!is.null(model3)) {
    myplot = myplot + geom_ribbon(data=MyPreds3,aes(x=data3$obs,ymin=pmax(MyPreds3$lwr,totalmin), ymax=pmin(MyPreds3$upr,totalmax), y=pmax(MyPreds3$lwr,totalmin)), fill="green", alpha=0.2)
  }  
  if (!is.null(model4)) {
    myplot = myplot + geom_ribbon(data=MyPreds4,aes(x=data4$obs,ymin=pmax(MyPreds4$lwr,totalmin), ymax=pmin(MyPreds4$upr,totalmax), y=pmax(MyPreds4$lwr,totalmin)), fill="magenta", alpha=0.2)
  }  
}

# now plot the points
myplot = myplot + geom_point(data=data1,shape=3, stroke=1.0, color="#8888FF") 
if (!is.null(model2)) {
  myplot = myplot + geom_point(data=data2,aes(x=data2$obs,y=data2$model),shape=3, stroke=1.0, color="#FF8888") 
}
if (!is.null(model3)) {
  myplot = myplot + geom_point(data=data3,aes(x=data3$obs,y=data3$model),shape=3, stroke=1.0, color="#88FF88") 
}
if (!is.null(model4)) {
  myplot = myplot + geom_point(data=data4,aes(x=data4$obs,y=data4$model),shape=3, stroke=1.0, color="#FF88FF") 
}

# now plot the regression lines
myplot = myplot + geom_smooth(data=data1,method=lm, col="blue",size=0.5,se=FALSE) 
if (!is.null(model2)) {
  myplot = myplot + geom_smooth(data=data2,aes(x=data2$obs,y=data2$model),method=lm, col="red",size=0.5,se=FALSE) 
}
if (!is.null(model3)) {
  myplot = myplot + geom_smooth(data=data3,aes(x=data3$obs,y=data3$model),method=lm, col="green",size=0.5,se=FALSE) 
}
if (!is.null(model4)) {
  myplot = myplot + geom_smooth(data=data4,aes(x=data4$obs,y=data4$model),method=lm, col="magenta",size=0.5,se=FALSE) 
}

print(myplot)

if (!is.null(output)) {
  dev.off()
}

}