profile_plot <- function(
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
  # STEP 1: TIME AVERAGING MODEL DATA AND OBTAINING STD, QUANTILES, OR MIN/MAX
  time_average_modeldata <- function(model, show_range) {
    if (!is.null(model)) {
      model$ave <- rowMeans(model$data,na.rm=TRUE)
      model$maxi=model$ave
      model$mini=model$ave
      if (show_range == 1) { mystd = apply(model$data,1,sd,na.rm=TRUE); model$mini = model$ave-mystd; model$maxi = model$ave+mystd}
      if (show_range == 2) { model$mini = apply(model$data,1,quantile,0.025,na.rm=TRUE); model$maxi = apply(model$data,1,quantile,0.975,na.rm=TRUE);}
      if (show_range == 3) { model$mini = apply(model$data,1,min,na.rm=TRUE); model$maxi = apply(model$data,1,max,na.rm=TRUE)}
    }
    return(model)
  }
  model1 <- time_average_modeldata(model1, plot_options$show_range)
  model2 <- time_average_modeldata(model2, plot_options$show_range)
  model3 <- time_average_modeldata(model3, plot_options$show_range)
  model4 <- time_average_modeldata(model4, plot_options$show_range)

  # STEP 2: IF DEPTH IS MEASURED ABOVE BOTTOM, CONVERT TO "BELOW SURFACE"  

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

  # STEP 3: IF my_max_depth IS LARGER THAN 20000, CHOOSE DEEPEST OBSERVATION
  if (my_max_depth > 20000) {
    my_max_depth <- max(measureddata$depth)
  }


  # STEP 4: FIND TOTAL RANGE FOR ALL VALUES

  # find range of values
  totalmax=max(model1$maxi,na.rm=TRUE)
  if (!is.null(model2)) {totalmax = max(totalmax,max(model2$maxi,na.rm=TRUE))}
  if (!is.null(model3)) {totalmax = max(totalmax,max(model3$maxi,na.rm=TRUE))}
  if (!is.null(model4)) {totalmax = max(totalmax,max(model4$maxi,na.rm=TRUE))}
  totalmin=min(model1$mini,na.rm=TRUE)
  if (!is.null(model2)) {totalmin = min(totalmin,min(model2$mini,na.rm=TRUE))}
  if (!is.null(model3)) {totalmin = min(totalmin,min(model3$mini,na.rm=TRUE))}
  if (!is.null(model4)) {totalmin = min(totalmin,min(model4$mini,na.rm=TRUE))}
  measurementsmax=max(measureddata$value,na.rm=TRUE)
  measurementsmin=min(measureddata$value,na.rm=TRUE)
  if (is.finite(measurementsmax)) {totalmax=max(totalmax,measurementsmax)}
  if (is.finite(measurementsmin)) {totalmin=min(totalmin,measurementsmin)}
  if (totalmin <= 0.5*totalmax) {totalmin=min(0.0,totalmin)}

  # broaden the depth range to be shown, in case that model depth diverges from observed depth
  if (!is.null(model1)) {my_max_depth <- max(my_max_depth,max(model1$depth_axis))}
  if (!is.null(model2)) {my_max_depth <- max(my_max_depth,max(model2$depth_axis))}
  if (!is.null(model3)) {my_max_depth <- max(my_max_depth,max(model3$depth_axis))}
  if (!is.null(model4)) {my_max_depth <- max(my_max_depth,max(model4$depth_axis))}
  if (!is.null(model1)) {my_min_depth <- min(my_min_depth,min(model1$depth_axis))}
  if (!is.null(model2)) {my_min_depth <- min(my_min_depth,min(model2$depth_axis))}
  if (!is.null(model3)) {my_min_depth <- min(my_min_depth,min(model3$depth_axis))}
  if (!is.null(model4)) {my_min_depth <- min(my_min_depth,min(model4$depth_axis))}

  
  # STEP 5: DO THE PLOTTING
  # if output contains something, we will store a PNG file
  if (!is.null(output)) {
    # if no width/height/dpi are given, use standard values
    if (is.null(width))  {width  <- 3000}
    if (is.null(height)) {height <- 4000}
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
  data2plot=data.frame(ave=model1$ave,depth_axis=model1$depth_axis)
  # first draw observations as black crosses
  myggplot=ggplot(data2plot,aes(ave,depth_axis)) +
           geom_point(data=measureddata,aes(measureddata$value,measureddata$depth),shape=3,stroke=plot_options$point_thickness)

  if (plot_options$show_range != 0) {
    # draw shaded area in the background
    # needs to be drawn as a counterclockwise polygon, here we define its x and y values
    x=c(model1$mini,rev(model1$maxi))
    y=c(model1$depth_axis,rev(model1$depth_axis))
    mypolygon=data.frame(x,y)
    # remove possible NA values and draw the polygon
    mypolygon=mypolygon[is.finite(mypolygon$x),]
    myggplot =myggplot + geom_polygon(data=mypolygon, aes(x,y), fill="blue", alpha=0.2)
    # do the same for each other model
    if (!is.null(model2)) {
      x=c(model2$mini,rev(model2$maxi))
      y=c(model2$depth_axis,rev(model2$depth_axis))
      mypolygon=data.frame(x,y)
      mypolygon=mypolygon[is.finite(mypolygon$x),]
      myggplot =myggplot + geom_polygon(data=mypolygon, aes(x,y), fill="red", alpha=0.2)
    }  
    if (!is.null(model3)) {
      x=c(model3$mini,rev(model3$maxi))
      y=c(model3$depth_axis,rev(model3$depth_axis))
      mypolygon=data.frame(x,y)
      mypolygon=mypolygon[is.finite(mypolygon$x),]
      myggplot =myggplot + geom_polygon(data=mypolygon, aes(x,y), fill="green", alpha=0.2)
    }  
    if (!is.null(model4)) {
      x=c(model4$mini,rev(model4$maxi))
      y=c(model4$depth_axis,rev(model4$depth_axis))
      mypolygon=data.frame(x,y)
      mypolygon=mypolygon[is.finite(mypolygon$x),]
      myggplot =myggplot + geom_polygon(data=mypolygon, aes(x,y), fill="violet", alpha=0.2)
    }  
  }

  # now draw the average line of model1
  myggplot=myggplot+geom_path(color="#0000FF")+
    theme_bw()+
    xlab(label=description) + ylab(label="depth in m") +
    theme(panel.grid.major =   element_line(colour = "#CCCCCC",size=0.5),panel.grid.minor=element_line(colour = "#CCCCCC",size=0.1))+
    scale_x_continuous(limits=c(totalmin,totalmax))+
    scale_y_reverse(limits=c(my_max_depth,my_min_depth))
  # draw the average lines for the other models as well
  if (!is.null(model2)) {
    data2plot=data.frame(model2$ave,model2$depth_axis)
    myggplot=myggplot+geom_path(data=data2plot,aes(model2$ave,model2$depth_axis),color="#FF0000")
  }
  if (!is.null(model3)) {
    data2plot=data.frame(model3$ave,model3$depth_axis)
    myggplot=myggplot+geom_path(data=data2plot,aes(model3$ave,model3$depth_axis),color="#00FF00")
  }
  if (!is.null(model4)) {
    data2plot=data.frame(model4$ave,model4$depth_axis)
    myggplot=myggplot+geom_path(data=data2plot,aes(model4$ave,model4$depth_axis),color="#FF00FF")
  }
  print(myggplot)
  
  if (!is.null(output)) {
    dev.off()
  }
}