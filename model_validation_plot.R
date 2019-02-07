source("read_model_data.R")
source("read_measured_data.R")
source("timeseries_plot.R")
source("profile_plot.R")
source("scatter_plot.R")
source("taylor_plot.R")
source("trend_plot.R")

setProgress <- function(message="", value=0, detail="") {
  write.csv(data.frame(message=message,value=value,detail=detail),file = "./temp/progress.csv")
}

stopWithError <- function(setProgress=NULL, message="") {
  if (!is.null(setProgress)) {setProgress(message="Error:",value=100,detail=message)}
  mcexit(message)
}

model_validation_plot <- function(plot_type    = c("timeseries","profile","scatter","taylor","hovmoeller"), # type of plot
                                  plot_options = NULL,                  # list of options for the chosen plot type
                                  measurements,                         # file name containing measured data, or keyword for different data access
                                  meas_options = NULL,                  # options for retreival of measured data, not required for standard "read from file" method
                                  model1data,                           # file name of model 1 data (shown in blue)
                                  model2data   = NULL,                  # file name of model 2 data (shown in red)
                                  model3data   = NULL,                  # file name of model 3 data (shown in green)
                                  model4data   = NULL,                  # file name of model 4 data (shown in violet)
                                  varname,                              # name of variable to compare
                                  description  = varname,               # description of variable including unit
                                  min_depth    = 0.0,                   # minimum depth in meters
                                  max_depth    = 0.0,                   # maximum depth in meters
                                  above_ground = 0.0,                   # set this value to the bottom depth in meters to measure the selected depth range above the ground instead of below the surface
                                  start_date   = as.Date("2000-01-01"), # start date in yyyy-mm-dd, if end date is less or equal to start date, time range of model 1 data will be chosen
                                  end_date     = as.Date("2000-01-01"), # end date in yyyy-mm-dd, if end date is less or equal to start date, time range of model 1 data will be chosen
                                  start_month  = 1,                     # start month for seasonal data selection, 1=January
                                  end_month    = 12,                    # end month for seasonal data selection, 12=December
                                  output       = NULL,                  # if different from NULL, it gives the file name of a PNG file to save; if NULL, plot will be returned as output
                                  width        = NULL,                  # plot width in pixels
                                  height       = NULL,                  # plot height in pixels
                                  dpi          = NULL,                  # plot resolution in dpi
                                  setProgress  = NULL                   # function to change progress bar
                                 ) {
  # STEP 1: SOME CONSISTENCY CHECKING OF INPUT ARGUMENTS
  # assert that start and end month are between 1 and 12
  if (!(start_month %in% 1:12)) stopWithError(setProgress=setProgress, message="start_month must be an integer value between 1 and 12")
  if (!(end_month %in% 1:12))   stopWithError(setProgress=setProgress, message="end_month must be an integer value between 1 and 12")

  # assert that start and and date are of date format
  start_date <- try( as.Date(start_date, format= "%Y-%m-%d") )
  if( class(start_date) == "try-error" || is.na(start_date) ) stopWithError(setProgress=setProgress, message="start_date must be of format YYYY-MM-DD")
  if (start_date < as.Date("1000-01-01")) stopWithError(setProgress=setProgress, message="start_date must be of format YYYY-MM-DD")
  end_date <- try( as.Date(end_date, format= "%Y-%m-%d") )
  if( class(end_date) == "try-error" || is.na(end_date) ) stopWithError(setProgress=setProgress, message="end_date must be of format YYYY-MM-DD")
  if (end_date < as.Date("1000-01-01")) stopWithError(setProgress=setProgress, message="end_date must be of format YYYY-MM-DD")

  # assert that numeric values are actually numeric
  if (!is.numeric(min_depth))    stopWithError(setProgress=setProgress, message="min_depth must have a numeric value")
  if (!is.numeric(max_depth))    stopWithError(setProgress=setProgress, message="max_depth must have a numeric value")
  if (!is.numeric(above_ground)) stopWithError(setProgress=setProgress, message="above_ground must have a numeric value")

  # STEP 2: READ IN MODEL1 DATA
  write("process in critical phase","./temp/busy.txt")
  if(!is.null(setProgress)) { setProgress(message="Plotting...",value=2,detail="reading model 1 data...") }
  model1 <- read_model_data(filename=model1data, varname=varname, 
                            start_date=start_date, end_date=end_date,
                            min_depth=min_depth, max_depth=max_depth, above_ground=above_ground,
                            setProgress=setProgress)
  # If no time range was chosen before (indicated by end_date <= start_date), 
  # we will use the range in which model1 data are present.
  # if no data are obtained because they are all outside the selected depth / time range, we typically accept that
  # but in this particular case, this causes an error
  if (end_date <= start_date) {
    if (is.null(model1)) {
      stopWithError(setProgress=setProgress, message="Model 1 has no data in given depth range, so the time range cannot be obtained from Model 1.")
    } else {
      start_date <- model1$start_date 
      end_date <- model1$end_date
    }
  }

  # STEP 3: READ IN MODEL2, MODEL3, MODEL4 DATA
  
  # initialize with NULL
  model2=NULL
  model3=NULL
  model4=NULL

  # if desired, load model2, ..., model4 data
  if (!is.null(model2data)) {
    if(!is.null(setProgress)) { setProgress(message="Plotting...",value=10,detail="reading model 2 data...") }
    model2 <- read_model_data(filename=model2data, varname=varname, 
                              start_date=start_date, end_date=end_date,
                              min_depth=min_depth, max_depth=max_depth, above_ground=above_ground,
                              setProgress=setProgress) }
  if (!is.null(model3data)) {
    if(!is.null(setProgress)) { setProgress(message="Plotting...",value=20,detail="reading model 3 data...") }
    model3 <- read_model_data(filename=model3data, varname=varname, 
                              start_date=start_date, end_date=end_date,
                              min_depth=min_depth, max_depth=max_depth, above_ground=above_ground,
                              setProgress=setProgress) }
  if (!is.null(model4data)) {
    if(!is.null(setProgress)) { setProgress(message="Plotting...",value=30,detail="reading model 4 data...") }
    model4 <- read_model_data(filename=model4data, varname=varname, 
                              start_date=start_date, end_date=end_date,
                              min_depth=min_depth, max_depth=max_depth, above_ground=above_ground,
                              setProgress=setProgress) }

  # STEP 4: READ IN MEASURED DATA
  stop=function(stopmessage) {
    stopWithError(setProgress=setProgress, message=stopmessage)
  }
  
  if(!is.null(setProgress)) { setProgress(message="Plotting...",value=40,detail="reading measurements...") }
  # check if name is a code string (which means using a different data collection method)
  if (meas_options$format=="sqldb") {
    # read from sql database
    measureddata <- read_measured_data_sqldb(directory=measurements,read_options=meas_options, 
                                             start_date=start_date, end_date=end_date,
                                             min_depth=min_depth, max_depth=max_depth, above_ground=above_ground,
                                             setProgress=setProgress)
  } else {
    # read from standard csv format
    measureddata <- read_measured_data(filename=measurements, 
                                       start_date=start_date, end_date=end_date,
                                       min_depth=min_depth, max_depth=max_depth, above_ground=above_ground)
  }
  # STEP 5: KICK OUT DATA FROM THE WRONG MONTHS
  if (file.exists("./temp/busy.txt")) {file.remove("./temp/busy.txt")}
  if(!is.null(setProgress)) { setProgress(message="Plotting...",value=68,detail="filtering months...") }
  # set data to NaN in model results
  delete_wrong_months <- function(model, start_month, end_month) {
    if (!is.null(model)) {
      modelmonths = as.POSIXlt(model$time_axis)$mon+1
      if (end_month >= start_month) {
        model$data[,(modelmonths<start_month) | (modelmonths>end_month)]=NA
      } else {
        model$data[,(modelmonths<start_month) & (modelmonths>end_month)]=NA
      }
    }
    return(model)
  }
  if(!is.null(setProgress)) { setProgress(message="Plotting...",value=70,detail="filtering months...") }
  model1 <- delete_wrong_months(model1, start_month, end_month)
  model2 <- delete_wrong_months(model2, start_month, end_month)
  model3 <- delete_wrong_months(model3, start_month, end_month)
  model4 <- delete_wrong_months(model4, start_month, end_month)
  # remove data points in measurements
  measuredmonths = as.POSIXlt(measureddata$date)$mon+1
  if (end_month >= start_month) {
    measureddata = measureddata[(measuredmonths>=start_month) & (measuredmonths<=end_month),]
  } else {
    measureddata = measureddata[(measuredmonths<=end_month) | (measuredmonths>=start_month),]
  }

  # STEP 6: CALL THE SPECIFIC PLOTTING ROUTINE FOR THE SELECTED PLOT TYPE

  if(!is.null(setProgress)) { setProgress(message="Plotting...",value=80,detail="calling plot routine...") }
  if (plot_type == "timeseries") {
    timeseries_plot(measureddata=measureddata, 
                           model1=model1, model2=model2, model3=model3, model4=model4,
                           start_date=start_date, end_date=end_date,
                           min_depth=min_depth, max_depth=max_depth, 
                           description=description, 
                           output=output, width=width, height=height, dpi=dpi)
  }
  if (plot_type == "profile") {
    profile_plot(plot_options=plot_options,
                           measureddata=measureddata, 
                           model1=model1, model2=model2, model3=model3, model4=model4,
                           start_date=start_date, end_date=end_date,
                           min_depth=min_depth, max_depth=max_depth, above_ground=above_ground,
                           description=description, 
                           output=output, width=width, height=height, dpi=dpi)
  }
  if (plot_type == "scatter") {
    scatter_plot(plot_options=plot_options,
                        measureddata=measureddata, 
                        model1=model1, model2=model2, model3=model3, model4=model4,
                        start_date=start_date, end_date=end_date,
                        min_depth=min_depth, max_depth=max_depth, above_ground=above_ground,
                        description=description, 
                        output=output, width=width, height=height, dpi=dpi)
  }
  if (plot_type == "taylor") {
    taylor_plot(plot_options=plot_options,
                       measureddata=measureddata, 
                       model1=model1, model2=model2, model3=model3, model4=model4,
                       start_date=start_date, end_date=end_date,
                       min_depth=min_depth, max_depth=max_depth, above_ground=above_ground,
                       description=description, 
                       output=output, width=width, height=height, dpi=dpi)
  }
  if (plot_type == "trend") {
    trend_plot(plot_options=plot_options,
                           measureddata=measureddata, 
                           model1=model1, model2=model2, model3=model3, model4=model4,
                           start_date=start_date, end_date=end_date,
                           min_depth=min_depth, max_depth=max_depth, 
                           description=description, 
                           output=output, width=width, height=height, dpi=dpi)
  }

  if (!is.null(output)) { return(list(src=output,width=width,height=height)) }

}
