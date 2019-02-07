read_model_data <- function(filename,     # file name of model data NetCDF file
                            varname,      # variable name (e.g. "sali" to read "MODEL_SALI" from NetCDF)
                            start_date,   # start date in yyyy-mm-dd, if end date is less or equal to start date, complete time range of model data will be chosen
                            end_date,     # end date in yyyy-mm-dd, if end date is less or equal to start date, complete time range of model data will be chosen
                            min_depth,    # minimum depth in meters
                            max_depth,    # maximum depth in meters
                            above_ground, # set this value to the bottom depth in meters to measure the selected depth range above the ground instead of below the surface
                            setProgress=NULL # function to show progress
                           ) {

  # STEP 1: SOME CONSISTENCY CHECKING OF INPUT ARGUMENTS
  # assert that input file exists
  if (!file.exists(filename)) {
    stopWithError(setProgress=setProgress, message=paste0("File ",filename," does not exist."))  
  }
  # assert that start and and date are of date format
  start_date <- try( as.Date(start_date, format= "%Y-%m-%d") )
  if( class(start_date) == "try-error" || is.na(start_date) ) stopWithError(setProgress=setProgress, message="start_date must be of format YYYY-MM-DD")
  end_date <- try( as.Date(end_date, format= "%Y-%m-%d") )
  if( class(end_date) == "try-error" || is.na(end_date) ) stopWithError(setProgress=setProgress, message="end_date must be of format YYYY-MM-DD")

  # assert that numeric values are actually numeric
  if (!is.numeric(min_depth))    stopWithError(setProgress=setProgress, message="min_depth must have a numeric value")
  if (!is.numeric(max_depth))    stopWithError(setProgress=setProgress, message="max_depth must have a numeric value")
  if (!is.numeric(above_ground)) stopWithError(setProgress=setProgress, message="above_ground must have a numeric value")

  # STEP 2: OPEN NETCDF FILE AND FIND OUT WHICH AXES THE DESIRED VARIABLE HAS
  # load package for NetCDF access
  library("RNetCDF")
  # open the NetCDF file
  ncfile=try(open.nc(filename))
  if( class(ncfile) == "try-error" || is.na(ncfile) ) stopWithError(setProgress=setProgress, message=paste0("Error opening NetCDF file ",filename))

  # create full variable name, e.g. "MODEL_CHL" from "chl"
  varname=paste0("MODEL_",toupper(varname))

  # get variable info, including dimension id numbers in correct order
  varinfo=try(var.inq.nc(ncfile, varname))
  if( class(varinfo) == "try-error" || is.na(varinfo) ) stopWithError(setProgress=setProgress, message=paste0("File ",filename," does not contain variable ",varname))
  
  # we need to find out the order of the dimensions to see which one is time and which one is depth
  # get info about all dimensions of that variable
  diminfos=list(rep(0,length(varinfo$dimids)))
  for (i in 1:length(varinfo$dimids)) {
    diminfos[[i]]=dim.inq.nc(ncfile, varinfo$dimids[i])
  }
  # we require that time dimension starts with t or T and depth dimension starts with z or Z.
  # now see what variable comes last
  for (i in 1:length(varinfo$dimids)) {
    # get first character of dimension name and convert to lowercase
    firstchar=tolower(substr(diminfos[[i]]$name,1,1))
    if (firstchar == "t") {
      lastvar="t"
      t_name=diminfos[[i]]$name
      t_index=i                 # store index of time axis for later use
    }
    if (firstchar == "z") {
      lastvar="z"
      z_name=diminfos[[i]]$name
    }
  }
  # if one of the variables is not found, we need to stop
  if (!exists("t_name")) stopWithError(setProgress=setProgress, message=paste0("Variable ",varname," in file ",filename," contains no time dimension starting with t or T."))
  if (!exists("z_name")) stopWithError(setProgress=setProgress, message=paste0("Variable ",varname," in file ",filename," contains no depth dimension starting with z or Z."))

  # STEP 3: READ IN TIME AND DEPTH AXIS AND POSSIBLY CONVERT TIME AXIS UNITS

  # read in the axes
  model_t = var.get.nc(ncfile,t_name)
  model_z = var.get.nc(ncfile,z_name)

  # find out origin and scaling of time axis and convert it to Windows/Excel time format: Days since 1899-12-30
  time_info=var.inq.nc(ncfile,t_name)
  # seek for a "units" attribute
  for (i in 0:(time_info$natts-1)) { 
    attribute=att.inq.nc(ncfile, t_name, i)
    if (tolower(attribute$name)=="units") {
    # this attribute is called "units", we will read it
    time_units=tolower(att.get.nc(ncfile, t_name, i))
      # check if unit is years, months, weeks, days, hours, minutes, or seconds and apply a scaling to days
      # then take the origin and shift it to 1899-12-30
      if (substr(time_units,1,11)=="years since") {
        origdate = substr(time_units,13,31)
        delta_time = as.numeric(strptime(origdate,"%Y-%m-%d %H:%M:%S")-strptime("1899-12-30","%Y-%m-%d"))
        model_t = model_t*365.25 + delta_time
      }
      if (substr(time_units,1,12)=="months since") {
        origdate = substr(time_units,14,32)
        delta_time = as.numeric(strptime(origdate,"%Y-%m-%d %H:%M:%S")-strptime("1899-12-30","%Y-%m-%d"))
        model_t = model_t*365.25/12 + delta_time
      }
      if (substr(time_units,1,11)=="weeks since") {
        origdate = substr(time_units,13,31)
        delta_time = as.numeric(strptime(origdate,"%Y-%m-%d %H:%M:%S")-strptime("1899-12-30","%Y-%m-%d"))
        model_t = model_t*7 + delta_time
      }
      if (substr(time_units,1,10)=="days since") {
        origdate = substr(time_units,12,30)
        delta_time = as.numeric(strptime(origdate,"%Y-%m-%d %H:%M:%S")-strptime("1899-12-30","%Y-%m-%d"))
        model_t = model_t + delta_time
      }
      if (substr(time_units,1,11)=="hours since") {
        origdate = substr(time_units,13,31)
        delta_time = as.numeric(strptime(origdate,"%Y-%m-%d %H:%M:%S")-strptime("1899-12-30","%Y-%m-%d"))
        model_t = model_t/24 + delta_time
      }
      if (substr(time_units,1,13)=="minutes since") {
        origdate = substr(time_units,15,33)
        delta_time = as.numeric(strptime(origdate,"%Y-%m-%d %H:%M:%S")-strptime("1899-12-30","%Y-%m-%d"))
        model_t = model_t/24/60 + delta_time
      }
      if (substr(time_units,1,13)=="seconds since") {
        origdate = substr(time_units,15,33)
        delta_time = as.numeric(strptime(origdate,"%Y-%m-%d %H:%M:%S")-strptime("1899-12-30","%Y-%m-%d"))
        model_t = model_t/24/3600 + delta_time
      }
    }
  }

  # STEP 4: CHOOSE THE SELECTED TIME AXIS RANGE

  # convert time axis to date
  model_t <- as.Date(model_t,origin="1899-12-30")

  # find out valid time indexes which are between start_date and end_date
  # if end_date is less or equal to start_date, this is interpreted as "choose full time range"
  if (start_date >= end_date) {
    firsttimeindex <- 1
    lasttimeindex  <- length(model_t)
  } else {
    firsttimeindex <- min(which(model_t >= start_date))
    lasttimeindex  <- max(which(model_t <= end_date))
  }

  # STEP 5: READ IN THE DATA
  
  # if no valid time points exist (firsttimeindex > lasttimeindex), this is okay, but we then return NULL
  if (firsttimeindex > lasttimeindex) {
    return(NULL)
  } else {
    # construct output list and already put start and end date in
    myoutput            <- list()
    myoutput$start_date <- model_t[firsttimeindex]
    myoutput$end_date   <- model_t[lasttimeindex]
    myoutput$time_axis  <- model_t[firsttimeindex:lasttimeindex]

    # construct a "start" and "count" vector to read in the data for the selected time range only
    start_vector = rep(1,length(varinfo$dimids))
    count_vector = rep(NA,length(varinfo$dimids)) # NA means all
    start_vector[t_index] = firsttimeindex
    count_vector[t_index] = lasttimeindex - firsttimeindex + 1

    # read in the data
    model_data = var.get.nc(ncfile,varname,start=start_vector,count=count_vector,collapse=TRUE)
    # here, collapse=TRUE means if we have single-point dimensions like lon and lat, these are removed
    # but we need to check if we have more than two dimensions remaining, in this case abort
    if (length(dim(model_data)) > 2) stopWithError(setProgress=setProgress, message=paste0("Variable ",varname," in file ",filename," has more than two dimensions."))

    # now we make sure that the data are stored in a matrix with 
    #   - columns containing vertical profiles
    #   - rows containing time series
    # that is, depth = first index, time = second index
    # to achieve that, we have to transpose in two cases:
    #  a) two dimensions, but time was first variable in the list
    if ((lastvar == "z") & (length(dim(model_data)) == 2)) { model_data <- t(model_data) }
    #  b) one dimension because only one depth index exists in the file
    #     ( make the column vector a row vector )
    if ((length(dim(model_data)) == 1) & (lasttimeindex > firsttimeindex)) { model_data <- t(model_data) }
    # if we have one time index only, we need to make sure that the column vector is still interpreted as a matrix with two dimensions
    # we can do that by transposing two times
    if ((length(dim(model_data)) <= 1) & (lasttimeindex == firsttimeindex)) { model_data <- t(t(model_data)) }

    # close the NetCDF file
    close.nc(ncfile)

    # set out-of-range values to NA
    model_data[abs(model_data)>1e20] = NA

    # STEP 6: CHOOSE SELECTED DEPTH RANGE

    # first we need to check if depth starts with a negative value
    if (model_z[1]<0) {
      # in this case, we need to do three things
      # a) multiply by -1 to get positive values
      model_z=model_z*(-1)
      # b) reverse the axis so it is still increasing
      model_z=rev(model_z)
      # c) reverse the data
      model_data=model_data[rev(1:length(model_z)),]
    }

    # now select valid depth range only
    if (above_ground == 0) {
      # if measured from surface, use given values
      my_min_depth <- min_depth
      my_max_depth <- max_depth
    } else {
      # if measured from bottom, we need to find out the depth range measured from the surface
      # find out which is the bottom depth - it is the highest depth with any non-NA value present
      data_present <- apply(is.finite(model_data),1,any)
      bottom_depth <- max(model_z[data_present])

      # obtain min and max depth
      my_min_depth <- bottom_depth - max_depth
      my_max_depth <- bottom_depth - min_depth
    }

    # shrink the data to the given depth range
    model_data <- model_data[(model_z >= my_min_depth) & (model_z <= my_max_depth),]
    # shrink the depth axis to the given depth range
    model_z    <- model_z   [(model_z >= my_min_depth) & (model_z <= my_max_depth)]

    # STEP 7: RETURN THE DATA INCLUDING THE AXES

    # check if any data remain, otherwise return NULL
    if (length(model_z) == 0) {
      return(NULL)
    } else {
      myoutput$depth_axis <- model_z
      myoutput$data       <- model_data
      return(myoutput)
    }
  }
}