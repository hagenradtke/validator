read_measured_data <- function(filename, 
                               start_date   = as.Date("2000-01-01"), # start date in yyyy-mm-dd, if end date is less or equal to start date, all data will be used
                               end_date     = as.Date("2000-01-01"), # end date in yyyy-mm-dd, if end date is less or equal to start date, all data will be used
                               min_depth    = 0.0,                   # minimum depth in meters
                               max_depth    = 100000.0,              # maximum depth in meters
                               above_ground = 0.0                    # set this value to the bottom depth in meters to measure the selected depth range above the ground instead of below the surface
                              ) {
  # STEP 1: SOME CONSISTENCY CHECKING OF INPUT ARGUMENTS

  # assert that start and and date are of date format
  start_date <- try( as.Date(start_date, format= "%Y-%m-%d") )
  if( class(start_date) == "try-error" || is.na(start_date) ) stop("start_date must be of format YYYY-MM-DD")
  end_date <- try( as.Date(end_date, format= "%Y-%m-%d") )
  if( class(end_date) == "try-error" || is.na(end_date) ) stop("end_date must be of format YYYY-MM-DD")

  # assert that numeric values are actually numeric
  if (!is.numeric(min_depth))    stop("min_depth must have a numeric value")
  if (!is.numeric(max_depth))    stop("max_depth must have a numeric value")
  if (!is.numeric(above_ground)) stop("above_ground must have a numeric value")

  # assert that file exists
  if (!file.exists(filename))    stop(paste0("Measurements file ",filename," does not exist."))

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


  # STEP 3: READ IN MEASUREMENTS FILE AND CONVERT DATETIME TO DATE

  measureddata <- read.csv(file=filename,sep=";")
  measureddata$date <- as.Date(measureddata$datetime,origin="1899-12-30")

  # STEP 4: REMOVE ALL DATA OUTSIDE TIME AND DEPTH RANGE

  measureddata=measureddata[measureddata$depth >= my_min_depth,]
  measureddata=measureddata[measureddata$depth <= my_max_depth,]
  if (start_date < end_date) {
    measureddata=measureddata[measureddata$date >= start_date,]
    measureddata=measureddata[measureddata$date <= end_date,]
  }

  # STEP 5: RETURN THE DATA, OR NULL IF NO DATA

  if (length(measureddata)>0) {
    return(measureddata)
  } else {
    return(NULL)
  }
}

read_measured_data_sqldb <- function(directory,
                               read_options, 
                               start_date   = as.Date("2000-01-01"), # start date in yyyy-mm-dd, if end date is less or equal to start date, all data will be used
                               end_date     = as.Date("2000-01-01"), # end date in yyyy-mm-dd, if end date is less or equal to start date, all data will be used
                               min_depth    = 0.0,                   # minimum depth in meters
                               max_depth    = 100000.0,              # maximum depth in meters
                               above_ground = 0.0,                   # set this value to the bottom depth in meters to measure the selected depth range above the ground instead of below the surface
                               setProgress  = NULL                   # function to update progress bar
                              ) {

  # STEP 1: SOME CONSISTENCY CHECKING OF INPUT ARGUMENTS
  # assert that start and and date are of date format
  start_date <- try( as.Date(start_date, format= "%Y-%m-%d") )
  if( class(start_date) == "try-error" || is.na(start_date) ) stop("start_date must be of format YYYY-MM-DD")
  end_date <- try( as.Date(end_date, format= "%Y-%m-%d") )
  if( class(end_date) == "try-error" || is.na(end_date) ) stop("end_date must be of format YYYY-MM-DD")

  # round the date to whole days
  start_date <- round(start_date)
  end_date <- round(end_date)

  # assert that numeric values are actually numeric
  if (!is.numeric(min_depth))    stop("min_depth must have a numeric value")
  if (!is.numeric(max_depth))    stop("max_depth must have a numeric value")
  if (!is.numeric(above_ground)) stop("above_ground must have a numeric value")

  # STEP 2: DELETE ALL FILES OLDER THAN 5 DAYS
  all_files  <- list.files(path=paste0(directory,"stationdata/"), pattern=glob2rx("sqldb_*.csv"), full.names=TRUE)
  file_times <- file.mtime(all_files)
  file_ages  <- difftime(Sys.time(), file_times, units = "days")
  old_files  <- all_files[file_ages > 5.0]
  file.remove(old_files)


  # STEP 3: MAKE A LIST OF ALL "sqldb_startdate_enddate_station_varname.csv" FILES
  # CHECK IF A FILE WITH startdate AND enddate SPANNING OUR DESIRED PERIOD EXISTS
  # IF NOT, WE NEED TO LOAD IT FROM THE DATABASE

  station_files  <- list.files(path=paste0(directory,"stationdata/"), pattern=glob2rx(paste0("sqldb_*_",tolower(read_options$stationname),"_",tolower(read_options$varname),".csv")), full.names=FALSE)
  # modify these character positions when replacing "sqldb" by a name of different length!
  start_dates    <- as.Date(substr(station_files,7,16))
  end_dates      <- as.Date(substr(station_files,18,27))

  # check if any time span is sufficiently long, if yes, use the first appropriate file
  time_span_ok   <- (start_dates <= start_date) & (end_dates >= end_date)
  if (any(time_span_ok)) {
    station_file <- paste0(directory,"stationdata/",station_files[min(which(time_span_ok))])
  } else {
    if(!is.null(setProgress)) { setProgress(message="Plotting...",value=41,detail="downloading data from database...") }
    # no station file yet, so get one from the database
    # first, find out to which database we connect
    database <- read.table(paste0(directory,"db_connection.txt"),header=TRUE,sep=";",stringsAsFactors=FALSE,quote = "'",)
    if (is.na(database$password)) {database$password<-""}
    
    # connect to the database
    library("RMariaDB")
    con <- NULL
    tryCatch({
      con <- dbConnect(MariaDB(),username=database$username,password=database$password,host=database$host,port=database$port)
    })
    if (is.null(con)) stop("Could not connect to SQL database")
    
    dbExecute(con,paste0("USE ",database$databasename,";"))
    
    # prepare values for SQL query
    startdatetime <- paste0(format(start_date,format="%Y-%m-%d")," 00:00:00")
    enddatetime   <- paste0(format(end_date,format="%Y-%m-%d")," 00:00:00")
    lat_min       <- read_options$latitude-1/60 # define +/- 1 n.m. as required accuracy for lon and lat
    lat_max       <- read_options$latitude+1/60 # i.e. we select all data from a lat/lon rect of 2x2 n.m.
    lon_min       <- read_options$longitude - 1/(60*max(0.01,cos(read_options$latitude*pi/180)))
    lon_max       <- read_options$longitude + 1/(60*max(0.01,cos(read_options$latitude*pi/180)))
    
    # SQL query - store data in a data frame, select all depths
    df <- dbGetQuery(con,paste0('SELECT measurements.depth,measurements.datetime,measurements.value ',
                                'FROM (measurements INNER JOIN variables ON measurements.variable=variables.id) ',
                                'WHERE (variables.varname="',read_options$varname,'" ',
                                'AND measurements.datetime>="',startdatetime,'" ',
                                'AND measurements.datetime<="',enddatetime,'" ',
                                'AND measurements.longitude>="',lon_min,'" ',
                                'AND measurements.longitude<="',lon_max,'" ',
                                'AND measurements.latitude>="',lat_min,'" ',
                                'AND measurements.latitude<="',lat_max,'" ',
                                ');'))
    
    # disconnect from the database
    dbDisconnect(con)
    
    # convert datetime to days since 1899-12-30
    df$datetime <- as.numeric(difftime(as.POSIXct(df$datetime,tz="UTC"),as.POSIXct("1899-12-30",tz="UTC"),units = "days"))

    # save data frame to file
    station_file  <- paste0(directory,"stationdata/sqldb_",format(start_date,format= "%Y-%m-%d"),"_",format(end_date,format= "%Y-%m-%d"),
                            "_",tolower(read_options$stationname),"_",tolower(read_options$varname),".csv")
    write.table(df,station_file,quote=FALSE,sep=";",row.names=FALSE)
        
    # delete all other files which are inside this range
    station_files     <- list.files(path=paste0(directory,"stationdata/"), pattern=glob2rx(paste0("sqldb_*_",tolower(read_options$stationname),"_",tolower(read_options$varname),".csv")), full.names=FALSE)
    start_dates       <- as.Date(substr(station_files,7,16))
    end_dates         <- as.Date(substr(station_files,18,27))
    time_span_smaller <- ((start_dates > start_date) & (end_dates <= end_date)) | ((start_dates >= start_date) & (end_dates < end_date))
    unneeded_files    <- station_files[time_span_smaller]
    if (length(unneeded_files)>0) {
      file.remove(paste0(directory,"stationdata/",unneeded_files))
    }
  }

  # STEP 4: USE STANDARD METHOD TO READ IN THE DATA WHICH WE JUST HAVE WRITTEN
  if(!is.null(setProgress)) { setProgress(message="Plotting...",value=65,detail="reading measurements file...") }
  return(read_measured_data(filename=station_file,start_date=start_date,end_date=end_date,
                            min_depth=min_depth,max_depth=max_depth,above_ground=above_ground))
}
