# THIS SCRIPT PREPARES AND EXECUTES THE PLOTTING FUNCTION CALLS
# IT IS SOURCED FROM WITHIN server.R TWICE IN DIFFERENT CONTEXTS

# FIRST, CHECK IF A PLOTTING PROCESS IS ALREADY RUNNING
# IN THIS CASE STOP IT FIRST

        if (do_this_in_parallel) {
          isolate({
            if (!is.null(saved$process$pid)) { 
              # an old process is running
              # check if in critical phase (doing file i/o, which is between progress of 2% and 68%)
              # if yes wait until that has finished
              while ((file.exists("./temp/busy.txt")) & (!is.null(saved$process$pid))) {
                Sys.sleep(0.1)
              }
              tools::pskill(saved$process$pid,signal=tools::SIGINT)
              saved$process <- list()
            }
          })
        }
        # make new progress bar
        setProgress(message="Plotting...",value=1,detail="checking input values...")
        
        # determine properties of chosen variable
        varnum = as.numeric(input$variablelist)
        myvarname=saved$variables$varname[varnum]
        mylongname=saved$variables$longname[varnum]
        myunit=saved$variables$unit[varnum]

        # determine name and depth of chosen station
        stationnum = as.numeric(input$stationlist)
        mystationname=saved$stations$stationname[stationnum]
        mystationdepth=as.numeric(saved$stations$depth[stationnum])
        mystationlon=as.numeric(saved$stations$longitude[stationnum])
        mystationlat=as.numeric(saved$stations$latitude[stationnum])

        # determine paths of chosen models
        model1num = as.numeric(input$model1)
        if (model1num > 0) {
          model1path = mymodels$path[model1num]
          if (substr(model1path, nchar(model1path), nchar(model1path)) != "/") {
            model1path <- paste0(model1path,"/")
          }
        }
        model2num = as.numeric(input$model2)
        if (model2num > 0) {
          model2path = mymodels$path[model2num]
          if (substr(model2path, nchar(model2path), nchar(model2path)) != "/") {
            model2path <- paste0(model2path,"/")
          }
        }
        model3num = as.numeric(input$model3)
        if (model3num > 0) {
          model3path = mymodels$path[model3num]
          if (substr(model3path, nchar(model3path), nchar(model3path)) != "/") {
            model3path <- paste0(model3path,"/")
          }
        }
        model4num = as.numeric(input$model4)
        if (model4num > 0) {
          model4path = mymodels$path[model4num]
          if (substr(model4path, nchar(model4path), nchar(model4path)) != "/") {
            model4path <- paste0(model4path,"/")
          }
        }

        # now prepare function arguments

        if (input$plottype == 1) { # time series plot
          plot_type    <- "timeseries"
          plot_options <- NULL
        }
        if (input$plottype == 2) { # vertical profile plot
          plot_type                    <- "profile"
          plot_options                 <- list()
          plot_options$show_range      <- as.numeric(input$plotoptions_profile)-1
          plot_options$point_thickness <- 0.3
        }
        if (input$plottype == 3) { # scatter plot
          plot_type                               <- "scatter"
          plot_options                            <- list()
          plot_options$show_confidence_intervals  <- ("1" %in% input$plotoptions_scatter)
          plot_options$monthly_color_model1       <- ("2" %in% input$plotoptions_scatter)
        }
        if (input$plottype == 4) { # Taylor diagram
          plot_type                         <- "taylor"
          plot_options                      <- list()
          plot_options$center_observations  <- ("1" %in% input$plotoptions_taylor)
          plot_options$normalize_sd         <- ("2" %in% input$plotoptions_taylor)
        }
        if (input$plottype == 5) { # Hovmoeller
          plot_type    <- "hovmoeller"
          plot_options <- NULL
        }
        if (input$plottype == 6) { # trend analysis
          plot_type    <- "trend"
          plot_options <- list()
          if ("1" %in% input$plotoptions_trend) {plot_options$components <- "longterm"}
          if ("2" %in% input$plotoptions_trend) {plot_options$components <- "seasonal"}
          if ("3" %in% input$plotoptions_trend) {plot_options$components <- "both"}
          if ("4" %in% input$plotoptions_trend) {plot_options$components <- "random"}
        }

        meas_options=list()
        meas_options$format <- saved$measurements_format
        if (saved$measurements_format == "standard") {
          measurements <- paste0(saved$datasetpath,"stationdata/",tolower(myvarname),"_",tolower(mystationname),".csv")
        } else if (saved$measurements_format == "sqldb") {
          measurements             <- saved$datasetpath
          meas_options$stationname <- mystationname
          meas_options$varname     <- myvarname
          meas_options$longitude   <- mystationlon
          meas_options$latitude    <- mystationlat
        } else {
          print(paste0("ERROR: unknown measurements format ",saved$measurements_format))
        }

        model1data <- as.character(paste0(model1path,tolower(mystationname),".nc"))
        if (as.numeric(model2num)>0) {
          model2data <- as.character(paste0(model2path,tolower(mystationname),".nc"))
        } else {
          model2data <- NULL
        }
        if (as.numeric(model3num)>0) {
          model3data <- as.character(paste0(model3path,tolower(mystationname),".nc"))
        } else {
          model3data <- NULL
        }
        if (as.numeric(model4num)>0) {
          model4data <- as.character(paste0(model4path,tolower(mystationname),".nc"))
        } else {
          model4data <- NULL
        }

        varname <- myvarname
        description <- paste0(mylongname," in ",myunit)

        if (input$depthrangecheckbox) {
          min_depth <- input$fromdepth
        } else {
          min_depth <- 0.0
        }
        if (input$depthrangecheckbox) {
          max_depth <- input$todepth
        } else {
          max_depth <- 100000.0
        }
        if (as.numeric(input$depthabovebottom)==2) {
          above_ground <- mystationdepth
        } else {
          above_ground <- 0.0
        }

        if (input$timerangecheckbox) {
          start_date <- input$fromtime
        } else {
          start_date <- "2000-01-01"
        }
        if (input$timerangecheckbox) {
          end_date <- input$totime
        } else {
          end_date <- "2000-01-01"
        }

        if (input$monthrangecheckbox) {
          start_month <- as.numeric(input$frommonth)
        } else {
          start_month <- 1
        }
        if (input$monthrangecheckbox) {
          end_month <- as.numeric(input$tomonth)
        } else {
          end_month <- 12
        }
        # call the plotting routine
        if (do_this_in_parallel) {
          saved$process <- mcparallel({model_validation_plot(
            plot_type = plot_type,
            plot_options = plot_options,
            measurements = measurements,
            meas_options = meas_options,
            model1data = model1data,
            model2data = model2data,
            model3data = model3data,
            model4data = model4data,
            varname = varname,
            description = description,
            min_depth = min_depth,
            max_depth = max_depth,
            above_ground = above_ground,
            start_date = start_date,
            end_date = end_date,
            start_month = start_month,
            end_month = end_month,
            output = con,
            width = maxwidth(),
            height = maxheight(),
            dpi = dpi,
            setProgress = setProgress
          )})
        } else {
          model_validation_plot(
            plot_type = plot_type,
            plot_options = plot_options,
            measurements = measurements,
            meas_options = meas_options,
            model1data = model1data,
            model2data = model2data,
            model3data = model3data,
            model4data = model4data,
            varname = varname,
            description = description,
            min_depth = min_depth,
            max_depth = max_depth,
            above_ground = above_ground,
            start_date = start_date,
            end_date = end_date,
            start_month = start_month,
            end_month = end_month,
            output = con,
            width = maxwidth(),
            height = maxheight(),
            dpi = dpi,
            setProgress = setProgress
          )
        }