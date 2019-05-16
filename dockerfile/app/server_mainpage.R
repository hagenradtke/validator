  ################################################
  # STEP 1: LOAD EXTERNAL FUNCTIONS AND PACKAGES #
  ################################################
 
  source("model_validation_plot.R")
  library("parallel")

  #########################################
  # STEP 2: READ IN STATIC / INITIAL DATA #
  #########################################

  # first, read in the lists of measurement datasets, models, and lat/lon regions for selecting stations on a map
  mydatasets=read.csv("datasets.txt",sep=";",header=TRUE,stringsAsFactors=FALSE,strip.white=TRUE)
  saved$models=read.csv("models.txt",sep=";",header=TRUE,stringsAsFactors=FALSE,strip.white=TRUE)
  myregions=read.csv("regions.txt",sep=";",header=TRUE,stringsAsFactors=FALSE,strip.white=TRUE)

  # then convert these lists to integer vectors with row names, as this is the required data type for shiny widgets
  datasetnames=1:length(mydatasets$name)
  names(datasetnames)=mydatasets$name
  regionnames=1:length(myregions$name)
  names(regionnames)=myregions$name

  # create a similar vector with month names
  months=1:12
  names(months)=c("January","February","March","April","May","June",
                  "July","August","September","October","November","December")

  # clean up if last session crashed
  if (file.exists("./temp/busy.txt")) {file.remove("./temp/busy.txt")}

  # create a list of all observers to destroy them if no longer needed
  observers <- list()
  
  #################################################
  # STEP 3: CREATE LEFT COLUMN FROM TOP TO BOTTOM #
  #################################################

  # dataset list
  output$datasetlist = renderUI(
                         selectInput("datasetlist",
                                     "measured dataset:",
                                     choices = datasetnames))
  outputOptions(output, "datasetlist", suspendWhenHidden=FALSE)

  # read station and variable list depending on dataset
  observers$stations <- observe({
    datasetnum = input$datasetlist
    saved$datasetpath=mydatasets$path[as.numeric(datasetnum)]
    saved$measurements_format=mydatasets$format[as.numeric(datasetnum)]
    if (file.exists(paste0(saved$datasetpath,"stations.csv"))) {
      saved$stations = read.csv(paste0(saved$datasetpath,"stations.csv"),sep=";",header=TRUE,stringsAsFactors=FALSE,strip.white=TRUE)
    } else {
      saved$stations=NULL
    }
    if (file.exists(paste0(saved$datasetpath,"variables.csv"))) {
      saved$variables = read.csv(paste0(saved$datasetpath,"variables.csv"),sep=";",header=TRUE,stringsAsFactors=FALSE,strip.white=TRUE)
    } else {
      saved$variables=NULL
    }
  },priority=10)

  ##################################

  # station list
  observers$stationnames <- observe({
    if (!is.null(saved$stations)) {
      stationnames=1:length(saved$stations$stationname)
      names(stationnames)=saved$stations$stationname
      output$stationlist = renderUI(
                           selectInput("stationlist",
                                       "station:",
                                       choices = stationnames))
    } else {
      output$stationlist = renderUI(
                           helpText(paste0("no stations given in data set - ",saved$datasetpath,"stations.csv does not exist")))
    }
  },priority=4)
  # select on map checkbox
  output$map_checkbox = renderUI(
                        checkboxInput("select_on_map",label="map...",value=FALSE))

  ##################################

  # station info label
  observers$infolabel <- observe({
      stationnum = as.numeric(input$stationlist)
      station_lon = saved$stations$longitude[stationnum]
      station_lat = saved$stations$latitude[stationnum]
      station_depth = saved$stations$depth[stationnum]
      output$stationcoords = renderUI(
                               helpText(paste0(as.character(station_lat)," deg N, ",
                                               as.character(station_lon)," deg E, depth=",
                                               as.character(station_depth)," m")))
  },priority=3)

  ##################################

  # variable list
  observers$variablelist <- observe({
    if (!is.null(saved$variables)) {
      variablenames=1:length(saved$variables$varname)
      names(variablenames)=saved$variables$varname
      output$variablelist = renderUI(
                           radioButtons("variablelist",
                                       "variable:",
                                       inline=TRUE,
                                       choices = variablenames))
    } else {
      output$variablelist = renderUI(
                           helpText(paste0("no variables given in data set - ",saved$datasetpath,"variables.csv does not exist")))
    }
  },priority=2)

  ##################################
  
  # model1, ..., model4 list
  observers$model1list <- observe({
    mymodels=saved$models
    isolate({

    # create a list of model names
    saved$modelnames=1:length(mymodels$name)
    names(saved$modelnames)=mymodels$name

    # also create an extended list which starts with "none"
    saved$extmodelnames=0:length(mymodels$name)
    names(saved$extmodelnames)=c("none",mymodels$name)

    # now fill them in
      output$model1list = renderUI(
                         selectInput("model1",
                                     "Models 1-4:",
                                     choices = saved$modelnames))
      output$model2list = renderUI(
                         selectInput("model2",
                                     NULL,
                                     choices = saved$extmodelnames
                                     ))
      output$model3list = renderUI(
                         selectInput("model3",
                                     NULL,
                                     choices = saved$extmodelnames))
      output$model4list = renderUI(
                         selectInput("model4",
                                     NULL,
                                     choices = saved$extmodelnames))
    })
  },priority=1)

  ##################################

  if (runs_in_private) {
    # models advanced options
    output$models_advanced = renderUI(
                         div(checkboxInput("modelscheckbox", 
                                       label = "other models...", 
                                       value = FALSE),float="right"))

    # if this checkbox is clicked, show upload/download button for model list
    observers$models_advanced <- observe({
      if (length(input$modelscheckbox) == 1) {
        if (input$modelscheckbox == 1) {
          output$download_model_list = renderUI(downloadButton("downloadml","download default model list"))
          output$downloadml = downloadHandler(file="models.txt", content=function(con) {
                                                                           file.copy("./models.txt",con)
                                                                         })
          output$upload_model_list = renderUI(
                          fileInput("uploaded_model_list",label="choose own model list"))
        } else {
          output$download_model_list = renderUI(div())
          output$upload_model_list = renderUI(div())
        }
      }
    },priority=10)
    observers$models <- observe({ 
      if (!is.null(input$uploaded_model_list)) {saved$models=read.csv(input$uploaded_model_list$datapath,sep=";",header=TRUE,stringsAsFactors=FALSE,strip.white=TRUE)}
    },priority=5)
  } else {
    # no models advanced options
    output$models_advanced = renderUI(div())
    output$download_model_list = renderUI(div())
    output$upload_model_list = renderUI(div())
  }


  ##################################

  # time range selection box
  output$timerange = renderUI(
                         checkboxInput("timerangecheckbox", 
                                       label = strong("select time range"), 
                                       value = FALSE))

  # if time range is selected, display inputs
  observers$timerange <- observe({
    if (length(input$timerangecheckbox) == 1) {
      if (input$timerangecheckbox == 1) {
        output$timeminmax = renderUI(conditionalPanel(
                          condition="input.timerangecheckbox == true",
                          actionButton("timeminmax", label = "set to min/max")))
        output$fromtime = renderUI(conditionalPanel(
                          condition="input.timerangecheckbox == true",
                          textInput("fromtime","from:",value="1990-01-01")))
        output$totime = renderUI(conditionalPanel(
                          condition="input.timerangecheckbox == true",
                          textInput("totime","to:",value="2000-12-31")))
      } else {
        output$timeminmax = renderUI(div())
        output$fromtime   = renderUI(div())
        output$totime     = renderUI(div())
      }
    }
  },priority=10)

  ##################################

  # month range selection box
  output$monthrange = renderUI(
                         checkboxInput("monthrangecheckbox", 
                                       label = strong("select month range"), 
                                       value = FALSE))

  # if month range is selected, display inputs

  output$frommonth = renderUI(conditionalPanel(
                        condition="input.monthrangecheckbox == true",
                        selectInput("frommonth","from:",choices=months,selected=1)))
  output$tomonth = renderUI(conditionalPanel(
                        condition="input.monthrangecheckbox == true",
                        selectInput("tomonth","to including:",choices=months,selected=12)))

  ##################################

  # depth range selection box
  output$depthrange = renderUI(
                         checkboxInput("depthrangecheckbox", 
                                       label = strong("select depth range in m"), 
                                       value = TRUE))

  # if depth range is selected, display inputs

  output$fromdepth = renderUI(conditionalPanel(
                        condition="input.depthrangecheckbox == true",
                        numericInput("fromdepth","from:",value=0)))
  output$todepth = renderUI(conditionalPanel(
                        condition="input.depthrangecheckbox == true",
                        numericInput("todepth","to:",value=5)))
  output$depthabovebottom = renderUI(conditionalPanel(
                        condition="input.depthrangecheckbox == true",
                        radioButtons("depthabovebottom",NULL,inline=TRUE,
                                   choices=list("below surface"=1,"above bottom"=2))))

  ################################################################
  # STEP 4: DECIDE WHETHER TO SHOW PLOT OR STATION SELECTION MAP #
  ################################################################

  observers$timerange_or_region <- observe({
#  observe({
    isolate({
      if (!is.null(observers$map))       {observers$map$destroy()      }
      if (!is.null(observers$maphover))  {observers$maphover$destroy() }
      if (!is.null(observers$mapclick))  {observers$mapclick$destroy() }
      if (!is.null(observers$startplot)) {observers$startplot$destroy()}
      if (!is.null(observers$showplot))  {observers$showplot$destroy() }
      if (!is.null(observers$plottype))  {observers$plottype$destroy() }
      if (!is.null(observers$plotoptions_profile))  {observers$plotoptions_profile$destroy()}
      if (!is.null(observers$plotoptions_scatter))  {observers$plotoptions_scatter$destroy()}
      if (!is.null(observers$plotoptions_taylor ))  {observers$plotoptions_taylor$destroy() }
      if (!is.null(observers$plotoptions_trend  ))  {observers$plotoptions_trend$destroy()  }
    })
    if (length(input$select_on_map) == 1) {
      if (input$select_on_map == 0) {
        isolate({
          ########################################################
          # BRANCH a: DRAW THE PLOT                              #
          ########################################################
          # STEP 4a: SHOW PLOT OPTIONS DEPENDING ON TYPE OF PLOT #
          ########################################################

          output$plottype_or_region = renderUI(
                                          radioButtons("plottype","plot type:",inline=TRUE,
                                                       choices = list("time series"=1,"vertical profile"=2,
                                                                      "scatter plot"=3,"Taylor diagram"=4,
                                                                      "trend analysis"=6),
                                                       selected = isolate(saved$plottype)))
          observers$plottype <<- observe({saved$plottype <- input$plottype},priority=1)
  
          output$plotoptions_profile = renderUI(conditionalPanel(
                                condition="input.plottype == 2 && input.select_on_map == 0",
                                radioButtons("plotoptions_profile","plot options:",inline=TRUE,
                                             choices = list("average only"=1,"+/- standard deviation"=2,"95th percentile"=3,"min/max"=4),
                                             selected = isolate(saved$plotoptions_profile))))
          observers$plotoptions_profile <<- observe({saved$plotoptions_profile <- input$plotoptions_profile},priority=1)

          output$plotoptions_scatter = renderUI(conditionalPanel(
                                condition="input.plottype == 3 && input.select_on_map == 0",
                                checkboxGroupInput("plotoptions_scatter","plot options:",inline=TRUE,
                                                   choices = list("show confidence intervals"=1),
                                                   selected = isolate(saved$plotoptions_scatter))))
          observers$plotoptions_scatter <<- observe({saved$plotoptions_scatter <- input$plotoptions_scatter},priority=1)

          output$plotoptions_taylor = renderUI(conditionalPanel(
                                condition="input.plottype == 4 && input.select_on_map == 0",
                                checkboxGroupInput("plotoptions_taylor","plot options:",inline=TRUE,
                                                   choices = list("center observations on x axis"=1, "normalize SD"=2),
                                                   selected = isolate(saved$plotoptions_taylor))))
          observers$plotoptions_taylor <<- observe({saved$plotoptions_taylor <- input$plotoptions_taylor},priority=1)

          output$plotoptions_trend = renderUI(conditionalPanel(
                                condition="input.plottype == 6 && input.select_on_map == 0",
                                radioButtons("plotoptions_trend","show which terms:",inline=TRUE,
                                             choices = list("longterm+const"=1,"seasonal+const"=2,"longterm+seasonal+const"=3,"random"=4),
                                             selected = isolate(saved$plotoptions_trend))))
          observers$plotoptions_trend <<- observe({saved$plotoptions_trend <- input$plotoptions_trend},priority=1)

          output$stationname_label <- renderUI(div())
          output$downloadplot_button <- renderUI(downloadButton('downloadplot','Save the plot as ...'))


          ############################
          # STEP 5a: DO THE PLOTTING #
          ############################

          # first observer to start the plotting
          observers$startplot <<- observe({
            mymodels <- isolate(saved$models)
            myprogress <- isolate(saved$progress)
            myplottime <- isolate(saved$plottime)
            nowtime <- isolate(Sys.time())
            # check if both a station and a variable list are loaded
            if (!is.null(saved$stations) & !is.null(saved$variables) & (length(input$model1)>0) & (length(input$model2)>0) & (length(input$model3)>0) & (length(input$model4)>0)) {
              # check if a plot type is selected
              if (length(input$plottype)==1) {
                # check whether another plot was started very short time ago
                # this happens e.g. by clicking on a numeric input 20 times to increase its value by 20
                # in this case, wait until nothing changes for a second
                if (difftime(nowtime, myplottime, units = "secs") < 0.8) {
                    saved$plottime <- nowtime
                    invalidateLater(1000, session) # we may do this plot, but only later if no input has changed
                } else {
                  saved$plottime <- nowtime
                  # prescribe the size of the plot depending on its type
                  if (input$plottype == 1) {maxwidth  = function() {isolate(min(session$clientData$output_mainPlot_width,1200))}
                                            maxheight = function() {isolate(min(session$clientData$output_mainPlot_width/2,600))}}
                  if (input$plottype == 2) {maxwidth  = function() {isolate(min(session$clientData$output_mainPlot_width,450))}
                                            maxheight = function() {isolate(min(session$clientData$output_mainPlot_width*4/3,600))}}
                  if (input$plottype == 3) {maxwidth  = function() {isolate(min(session$clientData$output_mainPlot_width,600))}
                                            maxheight = function() {isolate(min(session$clientData$output_mainPlot_width,600))}}
                  if (input$plottype == 4) {maxwidth  = function() {isolate(min(session$clientData$output_mainPlot_width,600))}
                                            maxheight = function() {isolate(min(session$clientData$output_mainPlot_width,600))}}
                  if (input$plottype == 6) {maxwidth  = function() {isolate(min(session$clientData$output_mainPlot_width,1200))}
                                            maxheight = function() {isolate(min(session$clientData$output_mainPlot_width/2,600))}}
                  dpi = 96.0

                  # do the actual plotting

                  con <- tempfile(fileext=".png") # temporary output file to save plot
                  do_this_in_parallel <- TRUE
                  source("do_the_plot.R",local=environment())
                }
              } 
            }
          },priority=0)
          
          # second observer to draw the progress bar
          observers$progressbar <<- observe({
            invalidateLater(100,session)
            isolate({
              # we want to read the file "progress.csv" to see what the current progress is
              if (file.exists("./temp/progress.csv")) {
                if (is.null(saved$progress)) {
                  # create new progress bar
                  saved$progress <- Progress$new(session, min=1, max=100)
                }
                try({ # need to put this into "try" because sometimes the file already exists but is still empty
                  df=read.csv("./temp/progress.csv")
                  saved$progress$set(message=df$message, value=df$value, detail=df$detail)
                })
              }
            })
          })

          # third observer to collect the plot when done
          observers$showplot <<- observe({
            invalidateLater(100, session)
            if (!is.null(saved$process$pid)) {
              a <- suppressWarnings(mccollect(saved$process,wait=F))
              if (!is.null(a)) {
                if (file.exists("./temp/busy.txt")) {file.remove("./temp/busy.txt")}
                saved$process <- list()
                if (class(a[[1]]) == "list") { # no error occurred, but return value is given
                  if (file.exists(a[[1]]$src)) {
                    # show image
                    output$mainPlot <- renderImage({
                                         list(src=a[[1]]$src, contentType = "image/png", alt="", width=a[[1]]$width, height=a[[1]]$height) 
                                       },deleteFile=TRUE)
                    # close progress bar
                    isolate({
                      file.remove("./temp/progress.csv")
                      if (!is.null(saved$progress)) { saved$progress$close(); saved$progress=NULL }
                    })
                    # free memory of all (possibly aborted) processes
                    suppressWarnings(mccollect())
                    
                  }
                }
              }
            }
          },priority=0)

          ###########################################################
          # STEP 6a: DO ALMOST THE SAME WHEN SAVE BUTTON IS CLICKED #
          ###########################################################

          output$downloadplot <- downloadHandler(
            filename = function() {
              # check if both a station and a variable list are loaded
              if ((!is.null(saved$stations)) & (!is.null(saved$variables))) {
                varnum = as.numeric(input$variablelist)
                myvarname=saved$variables$varname[varnum]
                stationnum = as.numeric(input$stationlist)
                mystationname=saved$stations$stationname[stationnum]
                if (input$plottype==1) {
                  return(paste0("timeseries_",myvarname,"_",mystationname,".png"))
                }
                if (input$plottype==2) {
                  return(paste0("profile_",myvarname,"_",mystationname,".png"))
                }
                if (input$plottype==3) {
                  return(paste0("scatter_",myvarname,"_",mystationname,".png"))
                }
                if (input$plottype==4) {
                  return(paste0("taylor_",myvarname,"_",mystationname,".png"))
                }
                if (input$plottype==4) {
                  return(paste0("trend_",myvarname,"_",mystationname,".png"))
                }
              }
            },
            content = function(con) {
              mymodels <- isolate(saved$models)
              myprogress <- isolate(saved$progress)
              if (!is.null(saved$stations) & !is.null(saved$variables)) {
                maxwidth  <- function() {return(NULL)}    # use standard size of the plot
                maxheight <- function() {return(NULL)}
                dpi=NULL
                do_this_in_parallel <- FALSE
                source("do_the_plot.R",local=TRUE)
                # close progress bar
                isolate({
                  if (file.exists("./temp/progress.csv")) {file.remove("./temp/progress.csv")}
                  if (!is.null(saved$progress)) { saved$progress$close(); saved$progress=NULL }
                })
                return(con)
              }
            }
          )

          #####################
          # END OF BRANCH a   #
          #####################

        })
      } else  {
        isolate({
          #######################################################
          # BRANCH b: DRAW THE MAP TO SELECT A STATION          #
          #######################################################
          # STEP 4b: SHOW REGION LIST AND MAP TO SELECT         #
          #######################################################

          output$plottype_or_region = renderUI(
                                        radioButtons("region","region:",inline=TRUE,
                                                     choices = regionnames,
                                                     selected=1))

          output$downloadplot_button <- renderUI(div())

          observers$map <<- observe({
            if (length(input$region)==1) {
              #get some values from selected region
              latmin <- myregions$latmin[as.numeric(input$region)]
              latmax <- myregions$latmax[as.numeric(input$region)]
              lonmin <- myregions$lonmin[as.numeric(input$region)]
              lonmax <- myregions$lonmax[as.numeric(input$region)]
              coastlinefile <- myregions$coastline[as.numeric(input$region)]

              # read the coastline and set invalid points to NA (they separate different islands)
              coastline <- read.csv(coastlinefile,sep=";",header=TRUE,stringsAsFactors=FALSE,strip.white=TRUE)
              coastline$latitude[abs(coastline$longitude)>180] <- NA
              coastline$longitude[abs(coastline$longitude)>180] <- NA

              # determine plot size
              latave <- (latmin+latmax)/2
              lon_lat_ratio <- cos(latave*pi/180)  # ratio between longitude and latitude distance
              x_y_ratio <- lon_lat_ratio*(lonmax-lonmin)/(latmax-latmin)
              maxwidth  = function() {isolate(min(session$clientData$output_mainPlot_width,800*x_y_ratio))}
              maxheight = function() {isolate(min(session$clientData$output_mainPlot_width/x_y_ratio,800))}

              # draw the plot
              output$mainPlot <- renderPlot(width  = maxwidth,height = maxheight,
              {
                par(mar=rep(0, 4), xpd = NA)  # no border around the plot
                # plot the coastline
                plot(coastline$longitude,coastline$latitude,"l",xlim=c(lonmin,lonmax),ylim=c(latmin,latmax),xaxs="i",yaxs="i")
                # plot the stations
                symbols(saved$stations$longitude,
                        saved$stations$latitude,
                        circles=rep((lonmax-lonmin)/200,length(saved$stations$longitude)),
                        inches=FALSE,
                        bg=rgb(saved$stations$red,saved$stations$green,saved$stations$blue),
                        add=TRUE)
              })
            }
          })

          # next observer prints the stationname if we hover over a station
          observers$maphover <<- observe({
            if (!is.null(input$plothover)) {
              # get some values from selected region
              latmin <- myregions$latmin[as.numeric(input$region)]
              latmax <- myregions$latmax[as.numeric(input$region)]
              lonmin <- myregions$lonmin[as.numeric(input$region)]
              lonmax <- myregions$lonmax[as.numeric(input$region)]
              latave <- (latmin+latmax)/2
              lon_lat_ratio <- cos(latave*pi/180)
            
              # get maximum distance in longitude and in latitude degrees
              maxdist_lon <- (lonmax-lonmin)/200
              maxdist_lat <- lon_lat_ratio * maxdist_lon

              # find a station which is close enough
              mystation <- -1
              for (i in seq(len=length(saved$stations$longitude))) {
                # determine distance to station
                dist_lon <- (saved$stations$longitude[i]-input$plothover$x)/maxdist_lon
                dist_lat <- (saved$stations$latitude[i]-input$plothover$y)/maxdist_lat
                # if inside the circle, select this station
                if ((dist_lon*dist_lon)+(dist_lat*dist_lat)<1) {
                  mystation <- i
                }
              }
              if (mystation > -1) {
                output$stationname_label <- renderUI(helpText(saved$stations$stationname[mystation]))
              } else {
                output$stationname_label <- renderUI(div())
              }
            }
          })

          # next observer selects the station if we click on it
          observers$mapclick <<- observe({
            if (!is.null(input$plotclick)) {
              # get some values from selected region
              latmin <- myregions$latmin[as.numeric(input$region)]
              latmax <- myregions$latmax[as.numeric(input$region)]
              lonmin <- myregions$lonmin[as.numeric(input$region)]
              lonmax <- myregions$lonmax[as.numeric(input$region)]
              latave <- (latmin+latmax)/2
              lon_lat_ratio <- cos(latave*pi/180)
            
              # get maximum distance in longitude and in latitude degrees
              maxdist_lon <- (lonmax-lonmin)/200
              maxdist_lat <- lon_lat_ratio * maxdist_lon

              # find a station which is close enough
              mystation <- -1
              for (i in seq(len=length(saved$stations$longitude))) {
                # determine distance to station
                dist_lon <- (saved$stations$longitude[i]-input$plotclick$x)/maxdist_lon
                dist_lat <- (saved$stations$latitude[i]-input$plotclick$y)/maxdist_lat
                # if inside the circle, select this station
                if ((dist_lon*dist_lon)+(dist_lat*dist_lat)<1) {
                  mystation <- i
                }
              }
              if (mystation > -1) {
                output$mainPlot <- renderImage({
                  list(src="./empty.png", contentType = "image/png", alt="", width=10, height=10) 
                }, deleteFile=FALSE)
                updateSelectInput(session, "stationlist", selected=mystation)
                updateCheckboxInput(session, "select_on_map", value=0)
              } 
            }
          })

          #####################
          # END OF BRANCH b   #
          #####################

        }) 
      }
    }
  },priority=10)

  observers$plottype_or_region