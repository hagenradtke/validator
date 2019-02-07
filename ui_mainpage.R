output$selected_page <- renderUI(fluidRow(column(12,

  ###############
  # HEADER LINE #
  ###############
  fluidRow(column(12,style = "background-color:#003a6d; color:#ffffff;",

    # Application title
        headerPanel("Interactive (multi-)model validation app")
  )),

  fluidRow(
    ###############
    # LEFT COLUMN #
    ###############
    column(3,style = "background-color:#e2e6ed;",
      # data set and station selection
      uiOutput("datasetlist"),   # drop-down list of datasets
      fluidRow(column(6,
                      uiOutput("stationlist")  # drop-down list of stations
                     ),
               column(6,
                      uiOutput("map_checkbox") # checkbox to select station on the map
                     )),   
      uiOutput("stationcoords"), # label with station coordinates

      # variable selection
      fluidRow(style = "background-color:#f1f3f6;",  # checkbox list of variables in dataset
               column(12,
                      uiOutput("variablelist")   
                     )),

      # model selection
      uiOutput("model1list",style = "background-color:#bbbbff;"), # model 1 drop down box
      uiOutput("model2list",style = "background-color:#ffbbbb;"), # model 2 drop down box
      uiOutput("model3list",style = "background-color:#bbffbb;"), # model 3 drop down box
      uiOutput("model4list",style = "background-color:#ffbbff;"), # model 4 drop down box
      # advanced model selection
      uiOutput("models_advanced"),
      fluidRow(width="100%",column(2),column(10,
        fluidRow(column(12,
                       uiOutput("models_advanced_options")
        )),
        fluidRow(column(12,
                       uiOutput("upload_model_list")
        )),
        fluidRow(column(12,
                       uiOutput("download_model_list")
        ))
      )),

      # time range selection
      fluidRow(style = "background-color:#f1f3f6;",
               column(8,
                       uiOutput("timerange")
                     ),
               column(4,
                       uiOutput("timeminmax")
                     )
      ),
      fluidRow(style = "background-color:#f1f3f6;",
               column(6,
                       uiOutput("fromtime")
                     ),
               column(6,
                       uiOutput("totime")
                     )
      ),

      # month range selection
      fluidRow(column(12,
                       uiOutput("monthrange")
                     )
      ),
      fluidRow(column(6,
                       uiOutput("frommonth")
                     ),
               column(6,
                       uiOutput("tomonth")
                     )
      ),

      # depth range selection
      fluidRow(style = "background-color:#f1f3f6;",
               column(12,
                       uiOutput("depthrange")
                     )
              ),
      fluidRow(style = "background-color:#f1f3f6;",
               column(6,
                       uiOutput("fromdepth")
                     ),
               column(6,
                       uiOutput("todepth")
                     )
      ),
      fluidRow(style = "background-color:#f1f3f6;",
               column(12,
                      uiOutput("depthabovebottom")
                     )
      )
    ),
    ################
    # RIGHT COLUMN #
    ################
    column(9,
      # Plot type selection, or region selection in case of "select on map" clicked
      fluidRow(column(12,style = "background-color:#e2e6ed;",
                      uiOutput("plottype_or_region")
                     )
      ),

      # different possible plot option boxes
      fluidRow(column(12,style = "background-color:#e2e6ed;",
                      uiOutput("plotoptions_profile")
                     )
      ),
      fluidRow(column(12,style = "background-color:#e2e6ed;",
                      uiOutput("plotoptions_scatter")
                     )
      ),
      fluidRow(column(12,style = "background-color:#e2e6ed;",
                      uiOutput("plotoptions_taylor")
                     )
      ),
      fluidRow(column(12,style = "background-color:#e2e6ed;",
                      uiOutput("plotoptions_trend")
                     )
      ),



    # plot area
      # the plot itself, or the map of stations
      fluidRow(column(12,
        plotOutput("mainPlot",width="auto",height="auto",
                   hover=hoverOpts(id="plothover",delay=100),
                   click=clickOpts(id="plotclick")
                  )
      )),

      # the line below the plot
      fluidRow(column(10,
                 # space for station name if map is chosen
                 uiOutput("stationname_label")
               ),
               column(2,
                 # save button
                 uiOutput("downloadplot_button")
               ))
    )
  )
)))