library(shiny)

# Define server logic
shinyServer(function(input, output, session) {

  #################################################################################
  # STEP 1: CREATE GLOBAL VARIABLES WHICH MAY BE READ OR WRITTEN BY THE OBSERVERS #
  #################################################################################

  runs_in_private <- TRUE

  go_to_page <- function(newpage, observers=list()) {
    # destroy all observers on current page
    for (i in seq(len=length(observers))) {
      if (!is.null(observers[[i]])) {observers[[i]]$destroy()}
    }
    # go to new page
    saved$selected_page <<- newpage
  }

  saved = reactiveValues(stations=NULL,
                         variables=NULL,
                         datasetpath="",
                         measurements_format="standard",
                         models=NULL,
                         modelnames=NULL,
                         extmodelnames=NULL,
                         process=list(),
                         progress=NULL,
                         selected_page=ifelse(runs_in_private,"mainpage","passwordpage"),
                         plottime=Sys.time(),
                         plottype=NULL,
                         plotoptions_profile=NULL,
                         plotoptions_scatter=NULL,
                         plotoptions_taylor=NULL,
                         plotoptions_trend=NULL) 

  #################################################################################################
  # STEP 2: CREATE A MULTI-PAGE APPLICATION WHICH SWITCHES THE PAGE IF saved$selected_page IS SET #
  #################################################################################################
  # in this case, we have two pages: "passwordpage" and "mainpage"

  observe({
    source(paste0("ui_",saved$selected_page,".R"),local=environment())
  })

  ####################################
  # STEP 3: HANDLE THE SELECTED PAGE #
  ####################################
  observe({
    r_script_for_page <- paste0("server_",saved$selected_page,".R")  
    isolate({
      source(r_script_for_page,local=environment())
    })
  })

})