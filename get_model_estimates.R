# For each data point in measureddata, this function gets the nearest 
# model value on the depth-time grid (without interpolation).

get_model_estimates <- function(measureddata, model, above_ground) {
  model1time <- model$time_axis
  model1depth <- model$depth_axis
  model1values <- model$data
  # determine which is the bottom cell
  bottomcell=1
  for (i in 1:length(model1depth)) {
    if (is.finite(model1values[i,1])) {bottomcell=i}
  }
  model1estimates=measureddata$value*0.0/0.0 # initialize with NaN
  for (i in (1:length(measureddata$value))) {
    #find timeindex and depthindex as closest indexes
    timeindex=min(which(abs(model1time-measureddata$date[i])==min(abs(model1time-measureddata$date[i]))))
    if (above_ground > 0) {
      depthindex=min(which(abs((model1depth[bottomcell]-model1depth)-(above_ground-measureddata$depth[i]))==
                             min(abs((model1depth[bottomcell]-model1depth)-(above_ground-measureddata$depth[i])))))
    } else {
      depthindex=min(which(abs(model1depth-measureddata$depth[i])==min(abs(model1depth-measureddata$depth[i]))))
    }  
    # however, if time index is first or last, we need to check whether measured time is before or after model range
    # in this case, next i (leave value=NaN)
    if (timeindex==1) {
      if ((model1time[timeindex]-measureddata$date[i]) > (model1time[2]-model1time[1])/2) { next }
    }
    if (timeindex==length(model1time)) {
      if ((measureddata$date[i]-model1time[timeindex]) > (model1time[timeindex]-model1time[timeindex-1])/2) { next }
    }
    # the same for depth
    if (above_ground > 0) {
      if (depthindex==1) {
        if (((above_ground-measureddata$depth[i])-(model1depth[bottomcell]-model1depth[depthindex])) > (model1depth[2]-model1depth[1])/2) {next}
      }
      if (depthindex==bottomcell) {
        if ((measureddata$depth[i]-above_ground) > (model1depth[bottomcell]-model1depth[bottomcell-1])/2) {next}
      }
    } else {
      if (depthindex==1) {
        if ((model1depth[1]-measureddata$depth[i]) > (model1depth[2]-model1depth[1])/2) {next}
      }
      if (depthindex==bottomcell) {
        if ((measureddata$depth[i]-model1depth[bottomcell]) > (model1depth[bottomcell]-model1depth[bottomcell-1])/2) {next}
      }
    }
    # otherwise (valid data point is chosen), put nearest model value to model1estimates array
    model1estimates[i]=model1values[depthindex,timeindex]
  }  
  return(model1estimates)
}
