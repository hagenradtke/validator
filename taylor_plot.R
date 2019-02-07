source("get_model_estimates.R")

taylor.diagram<-function(ref,model,add=FALSE,col="red",pch=19,pos.cor = TRUE, 
                         xlab = "", ylab = "", main = "Taylor Diagram", show.gamma = TRUE, 
                         ngamma = 3, gamma.col = 8, sd.arcs = 0, ref.sd = FALSE, sd.method = "sample", 
                         grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9), pcex = 1, cex.axis = 1, 
                         normalize = FALSE, mar = c(5, 4, 6, 6), center_observation=FALSE, ...) {
  # this function was taken from the "plotrix" package, but extended by the parameter center_observation, 
  # setting this parameter to TRUE means the observations are drawn in the center of the x axis
  require("plotrix")
  grad.corr.full <- c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99, 
                      1)
  # convert any list elements or data frames to vectors
  R <- cor(ref, model, use = "pairwise")
  if (is.list(ref)) 
    ref <- unlist(ref)
  if (is.list(model)) 
    ref <- unlist(model)
  SD <- function(x, subn) {
    meanx <- mean(x, na.rm = TRUE)
    devx <- x - meanx
    ssd <- sqrt(sum(devx * devx, na.rm = TRUE)/(length(x[!is.na(x)]) - 
                                                  subn))
    return(ssd)
  }
  subn <- sd.method != "sample"
  sd.r <- SD(ref, subn)
  sd.f <- SD(model, subn)
  if (normalize) {
    sd.f <- sd.f/sd.r
    sd.r <- 1
  }
  if (center_observation == FALSE) {
    maxsd <- 1.5 * max(sd.f, sd.r)
  } else {
    maxsd <- 2.0 * sd.r
  }
  oldpar <- par("mar", "xpd", "xaxs", "yaxs")
  if (!add) {
    # display the diagram
    if (pos.cor) {
      if (nchar(ylab) == 0) 
        if (normalize == TRUE) {
          ylab = "SD model / SD measurements"
        } else {
          ylab = "Standard deviation"
        }
      par(mar = mar)
      plot(0, xlim = c(0, maxsd), ylim = c(0, maxsd), xaxs = "i", 
           yaxs = "i", axes = FALSE, main = main, xlab = xlab, 
           ylab = ylab, type = "n", cex = cex.axis, ...)
      if (grad.corr.lines[1]) {
        for (gcl in grad.corr.lines) lines(c(0, maxsd * 
                                               gcl), c(0, maxsd * sqrt(1 - gcl^2)), lty = 3)
      }
      # add the axes
      segments(c(0, 0), c(0, 0), c(0, maxsd), c(maxsd, 
                                                0))
      axis.ticks <- pretty(c(0, maxsd))
      axis.ticks <- axis.ticks[axis.ticks <= maxsd]
      axis(1, at = axis.ticks, cex.axis = cex.axis)
      axis(2, at = axis.ticks, cex.axis = cex.axis)
      if (sd.arcs[1]) {
        if (length(sd.arcs) == 1) 
          sd.arcs <- axis.ticks
        for (sdarc in sd.arcs) {
          xcurve <- cos(seq(0, pi/2, by = 0.03)) * sdarc
          ycurve <- sin(seq(0, pi/2, by = 0.03)) * sdarc
          lines(xcurve, ycurve, col = "black", lty = 3)
        }
      }
      if (show.gamma[1]) {
        # if the user has passed a set of gamma values, use that
        if (length(show.gamma) > 1) gamma <- show.gamma
        # otherwise make up a set
        else gamma <- pretty(c(0, maxsd), n = ngamma)[-1]
        if (gamma[length(gamma)] > maxsd) 
          gamma <- gamma[-length(gamma)]
        labelpos <- seq(45, 70, length.out = length(gamma))
        # do the gamma curves
        for (gindex in 1:length(gamma)) {
          xcurve <- cos(seq(0, pi, by = 0.03)) * gamma[gindex] + sd.r
          # find where to clip the curves
          endcurve <- which(xcurve < 0)
          endcurve <- ifelse(length(endcurve), min(endcurve) - 
                               1, 105)
          ycurve <- sin(seq(0, pi, by = 0.03)) * gamma[gindex]
          maxcurve <- xcurve * xcurve + ycurve * ycurve
          startcurve <- which(maxcurve > maxsd * maxsd)
          startcurve <- ifelse(length(startcurve), max(startcurve) + 
                                 1, 0)
          lines(xcurve[startcurve:endcurve], ycurve[startcurve:endcurve], 
                col = gamma.col)
          if (xcurve[labelpos[gindex]] > 0) 
            boxed.labels(xcurve[labelpos[gindex]], ycurve[labelpos[gindex]], 
                         gamma[gindex], border = FALSE)
        }
      }
      # the outer curve for correlation
      xcurve <- cos(seq(0, pi/2, by = 0.01)) * maxsd
      ycurve <- sin(seq(0, pi/2, by = 0.01)) * maxsd
      lines(xcurve, ycurve)
      bigtickangles <- acos(seq(0.1, 0.9, by = 0.1))
      medtickangles <- acos(seq(0.05, 0.95, by = 0.1))
      smltickangles <- acos(seq(0.91, 0.99, by = 0.01))
      segments(cos(bigtickangles) * maxsd, sin(bigtickangles) * 
                 maxsd, cos(bigtickangles) * 0.97 * maxsd, sin(bigtickangles) * 
                 0.97 * maxsd)
      par(xpd = TRUE)
      # the inner curve for reference SD
      if (ref.sd) {
        xcurve <- cos(seq(0, pi/2, by = 0.01)) * sd.r
        ycurve <- sin(seq(0, pi/2, by = 0.01)) * sd.r
        lines(xcurve, ycurve)
      }
      points(sd.r, 0, cex = pcex)
      text(cos(c(bigtickangles, acos(c(0.95, 0.99)))) * 
             1.05 * maxsd, sin(c(bigtickangles, acos(c(0.95, 
                                                       0.99)))) * 1.05 * maxsd, c(seq(0.1, 0.9, by = 0.1), 
                                                                                  0.95, 0.99))
      text(maxsd * 0.8, maxsd * 0.8, "Correlation", srt = 315)
      segments(cos(medtickangles) * maxsd, sin(medtickangles) * 
                 maxsd, cos(medtickangles) * 0.98 * maxsd, sin(medtickangles) * 
                 0.98 * maxsd)
      segments(cos(smltickangles) * maxsd, sin(smltickangles) * 
                 maxsd, cos(smltickangles) * 0.99 * maxsd, sin(smltickangles) * 
                 0.99 * maxsd)
    }
    else {
      x <- ref
      y <- model
      R <- cor(x, y, use = "pairwise.complete.obs")
      E <- mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)
      xprime <- x - mean(x, na.rm = TRUE)
      yprime <- y - mean(y, na.rm = TRUE)
      sumofsquares <- (xprime - yprime)^2
      Eprime <- sqrt(sum(sumofsquares)/length(complete.cases(x)))
      E2 <- E^2 + Eprime^2
      if (add == FALSE) {
        # pourtour du diagramme (display the diagram)
        maxray <- 1.5 * max(sd.f, sd.r)
        plot(c(-maxray, maxray), c(0, maxray), type = "n", 
             asp = 1, bty = "n", xaxt = "n", yaxt = "n", 
             xlab = xlab, ylab = ylab, main = main, cex = cex.axis)
        discrete <- seq(180, 0, by = -1)
        listepoints <- NULL
        for (i in discrete) {
          listepoints <- cbind(listepoints, maxray * 
                                 cos(i * pi/180), maxray * sin(i * pi/180))
        }
        listepoints <- matrix(listepoints, 2, length(listepoints)/2)
        listepoints <- t(listepoints)
        lines(listepoints[, 1], listepoints[, 2])
        # axes x,y
        lines(c(-maxray, maxray), c(0, 0))
        lines(c(0, 0), c(0, maxray))
        # lignes radiales jusque R = +/- 0.8
        for (i in grad.corr.lines) {
          lines(c(0, maxray * i), c(0, maxray * sqrt(1 - 
                                                       i^2)), lty = 3)
          lines(c(0, -maxray * i), c(0, maxray * sqrt(1 - 
                                                        i^2)), lty = 3)
        }
        # texte radial
        for (i in grad.corr.full) {
          text(1.05 * maxray * i, 1.05 * maxray * sqrt(1 - 
                                                         i^2), i, cex = 0.6)
          text(-1.05 * maxray * i, 1.05 * maxray * sqrt(1 - 
                                                          i^2), -i, cex = 0.6)
        }
        # sd concentriques autour de la reference
        seq.sd <- seq.int(0, 2 * maxray, by = (maxray/10))[-1]
        for (i in seq.sd) {
          xcircle <- sd.r + (cos(discrete * pi/180) * 
                               i)
          ycircle <- sin(discrete * pi/180) * i
          for (j in 1:length(xcircle)) {
            if ((xcircle[j]^2 + ycircle[j]^2) < (maxray^2)) {
              points(xcircle[j], ycircle[j], col = "darkgreen", 
                     pch = ".")
              if (j == 10) 
                text(xcircle[j], ycircle[j], signif(i, 
                                                    2), cex = 0.5, col = "darkgreen")
            }
          }
        }
        # sd concentriques autour de l'origine
        seq.sd <- seq.int(0, maxray, length.out = 5)
        for (i in seq.sd) {
          xcircle <- (cos(discrete * pi/180) * i)
          ycircle <- sin(discrete * pi/180) * i
          if (i) 
            lines(xcircle, ycircle, lty = 3, col = "black")
          text(min(xcircle), -0.03 * maxray, signif(i, 
                                                    2), cex = 0.5, col = "black")
          text(max(xcircle), -0.03 * maxray, signif(i, 
                                                    2), cex = 0.5, col = "black")
        }
        text(0, -0.08 * maxray, "Standard Deviation", 
             cex = 0.7, col = "blue")
        text(0, -0.12 * maxray, "Centered RMS Difference", 
             cex = 0.7, col = "darkgreen")
        points(sd.r, 0, pch = 22, bg = "darkgreen", cex = 1.1)
        text(0, 1.1 * maxray, "Correlation Coefficient", 
             cex = 0.7)
      }
      S <- (2 * (1 + R))/(sd.f + (1/sd.f))^2
      #   Taylor<-S
    }
  }
  # display the points
  points(sd.f * R, sd.f * sin(acos(R)), pch = pch, col = col, 
         cex = pcex)
  invisible(oldpar)
}

taylor_plot <- function(
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


  # STEP 3: DO THE PLOTTING
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
  if (plot_options$center_observation) {
    if (plot_options$normalize_sd) {
      resu <- taylor.diagram(measureddata$value,model1estimates,col="blue",ref.sd=TRUE,
                     center_observation=plot_options$center_observation,
                     normalize=plot_options$normalize_sd,
                     sd.arcs=c(0.5,1.5),main="")
    } else {
      resu <- taylor.diagram(measureddata$value,model1estimates,col="blue",ref.sd=TRUE,
                     center_observation=plot_options$center_observation,
                     normalize=plot_options$normalize_sd,
                     sd.arcs=c(sd(measureddata$value)*0.5,sd(measureddata$value)*1.5),main="")    
    }
  } else {
    resu <- taylor.diagram(measureddata$value,model1estimates,col="blue",ref.sd=TRUE,
                   center_observation=plot_options$center_observation,
                   normalize=plot_options$normalize_sd,
                   sd.arcs=1,main="")
  }
  if (!is.null(model2)) {
    resu <- taylor.diagram(measureddata$value,model2estimates,col="red",add=TRUE,normalize=plot_options$normalize_sd)
  }
  if (!is.null(model3)) {
    resu <- taylor.diagram(measureddata$value,model3estimates,col="green",add=TRUE,normalize=plot_options$normalize_sd)
  }
  if (!is.null(model4)) {
    resu <- taylor.diagram(measureddata$value,model4estimates,col="magenta",add=TRUE,normalize=plot_options$normalize_sd)
  }
  if (!is.null(output)) {
    dev.off()
  }
  return(resu)
}