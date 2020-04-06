
#' @title plotNewCasesVsTotalCases
#' @description plots new cases (daily) vs total cases in log scale
#'
#' @param covid19Filtered data.frame with (filtered) data
#' @param plotPath full path for storing the plot
#' @param plotType type of plot to use. Options: c("line", "smooth")
#' @param plotCurve Curve to be plot (only one curve per plot!).
#' Options: c("detected", "deceased", "active)
#' @param xlims vector with x-coordinate plot limits
#' @param ylims vector with y-coordinate plot limits
#'
#' @export
#'
plotNewCasesVsTotalCases <- function( covid19Filtered
                                     ,plotPath
                                     ,plotType = "smooth"
                                     ,plotCurve = "detected"
                                     ,xlims = NULL
                                     ,ylims = NULL){


  p <- ggplot(covid19Filtered, aes(colour=country))

  if("line" %in% plotType){
    if("detected" %in% plotCurve){
      p <- p + geom_line(mapping = aes(y = delta.detected, x = detected, colour = country))
    }
    if("deceased" %in% plotCurve){
      p <- p + geom_line(mapping = aes(y = delta.deceased, x = deceased))
    }
    if("active" %in% plotCurve){
      p <- p + geom_line(mapping = aes(y = delta.active, x = active))
    }
  }
  if("smooth" %in% plotType){
    if("detected" %in% plotCurve){
      p <- p + geom_smooth(mapping = aes(y = delta.detected, x = detected, colour = country))
    }
    if("deceased" %in% plotCurve){
      p <- p + geom_smooth(mapping = aes(y = delta.deceased, x = deceased))
    }
    if("active" %in% plotCurve){
      p <- p + geom_smooth(mapping = aes(y = delta.active, x = active))
    }
  }
  p <- p + scale_x_log10(limits=xlims) + scale_y_log10(limits=ylims)

  p <- p + theme_bw()

  ggsave(filename = plotPath, plot = p)
}

#' @title plotTimeToDouble
#' @description plots the time to double
#'
#' @param covid19Filtered data.frame with (filtered) data
#' @param plotPath full path for storing the plot
#' @param plotType type of plot to use. Options: c("line", "smooth")
#' @param plotCurve Curve(s) to be plot. Options: c("detected", "deceased", "active)
#' @param ylims vector with y-coordinate plot limits
#' @param ncols number of columns to represent plot facets
#'
#' @export
#'
plotTimeToDouble <- function( covid19Filtered
                             ,plotPath
                             ,plotType = "smooth"
                             ,plotCurve = c("detected", "deceased")
                             ,ylims = c(0,20)
                             ,ncols = 4){

  p <- ggplot(covid19Filtered)
  p <- p + facet_wrap(country~., ncol = ncols)

  if("line" %in% plotType){
    if("detected" %in% plotCurve){
      p <- p + geom_line(mapping = aes(y = timeToDouble.detected, x = dateF), colour = "red")
    }
    if("deceased" %in% plotCurve){
      p <- p + geom_line(mapping = aes(y = timeToDouble.deceased, x = dateF), colour = "black")
    }
    if("active" %in% plotCurve){
      p <- p + geom_line(mapping = aes(y = timeToDouble.active, x = dateF), colour = "orange")
    }
  }
  if("smooth" %in% plotType){
    if("detected" %in% plotCurve){
      p <- p + geom_smooth(mapping = aes(y = timeToDouble.detected, x = dateF), colour = "red")
    }
    if("deceased" %in% plotCurve){
      p <- p + geom_smooth(mapping = aes(y = timeToDouble.deceased, x = dateF), colour = "black")
    }
    if("active" %in% plotCurve){
      p <- p + geom_smooth(mapping = aes(y = timeToDouble.active, x = dateF), colour = "orange")
    }
  }

  p <- p + theme_bw()
  p <- p + theme(axis.text.x = element_text(angle = 55, hjust = 1))
  if(!is.na(ylims)){
    p <- p + ylim(ylims)
  }
  p <- p + labs(x = "date", y = "time to double (in days)", colour = "Parameter")

  ggsave(filename = plotPath, plot = p)
}


#' @title plotDeltaDetected
#' @description plots the (daily) increase of cases
#'
#' @param covid19Filtered data.frame with (filtered) data
#' @param plotPath full path for storing the plot
#' @param plotType type of plot to use. Options: c("line", "smooth")
#' @param plotCurve Curve(s) to be plot. Options: c("detected", "deceased", "active)
#' @param ylims vector with y-coordinate plot limits
#' @param ncols number of columns to represent plot facets
#'
#' @export
#'
plotDeltaDetected <- function( covid19Filtered
                              ,plotPath
                              ,plotType = "smooth"
                              ,plotCurve = c("detected", "deceased")
                              ,ylims = NA
                              ,ncols = 4){

  p <- ggplot(covid19Filtered)
  p <- p + facet_wrap(country~., ncol = ncols)

  if("line" %in% plotType){
    if("detected" %in% plotCurve){
      p <- p + geom_line(mapping = aes(y = delta.detected, x = dateF), colour = "red")
    }
    if("deceased" %in% plotCurve){
      p <- p + geom_line(mapping = aes(y = delta.deceased, x = dateF), colour = "black")
    }
    if("active" %in% plotCurve){
      p <- p + geom_line(mapping = aes(y = delta.active, x = dateF), colour = "orange")
    }
  }
  if("smooth" %in% plotType){
    if("detected" %in% plotCurve){
      p <- p + geom_smooth(mapping = aes(y = delta.detected, x = dateF), colour = "red")
    }
    if("deceased" %in% plotCurve){
      p <- p + geom_smooth(mapping = aes(y = delta.deceased, x = dateF), colour = "black")
    }
    if("active" %in% plotCurve){
      p <- p + geom_smooth(mapping = aes(y = delta.active, x = dateF), colour = "orange")
    }
  }

  p <- p + theme_bw()
  p <- p + theme(axis.text.x = element_text(angle = 55, hjust = 1))
  if(!is.na(ylims)){
    p <- p + ylim(ylims)
  }
  p <- p + labs(x = "date", y = "delta cases", colour = "Parameter")

  ggsave(filename = plotPath, plot = p)
}

#' @title plotTotalCases
#' @description plots the total number of cases
#'
#' @param covid19Filtered data.frame with (filtered) data
#' @param plotPath full path for storing the plot
#' @param ylims vector with y-coordinate plot limits
#' @param ncols number of columns to represent plot facets
#'
#' @export
#'
plotTotalCases <- function(covid19Filtered
                           ,plotPath
                           ,ylims = NA
                           ,ncols = 4){

  q <- ggplot(covid19Filtered)
  q <- q + facet_wrap(country~.)
  if(sum(covid19Filtered$detected, na.rm = T) > 0){
    q <- q + geom_line(mapping = aes(y = detected, x = dateF), colour = "red")
  }
  if(sum(covid19Filtered$healed, na.rm = T) > 0){
    q <- q + geom_line(mapping = aes(y = healed, x = dateF), colour = "green")
  }
  if(sum(covid19Filtered$deaceased, na.rm = T) > 0){
    q <- q + geom_line(mapping = aes(y = deceased, x = dateF), colour = "black")
  }
  q <- q + theme_bw()
  q <- q + theme(axis.text.x = element_text(angle = 55, hjust = 1))
  q <- q + labs(x = "date", y = "# cases")
  if(!is.na(ylims)){
    q <- q + ylim(ylims)
  }

  ggsave(filename = plotPath, plot = q)

}
