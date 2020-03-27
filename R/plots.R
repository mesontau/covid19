
#' @title plotTimeToDouble
#' @description plots the time to double
#'
#' @param covid19Filtered data.frame with (filtered) data
#' @param plotPath full path for storing the plot
#' @param plotType type of plot to use. Options: c("line", "smooth")
#' @param ylims vector with y-coordinate plot limits
#' @param ncols number of columns to represent plot facets
#'
#' @export
#'
plotTimeToDouble <- function( covid19Filtered
                             ,plotPath
                             ,plotType = "smooth"
                             ,ylims = c(0,20)
                             ,ncols = 4){

  p <- ggplot(covid19Filtered)
  p <- p + facet_wrap(country~., ncol = ncols)

  if("line" %in% plotType){
    p <- p + geom_line(mapping = aes(y = timeToDouble.detected, x = dateF), colour = "red")
    p <- p + geom_line(mapping = aes(y = timeToDouble.deceased, x = dateF), colour = "black")
  }
  if("smooth" %in% plotType){
    p <- p + geom_smooth(mapping = aes(y = timeToDouble.detected, x = dateF), colour = "red")
    p <- p + geom_smooth(mapping = aes(y = timeToDouble.deceased, x = dateF), colour = "black")
  }

  p <- p + theme_bw()
  p <- p + theme(axis.text.x = element_text(angle = 55, hjust = 1))
  if(!is.na(ylims)){
    p <- p + ylim(ylims)
  }
  p <- p + labs(x = "date", y = "time to double (in days)", colour = "Parameter")

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
