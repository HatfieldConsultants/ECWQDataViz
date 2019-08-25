hat_col_list <- function(){
  #' @importFrom grDevices rgb
  #' @export
  #Hatfield approved colours, no transparency (200)
  hat_darkgreen   <- rgb(0,  113,97, 200, max = 255)
  hat_lightgreen  <- rgb(114,196,185,200, max = 255)
  hat_blue        <- rgb(47, 96, 175,200, max = 255)
  hat_orange      <- rgb(255,114,0,  200, max = 255)
  hat_lightorange <- rgb(255,168,97, 200, max = 255)
  hat_grey        <- rgb(149,161,170,255, max = 255)
  hat_yellow      <- rgb(255,255,20,200,  max = 255)
  hat_col = list(hat_darkgreen,hat_lightgreen,hat_blue,hat_orange,hat_lightorange,hat_grey,hat_yellow)
  names(hat_col) <- c("hat_darkgreen", "hat_lightgreen", "hat_blue", "hat_orange", "hat_lightorange", "hat_grey", "hat_yellow")
  
  hat_col
}


CreatePlotPerAnalyte <- function(gp_plot_data){
  #' @title CreatePlotPerAnalyte
  #' @description Creates plots for each analyte
  #' @import scales
  #' @import ggplot2
  #' @importFrom dplyr select filter ungroup group_by
  #' @importFrom tidyr gather
  #' @importFrom RColorBrewer brewer.pal
  #' @return list of plots for each analyte
  #' @export
  analyte_plots <- NULL
  
  ###sets values for linetype and color so that linetype and color remains the same throughout all the plots
  ##this is needed for a common legend
  for (an in unlist(unique(gp_plot_data$Analyte))){
    count_unit <- gp_plot_data[gp_plot_data$Analyte==an,'Unit'] %>% unique() %>% length
    if(count_unit!=1){
      message <- sprintf("More than one unit for analyte %s. Please check data",an)
      stop(message)
    }
    
    title=an
    unit <- gp_plot_data[gp_plot_data$Analyte==an,'Unit'] %>% unique() %>% as.character()
    if(unit==""){ylabel=an} else{ ylabel=paste0(an,'\n (',unit,')')}
    
    analyte_data <- gp_plot_data %>%
      filter(Analyte == an)
    minDate <- lubridate::ymd(format(min(analyte_data$Sample.time),format="%Y-%m-01")) %>% 
                as.character() %>%
                as.POSIXct(tz="")  
    
    maxDateFOM <- lubridate::ymd(format(max(analyte_data$Sample.time),format="%Y-%m-01"))
    maxDate <- lubridate::ceiling_date(maxDateFOM+30) %>%
                as.character() %>%
                as.POSIXct(tz="")
    
    if(minDate==maxDate){
      minDate <- analyte_data$Sample.time
      maxDate <- analyte_data$Sample.time
      lims <- c(minDate,maxDate)} else {
        lims <- c(minDate,maxDate)
      }
    timeLapse <- maxDate- minDate
    date_fm <- ifelse(timeLapse <= 31, "%d-%m-%Y",
                          ifelse(timeLapse <= 365, "%m-%Y",
                                 ifelse(timeLapse <= (365*5),"%m-%Y","%Y")))
  
    date_bk <- ifelse(timeLapse <= 31, "1 week",
                     ifelse(timeLapse <= 365, "3 months",
                            ifelse(timeLapse <= (365*5),"4 months","1 year")))
    
    pl <- 
       analyte_data %>%
      group_by(Analyte, Sample.time) %>%
      ggplot(aes(x = Sample.time, y = Value))+
      geom_point(aes(fill="Observation"),size=2,shape=21) +
      scale_fill_manual(values="powderblue",drop=FALSE) +
      geom_line(aes(x = Sample.time, y = Value),color='black', linetype = 'dashed') +
      guides(fill = guide_legend(override.aes = list(shape = 21))) +#enables proper colour symbol filling in legend
      xlab           ("") +
      ylab           (ylabel) +
      ggtitle(title) +
      scale_x_datetime(labels = date_format(date_fm),limits=lims,
                       breaks=date_breaks(date_bk))
    
    if(an != "pH"){
      pl <- pl +  expand_limits(y=0)
    } else {
      pl <- pl + expand_limits(y=5)
    }
  
    #### plot_formatting####
    hat_col <- hat_col_list()
    hat_lightgreen <- hat_col[["hat_lightgreen"]]
    
    
    pl <-  pl +
      theme_bw()+
      theme(legend.key.width = unit(1,"cm")) +
      theme(plot.title = element_text(size=10,hjust=0.5, vjust = 0.2,
                                      colour = rgb(0,  113,97, 255, max = 255))) +
      theme(plot.margin = unit(c(0,1,0,1), "cm")) +
      theme(plot.caption = element_text(hjust = 0.5, vjust = 0.9)) +
      theme(legend.position='bottom') +
      theme(legend.text=element_text(size=10),legend.spacing.x = unit(0.3, 'cm')) +
      theme(legend.title = element_blank()) +
      theme(strip.background =element_rect(fill=hat_lightgreen)) +
      theme(strip.text = element_text(colour = 'black')) +
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      guides(fill=guide_legend(nrow=2,byrow=TRUE),linetype=guide_legend(nrow=2,byrow=TRUE))
    
    analyte_plots[[an]] <-  pl
  }
  return(analyte_plots)
}

CombineAnalytePlots <- function(analytePlots){
  #' @title CombineAnalytePlots
  #' @description Combines the plots resulting from CreatePlotPerAnalyte
  #' @param plot_data with columns Analyte, DateTime (Datetime must be POSIXct (format = "%b-%d-%Y")),plot_sym, year, Value, Guideline, minGuideline, maxGuideline,
  #' @return facet plot for all analytes, important date (e.g start of mining operations) is shown as a vertical line.
  #' @export
  #' @importFrom dplyr filter
  #' @importFrom ggpubr ggarrange
  #' @importFrom RColorBrewer brewer.pal
  plotList <- analytePlots
  
  if(length(plotList)< 2){n_col = length(plotList)} else {n_col= 2}
  n_row=ceiling(length(plotList)/n_col)
  ggarrange(plotlist=plotList,common.legend = TRUE,legend='bottom',ncol=n_col,nrow=n_row)
  
}

plot_wq_data <- function(dataLong){
  #' @import ggplot2
  #' @import ggpubr
  #' @export
  library(ggplot2)
  library(ggpubr)
  library(scales)
  analytePlots <- CreatePlotPerAnalyte(dataLong)
  
  CombineAnalytePlots(analytePlots)
  
  
  
}
