numExtract <- function(string){
  str_extract(string,("\\.\\d\\."))
}


format_wq_data <- function(data){
#' @description This function formats data from the Environment Canada Freshwater Quality Monitoring and Surveillance - Online Data for further data wrangling and visualization in R.
#' @param data is a data.frame or tibble from the Environment Canada Freshwater Quality Monitoring and Surveillance - Online Data.   
#' @import dplyr
#' @import stringr
#' @import tidyr   
  cols <- c("Status", "Value.modifier.code","Unit.code")
  
  idCols <- c("Sample.time","Sample.number","Sample.type",names(data)[(grepl(paste(cols,collapse="|"),names(data)))])
  
  ##replaces the number in the id columns by the analyte name
  analyteCols <- names(data)[!names(data) %in% idCols]
  for(i in 1:length(analyteCols)){
    nm <- analyteCols[i]
    colStart <- which(names(data)==nm) + 1
    if(i != length(analyteCols)){
    nmNext <- analyteCols[1+i]
    colEnd <- which(names(data)==nmNext) - 1} else{
      colEnd <- ncol(data)
    }
    names(data)[colStart:colEnd] <- str_replace(names(data)[colStart:colEnd],"\\.\\d\\.",paste0("|",nm))
  }
  
  
  ###gathers into a long format
  dataLong <- data %>% gather(Descrip,Value,-c("Sample.time","Sample.number","Sample.type"))
  
  #adds the word analyte to the row that represents the analytes
  dataLong <- 
    dataLong %>%
              rowwise() %>%
              mutate(Descrip=ifelse(!grepl("|",Descrip,fixed=T),
                                    paste0("Concentration|",Descrip),Descrip))
  ##makes an analyte column
  dataLong <- dataLong %>%
              separate(Descrip,c("header","Analyte"),sep="\\|",remove=T)
  
  ####extract the concentration data
  concentrationValues <- dataLong %>% filter(header=="Concentration") %>% select(-header)
  
  #remove the concentration rows before speading metadata
  dataLong <- dataLong %>% filter(header!="Concentration") %>% spread(header,Value)
  
  #merge the concentration data
  dataLong <- dataLong %>% full_join(concentrationValues)
  
  ##split analyte Name from labname and VMV
  dataLong <- dataLong %>%
    mutate(Analyte=str_replace(Analyte,"\\..Lab..","|Lab|")) %>%
    mutate(Analyte=str_replace(Analyte,"\\...VMV..","|VMV|")) %>%
    separate(Analyte,c("Analyte", "LabLabel", "LabNum", "VMVLabel", "VMV"),sep="\\|") %>%
    select(-LabLabel,-VMVLabel)
  
  
  ##remove rows for which there is no data
  dataLong <- dataLong %>%
              mutate_at(vars(Status,Unit.code,Value.modifier.code,Value),
                         function(x)ifelse(x==""|is.na(x),NA,x)) %>%
              mutate(Value=as.numeric(Value)) %>%
              filter(!is.na(Value)) 
  
  ###format date
  dataLong <- dataLong %>% 
              mutate(Sample.time=as.POSIXct(Sample.time,format="%Y-%m-%d %H:%M:%OS"))%>%
              rename(Unit=Unit.code)
  
  dataLong

}
