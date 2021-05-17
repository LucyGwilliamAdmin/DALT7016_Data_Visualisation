create_dataframe<-function(xlsx_file){
  # get sheet names
  year<-str_replace(tail(strsplit(xlsx_file, " ")[[1]], n=1), ".xlsx", "")
  sheet_names<-xlsx_sheet_names(xlsx_file)
  if (substr(strsplit(xlsx_file, " Table ")[[1]][1], 1, 5)=="/PROV"){
    file_breakdown<-strsplit(strsplit(xlsx_file, " Table ")[[1]][1], "/PROV - ")[[1]][2]
  }
  else {
    file_breakdown<-strsplit(gsub("/", "", xlsx_file), " Table ")[[1]][1]
  }
  df<-data.frame()
  # loop through sheets
  for (sheet_name in sheet_names) {
    if (sheet_name=="Notes") {}
    else {
      tidyxl_data<-xlsx_cells(xlsx_file, sheets=sheet_name)
      tidyxl_format<-xlsx_formats(xlsx_file)
      acceptable<-tidyxl_data[tidyxl_data$local_format_id %in%
                                which(tidyxl_format$local$fill$patternFill$fgColor$rgb == "FF33CCCC"), c("address")] %>%
        mutate(comment="acceptable")
      reasonably_precise<-tidyxl_data[tidyxl_data$local_format_id %in%
                                        which(tidyxl_format$local$fill$patternFill$bgColor$rgb == "FF00FFFF"), c("address")] %>%
        mutate(comment="reasonably precise")
      comment<-bind_rows(acceptable, reasonably_precise)
      readxl_data<-read_xlsx(xlsx_file, sheet=sheet_name, trim_ws=F, col_names = FALSE, .name_repair = ~ ifelse(nzchar(.x), .x, LETTERS[seq_along(.x)]))
      readxl_data<-readxl_data %>%
        select(A, D) %>%
        tibble::rownames_to_column(var="rowname") %>%
        filter(complete.cases(.)) %>%
        mutate(address=paste0("D",rowname)) %>%
        tibble::column_to_rownames("rowname") %>%
        left_join(comment) %>%
        select(-address) %>%
        rename(!!file_breakdown :=A) %>%
        rename(Value=D) %>%
        slice(-1) %>%
        mutate(Year=year) %>%
        mutate(Units="Median")
      readxl_data<-add_columns(readxl_data, sheet_name)
      readxl_data[readxl_data$comment=="acceptable" & readxl_data$Value=="x","comment"]<-"unacceptable"
      df<-bind_rows(df, readxl_data)
    }
  }
  return(df)
}

add_columns<-function(readxl_data, sheet_name){
  if (sheet_name=="Male"){
    readxl_data<-readxl_data %>%
      mutate(Sex="Male")
  }
  if (sheet_name=="Female"){
    readxl_data<-readxl_data %>%
      mutate(Sex="Female")
  }
  if (sheet_name=="Full-Time"){
    readxl_data<-readxl_data %>%
      mutate(WorkingPattern="Full-Time")
  }
  if (sheet_name=="Part-Time"){
    readxl_data<-readxl_data %>%
      mutate(WorkingPattern="Part-Time")
  }
  if (sheet_name=="Male Full-Time"){
    readxl_data<-readxl_data %>%
      mutate(Sex="Male") %>%
      mutate(WorkingPattern="Full-Time")
  }
  if (sheet_name=="Male Part-Time"){
    readxl_data<-readxl_data %>%
      mutate(Sex="Male") %>%
      mutate(WorkingPattern="Part-Time")
  }
  if (sheet_name=="Female Full-Time"){
    readxl_data<-readxl_data %>%
      mutate(Sex="Female") %>%
      mutate(WorkingPattern="Full-Time")
  }
  if (sheet_name=="Female Part-Time"){
    readxl_data<-readxl_data %>%
      mutate(Sex="Female") %>%
      mutate(WorkingPattern="Part-Time")
  }
  return(readxl_data)
}


download_zips<-function(dataframe, years) {
  urlstart<-"https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/"
  for (i in 1:nrow(dataframe)) {
    tablename<-ashetables[i, "tablename"]
    tablenumber<-ashetables[i, "tablenumber"]
    for (year in years) {
      if (year==2020) {
        type<-"provisional"
      }
      else {
        type<-"revised"
      }
      if (ashetables[i, "tablename"]=="industry2digitsic" & year > 2017) {
        url<-paste0(urlstart,tablename,"ashetable",tablenumber,"/",year,type,"/sic2007table",tablenumber,year,type,".zip")
      }
      else {
        url<-paste0(urlstart,tablename,"ashetable",tablenumber,"/",year,type,"/table",tablenumber,year,type,".zip")
      }
      tempfile <- tempfile()
      download.file(url, tempfile)
      unzipped_files<-unzip(tempfile, list=TRUE)
      unzip(zipfile=tempfile, files = unzipped_files[grep(".7a", unzipped_files$Name), ][["Name"]])
      unlink(tempfile)
    }
  }
}