require(plyr)
require(dplyr)
require(lazyeval)
require(knitr)
require(rmarkdown)

get.today <- function(format = meta$date.format){
  #' A function which return today's date in the requested format. Default format: 02Apr16
  #' @param string. Format to use on today's date
  return(format(Sys.Date(), format = format))
}


save.report <- function(source.file, output.file, file.format = "html",
                        source.folder = dirs$reports$source,
                        output.folder = dirs$reports$today_report,
                        copy.to.current = TRUE,
                        current.folder = dirs$reports$current_report,
                        today.folder = dirs$reports$today_report,
                        date.format = meta$date.format){
  
  #' A function to save a report with some additional checks. Namely, it checks whether the folder
  #' listed as today's folder actually corresponds to today's date; if it doesn't, the function stops.
  #'
  #' @param source.file character. Name of the source .Rmd file. Must include file extension
  #' @param output.file character. Name of the output file. Date will be added to it automatically.
  #' @param file.format character. Can be "html" or "pdf"
  #' @param source.folder character. Folder containing the source file. By default, the folder from standard
  #'                file structure, created by my project initializing script.
  #' @param output.folder character. Folder for output files. By default - folder with today's date as name.
  #'                If this folder doesn't exist, it will be created.
  #' @param copy.to.current logical. If TRUE, the output file will be also copied to "current" folder, overwriting
  #'                any previous file with that name. If "current" folder doesn't exist, it will be created.
  #' @param current.folder character. Name of the "current" folder.
  #' @param today.folder character. Name of the folder with today's date as name
  #' @param date.format character. String specifying date.format, check Internet for acceptable formats. Used to check
  #'              whether today's folder corresponds to today's date; also used in the name of the output file.
  #' @return Name of the generated report (with extension)
  
  # save current wd
  save.wd <- getwd()
  
  # This monstrosity gets the name of the folder which is supposed to correspond to today
  today.str <- strsplit(output.folder,"/")[[1]][length(strsplit(output.folder,"/")[[1]])]
  
  # Make sure that today's fodler is actually today's, and not, say, yesterday's
  if (as.Date(today.str, format = date.format) != Sys.Date())
    stop ("The variable `today folder` contains a date which does not correspond to today's date.
          Change the variable.")
  
  # If today's folder doesn't exist, create it
  if (!file.exists(today.folder)){
    message(paste("The folder", today.folder,"doesn't exist. Creating it"))
    dir.create(today.folder)
  }
  
  # This is the name of the file after we add current date to it
  full.output.file.name <- paste0(output.file, "_", get.today(date.format))
  
  
  setwd(output.folder)
  # here we select whether the file will be an html file or a pdf file
  switch(file.format,
         html = {render(input = file.path(source.folder, source.file),
                        output_dir = output.folder,
                        output_file = paste0(full.output.file.name,".html"),
                        output_format = "html_document")
           extension <<- ".html"},
         pdf = {render(input = file.path(source.folder, source.file),
                       output_dir = output.folder,
                       output_file = paste0(full.output.file.name,".pdf"),
                       output_format = "pdf_document")
           extension <<- ".pdf"},
         stop("Incorrect file format! Choose html or pdf."))
  
  # if we want the most current report to be copied to current folder
  if (copy.to.current){
    # make sure that this folder exists
    if (!file.exists(current.folder)){
      dir.create(current.folder)
    }
    
    setwd(current.folder)
    
    # find for a previous file with the same name in the current folder. Of course,
    # we don't take date into account
    previous.file <- list.files(pattern = output.file)
    # remove the older file
    file.remove(previous.file)
    # cope the newer file
    file.copy(file.path(output.folder, paste0(full.output.file.name,extension)),
              paste0(full.output.file.name,extension),
              overwrite = TRUE)
    
    # get back to the wd we started at
    setwd(save.wd)
  }
  
  return(paste0(full.output.file.name,extension))
}


prepare_named_report <- function(proj.name, tags, values, tag.delim = ".",
                                 component.delim = "_", data.type,
                                 source.file, template.file,
                                 file.format = "html",
                                 source.folder = dirs$reports$source,
                                 output.folder = dirs$reports$today_report,
                                 template.folder = dirs$reports$templates,
                                 overwrite.source = c("ask","yes","no"),
                                 copy.to.current = TRUE,
                                 open.source.file = TRUE,
                                 compile.report = TRUE,
                                 open.compiled = TRUE,
                                 wait.to.recompile = TRUE,
                                 current.folder = dirs$reports$current_report,
                                 today.folder = dirs$reports$today_report,
                                 date.format = meta$date.format){
  
  
  #' Save a knitted report named in accordance with the current naming scheme, 
  #' based on a report template.
  #' 
  #' The function assumes that you have a report template, i.e. an .Rmd file 
  #' with the layout and the things which have to be output (tables, plots etc),
  #' but without specific analyses or comments. Then it turns this template into
  #' an actual report, where you can insert those specific comments. It works 
  #' like this:
  #' 
  #' 1) Copy the template file from \code{template.folder} to
  #' \code{source.folder} (it is assumed that the source folder keeps the source
  #' .Rmd files with specific comments). 2) Name this new source file according
  #' to naming schema currently in use ( see \code{naming_functions.R} in
  #' \code{ettools} package for more details on naming schemas). Also see help
  #' for \code{make_filename} in \code{ettools} package on more details on the
  #' parameters needed to make use of a naming schema. 3) If a source file with
  #' this name exists, ask whether to keep it or to replace it. You have to type
  #' "overwrite" in order to overwrite the file; otherwise, the function will
  #' proceed with already existing file. 4) Optionally open the source in
  #' RStudio for editing; parameter: \code{open.source.file} (default: TRUE) 5)
  #' Optionally, compile the empty report for oyu to see the tables and plots 
  #' and describe them; parameter: \code{compile.report} (default: TRUE) 6)
  #' Optionally, open the compiled report in RStudio Viewer pane or browser; 
  #' parameter: \code{open.compiled} (default: TRUE) 7) Optionally, wait
  #' indefinitely and recompile upon user request (this allows to enter whatever
  #' comments in the source file); parameter: \code{wait.to.recompile} (default:
  #' TRUE). Type "y" or "Y" to recompile. Notice that the R interpreter is
  #' unable to execute any other command before receiving input from you 
  #' (including, say, previewing a chunk in an R notebook, since notebook
  #' commands are execute in the current session and in the Global Environment).
  #' 
  #' The parameters are mostly identical to \code{make_filename} and 
  #' \code{save.report} with a few additions.
  #' 
  #' @param proj.name character. Project name; the filename will start with it
  #' @param tags list. List with a "tagging schema": list elements contain tags
  #'   for specific components of the name. E.g. list(markup = "mk", analysis =
  #'   "an").
  #' @param values list. List with a "values schema": lsit elements contain 
  #'   values for the components of the name associated with a specific 
  #'   analysis. E.g. list(markup = "like-parker", analysis = "variant4")
  #' @param tag.delim character. The character which will be used as a delimiter
  #'   between component tags and values
  #' @param component.delim character. The character which will be used as a
  #'   delimiter between components tags
  #' @param data.type optional string specifying type of the data stored in the
  #'   file. it will be added after the project name but before the tags. E.g.,
  #'   if \code{data.type = "data"}, and the tags and values are as in the 
  #'   examples above, the name could be
  #'   project1_data_mk.parker-like_an.variant4
  #' @param source.file character. Name of the source .Rmd file. Must include
  #'   file extension
  #' @param template.file character. Name of a report template (a normal .Rmd
  #'   file just without analysis specific information)
  #' @param file.format character. Format of the output report. Can be "html" or
  #'   "pdf"
  #' @param source.folder character. Folder containing the source file. By
  #'   default, the folder from standard file structure, created by my project
  #'   initializing script.
  #' @param output.folder character. Folder for output files. By default -
  #'   folder with today's date as name. If this folder doesn't exist, it will
  #'   be created.
  #' @param template.folder character. Folder containing report templates
  #' @param current.folder character. Name of the "current" folder, i.e. folder 
  #'   containing latest version of the reports.
  #' @param today.folder character. Name of the folder with today's date as name
  #' @param overwrite.source character. Three possible values:
  #'  + "ask" (default) -  if the source file already exists, ask the user whether 
  #'        it should be overwritten
  #'  + "no"  - if the source file already exists, do not overwrite and proceed with the
  #'        existing one
  #'  + "yes" -  if the source file already exists, overwrite it with the template and
  #'       proceed with this new file
  #' @param copy.to.current logical. If TRUE, the output file will be also
  #'   copied to "current" folder, overwriting any previous file with that name.
  #'   If "current" folder doesn't exist, it will be created.
  #' @param open.source.file logical. Open the .Rmd source file created from
  #'   template for editing?
  #' @param compile.report logical. Compile the report made from template before
  #'   any changes are made? (Useful if you want to look at the report while 
  #'   editing its source file, i.e. to know what the plots actually look like)
  #' @param open.compiled logical. Only matters if \code{compile.report ==
  #'   TRUE}. Open compiled report in browser/RStudio Viewer pane?
  #' @param wait.to.recompile logical. Wait for user input confirming that the
  #'   source file has been edited and needs recompiling?
  #' @param date.format character. String specifying date.format, check Internet
  #'   for acceptable formats. Used to check whether today's folder corresponds
  #'   to today's date; also used in the name of the output file.
  #'   
  #' @return Nothing
  
  # Filename without extension formed according to the naming schema
  filename <- make_filename(proj.name, tags, values, tag.delim = ".",
                            component.delim = "_", data.type)
  
  # Full path to the output file, including extension
  output_file_full_path <- file.path(source.folder, paste0(filename, ".Rmd"))
  
  # A flag to control overwriting when copying template to the "source" folder
  # If FALSE, do not overwrite
  overwrite_file <- FALSE
  
  overwrite.source <- match.arg(overwrite.source)
  output_file_exists <- file.exists(output_file_full_path) # check whether the output file actually exists
  
  if (output_file_exists){ # if output file exists...
    if (overwrite.source == "ask"){ # ... and if the user wants to decide its fate ...
      answer <- readline(message("Output file ", # ask the user, and make him type "overwrite" to do so
                                 paste0(filename, ".Rmd"), " exists! ",
                                 "Overwrite? (type `overwrite` to do so) "))
      
      if (answer == "overwrite"){
        # ... if yes, set the flag
        overwrite_file <- TRUE
      }
    } else if (overwrite.source == "yes") { # If the user already decided that the file should be overwritten no matter what...
      overwrite_file <- TRUE # ... set the overwrite flag
    } else {
      overwrite_file <- FALSE
    }
  }
  
  # If we are allowed to overwrite or there is no such source file yet, create it
  if (overwrite_file | !output_file_exists){
    copy_success <- file.copy(from = file.path(template.folder, template.file),
                              to = output_file_full_path,
                              overwrite = TRUE)
    # if the copy wasn't successful, warn and exit
    if(length(copy_success) == 0){
      stop("Creating source file failed!")
    }
  }
  
  # If we need to open the source file for editing...           
  if(open.source.file){
    if (output_file_exists & !overwrite_file){
      message("Opening existing file for editing...")
    } else {
      message("Opening new file for editing...")
    }
    file.edit(output_file_full_path)
  }
  
  # If we need to compile the report right after copying to see how it looks like...
  if (compile.report){
    compiled_report <- save.report(source.file = paste0(filename, ".Rmd"),
                                   output.file = filename, file.format = file.format,
                                   source.folder = source.folder,
                                   output.folder = output.folder,
                                   copy.to.current = TRUE,
                                   current.folder = current.folder,
                                   today.folder = today.folder,
                                   date.format = date.format)
  }
  
  # If we compiled report and also want to open it automatically in a browser
  if (compile.report & open.compiled){
    if (file.exists(file.path(output.folder, compiled_report))){
      utils::browseURL(file.path(output.folder, compiled_report))
    }
  }
  
  # If we are waiting for the user to edit the source and then confirm it, to 
  # recompile on confirmation
  if (wait.to.recompile){
    ready_to_recompile <- readline(message("Recompile report?"))
    
    if (ready_to_recompile %in% c("y","Y")){
      compiled_report <-  save.report(source.file = paste0(filename, ".Rmd"),
                                      output.file = filename, file.format = file.format,
                                      source.folder = source.folder,
                                      output.folder = output.folder,
                                      copy.to.current = TRUE,
                                      current.folder = current.folder,
                                      today.folder = today.folder, 
                                      date.format = date.format)
    }
  }
  
  
  # if (copy_file | output_file_exists){
  #   return(filename)
  # } else {
  #   return(NULL)
  # }
  
}
