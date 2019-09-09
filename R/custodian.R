#!/usr/bin/Rscript
header <- "
████████▄     ▄████████     ███        ▄████████       ▄████████ ███    █▄     ▄████████     ███      ▄██████▄  ████████▄   ▄█     ▄████████ ███▄▄▄▄   
███   ▀███   ███    ███ ▀█████████▄   ███    ███      ███    ███ ███    ███   ███    ███ ▀█████████▄ ███    ███ ███   ▀███ ███    ███    ███ ███▀▀▀██▄ 
███    ███   ███    ███    ▀███▀▀██   ███    ███      ███    █▀  ███    ███   ███    █▀     ▀███▀▀██ ███    ███ ███    ███ ███▌   ███    ███ ███   ███ 
███    ███   ███    ███     ███   ▀   ███    ███      ███        ███    ███   ███            ███   ▀ ███    ███ ███    ███ ███▌   ███    ███ ███   ███ 
███    ███ ▀███████████     ███     ▀███████████      ███        ███    ███ ▀███████████     ███     ███    ███ ███    ███ ███▌ ▀███████████ ███   ███ 
███    ███   ███    ███     ███       ███    ███      ███    █▄  ███    ███          ███     ███     ███    ███ ███    ███ ███    ███    ███ ███   ███ 
███   ▄███   ███    ███     ███       ███    ███      ███    ███ ███    ███    ▄█    ███     ███     ███    ███ ███   ▄███ ███    ███    ███ ███   ███ 
████████▀    ███    █▀     ▄████▀     ███    █▀       ████████▀  ████████▀   ▄████████▀     ▄████▀    ▀██████▀  ████████▀  █▀     ███    █▀   ▀█   █▀  
"
#' Custodian script
#'
#' Connects to database container linked @ database 
#' Database must serve at port 5432 and have a user
#' "custodian" with the proper permissions
#'
#' The target datbase must be defined in the env
#' variable TGT_DB

shh <- suppressPackageStartupMessages
shh(library(RSQLite))
shh(library(RPostgreSQL))
shh(library(readxl))
shh(library(tools))
shh(library(stringr))
shh(library(dplyr))
shh(library(lubridate))
shh(library(glue))

interval <- Sys.getenv('INTERVAL')
if(is.null(interval)){
   interval <- 0.5
}

log <- function(message){
   message <- gsub("INFO","\x1b[38;5;45mINFO\x1b[0m",message)
   message <- gsub("ADD","\x1b[38;5;41mADD\x1b[0m",message)
   message <- gsub("WARN","\x1b[38;5;160mWARN\x1b[0m",message)
   logf <- stdout()
   write(message,logf,append = TRUE)
}

# ================================================

# Debug or production
if(Sys.getenv('TESTING') == '' & !interactive()){
   log(" *** Main mode *** ")
   DRIVER <<- "postgres"
   datadir <- '/var/data' 
   scriptdir <- '/var/scripts' 

   con_values <- list(host = "database",
                   port = 5432,
                   user = "custodian",
                   db = "shiny",
                   drv = PostgreSQL())
} else {
   log(" *** Testing mode *** ")
   DRIVER <<- "sqlite"
   datadir <- './data'
   scriptdir <- './scripts'
   con_values <- list(drv = SQLite(),
                      dbname = "test.sqlite")
}

# ================================================

getStamps <- function(directory){
   # Returns filename@timestamp for all files in directory
   files <- list.files(directory, full.names = TRUE)
   if(length(files) > 0){
      finf <- file.info(files)
      stamps <- paste0(row.names(finf),'@',strftime(finf$ctime))
      extensions <- tools::file_ext(files)
   } else {
      files <- character()
      stamps <- character()
      extensions <- character()
   }

   data.frame(path = files, stamp = stamps,ext = extensions)
}

sanitizeName <- function(name){
   if(grepl('[^A-Za-z0-9_]',name)){
      invalid <- name 
      name <- gsub('[^A-Za-z0-9_]','_',name)
      log(paste0('WARN: invalid data name: "',invalid,'", using "',name,'"'))
   }
   name 
}

getDataName <- function(path){
   str_remove(path,'.*/') %>%
      file_path_sans_ext() %>%
      sanitizeName()
}

getScripts <- function(paths){
      scriptnames <- sapply(paths, str_extract,
                               pattern = '[a-zA-Z]+(?=\\.R)')
      functions <- lapply(paths,
                          dget)
      names(functions) <- scriptnames 
      functions 
}

# ================================================

readdata <- function(path){
   read_disp <- list(csv = read.csv,
                     xlsx = read_xlsx)
   ext <- tools::file_ext(path)
   if(ext %in% names(read_disp)){
      read_disp[[tools::file_ext(path)]](path)
   } else {
      NULL
   }

}

transformdata <- function(data,class,scriptdir){
   scripts <- getScripts(list.files(scriptdir, full.names = TRUE))
   if(class %in% names(scripts)){
      log(paste0('INFO: Transforming as ',class))
      data <- scripts[[class]](data)
      log(paste0('INFO: Successfully transformed!'))
   } else {
      log(paste0("No script found for data of type ",class))
   }
   data
}

pushdata <- function(data,con,name){

   if(any(grepl('[A-Z]',names(data)))){
      log(paste0('WARN: lowercasing names for ',name))
      names(data) <- tolower(names(data))   
   }

   if(name %in% dbListTables(con)){
      log(paste0('WARN: dropping preexisting ',name))
      dbRemoveTable(con,name)
   }

   res <- dbWriteTable(con, name, data)
   log(paste0('INFO: successfully added ',name))
   if(DRIVER == "postgres"){
      res <- dbExecute(con,paste0('GRANT SELECT ON ', name, ' TO public;'))
      log(paste0('INFO: made ', name, ' public'))
   } else {
      log("INFO: no permission work to do! (sqlite)")
   }
}

dropdata <- function(con, name){
   if(name %in% dbListTables(con)){
      dbRemoveTable(con,name)
      log(paste0('INFO: successfully dropped ',name))
   } else {
      log(paste0('WARN: ', name, ' not in DB'))
   }
}

# ================================================

diff <- function(now,old){
   added <- setdiff(now,old)
   removed <- setdiff(old,now)
   list(added=added,removed=removed)
}

filediff <- function(new,old){
   diff <- diff(new$stamp, old$stamp)
   list(added = new[new$stamp %in% diff$added,], 
        removed = old[old$stamp %in% diff$removed,])
}

# ================================================

dr <- dbDriver('PostgreSQL')
con_values$dr <- dr
con <- do.call(dbConnect,con_values) 

# ================================================

oldstamps <- data.frame(path = character(),stamp = character()) 
log(header)
log(' *** Ready to munch some data! *** ')

while(TRUE){

   stamps <- getStamps(datadir)
   filechanges <- filediff(stamps,oldstamps)

   if(nrow(filechanges$removed) > 0){
      apply(filechanges$removed, 1, function(stamped_file){
         log(paste0('REMOVE: ',stamped_file['stamp']))
         name <- getDataName(stamped_file['path'])
         dropdata(con, name)
      })
   }

   if(nrow(filechanges$added) > 0){
      apply(filechanges$added, 1, function(stamped_file){

         name <- getDataName(stamped_file['path'])
         class <- str_extract(name,'[A-Za-z]+')

         log(paste0('ADD: ',stamped_file['stamp'], ' of class ', class))

         data <- readdata(stamped_file['path'])

         if(!is.null(data)){
            data <- transformdata(data,class,scriptdir) %>%
               pushdata(con,name)
         } else {
            log(paste0('WARN: ', stamped_file['stamp'], ' was NULL'))
         }
      })
   }
   
   oldstamps <- stamps

   Sys.sleep(0.5)
}
