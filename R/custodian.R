#' Custodian script
#'
#' Connects to database container linked @ database 
#' Database must serve at port 5432 and have a user
#' "custodian" with the proper permissions
#'
#' The target datbase must be defined in the env
#' variable TGT_DB

library(RPostgreSQL)
library(readxl)
library(tools)

database <- Sys.getenv('TGT_DB')
dir <- '/var/data' 

con_values <- list(host = 'database',
                   port = 5432,
                   user = 'custodian',
                   db = database)


# ================================

log <- function(message){
   logf <- stdout()
   write(message,logf,append = TRUE)
   #close(logf)
}

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

   res <- data.frame(path = files, stamp = stamps,ext = extensions)
   res
}

fixdataname <- function(dataname){
   if(grepl('[^A-Za-z0-9_]',dataname)){
      inv <- dataname 
      dataname <- gsub('[^A-Za-z0-9_]','_',dataname)
      log(paste0('WARN: invalid data name: "',inv,'", using "',dataname,'"'))
   }
   dataname
}

pushdata <- function(con, file){
   read_disp <- list(csv = read.csv,
                     xlsx = read_xlsx)

   dataname <- strsplit(file,'/') 
   dataname <- dataname[[1]][length(dataname[[1]])]
   dataname <- gsub('\\..*$','',dataname)
   dataname <- fixdataname(dataname)

   ext <- tools::file_ext(file)
   if(ext %in% names(read_disp)){
      data <- read_disp[[tools::file_ext(file)]](file)

      if(any(grepl('[A-Z]',names(data)))){
         log(paste0('WARN: lowercasing names for ',dataname))
         names(data) <- tolower(names(data))   
      }

      if(dataname %in% dbListTables(con)){
         log(paste0('WARN: dropping preexisting ',dataname))
         dbRemoveTable(con,dataname)
      }

      res <- dbWriteTable(con, dataname, data)
      log(paste0('INFO: successfully added ',dataname))
      res <- dbExecute(con,paste0('GRANT SELECT ON ', dataname, ' TO public;'))
      log(paste0('INFO: made ',dataname, ' public'))

   } else {
      log(paste0('WARN: unsupported (or no) extension', file))
   }
}

dropdata <- function(con, file){
   dataname <- strsplit(file,'/') 
   dataname <- dataname[[1]][length(dataname[[1]])]
   dataname <- gsub('\\..*$','',dataname)
   dataname <- fixdataname(dataname)

   if(dataname %in% dbListTables(con)){
      dbRemoveTable(con,dataname)
      log(paste0('INFO: successfully dropped ',dataname))
   } else {
      log(paste0('WARN: ', dataname, ' not in DB'))
   }
}

# ================================

dr <- dbDriver('PostgreSQL')
con_values$dr <- dr
con <- do.call(dbConnect,con_values) 

# ================================

oldstamps <- data.frame(path = character(),stamp = character()) 

log('Starting service...')

while(TRUE){
   stamps <- getStamps(dir)

   newfiles <- stamps[!stamps$stamp %in% oldstamps$stamp,]
   removedfiles <- oldstamps[!oldstamps$stamp %in% stamps$stamp,]

   if(nrow(removedfiles) > 0){
      apply(removedfiles, 1, function(stamped_file){
         log(paste0('REMOVE: ',stamped_file['stamp']))
         dropdata(con, stamped_file['path'])
      })
   }

   if(nrow(newfiles) > 0){
      apply(newfiles, 1, function(stamped_file){
         log(paste0('ADD: ',stamped_file['stamp']))
         pushdata(con, stamped_file['path'])
      })
   }
   
   oldstamps <- stamps
   Sys.sleep(0.5)
}
