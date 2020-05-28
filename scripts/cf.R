function(cfs){

   names(cfs) <- tolower(names(cfs))

   codebook <- list(
      purpose_1 = list("1" = "Humanitarian", "2" = "Peace Process", 
                       "3" = "Holiday", "4" = "Election", "5" = "Other"),

      ceasefire_type_old = list("1" = "Definitive", "2" = "Preliminary", 
                            "-1" = "Unclear"),

      ceasefire_class = list("1" = "Cessation", "2" = "Cessation w/. compliance", 
			     "3" = "Definitive")

      mediator_nego = list( "0" = "Non-mediated", "1" = "Mediated", 
                           "-1" = "Unclear"),

      implement = list( "0" = "No mechanism", "1" = "Mechanism agreed", 
                       "-1" = "Unclear"),

      enforcement = list( "0" = "No enforcement", "1" = "External enforcement", 
                         "2" = "Intl. organization enforcement", "3" = "Unclear", 
                         "-1" = "Unclear"),

      written = list("-1" = "Unclear",
                     "0" = "No written agreement",
                     "1" = "Written agreement",
                     "2" = "Written agreement"))

   lookup <- function(x,dict){
      x <- as.character(as.numeric(x))
      vapply(x, function(value) {
                if(value %in% names(dict)){
                   dict[[as.character(value)]]
                } else {
                   '' 
                }}, 
             USE.NAMES = FALSE, FUN.VALUE = character(1))
   }

   varstodates <- function(varlist,fixNaDay = TRUE,fixNaMonth = TRUE){

      if(!all(c('year','month','day') %in% names(varlist))){
         stop('usage: varsToDates(list(year=year,month=month,day=day))')
         }


      fixNa <- function(x,replacement){
         if(is.na(x)) warning('Replacing ',names(x))
         x <- ifelse(is.na(x),replacement,x) 
         }

      vtd <- function(varlist){

         vars <- lapply(varlist,as.numeric)%>%
         as.data.frame(stringsAsFactors = FALSE)

         apply(vars,1,function(r){

         r <- lapply(r,function(v){ifelse(v > 0,v,NA)})

         if(fixNaDay){
            r['day'] <- fixNa(r['day'],15)
         }
         if(fixNaMonth){
            r['month'] <- fixNa(r['month'],6)
         }

         if(all(!is.na(r))){
         paste(r['year'],r['month'],r['day'],sep = '-')
         }

         else {
         NA
         }

         })
      }
      as.Date(vtd(varlist))
   }

   # ================================================
   # ================================================
   # ================================================

   cfs$start_date <- suppressWarnings(varstodates(list(year = cfs$cf_effect_yr,
                                      month = cfs$cf_effect_month,
                                      day = cfs$cf_effect_day), fixNaMonth = TRUE)) 

   cfs$actor_name <- str_replace_all(cfs$actor_name,'\\s+',' ')

   catvars <- c('ceasefire_type','purpose_1','written',
                'mediator_nego','implement','enforcement')

   for(v in catvars){
      cfs[[v]] <- lookup(as.character(cfs[[v]]), codebook[[v]])
   }

   # ================================================
   # TODO: Re-add categorical data once disparities =
   # in the data have been resolved. ================
   # ================================================

   cfs <- cfs %>%
      select(
         location,
         start = start_date,
         id = cf_id,
         cat_type = ceasefire_class,
         cat_purpose = purpose_1,
         cat_mediated = mediator_nego,
         cat_implementation = implement,
         cat_enforcement = enforcement, 
         cat_written = written,
         name = actor_name) 
   groupingVariables <- c('name')

   cfs <- cfs %>%
      group_by_at(names(cfs)[!names(cfs) %in% groupingVariables]) %>%
      summarize(name = glue_collapse(unique(name), sep = ' - '),
                cat_bilateral = ifelse(n() > 1,'Multilateral','Unilateral')) %>%
      ungroup()
   cfs[complete.cases(cfs),]
}
