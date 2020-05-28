
function(ged){
   ged$date_start <- as.Date(ged$date_start)
   ged$date_end <- as.Date(ged$date_end)

   rand_vect <- function(N, M, sd = 1, pos.only = TRUE) {
      # Yields a vector of length N and sum M
      # "randomly distributes" M outcomes on N cases

      vec <- rnorm(N, M/N, sd)
      if (abs(sum(vec)) < 0.01) vec <- vec + 1
         vec <- round(vec / sum(vec) * M)
         deviation <- M - sum(vec)
         for (. in seq_len(abs(deviation))) { 
         vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
      }
      if (pos.only) while (any(vec < 0)) {
         negs <- vec < 0
         pos  <- vec > 0
         vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
         vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
      }
      vec
   }

   explodeGed <- function(ged){
     # Distributes deaths from ged periods among period days
     # distribution is uniformly random
     
     ged$rowname <- row.names(ged)
     ged$diff <- ged$date_end - ged$date_start
     ged <- ged[rep(1:nrow(ged), ged$diff + 1),]
     
     ged$consec <- sequence(rle(ged$rowname)$lengths) -1
     ged$date <- ged$date_start + ged$consec
     
     ged$deathsdistrib <- apply(ged[ged$consec == 0,], 1, function(row){
       
       rdiff <- str_extract(row['diff'],'[0-9]+') %>%
         as.numeric() + 1
       vec <- rand_vect(rdiff , as.numeric(row['best']))
     }) %>% unlist()
     ged
   }

   gexpl <- explodeGed(ged)

   # Summarization =====================================
   print(names(gexpl))

   gexpl <- gexpl %>%
      select(dyad_id = dyad_new_id,
             type = type_of_violence,
             conflict_id = conflict_new_id,
             location = country,
             cow = country_id,
             deathsdistrib,
             side_a,side_b,
             date)
  gexpl <- gexpl %>%
      group_by_at(names(gexpl)[names(gexpl) != "deathsdistrib"]) %>%
      summarize(cnt = sum(deathsdistrib)) %>% 
      ungroup()

   gexpl$year <- year(gexpl$date)

   gexpl

}
