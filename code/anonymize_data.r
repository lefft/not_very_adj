### ANONYMIZE EXPERIMENTAL DATA BEFORE DATA CLEANUP AND ANALYSIS ##############
### written by timothy leffel, may/2018 ---------------------------------------
### 
### 
### this script masks private participant ids with anonymous ones. 
### here we replace IP addresses with strings of form "subjXXX", e.g. "subj007".
### 
### the identifiable data is not included in the public repository, so this 
### script is included just for the sake of completeness. 
### 
### input files: 
###   - Expt1-data.csv -- lightly cleaned ibex farm output, w subj IPs (expt 1)
###   - Expt2-data.csv -- lightly cleaned ibex farm output, w subj IPs (expt 2)
### 
### output files:
###   - Expt1-data_anon.csv -- same as `Expt1-data.csv`, but w anonymized ids
###   - Expt2-data_anon.csv -- same as `Expt2-data.csv`, but w anonymized ids
###   - Expt1-ip_id_table.csv -- correspondence between IPs and anonymous ids
###   - Expt2-ip_id_table.csv -- correspondence between IPs and anonymous ids





### 1. define anonymization function ------------------------------------------

# function to replace non-anonymous ids with arbitrary ones (e.g. "subj007")
anonymize_df <- function(dat, id_col, order_col, return_dict=FALSE){
  # params: 
  #   `dat` = a data frame containing columns named `id_col` and `order_col`
  #   `id_col` = name of column holding subj ids (e.g. IP addresses)
  #   `order_col` = column saying who gets earlier id (higher vals = higher id)
  
  # collect IP addresses (`id_col`) and timestamps (`order_col`)
  id_order_df <- data.frame(
    id_col = dat[[id_col]], order_col = dat[[order_col]])
  
  # get `order_col` max for each `id_col` value (to assign chronological ids)
  max_byid <- tapply(id_order_df$order_col, id_order_df$id_col, max)
  
  # make a df with old ids (IPs), to add masked ids to (e.g. "subj007")
  id_table <- data.frame(old_id = unique(dat[[id_col]]), stringsAsFactors=FALSE)
  
  # arrange ids table by most recent timestamp
  id_table$sorter <- as.numeric(max_byid[id_table$old_id])
  id_table <- id_table[order(id_table$sorter), ]
  
  # use the necessary number of leading zeros when assigning new ids
  formatter <- paste0("subj%0", nchar(as.character(nrow(id_table))), "d")
  
  # assign new ids to each row of the `id_table`
  id_table$new_id <- as.character(sprintf(formatter, 1:nrow(id_table)))
  
  # make a named vector associating new w old ids (for overwriting old ids)
  id_dict <- setNames(id_table$new_id, id_table$old_id)
  
  # replace the id column w the anonymous ids, and rename the column `subj_id`
  dat[[id_col]] <- as.character(id_dict[dat[[id_col]]])
  names(dat)[names(dat)==id_col] <- "subj_id"
  
  # if you want the id lookup table, return a list w it and the data
  if (return_dict)
    return(list(data=dat, id_dict=id_table[, c("old_id", "new_id")]))
  
  return(dat)
}



### 2. read data and anonymize it, collecting ip-id tables --------------------
root <- function(...) file.path("~/Dropbox/NotVeryExperiment/paper-repo", ...)

files <- list(
  expt1 = root("data/Expt1-data.csv"), 
  expt1_anon = root("data/check_before_pushing/Expt1-data_anon.csv"), 
  expt1_id_dict = root("data/check_before_pushing/Expt1-ip_id_table.csv"), 
  expt2 = root("data/Expt2-data.csv"),
  expt2_anon = root("data/check_before_pushing/Expt2-data_anon.csv"), 
  expt2_id_dict = root("data/check_before_pushing/Expt2-ip_id_table.csv"))


dat1 <- read.csv(files$expt1, stringsAsFactors=FALSE)
dat2 <- read.csv(files$expt2, stringsAsFactors=FALSE)

message("\nanonymizing datasets for Expt1 and Expt2...\n")
dat1_anon <- anonymize_df(dat1, id_col="IP", order_col="date", return_dict=TRUE)
dat2_anon <- anonymize_df(dat2, id_col="IP", order_col="date", return_dict=TRUE)



### 3. make some assertions to confirm that all data preserved ----------------

# check that column contents (except id col) are all identical 
stopifnot(all(sapply(setdiff(names(dat1), c("IP", "subj_id")), function(cname){
  identical(dat1[[cname]], dat1_anon$data[[cname]])})))

stopifnot(all(sapply(setdiff(names(dat2), c("IP", "subj_id")), function(cname){
  identical(dat2[[cname]], dat2_anon$data[[cname]])})))


# check that the distribution of subj id's over rows are identical 
stopifnot(identical(as.numeric(sort(table(dat1$IP))), 
                    as.numeric(sort(table(dat1_anon$data$subj_id)))))

stopifnot(identical(as.numeric(sort(table(dat2$IP))), 
                    as.numeric(sort(table(dat2_anon$data$subj_id)))))


# [and see comments at bottom for more assertions if you're still worried]
message("\nall tests passed, anonymization performed safely.\n")




### 4. write anonymized data to disk ------------------------------------------

# if session still live here, then it's safe to write + use the anonymized data


# save the id lookup tables (associates unmasked IP with arbitrary identifier)
message("writing table associating IPs with anonymized ids to files:\n  ", 
        ">> `", gsub(root(), "", files$expt1_id_dict), "`\n  ", 
        ">> `", gsub(root(), "", files$expt2_id_dict), "`\n ")
write.csv(dat1_anon$id_dict, files$expt1_id_dict, row.names=FALSE)
write.csv(dat2_anon$id_dict, files$expt2_id_dict, row.names=FALSE)


# save the anonymized datasets (exactly same as input file but with IPs masked)
message("writing anonymized experimental data to files:\n  ", 
        ">> `", gsub(root(), "", files$expt1_anon), "`, and\n  ", 
        ">> `", gsub(root(), "", files$expt2_anon), "`\n ")
write.csv(dat1_anon$data, files$expt1_anon, row.names=FALSE)
write.csv(dat2_anon$data, files$expt2_anon, row.names=FALSE)










# #### some additional verification in case you're still worried 
# suppressPackageStartupMessages(library(dplyr))
# summary1 <- dat1 %>% group_by(IP) %>% 
#   summarize(r=mean(as.numeric(response), na.rm=TRUE)) %>% 
#   select(-IP) %>% arrange(r)
# summary1_anon <- dat1_anon$data %>% 
#   group_by(subj_id) %>% summarize(r=mean(as.numeric(response), na.rm=TRUE)) %>%
#   select(-subj_id) %>% arrange(r)
# 
# summary2 <- dat2 %>% group_by(IP) %>% 
#   summarize(r=mean(as.numeric(response), na.rm=TRUE)) %>% 
#   select(-IP) %>% arrange(r)
# summary2_anon <- dat2_anon$data %>% 
#   group_by(subj_id) %>% summarize(r=mean(as.numeric(response), na.rm=TRUE)) %>% 
#   select(-subj_id) %>% arrange(r)
# 
# stopifnot(all(identical(summary1, summary1_anon), 
#               identical(summary2, summary2_anon)))
# 
# stopifnot(all(identical(as.numeric(sort(table(dat1$IP))), 
#                         as.numeric(sort(table(dat1_anon$data$subj_id)))), 
#               identical(as.numeric(sort(table(dat2$IP))), 
#                         as.numeric(sort(table(dat2_anon$data$subj_id))))))

