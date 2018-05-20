### EXPERIMENT 2: PREPARE DATA FOR ANALYSIS ###################################
###  
### this script cleans up the raw data for experiment 2: 
###   1. read in data, separate into subj-level data and actual expt data; 
###   2. identify subjs to exclude from analysis (see paper sec 3.1); 
###   3. remove excluded subjs from data, write clean + screened data to file; 
###   4. produce + save some sanity-check plots 



### 0. session setup (load dependencies, specify files) -----------------------
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))

source("cleanup_functions.r")

files <- list(
  # data file from expt2, to be read in and cleaned up 
  expt2_raw_data_fname = "../data/Exp2-data.csv",
  # expt2 analysis dataset -- gets written to disk at bottom of this script 
  expt2_clean_data_outname = "../data/Expt2-data_cleaned_screened-----.csv",
  # some diagnostic/summary plots that we'll save at the end 
  age_plot_outname = "../plots/expt2_age_distro.pdf",
  bysubj_plot_outname = "../plots/expt2_bysubj_response_distro.pdf",
  bypred_plot_outname = "../plots/expt2_bypred_response_distro.pdf")





### 1. read data, separate into subj-level and response data ------------------
message("\nreading raw experiment 2 data from file:\n  >> ", 
        files$expt2_raw_data_fname)

# read in raw data (default `results.txt` ibex farm output, w file renamed)
expt2_raw_data <- read.csv(files$expt2_raw_data_fname, stringsAsFactors=FALSE)




# extract subject info (will find excludable subjects after cleaning items)
subj_info <- dplyr::as_data_frame(expt2_raw_data) %>% 
  filter(content %in% c("age","language","mturkid")) %>% 
  group_by(IP) %>% summarize(
    mturk_id = unique(response[content=="mturkid"]),
    date = as.character(unique(date)),
    age = unique(response[content=="age"]),
    lang = unique(response[content=="language"]),
    adj = unique(Adj)
  ) %>% ungroup %>% 
  arrange(desc(IP))


# remove questionnaire items from data, then clean up a bit 
dat <- dplyr::as_data_frame(expt2_raw_data) %>% 
  filter(content=="item") %>% 
  select(-content) %>% 
  mutate(response = as.numeric(response))


# define column `Comparative` that states if `Pred` is comparative or positive
dat$Comparative <- grepl("er$", dat$Pred)

# define column `Pred2` according to mapping:
#   - tall(er)|hot(ter)|fast(ter)      --> Adj
#   - not Adj                          --> NotAdj
#   - very Adj|much Adj-er             --> VeryAdj
#   - not very Adj|not much Adj-er     --> NotVeryAdj
dat$Pred2 <- dplyr::case_when(
  dat$Pred %in% c("taller", "tall", "faster", "fast", 
                  "hotter", "hot") ~ "Adj", 
  dat$Pred %in% c("notTaller", "notTall", "notFaster", "notFast", 
                  "notHotter", "notHot") ~ "NotAdj", 
  dat$Pred %in% c("muchTaller", "veryTall", "muchFaster", "veryFast", 
                  "muchHotter", "veryHot") ~ "VeryAdj", 
  dat$Pred %in% c("notMuchTaller", "notVeryTall", "notMuchFaster","notVeryFast",
                  "notMuchHotter", "notVeryHot") ~ "NotVeryAdj", 
  TRUE ~ NA_character_)


# define a common scale `NormUnit` for the three adjs, so that: 
#   - for 'tall', NormUnit = inches - 70 
#   - for 'fast', NormUnit = (mph - 113) / 4
#   - for 'hot',  NormUnit = (degreesF - 83) / 2.5
dat$NormUnit <- dplyr::case_when(
  dat$Adj=="Tall" ~ (as.numeric(dat$Unit)-70), 
  dat$Adj=="Fast" ~ (as.numeric(dat$Unit)-113) / 4, 
  dat$Adj=="Hot" ~ (as.numeric(dat$Unit)-83) / 2.5, 
  TRUE ~ NA_real_
)





### 2. identify subjs to exclude from analysis (see paper sec 3.1) ------------

# a subj is excluded if:
#   2.1: their self-reported native lang is not english;    (`non_native_ips`)
#   2.2: they did the same expt more than once; or          (`double_take_ips`)
#   2.3: they perform < at chance on "extreme" items        (`inaccurate_ips`)



# 2.1 non-native ips (all are english for expt2, see `unique(subj_info$lang)`)
# [identifies *0* subj to exclude]
non_native_ips <- subj_info$IP[!subj_info$lang %in% c(
  "English", "english", "ENGLISH", "Englsh", "engllish", "Emglish")]



# 2.2 ips of subjs who took same expt twice (none for expt2)
# [identifies *0* subj to exclude] 
double_take_ips <- character(0)



# 2.3 identify subj's who performed below chance on extreme scale points. 
dat_extreme <- dat %>% 
  filter(NormUnit <= -6 | NormUnit >= 12) %>% 
  filter(Pred2 %in% c("Adj", "NotAdj")) %>% 
  select(IP, response, Adj, Pred, Pred2, Comparative, Unit, NormUnit) %>% 
  mutate(region = ifelse(NormUnit < 0, "lo", ifelse(NormUnit > 0, "hi", NA)))


dat_extreme <- dat_extreme %>% 
  mutate(correct = dplyr::case_when(
    Pred2=="Adj"    ~ case_when(region=="hi" ~ response > 50, 
                                region=="lo" ~ response < 50, TRUE ~ NA), 
    Pred2=="NotAdj" ~ case_when(region=="hi" ~ response < 50, 
                                region=="lo" ~ response > 50, TRUE ~ NA)))


subj_performance <- dat_extreme %>% group_by(IP) %>% summarize(
  n_extreme_trials = n(), 
  prop_correct_extreme = sum(correct) / n_extreme_trials
)

# join performance info with the main subj info data frame 
subj_info <- subj_info %>% left_join(subj_performance, by="IP")

# two subjs are below chance on performance metric 
# [identifies *2* subjs to exclude] 
inaccurate_ips <- subj_info$IP[subj_info$prop_correct_extreme < .5]


# extract bad subjs from `subj_info`, collect into a data frame 
subj_info_exclusions <- rbind(
  mutate(subj_info[subj_info$IP %in% non_native_ips, ], reason="non-native"),
  mutate(subj_info[subj_info$IP %in% inaccurate_ips, ], reason="performance"))




### 3. remove bad subjs, save clean + screened data ---------------------------

# remove all response data for the two subj's identified in 2. above 
dat_screened <- dat[!dat$IP %in% subj_info_exclusions$IP, ]

# define a subj-level data frame with only non-excluded subjs 
subj_info_screened <- subj_info[subj_info$IP %in% dat_screened$IP, ]


# check all trials either excluded or in screened dataset (shd be `TRUE`)
sum(dat$IP %in% subj_info_exclusions$IP)+nrow(dat_screened) == nrow(dat)

# write cleaned + screened data to disk for analysis 
message("\nwriting clean/screened experiment analysis dataset to file:\n  >> ", 
        files$expt2_clean_data_outname)
write.csv(dat_screened, file=files$expt2_clean_data_outname, row.names=FALSE)










### 4. produce + save some sanity-check plots ---------------------------------
message("\nproducing expt2 summary plots, saving to `/plots/`\n")

## 4.1 distribution of participant ages (non-excluded subjs only)
age_plot_caption <- paste0(
  "n = ", length(subj_info_screened$age), ", ", 
  "mean age = ", round(mean(as.numeric(subj_info_screened$age))), "yrs, ", 
  "range = ", min(subj_info_screened$age), "-", max(subj_info_screened$age))

age_plot <- qplot(subj_info_screened$age) + 
  labs(title="Experiment 2 participant info", subtitle=age_plot_caption) + 
  theme_bw()

## 4.2 distribution of responses for each non-excluded subj (0-100 slider scale)
bysubj_plot <- dat_screened %>% 
  ggplot(aes(x=response)) + 
  geom_density(fill="gray", alpha=.5) +
  facet_wrap(~IP, ncol=12) + 
  labs(title="Experiment 2 by-participant response distributions", 
       x="response, on 0-100 slider scale") + 
  theme_bw() + 
  theme(text=element_text(size=6), legend.position="top")


## 4.3 distribution of responses for each predicate (0-100 slider scale)
bypred_plot <- dat_screened %>% 
  ggplot(aes(x=response, fill=Comparative)) + 
  geom_density(alpha=.5) + 
  facet_wrap(~Pred2+Adj, ncol=3) + 
  labs(title="Experiment 2 by-predicate response distributions", 
       x="response, on 0-100 slider scale") + 
  theme_bw() + 
  theme(text=element_text(size=11), legend.position="top")



## 4.4 save each of the plots (filenames from top of script)
ggsave(filename=files$age_plot_outname, plot=age_plot, 
       width=8, height=4, units="in")

ggsave(filename=files$bysubj_plot_outname, plot=bysubj_plot, 
       width=10, height=7.5, units="in", scale=1.25)

ggsave(filename=files$bypred_plot_outname, plot=bypred_plot, 
       width=8, height=6, units="in")







