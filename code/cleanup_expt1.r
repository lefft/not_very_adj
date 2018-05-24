## TODO -- INTEGRATE ANONYMIZED DATA CONVENTIONS (IP ~~~> subj_id) 


### EXPERIMENT 1: PREPARE DATA FOR ANALYSIS ###################################
###  
### this script cleans up the raw data for experiment 1: 
###   1. read in data, separate into subj-level data and actual expt data; 
###   2. identify subjs to exclude from analysis (see paper sec 3.1); 
###   3. remove excluded subjs from data, write clean + screened data to file; 
###   4. produce + save some sanity-check plots 



### 0. session setup (load dependencies, specify files) -----------------------
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))

source("cleanup_functions.r")

files <- list(
  # data file from expt1, to be read in and cleaned up 
  expt1_raw_data_fname = "../data/Exp1-data.csv",
  # expt1 analysis dataset -- gets written to disk at bottom of this script 
  expt1_clean_data_outname = "../data/Expt1-data_cleaned_screened-----.csv",
  # some diagnostic/summary plots that we'll save at the end 
  age_plot_outname = "../plots/expt1_age_distro.pdf",
  bysubj_plot_outname = "../plots/expt1_bysubj_response_distro.pdf",
  bypred_plot_outname = "../plots/expt1_bypred_response_distro.pdf")





### 1. read data, separate into subj-level and response data ------------------
message("\nreading raw experiment 1 data from file:\n  >> ", 
        files$expt1_raw_data_fname)

# read in raw data (default `results.txt` ibex farm output, w file renamed)
expt1_raw_data <- read.csv(files$expt1_raw_data_fname, stringsAsFactors=FALSE)


# extract subject info (will find excludable subjects after cleaning items)
subj_info <- dplyr::as_data_frame(expt1_raw_data) %>% 
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
dat <- dplyr::as_data_frame(expt1_raw_data) %>% 
  filter(content=="item") %>% 
  select(-content, -X) %>% 
  mutate(response = as.numeric(response))

# define a common scale (`unit_to_common_scale()` from `cleanup_functions.r`):
#   ['tall': (inches - 70) * 3], ['late' -- minutes relative to 9am]
dat$NormUnit <- sapply(dat$Unit, expt1_unit_to_common_scale)

# inspect distribution of scale points across 'tall'- versus 'late'-items
table(dat$Adj, dat$NormUnit, useNA="ifany")





### 2. identify subjs to exclude from analysis (see paper sec 3.1) ------------

# a subj is excluded if:
#   2.1: their self-reported native lang is not english;    (`non_native_ips`)
#   2.2: they did the experiment more than once; or        (`double_take_ips`)
#   2.3: they perform < at chance on "extreme" items        (`inaccurate_ips`)


# 2.1 non-native ips (only valid options: "American English", "Other English")
# [identifies *1* subj to exclude] 
non_native_ips <- subj_info$IP[!grepl("English", subj_info$lang)]



# 2.2 ip of one subj who took expt twice 
# [identifies *1* subj to exclude] 
double_take_ips <- "a59cea6452b996807188aca9fbf059df"



# 2.3 identify subj's who performed below chance on extreme scale points. 
dat_extreme <- dat %>% 
  # collect the extreme items for unmarked and negated adj's. 
  filter(Pred %in% c("tall", "late", "notTall", "notLate")) %>% 
  # select rows with a maximal or minimal scale pos relative to 'tall'/'late'
  group_by(Adj) %>% filter(NormUnit %in% range(NormUnit)) %>% ungroup

# check scale positions of remaining rows for each A/notA pred: 
table(dat_extreme$NormUnit, dat_extreme$Pred)

# make table/dict to look up whether a predicate is "positive" (for filtering)
polarity_lkup <- setNames(c(TRUE, TRUE, FALSE, FALSE), 
                          c("tall", "late", "notTall", "notLate"))

# define an item as 'correct' if: 
#   - response is >= 50 for positive preds:  NormUnit == 36 (tall), 48 (late)
#   - response is <= 50 for negative preds:  NormUnit == -21
dat_extreme <- dat_extreme %>% mutate(correct = case_when(
  NormUnit > 0 &  polarity_lkup[Pred] ~ response >= 50, 
  NormUnit > 0 & !polarity_lkup[Pred] ~ response <= 50, 
  NormUnit < 0 &  polarity_lkup[Pred] ~ response <= 50, 
  NormUnit < 0 & !polarity_lkup[Pred] ~ response >= 50))

# only 12 "incorrect" responses to extreme items: 
table(dat_extreme$correct, useNA="ifany")

# get by-subj mean performance on extreme items (proportion "correct")
subj_performance <- dat_extreme %>% group_by(IP) %>% summarize(
  n_extreme_trials = n(), 
  prop_correct_extreme = sum(correct) / n_extreme_trials
)

# join performance info with the main subj info data frame 
subj_info <- subj_info %>% left_join(subj_performance, by="IP")

# no one is below chance on performance metric (`length(innacurate_ips) == 0`)
# [identifies *0* subjs to exclude] 
inaccurate_ips <- subj_info$IP[subj_info$prop_correct_extreme < .5]


# extract bad subjs from `subj_info`, collect into a data frame 
subj_info_exclusions <- rbind(
  mutate(subj_info[subj_info$IP %in% non_native_ips, ], reason="non-native"),
  mutate(subj_info[subj_info$IP %in% double_take_ips, ], reason="double-take"),
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
        files$expt1_clean_data_outname)
write.csv(dat_screened, file=files$expt1_clean_data_outname, row.names=FALSE)





### 4. produce + save some sanity-check plots ---------------------------------
message("\nproducing expt1 summary plots, to be saved to `/plots/`\n")

## 4.1 distribution of participant ages (non-excluded subjs only)
age_plot_caption <- paste0(
  "n = ", length(subj_info_screened$age), ", ", 
  "mean age = ", round(mean(as.numeric(subj_info_screened$age))), "yrs, ", 
  "range = ", min(subj_info_screened$age), "-", max(subj_info_screened$age))

age_plot <- qplot(subj_info_screened$age) + 
  labs(title="Experiment 1 participant info", subtitle=age_plot_caption) + 
  theme_bw()

## 4.2 distribution of responses for each non-excluded subj (0-100 slider scale)
bysubj_plot <- dat_screened %>% 
  ggplot(aes(x=response)) + 
  geom_density(fill="gray", alpha=.5) +
  facet_wrap(~IP, ncol=7) + 
  labs(title="Experiment 1 by-participant response distributions", 
       x="response, on 0-100 slider scale") + 
  theme_bw() + 
  theme(text=element_text(size=6), legend.position="top")


## 4.3 distribution of responses for each predicate (0-100 slider scale)
pred_plot_levels <- c(
  "tall", "late", "notTall", "notLate", "veryTall", "veryLate", 
  "notVeryTall", "notVeryLate", 
  "short", "early", "notShort", "notEarly", "neither", "onTime")

bypred_plot <- dat_screened %>% 
  # rearrange `Pred` levels for plot that is easier to inspect usefully 
  mutate(Pred = factor(Pred, levels=pred_plot_levels)) %>% 
  ggplot(aes(x=response, fill=Adj)) + 
  geom_density(alpha=.5) +
  facet_wrap(~Pred, dir="v", ncol=7) + 
  labs(title="Experiment 1 by-predicate response distributions", 
       x="response, on 0-100 slider scale") + 
  theme_bw() + 
  theme(text=element_text(size=11), legend.position="top")


## 4.4 save each of the plots (filenames from top of script)
ggsave(filename=files$age_plot_outname, plot=age_plot, 
       width=8, height=4, units="in")

ggsave(filename=files$bysubj_plot_outname, plot=bysubj_plot, 
       width=10, height=8, units="in")

ggsave(filename=files$bypred_plot_outname, plot=bypred_plot, 
       width=8, height=6, units="in")


