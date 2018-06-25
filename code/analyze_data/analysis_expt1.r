### EXPERIMENT 1: ANALYZE RESPONSE DATA #######################################
###  
### this script analyzes the response data from experiment 1: 
###   1. preprocess the data for cluster-permutation analysis (see secXXX); 
###   2. generate permutation distribution for 'not Adj' and 'not very Adj'; 
###   3. identify significant clusters; 
###   4. visualize clusters along the scales; 
###   5. check for effects of trial order (see fnXXXXXX in paper); 
###   6. reconstructed predicate analysis (see secXXXXXX in paper). 



### 0. read in data and load dependencies -------------------------------------
message("working on 0: read data and load deps...")

# `analysis_functions.r` loads the following functions:
#   >> `biggest.cluster(vect, threshold)`
#   >> `find.clusters(vect, threshold)`
#   >> `make.stats.lm(DV_vect, COND_vect, SUBJ_vect, CONT_vect)`
#   >> `make.stats.lmer(DV_vect, COND_vect, SUBJ_vect, CONT_vect)`
#   >> `MCsampling(DF, DV, COND, SUBJECT, CONT, threshold, N,verbose=F,mixed=T)`
source("analysis_functions.r")

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))


expt1_file <- "../../data/Expt1-data_cleaned_screened.csv"

outfiles <- list(
  expt1_cluster_plot = "expt1_output/expt1-cluster_plot.pdf", 
  expt1_perm_boxes = "expt1_output/expt1-perm_dist_boxplot.pdf",
  expt1_perm_dens = "expt1_output/expt1-perm_dist_density.pdf",
  expt1_perm_dist = "expt1_output/expt1-perm_dist.csv",
  expt1_recons_peaks = "expt1_output/expt1-recons_peaks.pdf",
  expt1_stats_tall = "expt1_output/expt1_observed-stats_tall.txt",
  expt1_stats_late = "expt1_output/expt1_observed-stats_late.txt", 
  perm_late_plot = "expt1_output/expt1-perm_clusters-late.png", 
  perm_tall_plot = "expt1_output/expt1-perm_clusters-tall.png")


dat <- read.csv(expt1_file, stringsAsFactors=FALSE)




### 1. preprocess the data ----------------------------------------------------
message("working on 1: preprocess the data...")

# scale responses by-participant 
dat <- dat %>% group_by(subj_id) %>% 
  mutate(response_scaled = scale(response)) %>% ungroup()


# subset to the two crucial conditions for 'tall', and center the 'Pred' factor
dat.tall <- dat %>% 
  filter(Adj=="Tall" & Pred %in% c("notTall", "notVeryTall")) %>% 
  mutate(Unit = as.numeric(Unit)) %>% 
  mutate(Pred = case_when(Pred == "notVeryTall" ~ .5, 
                          Pred == "notTall" ~ -.5, TRUE ~ NA_real_))

# subset to the two crucial conditions for 'late', and center the 'Pred' factor
dat.late <- dat %>% 
  filter(Adj=="Late" & Pred %in% c("notLate", "notVeryLate")) %>% 
  mutate(Pred = case_when(Pred == "notVeryLate" ~ .5, 
                          Pred == "notLate" ~ -.5, TRUE ~ NA_real_))





### 2. generate permutation distributions -------------------------------------
message("working on 2: generate permutation distributions...")

# draw `n_perms` samples from cluster size distro, set seed for reproducibility
n_perms <- 1e4
set.seed(6933)

# set to `TRUE` to run permutation analysis 
# (set to `FALSE` to skip sampling step and read samples from file)
run_analysis <- FALSE


# NOTE: analysis takes ~2hr to run! 
# NOTE: to avoid re-running, after distros generated, set `run_analysis=FALSE`
if (run_analysis){
  
  message("generating perm dists, using `", n_perms, "` random permutations...")
  start_time <- Sys.time()
  
  # for 'tall', use `$Unit` as the continuous dimension 
  perm_dist.tall <- MCsampling(DF=dat.tall, DV="response_scaled", COND="Pred",
                               SUBJECT="subj_id", CONT="Unit", 
                               threshold=2, N=n_perms, verbose=TRUE, mixed=TRUE)
  
  # for 'late', use `$NormUnit` as the continuous dimension 
  perm_dist.late <- MCsampling(DF=dat.late, DV="response_scaled", COND="Pred",
                               SUBJECT="subj_id", CONT="NormUnit", 
                               threshold=2, N=n_perms, verbose=TRUE, mixed=TRUE)
  
  # collect permutation distributions into a data frame for inspection 
  perm_dist <- dplyr::data_frame(
    iter = rep(1:n_perms, 2),
    adj  = rep(c("tall", "late"), each=n_perms),
    size = c(perm_dist.tall, perm_dist.late))
  
  write.csv(perm_dist, outfiles$expt1_perm_dist, row.names=FALSE)
  
  end_time <- Sys.time()
  mins_took <- as.numeric(round(difftime(end_time, start_time, units="min"), 1))
  message("perm dists took `", mins_took, "min` to build for expt 1")
  writeLines(paste0("perm dists took `", mins_took, "min` to build for expt 1"), 
             con="expt1_anal_output/time-expt1-permdist.txt")
  
} else {
  # analysis generates these objects, which we can recompute quickly from file 
  message("reading perm dist samples from `", outfiles$expt1_perm_dist, "`")
  perm_dist <- read.csv(outfiles$expt1_perm_dist, stringsAsFactors=FALSE)
  perm_dist.tall <- perm_dist$size[perm_dist$adj=="tall"]
  perm_dist.late <- perm_dist$size[perm_dist$adj=="late"]
}





### 3. summarize + visualize perm dists ---------------------------------------
# TODO -- TRIPLE CHECK INTER!!!! [was using 'tval' instead of 'size'!!]
message("working on 3: summarize and visualize perm dists...")

# summarize permutation distribution 
perm_dist %>% group_by(adj) %>% 
  summarize_at(vars(size), funs(min, median, mean, max)) %>% print()



# boxplots of distributions of largest sequence of significant clusters 
perm_boxplot_expt1 <- perm_dist %>% 
  ggplot(aes(x=adj, y=size)) + 
  geom_boxplot(notch=TRUE) + 
  # scale_y_log10() +
  labs(x="", y="size of biggest cluster (sum of t-values)", 
       title="Experiment 1 permutation distributions", 
       caption="(see Sec 3 of paper and 'analysis_expt1.r')") + theme_bw()

# density plots of distributions of largest sequence of significant clusters
perm_density_expt1 <- perm_dist %>% 
  ggplot(aes(x=size, fill=adj)) + 
  geom_density(alpha=.5, color="transparent") + 
  # scale_x_log10() + 
  labs(x="size of biggest cluster (sum of t-values)", 
       title="Experiment 1 permutation distributions", 
       subtitle="(see Sec 3 of paper and 'analysis_expt1.r')") + theme_bw()


if (run_analysis){
  ggsave(outfiles$expt1_perm_boxes, plot=perm_boxplot_expt1, 
         width=4, height=6, units="in")
  
  ggsave(outfiles$expt1_perm_dens, plot=perm_density_expt1, 
         width=7, height=5, units="in")
}





### 4. analyze results: check for significant clusters ------------------------
### TODO -- clean this section up!

### 4.1 Check clusters for 'tall' ---------------------------------------------

# get observed t-values at each scale point, for 'notAdj' versus 'notVeryAdj' 
stats.tall <- make.stats.lmer(
  DV_vect=dat.tall$response_scaled, COND_vect=dat.tall$Pred,
  SUBJ_vect=dat.tall$subj_id, CONT_vect=dat.tall$Unit)

# identify sequences of >1 significant scale points (in same direction)
C_Tall <- find.clusters(stats.tall$TVAL, threshold=2)

# get tval of biggest cluster, either in observed data or perm dist 
Max_y.tall <- max(
  biggest.cluster(stats.tall$TVAL, threshold=2), max(perm_dist.tall))


# recall correspondence btwn `$Unit` and `$NormUnit` (useful reminder point)
dat.tall %>% select(Unit, NormUnit) %>% unique() %>% arrange(Unit)



# plot the permutation distribution 
png(outfiles$perm_tall_plot)
plot(sort(perm_dist.tall), ylim=c(0, Max_y.tall), type='l')

# for each cluster identified:
for (cluster in C_Tall){
  
  # superimpose a red line denoting magnitude of significant cluster 
  abline(h=cluster[[2]], col="red")
  
  # get the cluster location 
  cluster_range <- sort(unique(dat.tall$Unit))[cluster[[1]]]
  
  # get the size of the cluster (sum of its constituent t-values)
  cluster_size <- cluster[[2]]
  
  # p-value is 1 - proportion of vals observed val is above  
  cluster_pval <- 1-ecdf(perm_dist.tall)(cluster_size)
  
  # for summarizing responses to 'not adj'/'not very adj' in cluster range
  clustrange_notTall_responses <- dat.tall %>% 
    filter(Pred==-.5) %>% filter(Unit %in% cluster_range) %>% 
    select(response) %>% unlist() %>% unname()
  
  clustrange_notVeryTall_responses <- dat.tall %>% 
    filter(Pred==.5) %>% filter(Unit %in% cluster_range) %>% 
    select(response) %>% unlist() %>% unname()
  
  
  # now going to write cluster info to text file 
  
  # header for output file 
  out <- c("Experiment 1: Cluster permutation analysis results",
           "'not tall' versus 'not very tall'",
           "=================================", "")
  
  # print some summary info 
  out <- c(
    out, 
    paste("Cluster range:", paste(cluster_range, collapse=" ")),
    paste("Cluster size:", round(cluster_size, digits=1)),
    paste("p-value:", round(cluster_pval, digits=5)),
    paste("Mean difference: ",
          round(mean(clustrange_notTall_responses)),      " (",
          round(sd(clustrange_notTall_responses)),        ") vs. ",
          round(mean(clustrange_notVeryTall_responses)),  " (",
          round(sd(clustrange_notVeryTall_responses)),    ")", sep=""))
  
  out <- c(out, "", "")
  
  write(out, outfiles$expt1_stats_tall, append=TRUE)
}
dev.off()
# expected result:
#   ## Experiment 1: Cluster permutation analysis results
#   ### 'not tall' versus 'not very tall'
#   Cluster range: 74 75 77
#   Cluster size: 8.2
#   p-value: 4e-04
#   Mean difference: 10 (13) vs. 16 (20)




### 4.2 Check clusters for 'late' ---------------------------------------------

# get observed t-values at each scale point, for 'notAdj' versus 'notVeryAdj' 
stats.late <- make.stats.lmer(
  DV_vect=dat.late$response_scaled, COND_vect=dat.late$Pred,
  SUBJ_vect=dat.late$subj_id, CONT_vect=dat.late$NormUnit)

# identify sequences of >1 significant scale points (in same direction)
C_Late <- find.clusters(stats.late$TVAL, threshold=2)

# get the biggest cluster, either in observed data or perm dist 
Max_y.late <- max(
  biggest.cluster(stats.late$TVAL, threshold=2), max(perm_dist.late))


# plot the permutation distribution 
png(outfiles$perm_late_plot)
plot(sort(perm_dist.late), ylim=c(0, Max_y.late), type='l')

# for each cluster identified:
for (cluster in C_Late){
  
  # superimpose a red line denoting magnitude of significant cluster 
  abline(h=cluster[[2]], col="red")
  
  # get the cluster location 
  cluster_range <- sort(unique(dat.late$Unit))[cluster[[1]]]
  
  # get the size of the cluster (sum of its constituent t-values)
  cluster_size <- cluster[[2]]
  
  # p-value is 1 - proportion of vals observed val is above  
  cluster_pval <- 1-ecdf(perm_dist.late)(cluster_size)
  
  # for summarizing responses to 'not adj'/'not very adj' in cluster range
  clustrange_notLate_responses <- dat.late %>% 
    filter(Pred==-.5) %>% filter(Unit %in% cluster_range) %>% 
    select(response) %>% unlist() %>% unname()
  
  clustrange_notVeryLate_responses <- dat.late %>% 
    filter(Pred==.5) %>% filter(Unit %in% cluster_range) %>% 
    select(response) %>% unlist() %>% unname()
  
  
  # now going to write cluster info to text file 
  
  # header for output file 
  out <- c("Experiment 1: Cluster permutation analysis results",
           "'not late' versus 'not very late'",
           "=================================", "")
  
  # print some summary info 
  out <- c(
    out, 
    paste("Cluster range:", paste(cluster_range, collapse=" ")),
    paste("Cluster size:", round(cluster_size, digits=1)),
    paste("p-value:", round(cluster_pval, digits=5)),
    paste("Mean difference: ",
          round(mean(clustrange_notLate_responses)),      " (",
          round(sd(clustrange_notLate_responses)),        ") vs. ",
          round(mean(clustrange_notVeryLate_responses)),  " (",
          round(sd(clustrange_notVeryLate_responses)),    ")", sep=""))
  
  out <- c(out, "", "")
  
  write(out, outfiles$expt1_stats_late, append=TRUE)
}
dev.off()
# expected result:
#   ## Experiment 1: Cluster permutation analysis results"
#   ### 'not late' versus 'not very late'"
#   Cluster range: 9:02 9:05 9:08 9:14 9:21 9:27 9:36" (= 2 5 8 14 21 27 36)
#   Cluster size: 50.4"
#   p-value: 0"
#   Mean difference: 20 (23) vs. 46 (34)"
# 
#   ## Experiment 1: Cluster permutation analysis results"
#   ### 'not late' versus 'not very late'"
#   Cluster range: 8:39 8:48 8:54 8:57 9:00" (=-21 -12 -6 -3 0)
#   Cluster size: 38.3"
#   p-value: 0"
#   Mean difference: 89 (16) vs. 59 (32)"





### 4. visualize the clusters -------------------------------------------------

# [code for cluster plot in separate script, not shown here bc is verbose]
# [creates basically the same plot as Figure 3 in the paper]
# [uncomment to run]
# message("working on 4: visualize the clusters...")
# source("cluster_plot_expt1.r")







### 5. check potential effects of trial order ---------------------------------
# TODO -- further cleanup/consolidation of repetitious code
message("working on 5: effects of trial order...")


# define a function that will analyze effects of trial order for 'tall'/'late'
order_effect_analysis <- function(dat, adj){
  
  ### load `parsimonious()`, implementing Bates et al (2015) algorithm
  source("http://semanticsarchive.net/Archive/GRhZmM4N/ParsimoniousMM.R")
  # requires package RePsychLing, install via:
  # devtools::install_github("dmbates/RePsychLing")
  
  ### sum-code predicate and order 
  dat.order <- dat %>% 
    filter(Adj==adj) %>% 
    mutate(Pred = factor(Pred)) %>% 
    mutate(Order = ifelse(Repetition==1, -.5, .5))
  
  # (sum-code pred w 7 levels -- `length(unique(dat.tall.order$Pred)) == 7`)
  contrasts(dat.order$Pred) <- contr.sum(length(unique(dat.order$Pred)))
  
  ### Collect chi2 for the comparison between a model which accounts for 
  ### order and its interactions with predicate, and a model that doesn't
  OrderEffect <- data.frame(ScalePoint=sort(unique(dat.order$Unit)))
  
  for (d in sort(unique(dat.order$Unit))){
    message("starting iteration for `", adj, "`, scale point `", d, "`...")
    
    mod0 <- parsimonious(lmer(
      response_scaled ~ Pred * Order + (Pred|subj_id) + (0+Repetition||subj_id),
      data=filter(dat.order, Unit==d), REML=FALSE))
    
    modRep <- update(mod0, data=model.frame(mod0))
    modNoRep <- update(modRep, .~.-Pred*Order+Pred)
    
    OrderEffect$Chi2[OrderEffect$ScalePoint==d] <- anova(
      modRep, modNoRep)[[6]][[2]]
    OrderEffect$Pval[OrderEffect$ScalePoint==d] <- anova(
      modRep, modNoRep)[[8]][[2]]
  }
  
  ### Bonferroni correction
  OrderEffect$CorrectedP <- length(unique(dat.order$Unit))*OrderEffect$Pval
  OrderEffect$CorrectedP <- length(unique(dat.order$Unit))*OrderEffect$Pval
  
  return(OrderEffect)
}



## apply the function to 'tall' and 'late' subsets of `dat`, inspect results
(order_effect_tall <- order_effect_analysis(dat, 'Tall'))
(order_effect_late <- order_effect_analysis(dat, 'Late'))

### expected results: 
# 
# > order_effect_tall
#    ScalePoint      Chi2       Pval
#         63    3.670486   0.81685181
#         66    6.904471   0.43889373
#         68   14.179245   0.04808362
#         69   11.974447   0.10140234
#         70    4.929055   0.66862073
#         71    8.868015   0.26227153
#         72    8.512343   0.28958766
#         73    3.365654   0.84924367
#         74    7.927049   0.33907062
#         75    4.698291   0.69672670
#         77    5.145134   0.64225699
#         79    7.764562   0.35381184
#         82   12.884556   0.07497161
# 
# 
# > order_effect_late
#    ScalePoint      Chi2        Pval
#         8:39   7.716648   0.358241417
#         8:48  12.080964   0.097927555
#         8:54  19.431019   0.006939133
#         8:57  11.930448   0.102869464
#         9:00  11.789729   0.107689248
#         9:02  11.137793   0.132720484
#         9:05   9.690708   0.138295849
#         9:08  15.984208   0.025261029
#         9:14   5.991454   0.540747604
#         9:21  10.741200   0.150312899
#         9:27   4.250161   0.750554947
#         9:36   7.444213   0.384137143
#         9:48   7.297369   0.398588373



# Inspect maximum effect:
message("maximum order effects for 'tall' and 'late', resp:\n")
print(order_effect_tall[which.min(order_effect_tall$Pval), ])
print(order_effect_late[which.min(order_effect_late$Pval), ])


### In the case of Late_8:54am, can check where the effects come from: 
#   
#   late_854 <- dat %>% 
#     filter(Adj=="Late") %>% 
#     filter(Unit=="8:54") %>% 
#     mutate(Order = ifelse(Repetition==1, -.5, .5)) %>% 
#     mutate(Pred = factor(Pred))
#   
#   contrasts(late_854$Pred) <- contr.sum(length(unique(late_854$Pred)))
#   
#   late_854$Pred <- factor(late_854$Pred, 
#                           levels=c("early","onTime","late","notLate",
#                                    "notEarly","veryLate","notVeryLate"))
#   
#   message("model summary for 'late', 8:54am:\n")
#   summary(parsimonious(lmer(
#     response_scaled~Pred*Repetition+(Pred|subj_id)+(0+Repetition||subj_id),
#     data=late_854, REML=FALSE)))
#   
#   rm(late_854)





### 6. post-hoc analysis: reconstructing [Adj] but not [very Adj] -------------
message("working on 6: analysis of reconstructed complex predicates...")

### demonstrate that we can model modifiers with algebraic ops
recons_preds <- c('tall', 'notTall', 'short', 'neither', 
                  'late', 'notLate', 'early', 'onTime')

# "Preliminary" ~~~> "dat.recon"
dat.recon <- dat %>% 
  filter(Pred %in% recons_preds) %>% 
  select(subj_id, Pred, Adj, NormUnit, Repetition, response) %>% 
  mutate(response = response/100) %>% 
  mutate(Pred2 = case_when(
    Pred %in% c("tall", "late") ~ "Adj", 
    Pred %in% c("notTall", "notLate") ~ "notAdj", 
    # TODO -- rename this to 'antAdj' (also use snake case instead of camel)
    Pred %in% c("short", "early") ~ "unAdj", 
    Pred %in% c("neither", "onTime") ~ "Neither", 
    TRUE ~ NA_character_))

dat.recon <- dat.recon %>% 
  group_by(NormUnit, Pred2, Adj) %>% 
  summarize(response = mean(response)) %>% ungroup()

# NOTE: `base::reshape()` doesnt play nice with dplyr tibbles!
#       so un-tibble before reshaping, and re-tibble later
dat.recon_wide <- data.frame(dat.recon) %>% 
  reshape(idvar=c("NormUnit","Adj"), timevar="Pred2", direction="wide") %>% 
  rename_all(function(x) gsub("response", "mean", x))
# strip tibble class attrs via `data.frame()` bc `reshape()` doesnt play nice


dat.recon_wide <- as_tibble(dat.recon_wide) %>% 
  mutate(negation_test = 1 - mean.Adj) %>% 
  mutate(zadeh_conj = pmin(1-mean.Adj, 1-mean.unAdj)) %>% 
  mutate(product_conj = (1-mean.Adj) * (1-mean.unAdj)) %>% 
  mutate(lukasiewicz_conj = pmax(0, mean.Adj + mean.unAdj - 1))

(dat.recon_wide$negation_test - dat.recon_wide$mean.notAdj) %>% 
  abs() %>% max()
dat.recon_wide %>% filter(Adj=="Tall") %>% {.$zadeh_conj-.$mean.Neither} %>%
  abs() %>% mean()
dat.recon_wide %>% filter(Adj=="Tall") %>% {.$product_conj-.$mean.Neither} %>% 
  abs() %>% mean()


# show that we can reconstruct meaning of `adj` using `1-mean(adj)`:
filter(dat.recon_wide, Adj=="Late") %>% {
  plot(mean.notAdj ~ NormUnit, data=., lty=1, ylim=c(0,1), type="b", col="red")
  lines(negation_test ~ NormUnit, data=., lty=2, pch=2, type="b", col="blue")
  legend(x=32, y=1, col=c("red","blue"), lty=c(1, 2), cex=.9, 
         legend=c("mean('not late')", "1 - mean('late')"))
}



# show 'neither tall nor short' versus constructed def'ns w 'tall' and 'short'
filter(dat.recon_wide, Adj=="Tall") %>% {
  plot(mean.Neither ~ NormUnit, data=., ylim=c(0,1), lty=1, type="b", col="red")
  lines(zadeh_conj ~ NormUnit,data=., lty=2, type="b", col="black")
  lines(product_conj ~ NormUnit, data=., lty=2, col="blue", type="b")
  lines(lukasiewicz_conj ~ NormUnit, data=., lty=2, col="darkgreen", type="b")
  legend(x=22, y=1, col=c("red","black","blue","darkgreen"), 
         lty=c(1, 2), cex=.9, 
         legend=c("mean('neither')", "zadeh", "product", "lukasiewicz"))
}


### reconstructing 'Adj but not very Adj' 
recons_preds2 <- c("tall", "notTall", "veryTall", 
                   "late", "notLate", "veryLate")

dat.recon2 <- dat %>% 
  filter(Pred %in% recons_preds2) %>% 
  select(subj_id, Pred, Adj, NormUnit, Repetition, response) %>% 
  mutate(response = response/100) %>% 
  mutate(Pred2 = case_when(
    Pred %in% c("tall", "late") ~ "Adj", 
    Pred %in% c("notTall", "notLate") ~ "notAdj", 
    Pred %in% c("veryTall", "veryLate") ~ "veryAdj", 
    TRUE ~ NA_character_))

dat.recon2 <- dat.recon2 %>% 
  group_by(subj_id, NormUnit, Pred2, Adj) %>% 
  summarize(response = mean(response)) %>% ungroup()

dat.recon2_wide <- data.frame(dat.recon2) %>% 
  reshape(idvar=c("subj_id","NormUnit","Adj"), 
          timevar="Pred2", direction="wide") %>%
  rename_all(function(x) gsub("response", "mean", x))

dat.recon2_wide <- as_tibble(dat.recon2_wide) %>% 
  mutate(pseudo_implic = pmin(mean.Adj, 1-mean.veryAdj)) %>% 
  mutate(pseudo_neither = pmin(mean.Adj, mean.notAdj))

# TODO -- GET RID OF 'TMP' 
tmp <- dat.recon2_wide %>% 
  reshape2::melt(id.var=c("subj_id","Adj","NormUnit")) %>% 
  rename(Pred2=variable, response=value) %>% 
  mutate(Pred2 = as.character(Pred2)) %>% 
  as_tibble()

peak_data <- tmp %>% 
  filter(Pred2 %in% c("pseudo_implic", "pseudo_neither")) %>% 
  filter(NormUnit >= -20 & NormUnit <= 30) %>% 
  group_by(subj_id, Pred2, Adj) %>% 
  summarize(max.response = max(response)) %>% ungroup() %>% 
  mutate(max.response = scale(max.response)) %>% 
  mutate(Pred2 = factor(Pred2, levels=c("pseudo_neither", "pseudo_implic"))) %>%
  mutate(Adj = factor(Adj, levels=c("Tall", "Late")))

peak_fit <- lme4::lmer(max.response ~ Adj * Pred2 + (1|subj_id), 
                       data=peak_data, REML=FALSE)
summary(peak_fit)

peak_fit.update <- update(peak_fit, .~.-Adj:Pred2)
anova(peak_fit, peak_fit.update)

# peak_model_results <- 
as_data_frame(summary(peak_fit)$coefficients) %>% 
  mutate(term = dimnames(summary(peak_fit)$coefficients)[[1]]) %>% 
  select(term, coef=Estimate, std_err=`Std. Error`, t_val=`t value`) %>% 
  mutate(p_val = 2*(1-pnorm(abs(t_val))))


peak_bysubj <- tmp %>% 
  filter(Pred2 == "pseudo_implic") %>% 
  group_by(subj_id, Adj) %>% 
  summarize(place = NormUnit[which.max(response)]) %>% ungroup()

(table(peak_bysubj$Adj, peak_bysubj$place, useNA="ifany"))

# [creates plot with same info as Figure 4]
subj_peaks_plot <- peak_bysubj %>% 
  group_by(Adj, place) %>% 
  count() %>% ungroup() %>% 
  ggplot(aes(x=place, y=n)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~Adj) + 
  geom_vline(aes(xintercept=0), color="blue", linetype="dashed", alpha=.5) + 
  labs(x="location on common scale", 
       y="count", 
       caption=paste("blue line indicates 9am (late), 70in (tall)", 
                     "1 scale unit = 1in (tall) = 3min (late)", sep="\n"),
       subtitle=paste0("By-subj peak values for ", 
                    "reconstructed predicate 'Adj but not very Adj'")) + 
  theme(plot.caption=element_text(hjust=0, size=10)) + theme_bw()

ggsave(outfiles$expt1_recons_peaks, subj_peaks_plot, 
       width=7, height=5, units="in")


