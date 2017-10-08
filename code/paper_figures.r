### CODE FOR FIGURES APPEARING IN 
###   'Vagueness in Implicature', Leffel, Cremers, Romoli, Gotzner (2017)

# EXPERIMENT 1 -- 'not very late', 'not very tall' 
#   figure 1: sample displays
#   figure 2: curves for all predicates 
#   figure 3: comparison of 'not Adj' and 'not very Adj', with cluster analysis 
#   figure 4: reconstructed curves, 'Adj and not Adj', 'Adj but not very Adj'

# EXPERIMENT 2 -- 'not very tall/hot/fast','not much taller/faster/hotter'
#   figure 5: sample displays 
#   figure 6: curves for increasing preds (Adj, very Adj)
#   figure 7: curves for decreasing preds, (not Adj, not very Adj) with clusters 
#   figure 8: reconstructed curves, 'Adj and not Adj', 'Adj but not very Adj'



### SETUP ---------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# load dependencies + set global plot theme 
lefftpack::lazy_setup(set_plot_theme=TRUE, font_size=11, sparse_grid=TRUE)
lefftpack::quiet_attach("gridExtra")

# load functions to be used for building some of the figures 
source("paper_figures_functions.r")

# set line labels/colors for reconstructed predicate plots (figures 4 and 8)
line_colors <- data_frame(
  pred = c("adj/[not adj]", "[very adj]/[not very adj]"), 
  color = c("orange", "blue"), 
  hex = c("#ff954f", "#1d30e0")
)
line_labels <- c("[Adj] and [not Adj]    ", "[Adj] but [not [very Adj]]")

# set this to true to build all figs in one shot 
save_figs <- TRUE
figure_save_path <- "../figures/"


### EXPERIMENT 1 FIGURES ------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# prep data by running first part of `Experiment1-clean.R`
# (up until "Graph all conditions", then save rda for quick loading)
# 
# this does the following: 
#   - read data 
#   - remove questionnaire stuff 
#   - organize data into form that reflects design 
#   - define a common scale for "tall" and "late" 
#   - exclude subj who did the expt twice: 
#       >> "a59cea6452b996807188aca9fbf059df"
#   - exclude subj's w non-native english lang: 
#       >> "df936b62260895f67470da8f084f930e" 
#   - exclude subj's performing below chance (none for expt1): 

# JUST RUN BOTH TILL NEEDED + THEN MAKE PLOTS -- CLEAN UP AFTER PAPER RESUBBED
load("e1-temp.rda")

# "questionnaire" is still in there for pred...levels(dat1$Pred)
dat1 <- dat1 %>% select(IP, Adj, Unit, Pred, NormUnit, Repetition, response) %>%
  set_names(tolower(names(.))) %>% 
  filter(pred != "questionnaire") %>% 
  mutate_if(is.factor, as.character)

lapply(dat1 %>% select(-ip, -response), unique)
# `$adj` ~~> Tall, Late
# `$pred` ~~> 14 vals, 7 for each adj
# `$unit` ~~> 26 vals, 13 for each adj 
# `$normunit` ~~> 18 vals 



### figure 2 ------------------------------------------------------------------
# -----------------------------------------------------------------------------
# mean response curves for all the preds, w two panes for each adj:
# 
#   - adj, very-adj, not-ant-adj (colored), neither (light)
#   - not-adj, not-very-adj, ant-adj (colored), neither (light)
pane_tall <- fig2_component(dat=dat1, A="Tall", which_legend="top")
pane_late <- fig2_component(dat=dat1, A="Late", which_legend="bottom")

legend_top <- extract_legend(pane_tall)
legend_bot <- extract_legend(pane_late)

pane_tall <- pane_tall + theme(legend.position="none")
pane_late <- pane_late + theme(legend.position="none")

figure2 <- grid.arrange(arrangeGrob(
  pane_tall, arrangeGrob(legend_top, legend_bot, nrow=2), pane_late, 
  ncol=3, widths=c(7, 2, 7)
))

if (save_figs){
  ggsave(plot=figure2, filename=paste0(figure_save_path, "figure2.pdf"), 
         width=6.5, height=4, units="in")
}



### figure 3 ------------------------------------------------------------------
# -----------------------------------------------------------------------------
# mean response curves for notAdj vs notVeryAdj, w one pane for each adj: 
#   - lines for notAdj + notVeryAdj
#   - cluster rectangles where significant 

pane_tall <- fig3_component(dat=dat1, A="Tall")
pane_late <- fig3_component(dat=dat1, A="Late")

fig3_legend <- extract_legend(pane_tall) # or pane_late, doesnt matter

pane_tall <- pane_tall + theme(legend.position="none")
pane_late <- pane_late + theme(legend.position="none")

figure3 <- grid.arrange(arrangeGrob(
  pane_tall, fig3_legend, pane_late, 
  ncol=3, widths=c(7, 2, 7)
))

if (save_figs){
  ggsave(plot=figure3, filename=paste0(figure_save_path, "figure3.pdf"), 
         width=6.5, height=2.5, units="in")
}



### figure 4 ------------------------------------------------------------------
# -----------------------------------------------------------------------------
# reconstructed predicates, w one pane for each adj. lines for:  
#   - adj and not very adj
#   - adj and not      adj  
relevant_preds <- c(
  "tall","notTall","veryTall","notVeryTall",
  "late","notLate","veryLate","notVeryLate"
)

dat1_subj_item_summary <- dat1 %>% filter(pred %in% relevant_preds) %>% 
  select(ip, pred, adj, normunit, repetition, response) %>% 
  mutate(pred2 = case_when(
    pred %in% c("tall", "late") ~ "Adj", 
    pred %in% c("notTall", "notLate") ~ "notAdj", 
    pred %in% c("veryTall", "veryLate") ~ "veryAdj",
    pred %in% c("notVeryTall", "notVeryLate") ~ "notVeryAdj"
  )) %>% 
  mutate(response01 = response / 100) %>% 
  group_by(ip, normunit, pred2, adj) %>% summarize(
    mean_resp01 = mean(response01)
  ) %>% ungroup() %>% 
  dcast(ip + normunit + adj ~ pred2, value.var="mean_resp01")

dat1_reconstructed <- dat1_subj_item_summary %>% 
  mutate(
    ANA_reconstructed = fl_and(Adj, notAdj), 
    # THIS DEFINITION USES '1 - VERY ADJ' TO DEFINE 'NOT VERY ADJ'
    ANVA_reconstructed = fl_and(Adj, fl_not(veryAdj))
    # FOR REFERENCE, THIS USES THE ACTUAL RESPONSES FOR 'NOT VERY ADJ' 
    # ANVA_reconstructed = fl_and(Adj, notVeryAdj), 
  ) %>% group_by(normunit, adj) %>% summarize(
    ANVA_mean=mean(ANVA_reconstructed), 
    ANVA_se=sd(replicate(1000, mean(sample(ANVA_reconstructed, replace=TRUE)))),
    ANA_mean=mean(ANA_reconstructed), 
    ANA_se=sd(replicate(1000, mean(sample(ANA_reconstructed, replace=TRUE))))
  ) %>% ungroup() %>% 
  melt(id.vars=c("normunit","adj")) %>% 
  mutate(variable = as.character(variable)) %>% 
  mutate(recon_pred = case_when(
    grepl("ANVA_", variable) ~ "ANVA", 
    grepl("ANA_", variable)  ~ "ANA"
  )) %>% 
  mutate(variable = gsub("ANVA_|ANA_", "", variable)) %>% 
  dcast(normunit + adj + recon_pred ~ variable, value.var="value") 

# x label for fig 4
x_label <- "Normalized Units (0 = 70in = 9:00am; 1 unit = 1in = 3min)"

figure4 <- 
  dat1_reconstructed %>% 
  mutate(adj = tolower(adj)) %>% 
  mutate(normunit = normunit / 3) %>% 
  mutate(recon_pred = factor(recon_pred, levels=c("ANA", "ANVA"), 
                             labels=line_labels)) %>% 
  ggplot(aes(x=normunit, y=mean, color=recon_pred, shape=recon_pred)) + 
  geom_point(size=rel(.95), alpha=.75) + 
  geom_line(alpha=.75, size=rel(.2)) + 
  facet_wrap(~adj, scales="free") + 
  geom_ribbon(aes(ymin=mean - se, ymax=mean + se, fill=recon_pred), 
              alpha=.25, color="transparent") + 
  scale_color_manual(values=line_colors$hex) + # or cols$hex for neither
  scale_fill_manual(values=line_colors$hex) + 
  # comment this out to lose shapes: 
  scale_shape_manual(values=c(17,16)) + 
  scale_x_continuous(breaks=seq(from=-6, to=15, by=3)) + 
  scale_y_continuous(breaks=c(0,.25,.5,.75,1),
                     labels=c("0",".25",".5",".75","1"),
                     limits=c(0,1)) + 
  labs(x=x_label, y="% Agree, scaled to [0, 1]") + 
  theme(legend.position="top", legend.key.height=unit(.75,"lines"), 
        legend.key.size=unit(.75,"lines"), 
        legend.text=element_text(size=rel(.75)), 
        panel.grid.major=element_line(linetype="dashed", size=.25), 
        panel.spacing.y=unit(1, "lines"), 
        strip.text=element_text(face="italic", size=11), 
        text=element_text(size=9),
        plot.title=element_text(hjust=.5, face="italic"))

if (save_figs){
  ggsave(plot=figure4, filename=paste0(figure_save_path, "figure4.pdf"), 
         width=6.5, height=3, units="in")
}




### EXPERIMENT 2 FIGURES ------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# prep data by running first part of `Experiment2-clean.R`
# (up until "Graph all conditions", then save rda for quick loading)
# 
# this does the following: 
#   - read data 
#   - remove questionnaire stuff 
#   - organize data into form that reflects design 
#   - define a common scale for "tall" and "late" 
#   - exclude subj's w non-native english lang (none for expt2)
#   - exclude subj's performing below chance (see expt2 script for measure): 
#       >> "92e0620c15a0e3ce832d22e96830718f"
#       >> "bc6d28279ff167c0ada606823949320b"

# JUST RUN BOTH TILL NEEDED + THEN MAKE PLOTS -- CLEAN UP AFTER PAPER RESUBBED
load("e2-temp.rda")
dat2 <- dat2 %>% 
  select(IP, Adj, Unit, Pred, Pred2, NormUnit, Comparative, response) %>% 
  set_names(tolower(names(.))) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(pred2 = 
           factor(pred2, levels=c("Adj","NotAdj","VeryAdj","NotVeryAdj"))) %>% 
  mutate(comparative = ifelse(comparative==TRUE, "comparative", "positive")) %>% 
  mutate(comparative = factor(comparative, levels=c("positive", "comparative")))

lapply(dat2 %>% select(-ip, -response), unique)
# `$adj` ~~> Tall, Fast, Hot
# `$pred` ~~> 24 vals, 4 for each construction
# `$pred2` ~~> 4 vals, Adj, NotAdj, VeryAdj, NotVeryAdj
# `$unit` ~~> 35 vals 
# `$normunit` ~~> 20 vals 
# `$comparative` ~~> "comparative", "positive"  

# say how some stuff will get displayed in the plot 
pred_levels <- c("tall_positive", "fast_positive", "hot_positive", 
                 "tall_comparative", "fast_comparative", "hot_comparative")
pred_labels <- c("tall", "fast", "hot", "taller than average", 
                 "faster than average", "hotter than average")
x_label <- paste0("Normalized Units ", 
                  "(0 = 113mph = 83ºF = 70in; 1 unit = 4mph = 2.5ºF = 1in)")




### figure 6 ------------------------------------------------------------------
# -----------------------------------------------------------------------------
# mean response curves for "increasing" preds, six panes total: 
#   - cols for Tall, Hot, Fast
#   - rows for positive and comparative 
#   - will be only two lines per pane 

figure6 <- fig6_fig7_component(
  dat=dat2, inc_dec="increasing", legend_loc="right", x_label=x_label
)

if (save_figs){
  ggsave(plot=figure6, filename=paste0(figure_save_path, "figure6.pdf"), 
         width=6.5, height=3.25, units="in")
}

# also build a fig with all the curves, for quick comparison
pane_inc <- fig6_fig7_component(
  dat=dat2, inc_dec="increasing", legend_loc="top", x_label=x_label
)
pane_dec <- fig6_fig7_component(
  dat=dat2, inc_dec="decreasing", legend_loc="top", x_label=x_label
)
figure6_allpreds <- grid.arrange(arrangeGrob(
  pane_inc, pane_dec, ncol=2, widths=c(7, 7)
))

if (save_figs){
  ggsave(plot=figure6_allpreds, 
         filename=paste0(figure_save_path, "figure6_allpreds.pdf"), 
         width=6.5, height=3.25, units="in")
}



### figure 7 ------------------------------------------------------------------
# -----------------------------------------------------------------------------
# mean response curves for notAdj + notVeryAdj, six panes total: 
#   - same format as fig5
#   - cluster rectangles where significant 

expt2_clusters <- data_frame(
  adj = rep(c("Fast", "Hot", "Tall"), each=2), 
  comparative = rep(c("positive", "comparative"), times=3), 
  pred2 = rep(c("NotAdj", "NotVeryAdj"), each=3), 
  positive_lo = c(NA, 1, NA, .8, NA, 1), 
  positive_hi = c(NA, 4, NA, 2.8, NA, 5), 
  negative_lo = c(NA, -7, NA, -6.8, NA, -7), 
  negative_hi = c(NA, 1, NA, 0, NA, 0)
) 
# pdat %>% select(adj, unit, normunit) %>% unique() %>% View()
# NEGATIVE CLUSTERS
# faster: 85 (-7) to 109 (-1)
# hotter: 66 (-6.8) to 83 (0)
# taller: 5'3"/63 (-7) to 5'10"/70 (0)
# 
# POSITIVE CLUSTERS 
# faster: 117 (1) to 129 (4)
# hotter: 85 (.8) to 90 (2.8)
# taller: 5'11"/71 (1) to 6'3"/75 (5)

figure7 <- fig6_fig7_component(
  dat=dat2, inc_dec="decreasing", legend_loc="right", x_label=x_label, 
  fig7=TRUE, expt2_clusters=expt2_clusters
)

if (save_figs){
  ggsave(plot=figure7, filename=paste0(figure_save_path, "figure7.pdf"), 
         width=6.5, height=3.25, units="in")
}



### figure 8 ------------------------------------------------------------------
# -----------------------------------------------------------------------------
# reconstructed predicates, six panes total: 
#   - same grid format as fig5/6
#   - curve for [adj and not very adj] and [adj and not      adj]

# wrangle the data and compute values for the reconstructed predicates 
dat2_wide <- dat2 %>% 
  select(pred, pred2, adj, normunit, comparative, response) %>% 
  filter(pred2 %in% c("VeryAdj", "NotAdj", "Adj")) %>% 
  mutate(response01 = response / 100) %>% 
  group_by(normunit, comparative, pred2, adj) %>% summarize(
    mean_resp01 = mean(response01), 
    mean_response = mean(response)
  ) %>% ungroup() %>% 
  dcast(normunit + comparative + adj ~ pred2, value.var="mean_response") %>% 
  mutate(
    ANVA_reconstructed = fl_and(Adj, fl_not(VeryAdj, scale_max=100)), 
    ANA_reconstructed = fl_and(Adj, NotAdj) 
  )

# get the mean and se for each reconstructed pred, at each scale point/pred 
dat2_reconstructed <- dat2_wide %>% 
  group_by(normunit, adj, comparative) %>% summarize(
    ANVA_mean=mean(ANVA_reconstructed), 
    ANVA_se=sd(replicate(100, mean(sample(ANVA_reconstructed, replace=TRUE)))),
    ANA_mean=mean(ANA_reconstructed), 
    ANA_se=sd(replicate(100, mean(sample(ANA_reconstructed, replace=TRUE))))
  ) %>% ungroup() %>% 
  melt(id.vars=c("normunit","adj","comparative")) %>% 
  mutate(variable = as.character(variable)) %>% 
  mutate(recon_pred = case_when(
    grepl("ANVA_", variable) ~ "ANVA", 
    grepl("ANA_", variable)  ~ "ANA"
  )) %>% 
  mutate(variable = gsub("ANVA_|ANA_", "", variable)) %>% 
  dcast(normunit + adj + recon_pred + comparative ~ variable, value.var="value") 

# rescale the response variable to [0, 1], and prep for plotting
dat2_reconstructed <- dat2_reconstructed %>% 
  mutate(mean = mean / 100) %>% 
  mutate(se = se / 100) %>% 
  mutate(adj = tolower(adj)) %>% 
  mutate(recon_pred = factor(recon_pred, levels=c("ANA", "ANVA"), 
                             labels=line_labels)) %>% 
  mutate(comparative = as.character(comparative)) %>% 
  mutate(adj_form = factor(paste(adj, comparative, sep="_"), 
                           levels=pred_levels, labels=pred_labels))

# build the plot and assign it to `figure8`
figure8 <- dat2_reconstructed %>% 
  ggplot(aes(x=normunit, y=mean, color=recon_pred, shape=recon_pred)) + 
  geom_point(size=rel(.95), alpha=.75) + 
  geom_line(alpha=.75, size=rel(.2)) + 
  facet_wrap(~adj_form, scales="free") + 
  geom_ribbon(aes(ymin=mean - se, ymax=mean + se, fill=recon_pred), 
              alpha=.25, color="transparent") + 
  scale_color_manual(values=line_colors$hex) + # or cols$hex for neither
  scale_fill_manual(values=line_colors$hex) + 
  scale_shape_manual(values=c(17,16)) + 
  scale_x_continuous(breaks=seq(from=-6, to=15, by=3)) + 
  scale_y_continuous(breaks=c(0,.25,.5,.75,1),
                     labels=c("0",".25",".5",".75","1"),
                     limits=c(0,1)) + 
  labs(x=x_label, y="% Agree, scaled to [0, 1]") + 
  theme(legend.position="top", legend.key.height=unit(.75,"lines"), 
        legend.key.size=unit(.75,"lines"), 
        legend.text=element_text(size=rel(.75)), 
        panel.grid.major=element_line(linetype="dashed", size=.25), 
        panel.spacing.y=unit(1, "lines"), 
        strip.text=element_text(face="italic"), 
        text=element_text(size=9),
        plot.title=element_text(hjust=.5, face="italic"))

if (save_figs){
  ggsave(plot=figure8, filename=paste0(figure_save_path, "figure8.pdf"), 
         width=6.5, height=3.75, units="in")
}

