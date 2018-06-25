### EXPT 1 FIGS -- not very late, not very tall ###############################
###   - figure 1: expt 1 sample displays [not created here]
###   - figure 2: curves for all predicates 
###   - figure 3: comparison of 'not Adj' and 'not very Adj', w cluster analysis
###   - figure 4: reconstructed curves 'Adj and not Adj', 'Adj but not very Adj'



### setup ---------------------------------------------------------------------

# load dependencies + set global plot theme 
lefftpack::lazy_setup(set_plot_theme=TRUE, font_size=11, sparse_grid=TRUE)
lefftpack::quiet_attach("gridExtra")

# load functions to be used for building some of the figures 
# (also has fuzzy logic funcs, for now at least)
source("figures_functions.r")

# TODO -- ONLY WANT TO HAVE TO DEFINE THESE ONCE! PUT IN FUNC MAYBE??? 
# set line labels/colors for reconstructed predicate plots (figures 4 and 8)
line_colors <- data_frame(
  pred = c("adj/[not adj]", "[very adj]/[not very adj]"), 
  color = c("orange", "blue"), 
  hex = c("#ff954f", "#1d30e0")
)
line_labels <- c("[Adj] and [not Adj]    ", "[Adj] but [not [very Adj]]")

# set this to true to build all figs in one shot 
save_figs <- TRUE
figure_save_path <- "../../figures"



expt1_clean_data_fname <- "../../data/Expt1-data_cleaned_screened.csv"
message("using experiment 1 data from file:\n  >> ", expt1_clean_data_fname)

dat <- read.csv(expt1_clean_data_fname, stringsAsFactors=FALSE)
names(dat) <- tolower(names(dat))

lapply(dat %>% select(-subj_id, -response), unique)
# `$adj` ~~> Tall, Late
# `$pred` ~~> 14 vals, 7 for each adj
# `$unit` ~~> 26 vals, 13 for each adj 
# `$normunit` ~~> 18 vals 



### figure 2 ------------------------------------------------------------------
message("working on figure 2...")

# mean response curves for all the preds, w two panes for each adj:
# 
#   - adj, very-adj, not-ant-adj (colored), neither (light)
#   - not-adj, not-very-adj, ant-adj (colored), neither (light)
pane_tall <- fig2_component(dat, A="Tall", which_legend="top")
pane_late <- fig2_component(dat, A="Late", which_legend="bottom")

legend_top <- extract_legend(pane_tall)
legend_bot <- extract_legend(pane_late)

pane_tall <- pane_tall + theme(legend.position="none")
pane_late <- pane_late + theme(legend.position="none")

figure2 <- grid.arrange(arrangeGrob(
  pane_tall, arrangeGrob(legend_top, legend_bot, nrow=2), pane_late, 
  ncol=3, widths=c(7, 2, 7)
))


### figure 3 ------------------------------------------------------------------
message("working on figure 3...")

# mean response curves for notAdj vs notVeryAdj, w one pane for each adj: 
#   - lines for notAdj + notVeryAdj
#   - cluster rectangles where significant 

pane_tall <- fig3_component(dat, A="Tall")
pane_late <- fig3_component(dat, A="Late")

fig3_legend <- extract_legend(pane_tall) # or pane_late, doesnt matter

pane_tall <- pane_tall + theme(legend.position="none")
pane_late <- pane_late + theme(legend.position="none")

figure3 <- grid.arrange(arrangeGrob(
  pane_tall, fig3_legend, pane_late, 
  ncol=3, widths=c(7, 2, 7)
))


### figure 4 ------------------------------------------------------------------
message("working on figure 4...")

# reconstructed predicates, w one pane for each adj. lines for:  
#   - adj and not very adj
#   - adj and not      adj  

# x-axis label for fig 4
x_label <- "Normalized Units (0 = 70in = 9:00am; 1 unit = 1in = 3min)"

# predicates needed to compute figure 4 curves 
relevant_preds <- c(
  "tall","notTall","veryTall","notVeryTall",
  "late","notLate","veryLate","notVeryLate")

# get mean responses by subj, scale point, pred, and adj 
dat1_subj_item_summary <- dat %>% 
  filter(pred %in% relevant_preds) %>% 
  select(subj_id, pred, adj, normunit, repetition, response) %>% 
  mutate(pred2 = case_when(
    pred %in% c("tall", "late") ~ "Adj", 
    pred %in% c("notTall", "notLate") ~ "notAdj", 
    pred %in% c("veryTall", "veryLate") ~ "veryAdj",
    pred %in% c("notVeryTall", "notVeryLate") ~ "notVeryAdj"
  )) %>% 
  mutate(response01 = response / 100) %>% 
  group_by(subj_id, normunit, pred2, adj) %>% summarize(
    mean_resp01 = mean(response01)
  ) %>% ungroup() %>% 
  dcast(subj_id + normunit + adj ~ pred2, value.var="mean_resp01")


# define the reconstructed interpretations for 'adj but not very adj' 
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

# build figure 4, after uniformizing the scales 
# (plots lines for 'Adj and not Adj', 'Adj and not very Adj')
figure4 <- dat1_reconstructed %>% 
  mutate(adj = tolower(adj)) %>% 
  # force pane layout to match other plots ('tall' on left, 'late' on right)
  mutate(adj = factor(adj, levels=c("tall", "late"))) %>% 
  # 1 norm unit = 1 inch = 3 minutes 
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
                     labels=c("0",".25",".50",".75","1"),
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






### save figures (if `save_figs=T`) -------------------------------------------

if (save_figs){
  message("writing experiment 1 figures to disk at `", figure_save_path, "`...")
  # save figure 2 
  ggsave(plot=figure2, filename=file.path(figure_save_path, "figure2.pdf"), 
         width=6.5, height=4, units="in")
  
  # save figure 3 
  ggsave(plot=figure3, filename=file.path(figure_save_path, "figure3.pdf"), 
         width=6.5, height=2.5, units="in")
  
  # save figure 4 
  ggsave(plot=figure4, filename=file.path(figure_save_path, "figure4.pdf"), 
         width=6.5, height=3, units="in")
}


