suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(dplyr))


### fuzzy logic functions -----------------------------------------------------

# pointwise-minimum definition of logical conjunction
fl_and <- function(var1, var2){
  stopifnot(length(var1) == length(var2))
  return(pmin(var1, var2, na.rm=TRUE))
}

# pointwise-maximum definition of logical disjunction (not used in paper)
fl_or <- function(var1, var2){
  stopifnot(length(var1) == length(var2))
  return(pmax(var1, var2, na.rm=TRUE))
}

# complement definition of logical negation 
fl_not <- function(var, scale_max=1){
  # `scale_max` can be set to 100 if computing over the raw agree-%'s 
  stopifnot(min(var, na.rm=TRUE) >= 0, max(var, na.rm=TRUE) <= scale_max)
  return(scale_max - var)
}


### quick utility funcs and string formatting funcs ---------------------------

# funcs for displaying pred names in plots with "camelCaseSpellingConvention"
adj_ <- function(A, case="lo"){
  ifelse(case=="lo", 
         paste0(tolower(substr(A, 1, 1)), substr(A, 2, nchar(A))),
         paste0(toupper(substr(A, 1, 1)), substr(A, 2, nchar(A))))
}
not_ <- function(A, sep=""){ paste("not", adj_(A, case="up"), sep=sep) }
very_ <- function(A, sep=""){ paste("very", adj_(A, case="up"), sep=sep) }
not_very_ <- function(A){ not_(adj_(very_(A), case="up")) }


# function for extracting legend from ggplot plot object (plobj) 
extract_legend <- function(gg_plobj){ 
  # source: https://stackoverflow.com/questions/
  #         12041042/how-to-plot-just-the-legends-in-ggplot2
  temp <- ggplot_gtable(ggplot_build(gg_plobj)) 
  leg <- which(sapply(temp$grobs, function(x) x$name) == "guide-box") 
  legend <- temp$grobs[[leg]] 
  return(legend)
} 



### functions for creating specific figures from paper ------------------------

# fig2: make a single plot panel 
fig2_component <- function(dat, A, which_legend="top"){
  
  # function to plot set of curves for adj A, given a df of expt1 results 
  # a surprisingly complicated process! (with mid-2017 ggplot2:: at least) 
  # example: fig2_component(dat=dat1, A="Tall")
  
  # requires that we use these column names
  needed_cols <- c("subj_id","adj","unit","pred","normunit","response")
  if (sum(names(dat) %in% needed_cols) < 6){
    message("cols need to include:\n  >> ", paste(names(dat), collapse=", "))
    return(NULL)
  }
  
  # only want to plot available values of `$adj` 
  dat <- droplevels(dat[dat$adj==A, ])
  
  antonym <- ifelse(A=="Tall", "short", ifelse(A=="Late", "early", NA))
  neither <- ifelse(A=="Tall", "neither", ifelse(A=="Late", "onTime", NA))
  
  pos_preds <- c(tolower(A), paste0("very", A), not_(antonym))
  neg_preds <- c(paste0(c("not","notVery"), A), tolower(antonym))
  
  # get means + bootstrap sem's for each scale point, predicate combo
  pdat <- dat %>% group_by(adj, pred, unit, normunit) %>% summarize(
    mean_response = mean(response), 
    se_response = sd(replicate(1000, mean(sample(response, replace=TRUE))))
  ) %>% ungroup() %>% 
    mutate(curve_shape = ifelse(pred %in% pos_preds, "increasing", ifelse(
      pred %in% neg_preds, "decreasing", "neither"
    ))) %>% 
    mutate(normunit = as.numeric(normunit))
  
  pdat_neither <- pdat[pdat$curve_shape=="neither", ] 
  
  if (which_legend=="top"){
    color_labels <- c(
      # A, very_(A), not_(antonym), neither
      "Adj", "very Adj", "not antonym", "neither")
    
  } else {
    if (which_legend=="bottom"){
      color_labels <- c(
        # not_(A), not_very_(A), antonym, neither 
        "not Adj", "not very Adj", "antonym", "neither")
      
    } else {
      color_labels <- c(
        paste0(A, "\nnot ", A, "   "),
        paste0("very ", A, "\nnot very ", A, "   "),
        paste0("not ", antonym, "   \n", antonym),
        paste0(neither, "   "))
    }
  }
  
  # put all the plotting data back together 
  pdat <- rbind(
    # want to have 'neither' preds on both increasing and decreasing panes 
    pdat_neither %>% mutate(curve_shape = "increasing"), 
    pdat_neither %>% mutate(curve_shape = "decreasing"), 
    # eliminate the original 'neither'-points (they get replaced by above) 
    pdat %>% filter(curve_shape != "neither")
  ) %>% 
    # want increasing to appear first, so code as a factor 
    mutate(curve_shape = factor(curve_shape, 
                                levels=c("increasing","decreasing"))) %>% 
    # assign colors to sets of predicates 
    mutate(color_cats = case_when(
      pred %in% c(adj_(A), not_(A))             ~ color_labels[1], 
      pred %in% c(very_(A), not_very_(A))       ~ color_labels[2], 
      pred %in% c(not_(antonym), adj_(antonym)) ~ color_labels[3], 
      pred %in% c(neither)                      ~ color_labels[4]
    )) %>% 
    # ensure they are ordered in legend as desired 
    mutate(color_cats=factor(color_cats, levels=color_labels))
  
  # TODO -- RESOLVE THIS + DELETE 
  #   or maybe these labels are clearer...
  #   c("no_very","very","antonym","neither") 
  #   c("tall","very_tall","not_short","neither") 
  
  # define color palette for plots 
  cols <- data_frame(
    color = c("orange", "blue", "aqua", "faint"),
    hex = c("#ff954f", "#1d30e0", "#43ad8d", "#bdbfbb"),
    pointline_hex = c("#ff954f", "#1d30e0", "#43ad8d", "transparent")
  )
  
  # define labels to appear in plot 
  inc_label <- paste0(
    "increasing predicates (", 
    paste(sapply(pos_preds, function(s) paste0("'",s,"'")), collapse=", "), ")"
    # " and ", ifelse(A=="Tall", "'neither tall nor short'", "'on time'")
  )
  dec_label <- paste0(
    "decreasing predicates (", 
    paste(sapply(neg_preds, function(s) paste0("'",s,"'")), collapse=", "), ")"
    # " and ", ifelse(A=="Tall", "'neither tall nor short'", "'on time'")
  )
  
  # get the appropriate x-axis label 
  x_label <- ifelse(A=="Tall","Height (inches)","Time from 9am (minutes)")
  
  # set desired ticks, then calculate breaks (since x-axis coded as numeric)
  if (A=="Tall"){
    # 7 ticks for 'tall' (distance from average height of 70in) 
    x_ticks <- seq(from=63, to=81, by=3) 
    x_breaks <- sapply(x_ticks, function(inches){ (inches-70) * 3 })
  }
  if (A=="Late"){
    # 8 ticks for 'late' (time (min) relative to 9am) 
    x_ticks <- seq(from=-20, to=50, by=10) 
    x_breaks <- x_ticks
  }
  
  # verify existence of 1-1 mapping from breaks to ticks 
  stopifnot(length(x_breaks) == length(x_ticks))
  
  # create the plot object `the_plot` (after labeling `$curve_shape`) 
  the_plot <- pdat %>% 
    mutate(curve_shape = 
             ifelse(curve_shape=="increasing", inc_label, dec_label)) %>%
    mutate(curve_shape = 
             factor(curve_shape, levels=c(inc_label, dec_label))) %>% 
    # filter(pred != "neither") %>% # uncomment to supress 'neither'/'on time'
    ggplot(aes(x=normunit, y=mean_response, group=pred, color=color_cats, 
               shape=color_cats)) + # cut out shape=color_cats to lose shape
    geom_point(size=rel(.95), alpha=.75) + 
    geom_line(alpha=.75, size=rel(.2)) + 
    facet_wrap(~curve_shape, nrow=2, scales="free_x") + 
    geom_ribbon(aes(ymin=mean_response - se_response, 
                    ymax=mean_response + se_response, fill=color_cats), 
                alpha=.25, color="transparent") + 
    scale_color_manual(values=cols$pointline_hex) + # or cols$hex for neither
    scale_fill_manual(values=cols$hex) + 
    scale_shape_manual(values=c(17,16,15,18)) + # comment out to lose shapes 
    scale_x_continuous(breaks=x_breaks, labels=x_ticks) + 
    scale_y_continuous(breaks=c(0,25,50,75,100),labels=c(0,25,50,75,100),
                       limits=c(0,100)) + 
    labs(x=x_label, y="% Agree", title=adj_(A)) + 
    theme(legend.position="right", legend.key.height=unit(.75,"lines"), 
          legend.key.size=unit(.75,"lines"), 
          legend.text=element_text(size=rel(.75)), 
          panel.grid.major=element_line(linetype="dashed", size=.25), 
          panel.spacing.y=unit(1, "lines"), 
          axis.title.y=element_text(hjust=.2),
          strip.background=element_rect(color="transparent", fill="#e5e5e5"),
          text=element_text(size=9), 
          strip.text=element_text(size=6), 
          plot.title=element_text(hjust=.5, face="italic")) 
  
  return(the_plot)
}





# fig3: make a single plot panel 
fig3_component <- function(dat, A){
  
  # set the labels for each line color 
  color_labels <- data_frame(
    color = c("orange", "blue"),
    hex = c("#ff954f", "#1d30e0"), 
    label = c("not Adj", "not very Adj")
  )
  
  # get means + bootstrap SEM's for each scale point + predicate combo
  pdat <- dat %>% 
    # just plotting 'Adj', 'notAdj', and 'notVeryAdj' 
    filter(adj==A, pred %in% c(not_(A), not_very_(A))) %>% 
    # for each value of `$adj`, `$pred`, `$unit`, and `$normunit`: 
    group_by(adj, pred, unit, normunit) %>% 
    # get the mean response and bootstrapped standard error of the mean (SEM) 
    summarize(
      mean_response = mean(response), 
      se_response = sd(replicate(1000, mean(sample(response, replace=TRUE))))
    ) %>% 
    ungroup() %>% 
    mutate(normunit = as.numeric(normunit)) %>% 
    # assign colors to curves as desired, then factorize for legend order 
    mutate(color_cats = case_when(
      pred == not_(A) ~ color_labels$label[1], 
      pred == not_very_(A) ~ color_labels$label[2]
    )) %>% 
    mutate(color_cats=factor(color_cats, levels=color_labels$label))
  
  # get the appropriate x-axis label 
  x_label <- ifelse(A=="Tall", "Height (inches)", "Time from 9am (minutes)")
  
  # set desired ticks, then calculate the breaks (since x-axis is numeric)
  if (A=="Tall"){
    # 7 ticks for 'tall' (distance from average height of 70in) 
    x_ticks <- seq(from=63, to=81, by=3) 
    x_breaks <- sapply(x_ticks, function(inches){ (inches-70) * 3 })
  }
  if (A=="Late"){
    # 8 ticks for 'late' (time (min) relative to 9am) 
    x_ticks <- seq(from=-20, to=50, by=10) 
    x_breaks <- x_ticks
  }
  
  # create the plot object 
  the_plot <- pdat %>% 
    ggplot(aes(x=normunit, y=mean_response, group=pred, color=color_cats, 
               shape=color_cats)) + # cut out shape=color_cats to lose shape
    geom_point(size=rel(.95), alpha=.75) + 
    geom_line(alpha=.75, size=rel(.2)) + 
    geom_ribbon(aes(ymin=mean_response - se_response, 
                    ymax=mean_response + se_response, fill=color_cats), 
                alpha=.25, color="transparent") + 
    scale_color_manual(values=color_labels$hex) + # or cols$hex for neither
    scale_fill_manual(values=color_labels$hex) + 
    scale_shape_manual(values=c(17,16)) + # comment to lose shapes # c(2,1,0,3)
    scale_x_continuous(breaks=x_breaks, labels=x_ticks) + 
    scale_y_continuous(breaks=c(0,25,50,75,100),labels=c(0,25,50,75,100),
                       limits=c(0,100)) + 
    labs(x=x_label, y="% Agree", title=adj_(A)) + 
    theme(legend.position="right", legend.key.height=unit(.75,"lines"), 
          legend.key.size=unit(.75,"lines"), 
          legend.text=element_text(size=rel(.75)), 
          panel.grid.major=element_line(linetype="dashed", size=.25), 
          panel.spacing.y=unit(1, "lines"), 
          # axis.title.y=element_text(hjust=.5),
          strip.background=element_rect(color="transparent", fill="#e5e5e5"),
          text=element_text(size=9), 
          plot.title=element_text(hjust=.5, face="italic")) 
  
  # final step: place clusters in their appropriate locations 
  if (A=="Late"){
    the_plot <- the_plot + 
      # 'Late' has a positive cluster and a negative cluster 
      geom_rect(aes(xmin=-21, xmax=0, ymin=0, ymax=100), 
                fill="red", alpha=.005, color="transparent") + 
      geom_rect(aes(xmin=2, xmax=36, ymin=0, ymax=100), 
                fill="green", alpha=.005, color="transparent")
  }
  if (A=="Tall"){
    # 'Tall' has only a positive cluster 
    the_plot <- the_plot + 
      geom_rect(aes(xmin=12, xmax=21, ymin=0, ymax=100), 
                fill="green", alpha=.005, color="transparent")
  }
  
  return(the_plot)
}


# fig 6/7: make a single plot panel 
fig6_fig7_component <- function(dat, inc_dec, legend_loc, x_label, 
                                fig7=FALSE, expt2_clusters=NULL){
  # NOTE: if `fig7=T`, must supply `expt2_clusters` df (see `paper_figures.r`) 
  
  # get the appropriate plot title and subset of the data 
  if (inc_dec=="increasing"){
    dat <- dat %>% filter(pred2 %in% c("Adj", "VeryAdj"))
    plot_title <- "increasing predicates ('Adj', 'very Adj')"
  }
  if (inc_dec=="decreasing"){
    dat <- dat %>% filter(pred2 %in% c("NotAdj", "NotVeryAdj"))
    plot_title <- "decreasing predicates ('not Adj', 'not very Adj')"
  }
  
  # TODO -- CHECK THIS 
  # plot palette -- orange, blue, aqua, purrp
  colorze <- c("#ff954f", "#1d30e0") #, "#43ad8d", "#e23f8b")
  
  # TODO -- CHECK THIS 
  pdat <- dat %>% 
    # hack to order factors right and have right-padding in legend
    group_by(adj, pred2, comparative, unit, normunit) %>% summarize(
      # get the n, mean, median, and bootstrapped sem of the responses
      num_obs = n(), 
      mean_response = mean(response), 
      se_response = sd(replicate(1000, mean(sample(response, replace=TRUE))))
    ) %>% ungroup()
  
  fac_levs <- c("Tall_positive", "Fast_positive", "Hot_positive", 
                "Tall_comparative", "Fast_comparative", "Hot_comparative")
  fac_labs <- c("tall", "fast", "hot", "taller than average", 
                "faster than average", "hotter than average")
  
  pdat <- pdat %>% 
    mutate(comparative = as.character(comparative)) %>% 
    mutate(adj_form = factor(paste(adj, comparative, sep="_"), 
                             levels=fac_levs, labels=fac_labs))
  
  # TODO -- POSSIBLE TO MOVE THIS DOWN LOW W THE OTHER FIG7-SPECIFIC STUFF?? 
  # for figure 7, join the data with the cluster information 
  if (fig7){
    pdat <- left_join(pdat %>% mutate(pred2 = as.character(pred2)), 
                      expt2_clusters, by=c("adj", "comparative", "pred2"))
  }
  
  # NB: nice confirmation w `color=adj` and `facet_grid(adj_type~construction)`
  the_plot <- pdat %>% 
    ggplot(aes(x=normunit, y=mean_response, color=pred2, shape=pred2)) + 
    # or: facet_wrap(comparative ~ adj, scales="free") + 
    facet_wrap(~adj_form, nrow=2, scales="free") + 
    geom_point(size=rel(.95), alpha=.75) + 
    geom_line(alpha=.75, size=rel(.2)) + 
    geom_ribbon(aes(ymin=mean_response - se_response, 
                    ymax=mean_response + se_response, fill=pred2), 
                alpha=.25, color="transparent") + 
    scale_color_manual(values=colorze) + 
    scale_fill_manual(values=colorze) + 
    scale_shape_manual(values=c(17,16)) + # comment out for no shapes # or15/18
    scale_x_continuous(breaks=seq(from=-6, to=12, by=3)) + 
    scale_y_continuous(breaks=c(0,25,50,75,100),labels=c(0,25,50,75,100),
                       limits=c(0,100)) + 
    # add the x-axis label here (if `fig7==TRUE`, will get overwritten below)
    labs(x=x_label, y="% Agree") + 
    theme(legend.position=legend_loc, legend.key.height=unit(.75,"lines"), 
          legend.key.size=unit(.75,"lines"), 
          legend.text=element_text(size=rel(.75)), 
          panel.grid.major=element_line(linetype="dashed", size=.25), 
          panel.spacing.y=unit(1, "lines"), 
          axis.title.y=element_text(hjust=.5),
          strip.text=element_text(face="italic"), 
          # strip.background=element_rect(color="transparent", fill="#e5e5e5"),
          # plot.title=element_text(hjust=.5, face="italic")
          text=element_text(size=9)) 
  
  # for figure 7, add geoms marking the significant cluster regions 
  if (fig7){
    the_plot <- the_plot + 
      geom_rect(aes(xmin=positive_lo, xmax=positive_hi, ymin=0, ymax=100), 
                fill="green", alpha=.0075, color="transparent", na.rm=TRUE) + 
      geom_rect(aes(xmin=negative_lo, xmax=negative_hi, ymin=0, ymax=100), 
                fill="red", alpha=.0075, color="transparent", na.rm=TRUE) + 
      labs(x=x_label)
  }
    
  return(the_plot)
}


