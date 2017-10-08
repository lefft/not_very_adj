
# quick string formatting functions for commonly used expressions 
# NOTE: `lefftpack::str_pos(string, n)` gets the `n`th character of `string` 
adj_ <- function(A, case="lo"){
  ifelse(case=="lo", 
         paste0(tolower(str_pos(A, 1)), str_pos(A, 2:nchar(A))), 
         paste0(toupper(str_pos(A, 1)), str_pos(A, 2:nchar(A))))
}

not_ <- function(A, sep="") paste("not", adj_(A, case="up"), sep=sep)

very_ <- function(A, sep="") paste("very", adj_(A, case="up"), sep=sep)

not_very_ <- function(A) not_(adj_(very_(A), case="up"))


### function for extracting legend from ggplot object
extract_legend <- function(gplot){ 
  # source:   https://stackoverflow.com/questions/12041042/
  #           how-to-plot-just-the-legends-in-ggplot2
  temp <- ggplot_gtable(ggplot_build(gplot)) 
  leg <- which(sapply(temp$grobs, function(x) x$name) == "guide-box") 
  legend <- temp$grobs[[leg]] 
  return(legend)
} 

# define connectives from fuzzy logic 
fl_and <- function(var1, var2){
  stopifnot(length(var1) == length(var2))
  return(pmin(var1, var2, na.rm=TRUE))
}
fl_or <- function(var1, var2){
  stopifnot(length(var1) == length(var2))
  return(pmax(var1, var2, na.rm=TRUE))
}
fl_not <- function(var, scale_max=1){
  stopifnot(min(var, na.rm=TRUE) >= 0, max(var, na.rm=TRUE) <= scale_max)
  return(scale_max - var)
}

### function that plots the set of curves for adj A, given a df of expt1 results
# example: fig2_component(dat=dat1, A="Tall")
fig2_component <- function(dat, A, which_legend="top"){
  
  # requires that we use these column names (**adjust this later**)
  if (sum(names(dat) %in% c("ip","adj","unit","pred","normunit","response"))<6){
    message("cols need to include:\n  >> ", paste(names(dat), collapse=", "))
    return(NULL)
  }
  
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
      "Adj", "very Adj", "not antonym", "neither"
      # A, very_(A), not_(antonym), neither
    )
  } else {
    if (which_legend=="bottom"){
      color_labels <- c(
        "not Adj", "not very Adj", "antonym", "neither"
        # not_(A), not_very_(A), antonym, neither 
      )
    } else {
      color_labels <- c(
        paste0(A, "\nnot ", A, "   "),
        paste0("very ", A, "\nnot very ", A, "   "),
        paste0("not ", antonym, "   \n", antonym),
        paste0(neither, "   ")
      )
    }
  }
  
  
  pdat <- rbind(
    pdat_neither %>% mutate(curve_shape = "increasing"), 
    pdat_neither %>% mutate(curve_shape = "decreasing"), 
    pdat %>% filter(curve_shape != "neither")
  ) %>% 
    mutate(curve_shape = factor(curve_shape, 
                                levels=c("increasing","decreasing"))) %>% 
    mutate(color_cats = case_when(
      pred %in% c(adj_(A), not_(A))             ~ color_labels[1], 
      pred %in% c(very_(A), not_very_(A))       ~ color_labels[2], 
      pred %in% c(not_(antonym), adj_(antonym)) ~ color_labels[3], 
      pred %in% c(neither)                      ~ color_labels[4]
    )) %>% 
    mutate(color_cats=factor(color_cats, levels=color_labels))
  
  # or maybe these labels are clearer...
  # c("no_very","very","antonym","neither") 
  # c("tall","very_tall","not_short","neither")
  cols <- data_frame(
    color = c("orange", "blue", "aqua", "faint"),
    hex = c("#ff954f", "#1d30e0", "#43ad8d", "#bdbfbb"),
    pointline_hex = c("#ff954f", "#1d30e0", "#43ad8d", "transparent")
  )
  
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
  
  x_label <- ifelse(A=="Tall","Height (inches)","Time from 9am (minutes)")
  
  # set desired ticks, then calculate the breaks (since x-axis is numeric)
  if (A=="Tall"){
    # 7 ticks for 'tall' 
    x_ticks <- seq(from=63, to=81, by=3) 
    x_breaks <- sapply(x_ticks, function(inches){(inches-70)*3})
  }
  if (A=="Late"){
    # 8 ticks for 'late' 
    # (just using relative time for 'late', not HH:MM)
    x_ticks <- seq(from=-20, to=50, by=10) 
    x_breaks <- x_ticks
  }
  
  # can quickly look at the units and norm units 
  # setNames(sort(unique(dat1$normunit[dat1$adj=="Tall"])), 
  #          sort(unique(dat1$unit[dat1$adj=="Tall"])))
  
  stopifnot(length(x_breaks) == length(x_ticks))
  
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
    # comment this out to lose shapes: 
    scale_shape_manual(values=c(17,16,15,18)) + # or c(2, 1, 0, 3)
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






fig3_component <- function(dat, A){
  
  # set the labels for each line color 
  color_labels <- data_frame(
    color = c("orange", "blue"),
    hex = c("#ff954f", "#1d30e0"), 
    label = c("not Adj", "not very Adj")
  )
  
  # get means + bootstrap sem's for each scale point + predicate combo
  pdat <- dat %>% filter(adj==A, pred %in% c(not_(A), not_very_(A))) %>% 
    group_by(adj, pred, unit, normunit) %>% summarize(
      mean_response = mean(response), 
      se_response = sd(replicate(1000, mean(sample(response, replace=TRUE))))
    ) %>% ungroup() %>% 
    mutate(normunit = as.numeric(normunit)) %>% 
    mutate(color_cats = case_when(
      pred == not_(A) ~ color_labels$label[1], 
      pred == not_very_(A) ~ color_labels$label[2]
    )) %>% 
    mutate(color_cats=factor(color_cats, levels=color_labels$label))
  
  x_label <- ifelse(A=="Tall","Height (inches)","Time from 9am (minutes)")
  
  # set desired ticks, then calculate the breaks (since x-axis is numeric)
  if (A=="Tall"){
    # 7 ticks for 'tall' 
    x_ticks <- seq(from=63, to=81, by=3) 
    x_breaks <- sapply(x_ticks, function(inches){(inches-70)*3})
  }
  if (A=="Late"){
    # 8 ticks for 'late' 
    # (just using relative time for 'late', not HH:MM)
    x_ticks <- seq(from=-20, to=50, by=10) 
    x_breaks <- x_ticks
  }
  
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
    # comment this out to lose shapes: 
    scale_shape_manual(values=c(17,16)) + 
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
  
  if (A=="Late"){
    the_plot <- the_plot + 
      geom_rect(aes(xmin=-21, xmax=0, ymin=0, ymax=100), 
                fill="red", alpha=.005, color="transparent") + 
      geom_rect(aes(xmin=2, xmax=36, ymin=0, ymax=100), 
                fill="green", alpha=.005, color="transparent")
  }
  if (A=="Tall"){
    the_plot <- the_plot + 
      geom_rect(aes(xmin=12, xmax=21, ymin=0, ymax=100), 
                fill="green", alpha=.005, color="transparent")
  }
  
  return(the_plot)
}



# if building figure 7, must supply cluster df (defined in `paper_figures.r`)
fig6_fig7_component <- function(dat, inc_dec, legend_loc, x_label, 
                                fig7=FALSE, expt2_clusters=NULL){
  
  if (inc_dec=="increasing"){
    dat <- dat %>% filter(pred2 %in% c("Adj", "VeryAdj"))
    plot_title <- "increasing predicates ('Adj', 'very Adj')"
  }
  if (inc_dec=="decreasing"){
    dat <- dat %>% filter(pred2 %in% c("NotAdj", "NotVeryAdj"))
    plot_title <- "decreasing predicates ('not Adj', 'not very Adj')"
  }
  # plot palette -- orange, blue, aqua, purrrp
  colorze <- c("#ff954f", "#1d30e0") #, "#43ad8d", "#e23f8b")
  
  pdat <- dat %>% 
    # hack to order facs right and have right-padding in legend
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
  # unique(paste(pdat$adj, pdat$comparative, sep="_")
  
  if (fig7==TRUE){
    pdat <- left_join(pdat %>% mutate(pred2 = as.character(pred2)), 
                      expt2_clusters, by=c("adj", "comparative", "pred2"))
  }
  
  # nb: also nice confirmation w color=adj, and facet_grid(adj_type~construction)
  the_plot <- pdat %>% 
    ggplot(aes(x=normunit, y=mean_response, color=pred2, shape=pred2)) + 
    # facet_wrap(comparative ~ adj, scales="free") + 
    facet_wrap(~adj_form, nrow=2, scales="free") + 
    geom_point(size=rel(.95), alpha=.75) + 
    geom_line(alpha=.75, size=rel(.2)) + 
    geom_ribbon(aes(ymin=mean_response - se_response, 
                    ymax=mean_response + se_response, fill=pred2), 
                alpha=.25, color="transparent") + 
    scale_color_manual(values=colorze) + 
    scale_fill_manual(values=colorze) + 
    # comment this out to lose shapes: 
    scale_shape_manual(values=c(17,16)) + #,15,18)) + # or c(2, 1, 0, 3)
    scale_x_continuous(breaks=seq(from=-6, to=12, by=3)) + 
    scale_y_continuous(breaks=c(0,25,50,75,100),labels=c(0,25,50,75,100),
                       limits=c(0,100)) + 
    labs(x=x_label, 
         # x="normalized units (standard of comparison is zero)"
         y="% Agree") + 
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
  
  
  if (fig7==TRUE){
    
    the_plot <- the_plot + 
      geom_rect(aes(xmin=positive_lo, xmax=positive_hi, ymin=0, ymax=100), 
                fill="green", alpha=.0075, color="transparent", na.rm=TRUE) + 
      geom_rect(aes(xmin=negative_lo, xmax=negative_hi, ymin=0, ymax=100), 
                fill="red", alpha=.0075, color="transparent", na.rm=TRUE) + 
      labs(x=x_label)
  }
    
  return(the_plot)
  
}






