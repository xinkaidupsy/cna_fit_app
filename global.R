# Install packages if needed:
install_if_needed <- function(pkg){
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dep = TRUE)
    library(pkg, character.only = TRUE)
  }
}

install_if_needed("shinythemes")
install_if_needed("shiny")
install_if_needed("ggplot2")
install_if_needed("dplyr")
install_if_needed("data.table")
install_if_needed("data.table")
install_if_needed("sjstats")
install_if_needed("RColorBrewer")
install_if_needed("tidyr")

dt <- readRDS("simres.RDS")

# options
metrics <- c("pvalue", "NFI","PNFI","TLI","RFI","IFI","RNI","CFI","RMSEA")
conditions_null <- c("n_person_factor","type","n_time_factor","n_node_factor")
conditions_mean <- c("mean_trend_factor","type","n_person_factor","n_time_factor","n_node_factor")
conditions_rt <- c("rewire_temporal_factor","type","n_person_factor","n_time_factor","n_node_factor")
conditions_rc <- c("rewire_contemporaneous_factor","type","n_person_factor","n_time_factor","n_node_factor")
conditions_nt <- c("rewire_temporal_factor","type","n_person_factor","n_time_factor","n_node_factor")
conditions_nc <- c("rewire_contemporaneous_factor","type","n_person_factor","n_time_factor","n_node_factor")

conditions_ls <- list(conditions_null, conditions_mean, conditions_rt, 
                      conditions_rc, conditions_nt, conditions_nc)

names(conditions_ls) <- c("dt_null", "dt_mean", "dt_rt", "dt_rc", "dt_nt", "dt_nc")


# colors
line_colors <- RColorBrewer::brewer.pal(9, "Paired")

# ----- functions -----

# *--- notin ---
`%!in%` <- Negate(`%in%`)

# *--- boxplot function ---
box_plotter <- function(simres, metric = c("NFI", "PNFI", "TLI", "NNFI", "RFI", "RNI", "IFI", "RMSEA","CFI"), 
                        xaxis = "n_person_factor", x_facet, y_facet, color, xlab="",ylab="",legend_title="",legend.position ="right"){
  
  # convert to longer
  longer <- simres %>% tidyr::pivot_longer(NFI:RMSEA, names_to = "metric")
  
  # indices
  metric <- match.arg(metric)
  
  # filter
  sub_data <- longer[longer$metric == metric,]
  
  title <- switch(metric,
                  NFI = "Bentler-Bonett Normed Fit Index (NFI)",
                  NNFI = "Bentler-Bonett Nonnormed Fit Index (NNFI)",
                  PNFI = "The Parsimony-Adjusted NFI (PNFI)",
                  TLI = "Tucker Lewis index (TLI)",
                  RFI = "Bollen's Relative Fit Index (RFI)",
                  IFI = "Bollen's Incremental Fit Index (IFI)",
                  RNI = "McDonald and Marsh’s Relative Noncentrality Index (RNI)",
                  CFI = "Bentler's Comparative Fit Index (CFI)",
                  RMSEA = "Root Mean Square Error of Approximation (RMSEA)")
  
  # subtitle <-  switch(metric,
  #                     NFI = "Tyical SEM interpretation: > 0.9 adequate fit, > 0.95 good fit.",
  #                     NNFI = "Tyical SEM interpretation: > 0.9 adequate fit, > 0.95 good fit.",
  #                     PNFI = "Tyical SEM interpretation: > 0.5 adequate fit",
  #                     TLI = "Tyical SEM interpretation: > 0.9 adequate fit, > 0.95 good fit.",
  #                     RFI = "Tyical SEM interpretation: > 0.9 adequate fit, > 0.95 good fit.",
  #                     IFI = "Tyical SEM interpretation: > 0.9 adequate fit, > 0.95 good fit.",
  #                     RNI = "Tyical SEM interpretation: > 0.9 adequate fit, > 0.95 good fit.",
  #                     CFI = "Tyical SEM interpretation: > 0.9 adequate fit, > 0.95 good fit.",
  #                     RMSEA = "Tyical SEM interpretation: < 0.08 adequate fit, < 0.05 good fit.")
  # 
  line1 <-  switch(metric,
                   NFI = 1,
                   NNFI = 1,
                   PNFI = 1,
                   TLI = 1,
                   RFI = 1,
                   IFI = 1,
                   RNI = 1,
                   CFI = 1,
                   RMSEA = 0)
  
  line2 <-  switch(metric,
                   NFI = 0.95,
                   NNFI = 0.95,
                   PNFI = 1,
                   TLI = 0.95,
                   RFI = 0.95,
                   IFI = 0.95,
                   RNI = 0.95,
                   CFI = 0.95,
                   CFI = 0.95,
                   RMSEA = 0.05)
  
  line3 <-  switch(metric,
                   NFI = 0.9,
                   NNFI = 0.9,
                   PNFI = 0.5,
                   TLI = 0.9,
                   RFI = 0.9,
                   IFI = 0.9,
                   RNI = 0.9,
                   CFI = 0.9,
                   CFI = 0.9,
                   RMSEA = 0.08)
  
  # Plot:
  if (missing(x_facet)) x_facet <- "."
  if (missing(y_facet)) y_facet <- ""
  
  if (missing(color)){
    AES <- aes_string(x = xaxis, y = "value")
  } else {
    AES <- aes_string(x = xaxis, y = "value", fill = color)
  }
  
  ggplot(sub_data, AES) + 
    facet_grid(as.formula(paste(y_facet,"~",x_facet)), scales = "free_y") + geom_boxplot() + theme_bw() + 
    # scale_y_continuous(breaks = seq(-10,10,by=0.1), minor_breaks =  seq(-10,10,by=0.1)) +
    geom_hline(yintercept=line1) + 
    geom_hline(yintercept=line2, lty = 2) + 
    geom_hline(yintercept=line3, lty = 3) + 
    ylab(ylab) + xlab(xlab) + 
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    # ggtitle(title,subtitle) +
    ggtitle(title) +
    # guides(fill=guide_legend(title=legend_title)) + 
    theme(plot.title = element_text(size = 18)) +
    # theme(plot.subtitle = element_text(size = 12)) +
    theme(strip.text = element_text(size = 12)) +
    theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.1, size = 12, margin = margin(t = -15)),
          panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + 
    theme(axis.text.y = element_text(size = 12), panel.grid.major.y = element_blank()) + 
    geom_vline(xintercept=seq(1.5, length(unique(eval( parse(text=xaxis),envir = sub_data)))-0.5, 1), 
               lwd=0.5, colour="black", alpha = 0.25) + 
    theme(legend.position = legend.position)
  
}

# *--- function to set up sliders --- 

slider_set_up <- function(index, default_value){
  sliderInput(inputId = paste0(index, "_cut"), label = index, 
              value = default_value, min = .01, max = .99, step = .01)
}

# *--- line plotter --- 
line_plotter <- function(data, x, y, color, 
                         idvars, measure_vars,
                         color_var, xfacet, yfacet){
  
  ggplot(data, aes(x = get(x), y = get(y), colour = get(color_var))) +
    geom_line(aes(x = get(x), y = get(y), color = get(color_var), 
                  group = interaction(get(color_var), get(xfacet), get(yfacet)))) +
    facet_grid(get(yfacet) ~ get(xfacet)) +
    labs(x = "", y = "", color = "Cutoffs") +
    scale_color_manual(values = color) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    theme_bw() +
    theme(strip.text = element_text(size = 12)) +
    theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.1, size = 12, margin = margin(t = -15)),
          panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + 
    theme(axis.text.y = element_text(size = 12), panel.grid.major.y = element_blank())
  
}

# *--- round numbers ---
roundif <- function(x) {
  
  x = as.numeric(x)
  
  y = numeric(length(x))
  
  for (i in 1:length(x)) {
    if (abs(x[i]) > 10){
      y[i] = sprintf("%.1f", x[i])
    } else if(abs(x[i]) > 1 & abs(x[i]) < 10) {
      y[i] = sprintf("%.2f", x[i])
    } else if(abs(x[i]) > .01 & abs(x[i]) < 1) {
      y[i] = sprintf("%.2f", x[i])
    } else if(abs(x[i]) < .001) {
      y[i] = sprintf("%.4f", x[i])
    } else {
      y[i] = sprintf("%.3f", x[i])
    }
  }
  
  return(y)
  
}

