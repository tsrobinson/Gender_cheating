het_plot <- function(df, treat.col, treat.desc, binw = 250, reduced = FALSE) {
  
  require(ggplot2)
  require(ggpubr)
  
  df$treat.effect <- df[,treat.col]
  df <- df[order(df$treat.effect),]
  n <- length(df$treat.effect)
  df$id <- c(1:n)
  
  # Convert covariates to binary factors
  # CATE Heterogeneity plot
  
  effectsPlot <- ggplot(df, aes(x=id, y = treat.effect)) +
    geom_line() +
    geom_hline(yintercept= 0, linetype="dashed", color="red") +
    geom_hline(yintercept = mean(df$treat.effect), color = "blue") +
    labs(x="Individual",y = "CATE", title = treat.desc) +
    theme_minimal()
  
  ## Create histogram plots
  genderPlot <- ggplot(df, aes(x=id, fill=gender_lab)) +
    geom_histogram(binwidth = binw, position="stack") +
    theme(legend.position="bottom") +
    scale_fill_discrete(name="Gender") +
    labs(y = "Count")
  
  onlinePlot <- ggplot(df, aes(x=id, fill=modes)) +
    geom_histogram(binwidth = binw, position="stack") +
    theme(legend.position="bottom") +
    scale_fill_discrete(name="Mode") +
    labs(y = "Count")
  
  countryPlot <- ggplot(df, aes(x=id, fill=country)) +
    geom_histogram(binwidth = binw, position="stack") +
    theme(legend.position="bottom") +
    scale_fill_discrete(name="Country") +
    labs(y = "Count")

  # Combine all plots into one chart, incl. reduced form to just show gender
  if (reduced == TRUE) {
    figure <- ggarrange(effectsPlot, genderPlot,
                        ncol = 1, nrow = 2, heights = c(2,1.3))
  } else {
    figure <- ggarrange(effectsPlot, genderPlot, countryPlot, onlinePlot,
                        ncol = 1, nrow = 4, heights = c(2,1.3,1.3,1.3))
  }
  
  return(figure)
}