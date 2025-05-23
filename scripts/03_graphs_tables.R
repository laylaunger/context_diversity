# Title: Degree & Divergence
# Author: Layla Unger
# Last Updated: 2025-05-23
# R version: 4.3.2
# Packages: dplyr, tidyr, purrr, stringr, scam, ggplot2, cowplot, ggrepel, grid, gridExtra, patchwork, scales

#####################################################
#################### DESCRIPTION ####################
#####################################################

# Use this script after the 02_predict_aoa.R script.

# This script generates the following tables and graphs:
# A comparison of the words analyzed in this project and related
# prior studies
# A table and graphs of the relationship between contextual 
# diversity and frequency
# Graphs of the relationship between contextual diversity and
# word learning

#####################################################
################### LOAD PACKAGES ###################
#####################################################

library(plyr)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

library(scam)

library(cowplot)
library(grid)
library(gridExtra)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(scales)

#####################################################
#################### LOAD DATA ######################
#####################################################

# Load data for contextual diversity and its relationship with
# word learning

diversity_file <- here::here("data", "context_diversity", "diversity.rds")
diversity_control_file <- here::here("data", "context_diversity", "diversity_control.rds")

diversity <- readRDS(diversity_file)
diversity_control <- readRDS(diversity_control_file)


#####################################################
################# FORMAT VARIABLES ##################
#####################################################

# Format variables for plotting.

diversity_control <- diversity_control %>%
  dplyr::mutate(sign = factor(sign, levels = c("positive", "negative")),
                correction = factor(correction, levels = c("uncorrected", "freq", "shuffle", "both")),
                language = factor(language, levels = c("english", "spanish", "french", "german")),
                measure = factor(measure, levels = c("degree", "divergence"))
                )

levels(diversity_control$measure) <- c("Degree", "Divergence")

#####################################################
############ DIRECTORY FOR SAVING FILES #############
#####################################################

# Create a directory for saving files 

path_figures_tables <- "figures_tables"
if (!dir.exists(path_figures_tables)) dir.create(path_figures_tables)

#####################################################
########## DISTRIBUTION OF LEXICAL CLASSES ##########
#####################################################

# This project tackles a conflict between two prior studies:
# Hills et al. (2010), which showed that children learn words
# more easily when they appear in diverse contexts, and Roy et.
# al. (2015), which showed that children learn words more easily
# when they appear in consistent contexts.

# For the sake of comparison, this section contrasts the sets
# of words that were analyzed in those prior studies and in this
# project. Here, we contrast the number of words in lexical classes
# (e.g., nouns, verbs, etc.)


# Numbers of words in lexical classes in Hills et al. (2010),
# taken from the paper.
classes_hills <- data.frame(language = "Hills",
                            lexical_class = c("nouns", "verbs", 
                                              "adjectives", "function_words"),
                            N = c(330,96,58,88))


# Numbers of words in lexical classes in Roy et al (2015),
# taken from the paper.
classes_roy <- data.frame(language = "Roy",
                          lexical_class = c("adjectives", "function_words", 
                                            "nouns", "other", "verbs"),
                          N = c(90, 64, 380, 34, 111))


# Get numbers of words in lexical classes across languages 
# in present study
classes <- aoa_resid %>%
  dplyr::select(language, lexical_class, lemma) %>%
  dplyr::distinct() %>%
  group_by(language, lexical_class) %>%
  dplyr::summarise(N = length(lemma))

# Combine classes across studies
classes <- rbind(classes, classes_hills, classes_roy)

# Calculate percentages of words in each class
classes <- classes %>%
  group_by(language) %>%
  dplyr::mutate(percent = N / sum(N) )

# Format factors for plotting
classes$language <- factor(classes$language, levels = c("Hills", "Roy", 
                                                        "english", "spanish",
                                                        "french", "german"))

classes$lexical_class <- factor(classes$lexical_class, levels = c("nouns", "verbs",
                                                                  "adjectives", "function_words", 
                                                                  "other"))

# Plot composition across languages/studies
classes_plot <- ggplot(classes, aes(x=language, y=percent, fill=lexical_class)) +
  geom_bar(stat="identity", color = "black", width = .7, lwd=0.1, alpha = .3) +
  geom_text(aes(label=paste0(sprintf("%.0f", percent*100),"%")),
            position=position_stack(vjust=0.5), colour="black") +
  scale_x_discrete(labels = c("Hills et al. (2010)", "Roy et al. (2014)",
                              "English", "Spanish", "French", "German")) +
  scale_fill_manual(name = "Lexical Class", 
                    values = c("#37A5F5", "#3546FF", "#702DE6", "#CC0FFF", "#F51AB8"),
                    labels = c("Nouns", "Verbs", "Adjectives", "Function Words", "Other")) + 
  scale_y_continuous(labels = percent_format()) +
  labs(y="Composition", x="") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = .5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Look:
classes_plot

# Export to file
lexical_classes_file <- paste(path_figures_tables, "/Lexical Classes.pdf", sep = "")

pdf(file = lexical_classes_file,
    height = 4.25, width = 5)
classes_plot
dev.off()

#####################################################
############## DIVERSITY AND FREQUENCY ##############
#####################################################

# ---------- TABLES SHOWING RELATIONSHIP ---------- #

# Generate tables that show the relationship between diversity
# measures and frequency.

# This function computes the variance in a diversity measure that
# is accounted for by frequency (based on )
diversity_frequency_var <- function(input_language_window, measure) {

  input_language_window <- as.data.frame(input_language_window)
  # Get the name of the column containing the values from the shuffled corpus
  measure_shuffled <- paste(measure, "shuffled", sep = "_")

  
  freq_model <- scam(input_language_window[,measure] ~ s(input_language_window[,"log_freq"], k = 6, bs = "mpi") ) 
  shuffle_model <- scam(input_language_window[,measure] ~ s(input_language_window[,measure_shuffled], k = 6, bs = "mpi") ) 
  
  
  var_explained <- data.frame(log_freq = round(summary(freq_model)["dev.expl"]$dev.expl[1], digits = 2), 
                              shuffled = round(summary(shuffle_model)["dev.expl"]$dev.expl[1], digits = 2),
                              measure = measure)
  
  return(var_explained)
}

degree_table <- diversity %>%
  dplyr::filter(window_size == 5) %>%
  group_by(language) %>%
  group_modify(~ {
    input <- bind_cols(.y, .x)  # add grouping vars back
    diversity_frequency_var(input, measure = "degree")
  })

divergence_table <- diversity %>%
  dplyr::filter(window_size == 5) %>%
  group_by(language) %>%
  group_modify(~ {
    input <- bind_cols(.y, .x)  # add grouping vars back
    diversity_frequency_var(input, measure = "divergence")
  })

diversity_table <- rbind(degree_table, divergence_table)

diversity_table$language <- str_to_title(diversity_table$language)

diversity_table <- diversity_table %>%
  pivot_wider(names_from = "measure", values_from = c("log_freq", "shuffled")) %>%
  dplyr::select(language, log_freq_degree, shuffled_degree, log_freq_divergence, shuffled_divergence)

# Look:
diversity_table

# Format for pasting into LaTex document
diversity_table_latex <- diversity_table %>% unite(line, sep = " & ")
paste(diversity_table_latex$line, collapse = " \\ ")


# ----------- PLOT EXAMPLE RELATIONSHIP ----------- #

# Plot an example of the relationship between frequency
# and the degree measure of diversity in English

# Specify data to plot in example 
input_language_window <- diversity %>%
  dplyr::filter(language == "english" & window_size == 5)

measure = "degree"

# This function generates plots of the relationship between diversity
# and frequency for a given language/window size. To illustrate the 
# relationship, it additionally visualizes the residual variance in
# diversity after frequency is controlled, and highlights examples of
# words for which residuals are positive or negative.
vis_resid_window <- function(input_language_window, measure) {
  
  # Convert measure name to title case for plotting
  measure_name <- str_to_title(measure)
  
  # Get the name of the column containing the values from the shuffled corpus
  measure_shuffled <- paste(measure, "shuffled", sep = "_")
  
  # Specify the identifying variables - language, word, and window size
  id_variable_names <- c("language", "word", "window_size")
  id_variables_freq <- dplyr::select(input_language_window, c(id_variable_names, "log_freq"))
  id_variables_shuffle <- dplyr::select(input_language_window, c(id_variable_names, measure_shuffled))
  
  # Get the uncorrected measure of diversity, and regress out
  # the control measures of frequency (log freq and shuffle)
  uncorrected <- input_language_window[,measure]
  
  freq_model <- scam(input_language_window[,measure] ~ s(input_language_window[,"log_freq"], k = 6, bs = "mpi") ) 
  shuffle_model <- scam(input_language_window[,measure] ~ s(input_language_window[,measure_shuffled], k = 6, bs = "mpi") ) 
  
  # Get predicted values and residuals from the regression models
  # and store in a dataframe.
  freq_correction <- data.frame(id_variables_freq, control_variable = "Log Freq", uncorrected, predict(freq_model), resid(freq_model))
  shuffle_correction <- data.frame(id_variables_shuffle, control_variable = paste(measure_name, "from Shuffled Corpus"), uncorrected, predict(shuffle_model), resid(shuffle_model))
  
  # Standardize the names of the dataframes 
  names(freq_correction) <- names(shuffle_correction) <- c(id_variable_names, "control_value", "control_variable", 
                                                           "uncorrected", "predict", "resid")
  # Combine the dataframes
  correction <- rbind(freq_correction, shuffle_correction)
  
  # Add mcdi_aoa data to get lexical class info
  correction <- left_join(correction, mcdi_aoa)
  
  # Format the control_variable factor
  correction$control_variable <- as.factor(correction$control_variable)
  correction$control_variable <- factor(correction$control_variable, levels=rev(levels(correction$control_variable)))
  
  # For each lexical class, select an example word with
  # positive residual variance after controlling for frequency,
  # (i.e., higher diversity than predicted by frequency), and
  # an example with negative residual variance (i.e., lower diversity
  # than predicted by frequency) 
  pos_to_label <- correction %>%
    dplyr::filter(!(word %in% c("i", "vagina"))) %>%
    group_by(lexical_class) %>%
    dplyr::summarise(resid_max = max(resid),
                     word_label = sample(word[resid == resid_max], 1))
  
  neg_to_label <- correction %>%
    dplyr::filter(!(word %in% c("i", "vagina"))) %>%
    group_by(lexical_class) %>%
    dplyr::summarise(resid_min = min(resid),
                     word_label = sample(word[resid == resid_min], 1))
  
  # Add a column with just the example words
  correction$label <- ifelse(correction$word %in% pos_to_label$word_label | correction$word %in% neg_to_label$word_label,
                             correction$word, '')
  # Indicate whether the example word is an example
  # with positive or negative residual variance
  correction$label_type <- ifelse(correction$resid > 0, "Pos", "Neg")
  
  # Choose limits for the scale of residual values
  resid_limit <- max( c(abs(min(correction$resid)), abs(max(correction$resid)) )) 
  resid_precision <- ifelse(resid_limit < 1, 1, 0)
  resid_limit <- round(resid_limit, resid_precision)
  
  # Generate the plot
  vis_plot <- ggplot(correction, aes(x = control_value, y = uncorrected)) +
    facet_wrap(~control_variable, scales = "free", strip.position = "bottom") +
    geom_point(aes(color = resid), alpha = .75) +
    geom_line(aes(y = predict), color = "blue") +
    geom_label_repel(aes(label = label, fill = label_type), 
                     color="white", segment.color="black", 
                     point.padding = .1, max.overlaps = 500, force = 10, min.segment.length = .001,
                     force_pull = .3,
                     show.legend = F) +
    scale_x_continuous(name = "", expand = expansion(mult = c(.1, .12))) +
    scale_y_continuous(name = measure_name, expand = expansion(mult = c(.1, .1))) +
    scale_color_gradient2(midpoint = 0, low = rgb(.12, .03, .92), mid = "lightgray",
                          high = rgb(.98,.1,.46) , space = "Lab", 
                          limits = c(resid_limit * -1, resid_limit),
                          breaks = breaks_pretty(n = 5),
                          #breaks = seq(-.4, .4, by = .2),
                          name = "Residual\n") +
    scale_fill_manual(values = rgb(c(.12, .98), c(.03,.1), c(.92, .46) )) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.placement = "outside",
          strip.background = element_blank(),
          legend.title.align = .5,
          legend.spacing.y = unit(5, "point"))
  
  return(vis_plot)
  
}

# Generate the plot for degree and divergence
# (English in a window size of 5 chosen as the illustrative example)
degree_vis_resid <- vis_resid_window(input_language_window = diversity[diversity$language == "english" & diversity$window_size == 5,], 
                                     measure = "degree")
divergence_vis_resid <- vis_resid_window(input_language_window = diversity[diversity$language == "english" & diversity$window_size == 5,], 
                                         measure = "divergence")

# Look:
plot_grid(degree_vis_resid, divergence_vis_resid, nrow = 2)

# Export to file
diversity_frequency_file <- paste(path_figures_tables, "/Diversity and Frequency.pdf", sep = "")

pdf(file = diversity_frequency_file,
    height = 8, width = 9)
plot_grid(degree_vis_resid, divergence_vis_resid, nrow = 2)
dev.off()


####################################################
########### DIVERSITY AND WORD LEARNING ############
####################################################

# -------------- RESULTS USING GAMs -------------- #

# For a given window size, plot the variance in AoA explained by 
# diversity for each measure (degree and divergence), and each variant: 
# uncorrected, or corrected for one or both of the control measures of frequency.
# The function includes an option to add text to the x and y axes
# that is used in supplemental analyses below to indicate that
# these supplemental analyses used linear regression rather than
# GAMs. The default text to add is blank.

vis_diversity_vs_control <- function(input_data, amend_axis = "", add_window = FALSE ) {
  
  # Use add_window to determine whether to add room at top of plots for indicating
  # window size
  if(add_window) {
    top_margin = 20
  } else {
    top_margin = 5
  }
  
  # The y-axis label takes up two lines and includes a math 
  # expression, so it needs to be added separately.
  line_1 <- paste("Variance Explained", amend_axis, sep = "")
  line_2 <- expression("Earlier AoA" %<->% "Later AoA")
  line1_grob <- textGrob(line_1, rot = 90)
  line2_grob <- textGrob(line_2, rot = 90)
  
  y_axis <- wrap_elements(line1_grob) + wrap_elements(line2_grob)
  
  x_name <- paste("Diversity Correction", amend_axis, sep = "")
  
  vis_plot <- ggplot(input_data, aes(x = correction, y = var_sign)) +
    facet_wrap(~measure, scales = "free") +
    geom_jitter(aes(fill = sign, shape = language), height = 0, width = .1, size = 2.5, show.legend = T) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_discrete(name = x_name, labels = c("Uncorrected", "Log Freq", "Shuffled", "Log Freq +\nShuffled")) +
    scale_y_continuous(name = "",
                       labels=abs(seq(-.5, .5, by = .25)), breaks=seq(-.5, .5, by = .25), limits=c(-.5, .5)) +
    scale_shape_manual(name = "Language", values = 21:24, labels = c("English", "Spanish", "French", "German")) +
    scale_fill_manual(values = rgb( c(.98, .12), c(.1, .03), c(.46, .92), c(.7, .7)  )) +
    guides(fill = "none") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.background = element_blank(),
          plot.margin = margin(t = top_margin, r = 5, b = 5, l = -25, unit = "pt"),
          strip.text = element_text(size = 12),
          axis.text.x = element_text(size = 11, angle = 45, vjust = .5))
  
  layout <- c(
    area(t = 1.85, l = 1, b = 5.9, r = 2),
    area(t = 1, l = 3, b = 6, r = 20)
  )
  
  output_plot <- wrap_elements(y_axis) + wrap_elements(vis_plot) +
    plot_layout(design = layout)
  
  return(output_plot)
}

# Store the plot for each window size
diversity_vs_control <- diversity_control %>%
  group_by(window_size) %>%
  group_split() %>%
  set_names(map_chr(., ~ as.character(unique(.x$window_size)))) %>%
  map(~ vis_diversity_vs_control(.x, add_window = TRUE))


# Look at one of the plots (note that it needs a decent amount of 
# vertical space to view the plotting region)
plot_grid(diversity_vs_control$`5`, diversity_vs_control$`11`, diversity_vs_control$`21`,
          nrow = 3,
          labels = c("Window Size = 5", "Window Size = 11", "Window Size = 21"),
          label_size = 11)


# Export to file

# Just window size = 5 for main text
diversity_aoa_file <- paste(path_figures_tables, "/Diversity and AoA.pdf", sep = "")

pdf(file = diversity_aoa_file,
    height = 4.5, width = 9)
export_plot <- vis_diversity_vs_control(diversity_control[diversity_control$window_size == 5,]) 
grid.draw(export_plot)
dev.off()



# All windows for supplement
diversity_aoa_windows_file <- paste(path_figures_tables, "/Diversity and AoA - Windows.pdf", sep = "")

pdf(file = diversity_aoa_windows_file,
    height = 4*4, width = 10)
plot_grid(diversity_vs_control$`5`, diversity_vs_control$`11`, diversity_vs_control$`21`,
          nrow = 3,
          labels = c("Window Size = 5", "Window Size = 11", "Window Size = 21"),
          label_size = 11)
dev.off()