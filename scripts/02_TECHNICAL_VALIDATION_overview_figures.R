################################################################################
# Author: Flavian Tschurr
# Email: flavian.tschurr@usys.ethz.ch 
# Project: NiWUE
# Date: 20260323
# Script purpose: visual / techincal validation for dataset
################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Sys.setlocale("LC_ALL", "English")
# Load libraries
# list of required packages
packages <- c("readr", "ggplot2", "dplyr", "cowplot", "tidyr", "patchwork")
# Loop through and install/load each one
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

library(readr)
library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)
library(patchwork)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# retrieve data from zenodo
record_id <- "19201634"
isodata_file_name <- "winter_wheat_isotopes_management.csv" 
yielddata_file_name <- "winter_wheat_yield.csv" 

# Read the data directly into R
# Note: Zenodo might redirect, read_csv handles this automatically
iso_data <- read_csv(paste0("https://zenodo.org/api/records/", record_id,
                            "/files/",isodata_file_name, "/content"))
yield_data <- read_csv(paste0("https://zenodo.org/api/records/", record_id,
                              "/files/", yielddata_file_name, "/content"))



all_data <- merge(iso_data,yield_data[,c("UID","yield_t_ha")], by="UID")
#convert dates to date format
all_data$harvest_date <- as.Date(as.character(all_data$harvest_date),
                                 format = "%Y%m%d")
all_data$sowing_date <- as.Date(as.character(all_data$sowing_date),
                                format = "%Y%m%d")

# clean year for the askov experiment, as there the samples are pooled over 
# multiple years --> select the mean
all_data$year_clean <- NA
for(i in 1:length(all_data$year)){
  if(all_data$trial_ID[i]=="AS"){
    all_data$year_clean[i] <-round(median(as.numeric(unlist(strsplit(
      all_data$year[i],"-")))))
    
  }else{
    all_data$year_clean[i] <- all_data$year[i]
  }
}

################################################################################
# management
################################################################################

##
# Prepare the variables
hist_data <- all_data %>%
  mutate(
    year_num    = as.numeric(year_clean),
    yield_num   = as.numeric(yield_t_ha),
    harvest_doy = as.numeric(format(harvest_date, "%j")),
    sowing_doy  = as.numeric(format(sowing_date, "%j"))
  ) %>%
  select(trial_name,N_tot_input, year_num, yield_num, harvest_doy, sowing_doy, material) %>%
  pivot_longer(cols = -c(trial_name,material),
               names_to = "variable",
               values_to = "value")

hist_data$variable <- ifelse(hist_data$variable =="yield_num", paste(
  hist_data$variable,hist_data$material,sep="_"),hist_data$variable)

# Set custom order for facets
hist_data <- hist_data %>%
  mutate(variable = factor(variable,
                           levels = c("year_num", "sowing_doy", "harvest_doy",
                                      "N_tot_input", "yield_num_grain", "yield_num_straw")))


# Nice labels for facets
labels <- c(
  N_tot_input = "Total N Input (kg/ha)",
  year_num    = "Year",
  yield_num_grain   = "Grain Yield (t/ha)",
  yield_num_straw   = "Straw Yield (t/ha)",
  harvest_doy = "Harvest Day of Year",
  sowing_doy  = "Sowing Day of Year"
)


location_colors <- c(
  "Static fertilization experiment"   = "#E6007A",  
  "Askov long-term experiment"        = "#003F5C",  
  "PROspective"                       = "#56B4E9",  
  "QualiAgro"                         = "#1B9E77",  
  "DFG experiment"                    = "#F0E442",  
  "Broadbalk"                         = "#CC79A7",  
  "P24A"                              = "#D55E00",  
  "DOK - long-term system comparison" = "#332288",  
  "FAST"                              = "#A6D854",  
  "RELOAD"                            = "#882255"   
)



hist_data <- hist_data %>%
  mutate(trial_name = factor(trial_name, levels = sort(unique(trial_name))))
hist_data <- hist_data %>%
  mutate(trial_name_stack = factor(trial_name, levels = rev(sort(unique(trial_name)))))

# order facets
facet_levels <- unique(hist_data$variable)
facet_levels <- c(
  "year_num",
  "N_tot_input",
  "sowing_doy",
  "harvest_doy",
  "yield_num_grain",
  "yield_num_straw"
)

# panel letters
letters_df <- tibble(
  variable = facet_levels,
  letter = letters[1:length(facet_levels)]
)

make_panel <- function(var, letter) {
  df <- hist_data %>% filter(variable == var)
  
  ggplot(df, aes(x = value, fill = trial_name_stack)) +
    geom_histogram(bins = 30, color = "white", alpha = 0.8) +
    scale_fill_manual(
      values = location_colors,
      breaks = levels(hist_data$trial_name),
      labels = levels(hist_data$trial_name),
      name = "Trial"
    ) +
    labs(
      x = labels[[var]],   
      y = "Count"
    ) +
    annotate(
      "text",
      x = -Inf, y = Inf,
      label = letter,
      hjust = -0.2, vjust = 1,
      size = 5
    ) +
    theme_cowplot() +
    theme(
      legend.position = "none",
      panel.grid = element_blank()
    )
}

plots <- purrr::map2(facet_levels, letters_df$letter, make_panel)

legend <- cowplot::get_legend(
  make_panel(facet_levels[1], "a") +
    theme(legend.position = "right")
)


legend_padded <- patchwork::wrap_elements(
  legend,
  clip = FALSE
) + theme(
  plot.margin = margin(l = 20)  
)


final_plot_management <- wrap_plots(plots, ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

final_plot_management <- wrap_plots(
  wrap_plots(plots, ncol = 2),
  legend_padded,
  ncol = 2,
  widths = c(4, 1.75)
)

print(final_plot_management)

################################################################################
################################################################################
# isotope
################################################################################


#prepare data
iso_hist_data <-  all_data %>%
  select(trial_name,material, d13C, d15N, C_N_ratio) %>%
  pivot_longer(cols = -c(trial_name, material),
               names_to = "variable",
               values_to = "value")
# add facet labels
iso_hist_data$facet_level <- paste(iso_hist_data$variable,
                                   iso_hist_data$material,sep=" ")
# add delta annotation
iso_hist_data$facet_level<- gsub("d13C","\u03B4\u00B9\u00B3C",iso_hist_data$facet_level)
iso_hist_data$facet_level<- gsub("d15N","\u03B4\u00B9\u2075N",iso_hist_data$facet_level)
iso_hist_data$facet_level<- gsub("C_N_ratio","C:N",iso_hist_data$facet_level)

# factorise levels
iso_hist_data <- iso_hist_data %>%
  mutate(trial_name = factor(trial_name, levels = sort(unique(trial_name))))
iso_hist_data <- iso_hist_data %>%
  mutate(trial_name_stack = factor(trial_name, levels = rev(sort(unique(trial_name)))))



# create plot
isotope_overview <- ggplot(iso_hist_data, aes(x = value, fill = trial_name_stack)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.8) +
  facet_wrap(~facet_level, scales = "free", ncol=2
  ) +
  scale_fill_manual(
    values = location_colors,          # palette named by trial_name
    breaks = levels(iso_hist_data$trial_name), 
    labels = levels(iso_hist_data$trial_name),  
    name = "Trial"
  ) +
  labs(x = "\u2030", y = "Count") +
  theme_cowplot() +
  theme(
    legend.position = "right",
    panel.grid = element_blank()
  )+
  theme(
    strip.background = element_blank(),
    strip.text = element_text(
      hjust = 0, vjust = 1,
      margin = margin(t = 0, r = 0, b = -5, l = 8)   # negative bottom margin
    ),
    strip.clip = "off"
  )


print(isotope_overview)


