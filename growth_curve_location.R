library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(viridis)

##### Input Files #####

# growth_assay: growth assay results output from instrument
#           Column 1: Time (numerical value)
#           Columns 2-end: assay values with well ID as column name
# well_IDs: (.csv) table annotated with:
#           Column 1: well IDs as "A1", "A2"....
#           Column 2: Condition (ie. bacteria strain)
#           Column 3: Treatment (ie. media additive)

setwd("C:/Users/Olivia.Schwartz/OneDrive - University of Denver/Projects/Proteus Ybt/Growth Assays/20260217_Condensation_Test")
directory <- getwd()

growth_assay <- read.csv("20260217_Condensation_formatR.CSV")
well_IDs <- read.csv("20260217_Condensation_Well_IDs.csv")
proximity_table <- read.csv("C:/Users/Olivia.Schwartz/OneDrive - University of Denver/Projects/Proteus Ybt/Growth Assays/96_proximity_table.csv")

##### Input Parameters #####
plot_titles <- "OD by Proximity to Perimeter"
fill_point <- "Row" #Column used to fill plot point color

##### Functions ####

#Generate a growth curve of the specific condition, separate plots for each treatment
plot_growth_condition <- function(data, condition, title) {
  data %>%
    dplyr::filter(.data$Condition == condition) %>%
    ggplot(aes(x = time, y = OD, color = Condition)) +
    geom_point() +
    theme(text = element_text(size = 25)) +
    labs(title = title) +
    scale_color_brewer(palette = "Dark2") +
    facet_wrap(~Treatment)  
  
  ggsave(file=paste(directory,"/",plot_titles,"_",title,".svg",sep=""), plot=last_plot())
}
#####

#Add in columns denoting row and column on the plate
colnames(well_IDs)[1] <- "well_ID"
colnames(proximity_table)[1] <- "well_ID"
well_IDs <- left_join(well_IDs, proximity_table, by = "well_ID")

well_IDs <- well_IDs %>%
  mutate(
    Row = str_extract(well_ID, "[A-Za-z]+"),
    Column = str_extract(well_ID, "\\d+")
  )
well_IDs$Column <- as.factor(well_IDs$Column)

#Reformat the OD values for plotting
growth_assay <- growth_assay %>%
  pivot_longer(cols=2:ncol(growth_assay),
               names_to='well_ID',
               values_to='OD') 

#merge data
growth_assay <- left_join(growth_assay, well_IDs, by ="well_ID")

growth_assay$Condition <- factor(
  growth_assay$Condition,
  levels = unique(growth_assay$Condition)
)

growth_assay %>%
  dplyr::filter(growth_assay$Condition == "Media") %>%
  ggplot(aes(x = time, y = OD, color = Proximity)) +
  geom_point() +
  theme(text = element_text(size = 15)) +
  labs(title = "WT Proximity to Edge", color = "Proximity") +
  scale_color_viridis_c(option = "magma") +
  facet_wrap(~Treatment)
#ggsave(file=paste(directory,"/",plot_titles,".svg",sep=""), plot=last_plot())

#ggsave(file=paste(directory,"/",plot_titles,".svg",sep=""), plot=last_plot())

