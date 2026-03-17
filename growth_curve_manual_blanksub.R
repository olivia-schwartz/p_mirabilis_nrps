library(tidyverse)
library(stats)

##### Input Files #####

# growth_assay: growth assay results output from instrument
#           Column 1: Time (numerical value)
#           Columns 2-end: assay values with well ID as column name
# well_IDs: (.csv) table annotated with:
#           Column 1: well IDs as "A1", "A2"....
#           Column 2: Condition (ie. bacteria strain)
#           Column 3: Treatment (ie. media additive)

setwd("C:/Users/Olivia.Schwartz/OneDrive - University of Denver/Projects/Proteus Ybt/Growth Assays/20260313_CP_MIC_LB")
directory <- getwd()

growth_assay <- read.csv("20260313_CP_MIC_LB_formatR_Raw.CSV")
well_IDs <- read.csv("20260313_CP_MIC_LB_Well_IDs.csv")

colnames(growth_assay)[1] <- "time"
colnames(well_IDs)[1] <- "well_ID"
colnames(well_IDs)[2] <- "Condition"
colnames(well_IDs)[3] <- "Treatment"


#pivoting to long format 
growth_assay <- growth_assay %>%
  pivot_longer(cols=2:ncol(growth_assay),
               names_to='well_ID',
               values_to='OD') 
#merge data
growth_assay <- left_join(growth_assay, well_IDs, by ="well_ID")

#Grab unique values from each column of growth_assay
column_values <- list()
for (i in 1:ncol(growth_assay)) {

#column_values[[1]] = times, [[2]] = well_IDs, [[3]] = ODs, [[4]].... metadata
  column_values[[i]] <- unique(growth_assay[,i])
}

#Constructing all blank OD values for each set of conditions
blank_OD_averages <- growth_assay %>%
  group_by(across(c(1, 4:ncol(growth_assay)))) %>%
  summarise(mean_OD = mean(OD, na.rm = TRUE), .groups = "drop") %>%
  dplyr::filter(Condition == "Blank")

blank_OD_averages <- blank_OD_averages[,c(1, 3:ncol(blank_OD_averages))]
blank_OD_averages <- blank_OD_averages %>%
  rename(sub_blank_val = mean_OD)

#Removing N/As manually
growth_assay <- growth_assay %>%
  dplyr::filter(growth_assay$Condition != "N/A")

growth_assay <- full_join(growth_assay, blank_OD_averages)
growth_assay$new_OD <- growth_assay$OD - growth_assay$sub_blank_val

#Generate a growth curve of the specific treatment
plot_growth_condition_3 <- function(data) {
  data %>%
    #    dplyr::filter(.data$time < 24 & .data$Condition == "WT" & .data$Treatment == "CP" |.data$time < 24 & .data$Condition == "WT" & .data$Treatment == "Buffer") %>%
    dplyr::filter(.data$Condition == "WT" ) %>%
    ggplot(aes(x = time, y = new_OD, color = Conc)) +
    geom_point(size=1) +
    theme(text = element_text(size = 20)) +
    labs(title = "WT in CP (Manual Blank sub)") + scale_colour_viridis_d(option="magma") 
  #+ scale_color_breweWTr(palette = "Spectral")
  #  scale_colour_viridis_d(option="magma")   + scale_y_log10()
  #+ facet_wrap(~Treatment) 
  #
  
}

plot_growth_condition_3(
  data = growth_assay)
ggsave(file=paste(directory,"/20260313_blank_manualblanksub.svg",sep=""), plot=last_plot())

