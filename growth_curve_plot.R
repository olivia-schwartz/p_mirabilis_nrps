library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(viridisLite) #Color blind accessibility

##### Input Files #####

# growth_assay: growth assay results output from instrument
#           Column 1: Time (numerical value)
#           Columns 2-end: assay values with well ID as column name
# well_IDs: (.csv) table annotated with:
#           Column 1: well IDs as "A1", "A2"....
#           Column 2: Condition (ie. bacteria strain)
#           Column 3: Treatment (ie. media additive)

setwd("C:/Users/Olivia.Schwartz/OneDrive - University of Denver/Projects/Proteus Ybt/Growth Assays/20260305_CP_Buffer_Test")
directory <- getwd()

growth_assay <- read.csv("20260305_CP_Buffer_Test_formatR_Raw.CSV")
well_IDs <- read.csv("20260305_CP_Buffer_Test_Well_IDs.csv")

##### Input Parameters #####
plot_titles <- "CPBufferTest_"
fill_point <- "Condition" #Column used to fill plot point color

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
  
#ggsave(file=paste(directory,"/",plot_titles,"_",title,".svg",sep=""), plot=last_plot())
}

#Generate growth curve, plotting treatments together
plot_growth_condition_2 <- function(data, condition, title) {
  data %>%
    dplyr::filter(.data$Condition == condition) %>%
    ggplot(aes(x = time, y = OD, color = Treatment)) +
    geom_point() +
    theme(text = element_text(size = 25)) +
    labs(title = title) +
    scale_colour_viridis_d()
  
#ggsave(file=paste(directory,"/",plot_titles,"treatment_",title,".svg",sep=""), plot=last_plot())
  
}

#####

colnames(well_IDs)[1] <- "well_ID"
colnames(well_IDs)[2] <- "Condition"
colnames(well_IDs)[3] <- "Treatment"

#pivoting to long format for plotting
growth_assay <- growth_assay %>%
  pivot_longer(cols=2:ncol(growth_assay),
               names_to='well_ID',
               values_to='OD') 
#merge data
growth_assay <- left_join(growth_assay, well_IDs, by ="well_ID")

plots <- list() #empty list to fill with each plot
conditions <- unique(growth_assay$Condition) #conditions which will each have their own plot
for (i in seq_along(conditions)) {
  plots[[i]] <- plot_growth_condition(
    data = growth_assay,
    condition = conditions[i],
    title = conditions[i]
  )
}

plots <- list() #empty list to fill with each plot
conditions <- unique(growth_assay$Condition) #conditions which will each have their own plot
for (i in seq_along(conditions)) {
  plots[[i]] <- plot_growth_condition_2(
    data = growth_assay,
    condition = conditions[i],
    title = conditions[i]
  )
}

#growth_assay$Conc <- as.factor(growth_assay$Conc)
#growth_assay$Conc <- factor(growth_assay$Conc, levels=c('N/A', '0.00', '8', '16','31','63','125','250','500','1000'))

#Generate a growth curve of the specific treatment
plot_growth_condition_3 <- function(data) {
  data %>%
#    dplyr::filter(.data$time < 24 & .data$Condition == "WT" & .data$Treatment == "ΔHis CP" |.data$time < 24 & .data$Condition == "WT" & .data$Treatment == "Buffer") %>%
    dplyr::filter(.data$Condition != "N/A") %>%
        ggplot(aes(x = time, y = OD, color = Treatment)) +
    geom_point() +
    theme(text = element_text(size = 20)) +
    labs(title = "CP Buffer Test (Blank sub Data)") + scale_colour_viridis_d(option="magma") + facet_wrap(~Condition) + scale_y_log10()
  #+ scale_color_brewer(palette = "Spectral")
  #  scale_colour_viridis_d(option="magma")   + scale_y_log10()
  #+ facet_wrap(~Treatment) 
  #

}

plot_growth_condition_3(
  data = growth_assay)
#ggsave(file=paste(directory,"/WTvBlankBlankSub.svg",sep=""), plot=last_plot())
