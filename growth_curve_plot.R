library(tidyverse)
library(ggplot2)
library(RColorBrewer)

setwd("C:/Users/Olivia.Schwartz/OneDrive - University of Denver/Projects/Proteus Ybt/Growth Assays")

growth_assay <- read.csv("20260108_Growth_Chelators_formatR.csv")

#pivoting to long format for plotting
growth_assay %>%
  pivot_longer(cols=2:ncol(growth_assay),
               names_to='well_ID',
               values_to='OD')%>%
  #creating new variables based on plate location
  mutate(chelator=case_when(str_detect(well_ID,'B')==T ~ 'Blank',
                                str_detect(well_ID,'C')==T ~ 'Control',
                                str_detect(well_ID,'D')==T ~ 'DFOB',
                                str_detect(well_ID,'E')==T ~ 'TPEN',
                                str_detect(well_ID,'F')==T ~ 'BCS',
                                str_detect(well_ID,'G')==T ~ 'DMG+Ni'),
         chelator_conc=case_when(str_detect(well_ID,'[2-4]')==T ~ "5",
                              str_detect(well_ID,'[5-7]')==T ~ "10",
                              str_detect(well_ID,'8|9|10')==T ~ "20"),
         treat_med='NM',
         treat_date='0108',
         treat_inoc='0.02',
         treat_mod='chelator')-> growth_assay_long


#Plotting chelated media separately
growth_assay_chel <- growth_assay_long %>% dplyr::filter(growth_assay_long$chelator != "Blank" & growth_assay_long$chelator != "Control")
growth_assay_chel %>%
  ggplot(aes(x=time, y=OD, color=chelator_conc)) + geom_point(aes(fill=chelator_conc)) + facet_wrap(~chelator) + theme(text = element_text(size = 25))
#ggsave(file=paste("Chelator_Growth_Curves.svg"), plot=last_plot())

#Plotting culture controls 
growth_assay_ctrl <- growth_assay_long %>% dplyr::filter(growth_assay_long$chelator == "Control") 

growth_assay_ctrl <- growth_assay_ctrl %>%
  mutate(
    chelator = case_when(
      grepl("[6-8]", well_ID)        ~ "EtOH",
      grepl("[2-5]", well_ID)        ~ "N/A",
      grepl("9|10|11", well_ID)      ~ "Ni",
      TRUE                           ~ chelator
    )
  )
growth_assay_ctrl$chelator_conc <- "N/A"


growth_assay_long <- full_join(growth_assay_chel, growth_assay_ctrl)

growth_assay_long$chelator_conc <- factor(growth_assay_long$chelator_conc, levels = c("N/A", "5", "10", "20"))
growth_assay_long$chelator <- factor(growth_assay_long$chelator_conc, levels = c("DFOB", "BCS", "DMG+Ni", "TPEN", "Ni", "EtOH", "N/A"))

growth_assay_long %>% dplyr::filter(growth_assay_long$chelator == "TPEN" | growth_assay_long$chelator == "EtOH") %>%
  ggplot(aes(x=time, y=OD, color=chelator_conc)) + geom_point(aes(fill=chelator_conc)) + facet_wrap(~chelator) + theme(text = element_text(size = 25)) + scale_color_brewer(palette = "RdGy")
ggsave(file=paste("GC_TPEN.svg"), plot=last_plot())

growth_assay_long %>% dplyr::filter(growth_assay_long$chelator == "Ni" | growth_assay_long$chelator == "DMG+Ni") %>%
  ggplot(aes(x=time, y=OD, color=chelator_conc)) + geom_point(aes(fill=chelator_conc)) + facet_wrap(~chelator) + theme(text = element_text(size = 25)) + scale_color_brewer(palette = "RdGy")
ggsave(file=paste("GC_TPEN.svg"), plot=last_plot())

growth_assay_long %>% dplyr::filter(growth_assay_long$chelator == "DFOB" | growth_assay_long$chelator == "BCS" |  growth_assay_long$chelator == "N/A") %>%
  ggplot(aes(x=time, y=OD, color=chelator_conc)) + geom_point(aes(fill=chelator_conc)) + facet_wrap(~chelator) + theme(text = element_text(size = 25)) + scale_color_brewer(palette = "RdGy")



#ggsave(file=paste("Chelator_Growth_Curves.svg"), plot=last_plot())

# growth_assay_ctrl %>% 
#   ggplot(aes(x=time, y=OD, color=chelator)) + geom_point(aes(fill=chelator)) + theme(text = element_text(size = 25)) + facet_wrap(~chelator)
# ggsave(file=paste("Ctrl_Growth_Curves.svg"), plot=last_plot())

