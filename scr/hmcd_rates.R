##############################################################################·
# Author: Marti Rovira
# Date: 27/11/2022
# Input: Data on homicide rates per country World Bank 
        # Downloaded from: https://data.worldbank.org/indicator/VC.IHR.PSRC.P5
        # Data on regions & Subregions is taken from the UN
# Output: Graphs (.png) on homicide rates per country for each continent
##############################################################################·

#### Uploading libraries ####
library(tidyverse) # Main functions
library(readxl) # Reading excel
library(ggpubr) # Advancing features for ggplot2
library(gridExtra) # Advancing features for ggplot2


#### Preparing and cleaning data ####

## Uploading data

hmcd_rts_wide <- read_csv("./raw_data/rates.csv", skip=3) %>%
  select(!`1960`:`1989`) %>%
  select(!`...67`) 

subregions_UN <- read_excel("./raw_data/Prison_Rates_ONU.xlsx") %>%
  select(`Country Name`="Countries", 
         subregion_UN="Sub Region", 
         region_UN="Region") %>%
  distinct(`Country Name`, .keep_all = TRUE) %>%
  mutate(`Country Name`=case_when(
        `Country Name`=="United Kingdom (England and Wales)" ~ "United Kingdom",
        `Country Name`=="United States of America" ~ "United States",
          TRUE ~ `Country Name`)
  )

## Transforming wide to long

hmcd_rts_long <- hmcd_rts_wide %>%
  select("Country Name", "Country Code", `1990`:`2021`) %>%
  pivot_longer(`1990`:`2021`, names_to="year", values_to="hmcd_rate") %>%
  right_join(subregions_UN)

rm(hmcd_rts_wide)

## Selecting only those countries with more than 10 observations

hmcd_rts_long_n <- hmcd_rts_long %>%
  filter(!is.na(hmcd_rate)) %>%
  group_by(`Country Name`) %>%
  summarize(
    n=n()
  ) 

hmcd_rts_long <- hmcd_rts_long %>%
  right_join(hmcd_rts_long_n) %>%
  filter(n>10) # Deleting data from countries w/ < 10 obs.

rm(hmcd_rts_long_n)
#### Graph Building ####

x_lab = "Year"
y_lab = "Homicide rate"

p <- list()
for (i in unique(hmcd_rts_long$region_UN)) {
  hmcd_rts_long_i<- hmcd_rts_long %>%
    filter(
      region_UN==i
    )
  
  p[[i]] <- ggplot(
    hmcd_rts_long_i,
    aes(x = year, 
        y = hmcd_rate, 
        color = `Country Name`, 
        group = `Country Name`)) +
    geom_line() + 
    facet_wrap(~ `subregion_UN` + `Country Name`) +
    ggtitle(i) +
    theme(
      plot.title=element_text(face="bold", 
                              size=18, 
                              hjust = 0.5, 
                              family="Times New Roman"),
      legend.position="none", 
      axis.text.x = element_text(angle=90, 
                                 hjust=1, 
                                 size = 16, 
                                 family="Times New Roman"),
      axis.text=element_text(size=16, 
                             family="Times New Roman"),
      axis.title=element_text(size=16, 
                              colour = "grey30", 
                              family="Times New Roman"),
      strip.text.x = element_text(size = 16, 
                                  family="Times New Roman"),
      legend.text=element_text(size=16,  
                               face="italic", 
                               family="Times New Roman"),
      legend.title = element_blank(),
      strip.text = element_text(size = 16, 
                                face= c("bold")),
      panel.background = element_rect(colour = NA, fill = NA),
      #strip.background =element_rect(fill="white"),
      #legend.background=element_blank(),
      #legend.key = element_rect(fill = NA)
    ) + 
    scale_x_discrete(breaks=c(1990,1995,2000,2005,2010,2015,2020)) +
    guides(linetype = "none")
  
  file_name=str_c("./viz/",i,".png")
  
  ggsave(plot=p[[i]], file=file_name, width = 27, height = 16)

  rm(hmcd_rts_long_i)
}

rm(p, file_name, i, x_lab, y_lab)
