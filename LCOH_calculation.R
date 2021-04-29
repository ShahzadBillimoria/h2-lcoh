#Colour plot for Levelised Cost Of Hydrogen (LCOH) ----
#Compare how LCOH changes for variation in capacity factor and cost of electricity.

#For faster, simpler equation, see https://www.e-education.psu.edu/eme801/node/560

#Get required packages ----
library(tidyverse)
library(RColorBrewer)
library(ggplot2)

#Clear all variables
rm(list = ls())

#Define static variables ----
plantLife <- 20 #Lifetime of plant in years
hoursInYear <- 8760 #hours in year
plantSize <- 1 #capacity of plant, kW
capPayment <- 0 #AUD/kW/year

#Define sensitivity variables ----

#Create vectors of variables
capFac <- #0.9 %>% data.frame(capFac=.) #capacity factor as a fraction, where 0.5 = 50%
  seq(from = 0, to = 1, length = 101) %>% data.frame(capFac=.)
elecCost <- #25 %>% data.frame(elecCost=.)
  seq(from = 0, to = 100, length = 101) %>% data.frame(elecCost=.) #AUD/MWh
  #c(0, 10, 20) %>% data.frame(elecCost=.)
capex <- 700 %>% data.frame(capex=.) #AUD/kW
  #seq(from = 0, to = 2000, length= 101) %>% data.frame(capex=.)
  #c(500, 1000, 1500) %>% data.frame(capex=.)
opex_pct <- 2 %>% data.frame(opex_pct=.) #percentage of capex
  #seq(from = 0, to = 1, length= 11) %>% data.frame(opex=.)
r <- 0.07 %>% data.frame(r=.) #WACC or discount rate, where 0.07 = 7%
  #seq(from = 0, to = 0.2, length= 101) %>% data.frame(r=.)
elecEff_pct <- 75 %>% data.frame(elecEff_pct=.) #electrolyser efficiency as a percentage
  #seq(from = 50, to = 100, length= 100) %>% data.frame(elecEff_pct=.)
elecEff <- 39.4 / (elecEff_pct/100) %>% data.frame() %>% 
  dplyr::transmute(elecEff = elecEff_pct) #electrolyser efficiency in kWh/kg H2
t <- seq(from = 0, to = plantLife, by = 1) %>% data.frame(t=.) #create vector of years

# #Combine vectors into dataframe of all unique combinations

working_df <- tidyr::crossing(capFac, elecCost, capex, opex_pct, r, elecEff_pct)

# df_list <- list(capFac, elecCost, capex, opex_pct, r, elecEff_pct, t)
# working_df <- df_list %>%
#   purrr::reduce(tidyr::crossing)

#Calculation for plot ----

# #Perform calculation
# working_df <- working_df %>%
#   dplyr::mutate(elecEff = 39.4 / (elecEff_pct/100),
#                 opex = capex * (opex_pct/100),
#                 kg_H2_produced = ifelse(t == 0, 0, plantSize * hoursInYear * capFac / elecEff),
#                 kg_H2_produced_disc = kg_H2_produced / (1 + r)^t,
#                 capex_AUD = ifelse(t == 0, plantSize * capex, 0),
#                 opex_AUD = ifelse(t == 0, 0, plantSize * opex),
#                 elecCost_AUD = ifelse(t == 0, 0, kg_H2_produced * (elecEff / 1000) * elecCost),
#                 capPayment_AUD = ifelse(t == 0, 0, plantSize * capPayment),
#                 annualCost = capex_AUD + opex_AUD + elecCost_AUD - capPayment_AUD,
#                 annualCostDisc = annualCost / (1 + r)^t
#   ) %>%
#   dplyr::group_by(capFac, elecCost, capex, opex_pct, opex, r, elecEff_pct, elecEff) %>%
#   dplyr::summarise(kg_H2_produced = sum(kg_H2_produced),
#                    kg_H2_produced_disc = sum(kg_H2_produced_disc),
#                    totalCost = sum(annualCost),
#                    totalCostDisc = sum(annualCostDisc)
#                    ) %>%
#   dplyr::mutate(LCOH = totalCostDisc / kg_H2_produced_disc) %>%
#   dplyr::ungroup() %>% 
#   dplyr::mutate(LCOH_alt = (capex * r) / (1-(1+r)^(-plantLife)) / (kg_H2_produced/plantLife)
#                + (opex + (kg_H2_produced/plantLife * (elecEff / 1000) * elecCost)) / (kg_H2_produced/plantLife))


#Alternative simpler, faster calculation that does NOT require each year to be calculated separately and then summed
#Requires certain things to be true e.g. all capex incurred at t=0, same annual ouput per year for entire life,...
  #...same operating cost per year for entire life, etc.

working_df <- working_df %>% 
  dplyr::mutate(elecEff = 39.4 / (elecEff_pct/100),
                opex = capex * (opex_pct/100),
                capPayment_annual = plantSize * capPayment,
                kg_H2_produced_annual = plantSize * hoursInYear * capFac / elecEff,
                elecCost_annual = kg_H2_produced_annual * (elecEff / 1000) * elecCost,
                LCOH = ifelse(r==0, capex / plantLife / kg_H2_produced_annual, (capex * r) / (1-(1+r)^(-plantLife)) / kg_H2_produced_annual)
                      + (opex + elecCost_annual - capPayment_annual) / kg_H2_produced_annual
                #simplified LCOH formula taken from https://www.e-education.psu.edu/eme801/node/560
                )



#Decide what to filter on ----
working_df <- working_df %>% 
  dplyr::mutate(capFac = capFac * 100)

final_df <- working_df %>% 
  #dplyr::filter(round(capFac) == 50 &
  #              round(elecCost) == 90
  #              )
  #dplyr::filter(LCOH <= 2)
  dplyr::filter(capFac >= 1)
  #dplyr::filter(elecCost <= 30)
  #dplyr::filter(LCOH <= 2)
  #dplyr::filter(r >= 0 & r <= 0.01)
  #dplyr::filter(capex >= 500)
  #dplyr::filter(elecCost >= 25 & elecCost <= 75) %>% 
  #dplyr::filter(capFac >= 25 & capFac <= 75)


#Plot results ----

minLCOH <- round(min(final_df$LCOH), 2)
maxLCOH <- #round(max(final_df$LCOH), 2)
  6
#breaks <- seq(from = minLCOH, to = maxLCOH, length = 12) %>% round(.,2)

#heatmap(as.matrix(final_df), Rowv = NA, Colv = NA, scale = "column")
ggplot(final_df, aes(capFac, elecCost)) + 
  geom_raster(aes(fill = LCOH)) +
  #geom_contour(aes(z = LCOH), color = "black", size = 1, breaks = seq(from = 2, to = 2, by = 1), linetype = "solid") +
  #geom_text_contour(aes(z = LCOH), color = "white") +
  #scale_fill_gradient2(low = "green", mid = "white", high = "red", midpoint = 8)
  labs(x = "Electrolyser annual capacity factor (%)",
       #x = "WACC (%)",
       #x = "Electrolyser efficiency (%)",
       #x = "Electricity cost (AUD/MWh)",
       y = "Electricity cost (AUD/MWh)",
       #y = "Capex (AUD/kW)",
       fill = "LCOH
(AUD/kg H2)",
        title = "Levelised cost of H2 (AUD/kg) for varying electrolyser capacity factor
and electricity cost",
       subtitle = paste("System capex ", capex, " AUD/kW, WACC ", r * 100,  "%, Opex ", opex_pct, "% of capex per year, 
Lifetime ", plantLife, " years, ", elecEff_pct, "% electrolyser efficiency (", round(elecEff,1), " kWh/kg H2)", sep="")) +
  scale_fill_gradientn(#colors = rev(rainbow(10)),
                       #low = "green4", mid = "gold", high = "firebrick", midpoint = (minLCOH+maxLCOH)/2,
                       colors = brewer.pal(n = 10, name = "RdYlGn") %>% rev(),
                       limits = c(minLCOH, maxLCOH),
                       #breaks = c(minLCOH, 2, 4, 6, 8, 10, 12, 14, maxLCOH)
                       breaks = c(minLCOH, 1, 2, 3, 4, 5, 6, 7, maxLCOH),
                       na.value = "white"
                       #breaks = breaks
                       ) + 
  guides(fill = guide_colourbar(barheight = 10)) +
  theme(panel.ontop = TRUE,
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10)
  ) +
  scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10)
                     ) + 
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10))
  #annotate(geom = "text", label = "<= 2 AUD/kg", x = 70, y = 18, color = "black")
  #facet_wrap(~ capex)

