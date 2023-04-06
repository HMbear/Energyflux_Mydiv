# Energyflux_Mydiv
Use for second project, to construct the food web and calculate the energy fluxes 
Collembola_Final <- Collembola_Community %>% full_join(Collembola_Alllength) %>% 
# The abundance == 1, but there is bodylength_mm 
# Collembola_twice %>% filter (Sample_ID %in% c("2021_May_61_5", "2021_May_73_5", "2021_May_61_510")) %>% 
#   filter(family %in% c("Arrhopalitidae", "Hypogastruridae", "Neanuridae")) --> "2021_May_61_5", "Arrhopalitidae" == 0.5
# -->  "2021_May_73_5", "Hypogastruridae" 0.5,  "2021_May_61_510", "Neanuridae", 0.3mm
  mutate(bodylength_mm = ifelse(Sample_ID == "2021_May_61_5" & family == "Arrhopalitidae", 0.5, 
                                ifelse(Sample_ID == "2021_May_73_5" & family == "Hypogastruridae", 0.5, 
                                       ifelse(Sample_ID == "2021_May_61_510" & family == "Neanuridae", 0.3, bodylength_mm )))) %>% 
  filter(!is.na(Abundance)) %>% 
  left_join(
  Collembola_twice %>% select(order, suborder, family) %>% unique 
) %>% left_join(LWbiomass %>% rename("suborder" = "Study_Group") %>% 
                  select(suborder, Measures, Functions, LC, a, b1, b2,`Function(WW-FW)`, c,d, lni0, s, E_evK) %>% mutate(b1 = as.numeric(b1))) %>% 
  mutate(DW_ug = a*(LC*bodylength_mm)^b1) %>% mutate(FW_ug = c*DW_ug^d)   %>%  ### The argument here, the function was attained by using the photography pictures
  mutate(Biomass_mgm2 = FW_ug*Density_m2/1000) %>% 
  mutate(Tk = ifelse(month == "May" & depth == 5,Temp_AprMay_soil5cm, ###Asing the environmental temperature based on the month and depth 
                     ifelse (month == "May" & depth == 510, Temp_AprMay_soil10cm, 
                             ifelse(month == "September" & depth == 5, Temp_AugSep_soil5cm, 
                                  Temp_AugSep_soil10cm)))) %>% ###Here is the hourly temperature, attained from 21 days poriors to sampling date, they are differed by time, and depth 
  mutate(Metabolic_Jhm2 = (exp(lni0))*(Biomass_mgm2^s)*(exp(-E_evK/(8.62*10^(-5)*Tk)))) %>%
   select(Sample_ID,year, month, plot, depth, order, suborder, family, Abundance,Density_m2, bodylength_mm, DW_ug, FW_ug, Biomass_mgm2, Metabolic_Jhm2)%>%
  ###this the average of individual biomass and biomass (both dry and fresh) of each family in each plot 
  ###0-5cm, what is the density of collembola, and what the biomass/m2, same for 5-10cm (have to remember to mentioned about depths)
  left_join(Feedgroup[,c("family","trophic.group")]) 

