# Energyflux_Mydiv
Use for second project, to construct the food web and calculate the energy fluxes 
#load the datasets to change for the efficiency 
Prop_CN <- read_excel("PropCandPropN_Functionalgroup.xlsx") 
Prop_CN$Logit_ea = 0.471*(Prop_CN$PropN*100) - 2.097
Prop_CN$efficiency = round(exp(Prop_CN$Logit_ea)/(1+exp(Prop_CN$Logit_ea)),2)
#load the codes for mydiv treatments 
Mydiv_code <- read_csv("H:/1. Huimin_2021/Energy flux/Datasets/Copy of Mydiv_code.csv")

####The functions what we are interested in  
functions <- c("month", "Plot","tot.flux", "pred.flux" , "herb.flux", "algi.flux" , "detr.flux", 
                           "fungi.flux", "bact.flux", "decom.fluxes")

##Create the data frame
fluxe.frame <- as.data.frame(matrix(ncol = length(functions), nrow = 160, byrow = TRUE))
colnames(fluxe.frame) <- functions

Sampling_time <- c("May", "September")
resource <- c("Plants", "Algea", "Detritus", "Bacteria", "Fungi" )
for (m in 1:2) { #month
for (p in 1:80) { #p is the plot values, numeric 
  ##Prepare the data sets of each plot, biomass, metabolic.loss, assimulation efficiency
  #add the resource data before the consumers (all animals)
  ##m = 1; p = 15
Data.plot <- data.frame(month = rep(Sampling_time[m], length = length(resource)), 
                   plot = rep(p, length = length(resource)), 
                   Group = resource, 
                   sum_Biomass_mgm2 = c(10^6, 1000, 10^5, 2000,64000), ###Still need to change based on the information we have 
                   sum_Metabolic_Jhm2 = 0) %>% bind_rows( 
                     ##prepare the data of animals 
  AllBiomass %>% filter(month == Sampling_time[m] & plot == p) %>% 
    group_by(month, plot, Group) %>% summarise(sum_Biomass_mgm2 = sum(Biomass_mgm2, na.rm = TRUE),
                                               sum_Metabolic_Jhm2 = sum(Metabolic_Jhm2, na.rm = TRUE),.groups = "drop" ) ) %>%
  ##include the efficiency data in 
  left_join(Prop_CN[,c("Group", "efficiency")]) 

biomass <- Data.plot$sum_Biomass_mgm2
metabolic.rates <- Data.plot$sum_Metabolic_Jhm2
efficiencies <- Data.plot$efficiency
consumer <- Data.plot %>% filter(!(Group %in% resource)) %>% select(Group) %>% unique %>% as.vector()
consumer <- consumer[["Group"]]

################Based on the functional groups we foun in each plot, create the interaction matrix at plot 
#####meta-food webs for all plots 
PPMAT.plot <- as.matrix(PPMAT[consumer, consumer])*matrix_Adjacency[consumer, consumer]

# The preference of the animals will also depend on the biomass of the preys 
PPMAT.plot <- PPMAT.plot*biomass[(Data.plot$Group %in% consumer)]

# down-weighing cannibalism
diag(PPMAT.plot) = diag(PPMAT.plot)*0.01

#hist(colSums(matrix_Adjacency))
# Scale the contribution of animals to predators and omnivores to 1 separately
# Scale the matrix while preserving zero columns

# down-weighing cannibalism
diag(PPMAT.plot) = diag(PPMAT.plot)*0.01

#hist(colSums(matrix_Adjacency))
# Scale all fauna separately
# Scale the matrix while preserving zero columns
PPMAT.plot <-
  scale(PPMAT.plot[,which(colSums(PPMAT.plot) != 0)], ##There are around 4 columns with all zero 
        center = FALSE,
        scale = colSums(PPMAT.plot[,which(colSums(PPMAT.plot) != 0)], na.rm = T)) %>% 
  bind_cols(PPMAT.plot[,which(colSums(PPMAT.plot) == 0)])

PPMAT.plot <- PPMAT.plot[, consumer]

# here change proportion of 1 to proportion of Fa
matrix_Resources.plot <- matrix_Resources2[,consumer]
for (c in seq(length(consumer))) {
  for (r in seq(length(consumer))){
    PPMAT.plot[r,c] <-  PPMAT.plot[r,c] * matrix_Resources.plot["Animals",c]
  }
}

##used to check whether we scale the proportion of the preys 
#plot(colSums(PPMAT.plot)~matrix_Resources.plot["Animals",])
#combine the preys + other resource (plants, algea, detritus, becateria, fungi)
PPMAT.plot <- rbind(matrix_Resources.plot[resource,], PPMAT.plot)

PPMAT.plot <-  data.frame(#combine the resource in, to make sure the rows and columns are same
    Plants = rep(0,length = length(resource) + length(consumer)),  
    Algae = rep(0,length = length(resource) + length(consumer)),           
    Detritus = 0,
    Bacteria = 0,
    Fungi = 0,
    stringsAsFactors = FALSE       # Ensures character columns are not treated as factors
  ) %>% bind_cols( as.data.frame(PPMAT.plot)) %>% as.matrix()
PPMAT.plot["Detritus", "Bacteria"] <- 1
PPMAT.plot["Detritus", "Fungi"] <- 0.5
PPMAT.plot["Plants", "Fungi"] <- 0.5
rownames(PPMAT.plot) <-  c(resource, consumer)


# run the energy flux estimation
Data_Fluxes <- fluxing(PPMAT.plot,biomass,metabolic.rates,efficiencies,
                       bioms.prefs = F, bioms.losses = F, ef.level = "prey") ### make sure they are false 

month = unique(Data.plot$month)
Plot = unique(Data.plot$plot)
tot.flux  = sum(Data_Fluxes)
pred.flux = sum(Data_Fluxes[consumer,])
herb.flux = sum(Data_Fluxes["Plants", consumer])
algi.flux = sum(Data_Fluxes["Algea", consumer])
detr.flux = sum(Data_Fluxes["Detritus",consumer])
fungi.flux = sum(Data_Fluxes["Fungi", consumer])
bact.flux = sum(Data_Fluxes["Bacteria", consumer])
decom.flux = sum(Data_Fluxes["Detritus",])

fluxes.results <- vector()
fluxes.results <- c(month , Plot ,tot.flux, pred.flux , herb.flux, algi.flux , detr.flux , 
                    fungi.flux , bact.flux, decom.flux)
fluxe.frame[((m-1)*80+p),] <- paste( fluxes.results)
}
}

fluxe.frame <- fluxe.frame %>% mutate(across(.cols = functions[(grepl("flux", functions) == TRUE)] ,~as.numeric(.))) %>% 
  mutate(across(.cols = functions[(grepl("flux", functions) == TRUE)] ,~log10(.))) %>%
  rename("plot" = "Plot") %>% left_join((Mydiv_code %>% mutate(plot = as.character(plot))))
