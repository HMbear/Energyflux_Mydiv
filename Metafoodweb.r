library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)
library(ggridges)

###The links between animals, is there any links between them, potentailly 
Feedgroup <-  read_excel("Dataset_May/Taxonomic_Checklist_MyDiv_Energyfluxes.xlsx")

#####################################Potential links ###############################################
groups <- unique(Feedgroup$Group)
PPmat <- as.data.frame(matrix(0, nrow = length(groups), ncol = length(groups)))
colnames(PPmat) <- rownames(PPmat) <- groups

##
PPmat[which(grepl("Nemotoda", rownames(PPmat)) == TRUE), which(grepl("Nemotoda" , colnames(PPmat)) == TRUE) ] <- 1 #

PPmat[c(which(grepl("Nemotoda", rownames(PPmat)) == TRUE)), which(grepl("Oribatida", colnames(PPmat)) == TRUE) ] <- 1 #
PPmat[which(grepl("Nemotoda", rownames(PPmat)) == TRUE),  which(grepl("Collembola", colnames(PPmat)) == TRUE)  ] <- 1 #
PPmat[which(grepl("Nemotoda", rownames(PPmat)) == TRUE), which(grepl("Astigmata" , colnames(PPmat)) == TRUE) ] <- 1 #
PPmat[which(grepl("Nemotoda", rownames(PPmat)) == TRUE), which(grepl("Symphyla" , colnames(PPmat)) == TRUE) ] <- 1 #
PPmat[which(grepl("Nemotoda", rownames(PPmat)) == TRUE), which(grepl("Lumbricina" , colnames(PPmat)) == TRUE) ] <- 1 # 
PPmat[which(grepl("Nemotoda", rownames(PPmat)) == TRUE), which(grepl("Haplotaxida" , colnames(PPmat)) == TRUE) ] <- 1 

SoftMesoPreys <- c(which(grepl("Astigmata", rownames(PPmat)) == TRUE),
                   which(grepl("Protura", rownames(PPmat)) == TRUE),
                   which(grepl("Pauropoda", rownames(PPmat)) == TRUE),
                   which(grepl("Symphyla", rownames(PPmat)) == TRUE),
                   which(grepl("Collembola", rownames(PPmat)) == TRUE),
                   which(grepl("Mesostigmata", rownames(PPmat)) == TRUE),
                   which(grepl("Prostigmata", rownames(PPmat)) == TRUE))

PPmat[c(which(grepl("Nemotoda", rownames(PPmat)) == TRUE), SoftMesoPreys) , which(grepl("Prostigmata", colnames(PPmat)) == TRUE) ] <- 1
PPmat[c(which(grepl("Nemotoda", rownames(PPmat)) == TRUE), SoftMesoPreys) , which(grepl("Mesostigmata", colnames(PPmat)) == TRUE) ] <- 1

###Prostigmata prasites and Mesostigmata 
#They are parasite on a wide range of vertebrates and invertebrates, 
# free living ones can predate soil inhabiting mites and insects -- flies, thrips, fly larve, anthropod pests 
PPmat[which(grepl("Oribatida", rownames(PPmat)) == FALSE) , "Mesostigmata:Predators:Parasites"] <- 1 

Prostig_host <- c( ###similar as prey list 
  which(grepl("Coleoptera",rownames(PPmat)) == TRUE),
  which(grepl("Hymenoptera",rownames(PPmat)) == TRUE),
  which(grepl("Formicidae",rownames(PPmat)) == TRUE),
  which(grepl("Diptera",rownames(PPmat)) == TRUE),
  which(grepl("Hemiptera",rownames(PPmat)) == TRUE),
  which(grepl("Orthoptera",rownames(PPmat)) == TRUE),
  which(grepl("Neuropterida",rownames(PPmat)) == TRUE),
  which(grepl("Thysanoptera",rownames(PPmat)) == TRUE),
  which(grepl("Araneae",rownames(PPmat)) == TRUE),
  which(grepl("Opiliones",rownames(PPmat)) == TRUE),
  which(grepl("Lepidoptera",rownames(PPmat)) == TRUE)
)
PPmat[Prostig_host , "Prostigmata:Predators:Parasites"] <- 1 
 

PPmat[c(which(grepl("Diplura", rownames(PPmat)) == TRUE),
            which(grepl("Pseudoscorpiones", rownames(PPmat)) == TRUE) , 
            which(grepl("Larve", rownames(PPmat)) == TRUE), SoftMesoPreys) , which(grepl("Diplura", colnames(PPmat)) == TRUE) ] <- 1

PPmat[c(which(grepl("Psocoptera", rownames(PPmat)) == TRUE),
            which(grepl("Thysanoptera", rownames(PPmat)) == TRUE),
        which(grepl("Coleoptera-Larve", rownames(PPmat)) == TRUE) , 
        which(grepl("Oribatida", rownames(PPmat)) == TRUE) ,  ## Olga suggested
        which(grepl("Diplura", rownames(PPmat)) == TRUE) ,  ## Olga suggested
        which(grepl("Pseudoscorpiones", rownames(PPmat)) == TRUE) ,  ## Olga suggested
        SoftMesoPreys) , which(grepl("Pseudoscorpiones", colnames(PPmat)) == TRUE) ] <- 1
###Potapova et al., 2022,suggested "Pseudoscorpiones" may predate on beetles' larve, 


#####	Ixodida, they are parasites, blood sucking 
##
Ixodia_Predators <- c(
  which(grepl("Prostigmata", colnames(PPmat)) == TRUE),
  which(grepl("Mesostigmata", colnames(PPmat)) == TRUE),  
  which(grepl("Coleoptera:", colnames(PPmat)) == TRUE),  
  which(grepl("Formicidae", colnames(PPmat)) == TRUE), 
  which(grepl("Diptera:", colnames(PPmat)) == TRUE), 
  which(grepl("Hemiptera:", colnames(PPmat)) == TRUE), 
  which(grepl("Neuropterida", colnames(PPmat)) == TRUE), 
  which(grepl("Araneae:Wondering",colnames(PPmat)) == TRUE),
  which(grepl("Opiliones",colnames(PPmat)) == TRUE),
  which(grepl("Dermaptera", colnames(PPmat)) == TRUE)
  )
PPmat[which(grepl("Ixodida", rownames(PPmat)) == TRUE ) , Ixodia_Predators] <- 1 


#### Isopoda: don't eat animals 

###Chilopoda --> Predators Reference on Potapov 2022, convert the text as following codes 
# "Chilopoda:Lithobiomorpha"  --> Fungal channels, Collembola, and earthworms                             
# "Chilopoda:Geophilomorpha" --> Gastropoda, Bacterial channels
Chilop_Preys <- c(
  which(grepl("Diplura", rownames(PPmat)) == TRUE),
  which(grepl("Lumbricina", rownames(PPmat)) == TRUE), ##potapova et al., 2022
  which(grepl("Diplopoda", rownames(PPmat)) == TRUE ), ##potapova et al., (2022)
  which(grepl("Orthoptera", rownames(PPmat)) == TRUE), 
  which(grepl("Psocoptera", rownames(PPmat)) == TRUE), ## smaller size, primary decomposer (potapova et al., 2022)
  which(grepl("Thysanoptera", rownames(PPmat)) == TRUE ), ##smaller size, secondary decomposer (potapova)
  which(grepl("Isopoda", rownames(PPmat)) == TRUE ), ##suggested by Schuldt 
  which(grepl("Coleoptera",rownames(PPmat)) == TRUE),##suggested by schuldt 
  which(grepl("Hemiptera", rownames(PPmat)) == TRUE), #suggested by schuldt
  which(grepl("Araneae", rownames(PPmat)) == TRUE), ##potapova et al., 2022 
  which(grepl("Opiliones", rownames(PPmat)) == TRUE), 
  which(grepl("Dermaptera", rownames(PPmat)) == TRUE), 
  which(grepl("Gastropoda",rownames(PPmat)) == TRUE), ## potapova et al., 2022 
  which(grepl("Enchytraeidae", rownames(PPmat)) == TRUE), ##potapova et al., 2022
  which(grepl("Hemiptera-Juvenile", rownames(PPmat)) == TRUE), ##smaller sizes
  which(grepl("Larve", rownames(PPmat)) == TRUE), #smaller size
  which(grepl("Pupea", rownames(PPmat)) == TRUE)) ##smaller size

PPmat[c(SoftMesoPreys, Chilop_Preys ), which(grepl("Chilopoda", colnames(PPmat)) == TRUE )] <- 1


####Beetles
Carabi_Preys <- c( ## Mediumand large ground beetles predominantl feed on larger prey and rarely onmicroarthropods 
  which(grepl("Diplura", rownames(PPmat)) == TRUE),
   which(grepl("Collembola", rownames(PPmat)) == TRUE ),# ## smaller sizes Potapov et al., 2022
  which(grepl("Lumbricina", rownames(PPmat)) == TRUE), #Potapov et al., 2022
  which(grepl("Diplopoda",rownames(PPmat)) == TRUE),
  which(grepl("Isopoda", rownames(PPmat)) == TRUE ),#
  which(grepl("Chilopoda",rownames(PPmat)) == TRUE),
  which(grepl("Coleoptera",rownames(PPmat)) == TRUE),
  which(grepl("Formicidae", rownames(PPmat)) == TRUE), 
  which(grepl("Hemiptera", rownames(PPmat)) == TRUE), #
  which(grepl("Orthoptera", rownames(PPmat)) == TRUE), #
  #which(grepl("Raphidioptera", rownames(PPmat)) == TRUE), # 
  which(grepl("Thysanoptera",rownames(PPmat)) == TRUE),## smaller sizes
  which(grepl("Psocoptera", rownames(PPmat)) == TRUE), ## smaller sizes
  which(grepl("Opiliones", rownames(PPmat)) == TRUE), 
  which(grepl("Araneae", rownames(PPmat)) == TRUE), 
  which(grepl("Gastropoda",rownames(PPmat)) == TRUE), #Potapov et al., 2022
  which(grepl("Dermaptera",rownames(PPmat)) == TRUE),
  which(grepl("Enchytraeidae", rownames(PPmat)) == TRUE),
  which(grepl("Larve", rownames(PPmat)) == TRUE), #smaller size
  which(grepl("Pupea", rownames(PPmat)) == TRUE) ##smaller size
 ) 
PPmat[Carabi_Preys,  which(grepl("Coleoptera:", colnames(PPmat)) == TRUE ) ] <- 1 ###this is specific for adults 


#########rove beetles also adapted to hunt mites, so we add more preys on geenral predators 
PPmat[c(SoftMesoPreys, 
            which(grepl("Oribatida",rownames(PPmat)) == TRUE)),  which(grepl("Staphylinidae", colnames(PPmat)) == TRUE ) ] <- 1

####The larve of beetles 
PPmat[c(
  which(grepl("Enchytraeidae", rownames(PPmat)) == TRUE),
  which(grepl("Collembola", rownames(PPmat)) == TRUE ),
  which(grepl("Lumbricina", rownames(PPmat)) == TRUE), #Potapov et al., 2022
  which(grepl("Gastropoda",rownames(PPmat)) == TRUE), #Potapov et al., 2022
  which(grepl("Larve", rownames(PPmat)) == TRUE)
),  which(grepl("Coleoptera-Larve", colnames(PPmat)) == TRUE ) ] <- 1 ###this is specific for adults 


#####Hymenoptera 
Hemeno_Preys <- c( # Barnes et al., 2020; Cedar Creek BEstudy, correlation matrix 
  which(grepl("Diplopoda",rownames(PPmat)) == TRUE),
  which(grepl("Coleoptera",rownames(PPmat)) == TRUE), ## Are the sizes relaly
  which(grepl("Hymenoptera",rownames(PPmat)) == TRUE),
  which(grepl("Diptera", rownames(PPmat)) == TRUE), #
  which(grepl("Hemiptera", rownames(PPmat)) == TRUE), #
  #which(grepl("Raphidioptera", rownames(PPmat)) == TRUE), # 
  which(grepl("Neuropterida", rownames(PPmat)) == TRUE), 
  which(grepl("Psocoptera", rownames(PPmat)) == TRUE), ## smaller sizes 
  which(grepl("Dermaptera",rownames(PPmat)) == TRUE), 
  which(grepl("Araneae", rownames(PPmat)) == TRUE), 
  which(grepl("Opiliones", rownames(PPmat)) == TRUE), 
  which(grepl("Thysanoptera",rownames(PPmat)) == TRUE), ##smaller sizes
  which(grepl("Pseudoscorpiones", rownames(PPmat)) == TRUE), ## smaller sizes 
  which(grepl("Lepidoptera", rownames(PPmat)) == TRUE), 
  which(grepl("Orthoptera",rownames(PPmat)) == TRUE) ##suggested by schuldt 
)
PPmat[Hemeno_Preys, which(grepl("Hymenoptera:", colnames(PPmat)) == TRUE )] <- 1 ###Here only for adults 

###does normal predators of Hymenoptera differ from Hymenoptera:Parasites 
##Currently the codes for  `Hymenoptera:Predators:Parasites` are same as other predators 
##add the extra links with Hymenoptera:Predators:Parasites
PPmat[c(Hemeno_Preys, 
        which(grepl("Formicidae",rownames(PPmat)) == TRUE)), #suggested by schuldt 
      which(grepl("Hymenoptera:Predators:Parasites", colnames(PPmat)) == TRUE )] <- 1 ###Here only for adults 

####What is "Formicidae
Ant_Preys <- c(
  which(grepl("Diplura", rownames(PPmat)) == TRUE),
  which(grepl("Oribatida",rownames(PPmat)) == TRUE),
  which(grepl("Pauropoda",rownames(PPmat)) == TRUE), 
  which(grepl("Symphyla",rownames(PPmat)) == TRUE), 
  which(grepl("Protura",rownames(PPmat)) == TRUE), # suggested by olga 
  which(grepl("Collembola",rownames(PPmat)) == TRUE),
  which(grepl("Diplopoda",rownames(PPmat)) == TRUE), 
  which(grepl("Isopoda",rownames(PPmat)) == TRUE), 
  which(grepl("Chilopoda",rownames(PPmat)) == TRUE),
  which(grepl("Formicidae",rownames(PPmat)) == TRUE),
  which(grepl("Coleoptera",rownames(PPmat)) == TRUE), 
  which(grepl("Diptera",rownames(PPmat)) == TRUE), 
  which(grepl("Sternorrhyncha",rownames(PPmat)) == TRUE),
  which(grepl("Orthoptera",rownames(PPmat)) == TRUE), 
  which(grepl("Psocoptera",rownames(PPmat)) == TRUE), 
  which(grepl("Araneae",rownames(PPmat)) == TRUE),
  which(grepl("Opiliones", rownames(PPmat)) == TRUE), 
  which(grepl("Thysanoptera",rownames(PPmat)) == TRUE), 
  which(grepl("Gastropoda",rownames(PPmat)) == TRUE), 
  which(grepl("Lepidoptera",rownames(PPmat)) == TRUE), 
  which(grepl("Pupea",rownames(PPmat)) == TRUE), 
  which(grepl("Larve",rownames(PPmat)) == TRUE), 
  which(grepl("Juvenile",rownames(PPmat)) == TRUE), 
  which(grepl("Enchytraeidae", rownames(PPmat)) == TRUE),
  which(grepl("Lumbricina", rownames(PPmat)) == TRUE)
  )
PPmat[Ant_Preys, which(grepl("Formicidae", colnames(PPmat)) == TRUE )] <- 1 

PPmat[which(grepl("Hemiptera", rownames(PPmat)) == TRUE), #suggested by Schuldt
      c(which(grepl("Formicidae:Predators", colnames(PPmat)) == TRUE ), 
      which(grepl("Formicidae:Omnivores", colnames(PPmat)) == TRUE)) ] <- 1


####Diptera
###Dipera adults, predators, hunting, or normal predators

Diptera_Preys <- c(
  SoftMesoPreys, 
  which(grepl("Diplura", rownames(PPmat)) == TRUE),
  which(grepl("Diplopoda",rownames(PPmat)) == TRUE), 
  which(grepl("Isopoda",rownames(PPmat)) == TRUE), 
  which(grepl("Chilopoda",rownames(PPmat)) == TRUE),
  which(grepl("Coleoptera",rownames(PPmat)) == TRUE), 
  which(grepl("Diptera",rownames(PPmat)) == TRUE), 
  which(grepl("Sternorrhyncha",rownames(PPmat)) == TRUE),
  which(grepl("Neuropterida",rownames(PPmat)) == TRUE), 
  which(grepl("Psocoptera",rownames(PPmat)) == TRUE), 
  which(grepl("Araneae",rownames(PPmat)) == TRUE),
  which(grepl("Thysanoptera",rownames(PPmat)) == TRUE)
)
PPmat[Diptera_Preys, which(grepl("Diptera:", colnames(PPmat)) == TRUE )] <- 1  ###only apply to adults

PPmat[c(which(grepl("Formicidae",rownames(PPmat)) == TRUE), #suggested by schuldt 
which(grepl("Hymenoptera",rownames(PPmat)) == TRUE)),#suggested by schuldt 
                                       which(grepl("Diptera:Predators", colnames(PPmat)) == TRUE )] <- 1 

###The difference between normal predators and parasites 
PPmat[c(which(grepl("Hemiptera", rownames(PPmat)) == TRUE), #suggested by schuldt 
        which(grepl("Orthoptera",rownames(PPmat)) == TRUE),#suggested by schuldt 
        which(grepl("Opiliones",rownames(PPmat)) == TRUE),#suggested by schuldt 
        which(grepl("Larve",rownames(PPmat)) == TRUE)), 
                                      which(grepl("Diptera:Predators:Parasites", colnames(PPmat)) == TRUE )] <- 1 


##Diptera larve perform differently 
DipLarve_Preys <- c(SoftMesoPreys, ## Feeding on other insects, particularly Larve 
                    which(grepl("Diplura", rownames(PPmat)) == TRUE), ##
  which(grepl("Nemotoda", rownames(PPmat)) == TRUE), ## Feeding on soil microfuana 
  which(grepl("Lumbricina", rownames(PPmat)) == TRUE), ## Feeding on soil oligochaetes
  which(grepl("Enchytraeidae", rownames(PPmat)) == TRUE),
  which(grepl("Larve", rownames(PPmat)) == TRUE)
)
PPmat[c(DipLarve_Preys), which(grepl("Diptera-Larve", colnames(PPmat)) == TRUE )] <- 1  ###only apply to adults
PPmat["Neuropterida-Larve", which(grepl("Diptera-Larve", colnames(PPmat)) == TRUE )] <- 0  ###only apply to adults


###Hemiptera  # Barnes et al., 2020; Cedar Creek BEstudy, correlation matrix  and Anton's paper 
Hemi_Preys <- c(  which(grepl("Psocoptera", rownames(PPmat)) == TRUE), ## smaller sizes 
                  which(grepl("Diplura", rownames(PPmat)) == TRUE),
                 which(grepl("Diplopoda", rownames(PPmat)) == TRUE), 
                 which(grepl("Formicidae", rownames(PPmat)) == TRUE),
                 which(grepl("Hemiptera", rownames(PPmat)) == TRUE), #
                 which(grepl("Orthoptera", rownames(PPmat)) == TRUE), #
                 which(grepl("Neuropterida:", rownames(PPmat)) == TRUE), #
                 which(grepl("Thysanoptera",rownames(PPmat)) == TRUE),
                 which(grepl("Opiliones", rownames(PPmat)) == TRUE), 
                 which(grepl("Araneae", rownames(PPmat)) == TRUE), 
                 which(grepl("Dermaptera",rownames(PPmat)) == TRUE),
                 which(grepl("Gastropoda",rownames(PPmat)) == TRUE), #the size is way much bigger than Hemi_Predators 
                 which(grepl("Pseudoscorpiones", rownames(PPmat)) == TRUE), ## smaller sizes 
                 which(grepl("Coleoptera-Larve",rownames(PPmat)) == TRUE), ## Coleopera:Herbivores in our study is way more larger
                 which(grepl("Lepidoptera-Larve",rownames(PPmat)) == TRUE),
                 which(grepl("Diptera-Larve",rownames(PPmat)) == TRUE) ##The HemiPedators can barely fly 
) 

PPmat[Hemi_Preys,  which(grepl("Hemiptera", colnames(PPmat)) == TRUE )]   <- 1

### The hemiptera who only rely on predation, they probably have a wider niches 
PPmat[c(
  which(grepl("Collembola", rownames(PPmat)) == TRUE), ## suggested by schuldt 
  which(grepl("Chilopoda:Geophilomorpha", rownames(PPmat)) == TRUE), ## suggested by schuldt 
  which(grepl("Coleoptera", rownames(PPmat)) == TRUE), ## suggested by schuldt 
  which(grepl("Hymenoptera-Larve", rownames(PPmat)) == TRUE)),  ## suggested by schuldt 
                              which(grepl("Hemiptera:Predators", colnames(PPmat)) == TRUE )]   <- 1

##Orthoptera
Orth_Preys <- c ( ### Predate small insects ## Song 2018 
  which(grepl("Collembola", rownames(PPmat)) == TRUE ),# 
  which(grepl("Diplura", rownames(PPmat)) == TRUE),
  which(grepl("Diptera",rownames(PPmat)) == TRUE),
  which(grepl("Hemiptera", rownames(PPmat)) == TRUE), #
  which(grepl("Thysanoptera",rownames(PPmat)) == TRUE),
  which(grepl("Psocoptera",rownames(PPmat)) == TRUE)
)
PPmat[Orth_Preys,  which(grepl("Orthoptera", colnames(PPmat)) == TRUE )]   <- 1
 

## "Trichoptera:Predators"  --> Rhyacophilidae, the adults don't eat, like moth 
Neurop_Preys <- c(
  which(grepl("Prostigmata", rownames(PPmat)) == TRUE),
  which(grepl("Hymenoptera:", rownames(PPmat)) == TRUE),
  which(grepl("Formicidae", rownames(PPmat)) == TRUE),
  which(grepl("Diptera", rownames(PPmat)) == TRUE),
  which(grepl("Hemiptera:Herbivores", rownames(PPmat)) == TRUE)
  # Devetak and Klokocovik 2016, a review on Neuropterida 
)
PPmat[Neurop_Preys,  which(grepl("Neuropterida:", colnames(PPmat)) == TRUE )]   <- 1

##Neurope_Larve, the predators
NeuropLarve_Preys <- c(
  SoftMesoPreys, 
  which(grepl("Prostigmata", rownames(PPmat)) == TRUE),
  which(grepl("Oribatida", rownames(PPmat)) == TRUE),
  which(grepl("Ixodida", rownames(PPmat)) == TRUE),
  which(grepl("Formicidae", rownames(PPmat)) == TRUE)
  # Devetak and Klokocovik 2016, a review on Neuropterida 
)
PPmat[NeuropLarve_Preys,  which(grepl("Neuropterida-Larve", colnames(PPmat)) == TRUE )]   <- 1

####Raphidioptera
Odonate_Preys <- c(
  # which(grepl("Diplura", rownames(PPmat)) == TRUE),
  # which(grepl("Collembola", rownames(PPmat)) == TRUE),
  which(grepl("Coleoptera:", rownames(PPmat)) == TRUE), 
  which(grepl("Hymenoptera:", rownames(PPmat)) == TRUE),
  which(grepl("Diptera:", rownames(PPmat)) == TRUE),
  which(grepl("Hemiptera:", rownames(PPmat)) == TRUE),
  which(grepl("Orthoptera", rownames(PPmat)) == TRUE),
  which(grepl("Neuropterida:", rownames(PPmat)) == TRUE),
  which(grepl("Lepidoptera:", rownames(PPmat) ) == TRUE), 
  which(grepl("Odonate", colnames(PPmat)) == TRUE)
)
PPmat[Odonate_Preys,  which(grepl("Odonate", colnames(PPmat)) == TRUE )]   <- 1

# Have combine this into Neuropterida 
##Easily miss how is prey on this one 
# Raphid_Predators <- c(
#   which(grepl("Araneae:Weavers", colnames(PPmat)) == TRUE),
#   which(grepl("Coleoptera:", colnames(PPmat)) == TRUE),
#   which(grepl("Hymenoptera:", colnames(PPmat)) == TRUE),
#   which(grepl("Opiliones", colnames(PPmat)) == TRUE),
#   which(grepl("Odonate", colnames(PPmat)) == TRUE)
# )
# PPmat[ which(grepl("Raphidioptera",rownames(PPmat)) == TRUE ), Raphid_Predators]   <- 1


###Spiders 
SpiderWonder_Preys <- c( ## Anton's paper,  # Barnes et al., 2020; Cedar Creek BEstudy, correlation matrix
  ## Hines et al., 2019 Jena interaction matrixes 
  which(grepl("Pauropoda", rownames(PPmat)) == TRUE), ##suggested by schuldt
  which(grepl("Symphyla", rownames(PPmat)) == TRUE), ##suggested by schuldt
  which(grepl("Diplura", rownames(PPmat)) == TRUE),
  which(grepl("Collembola", rownames(PPmat)) == TRUE),
  which(grepl("Chilopoda", rownames(PPmat)) == TRUE),
  which(grepl("Isopoda", rownames(PPmat)) == TRUE),
  which(grepl("Diplopoda", rownames(PPmat)) == TRUE),
  which(grepl("Coleoptera", rownames(PPmat)) == TRUE), 
  which(grepl("Hymenoptera", rownames(PPmat)) == TRUE),
  which(grepl("Formicidae", rownames(PPmat)) == TRUE),
  which(grepl("Diptera", rownames(PPmat)) == TRUE),
  which(grepl("Hemiptera", rownames(PPmat)) == TRUE),
  which(grepl("Orthoptera", rownames(PPmat)) == TRUE),
  which(grepl("Thysanoptera", rownames(PPmat)) == TRUE),
  which(grepl("Psocoptera", rownames(PPmat)) == TRUE),
  which(grepl("Neuropterida", rownames(PPmat)) == TRUE),
  which(grepl("Opiliones", rownames(PPmat)) == TRUE),
  which(grepl("Araneae", rownames(PPmat)) == TRUE),
  which(grepl("Dermaptera", rownames(PPmat)) == TRUE),
  which(grepl("Pseudoscorpiones", rownames(PPmat)) == TRUE),
  which(grepl("Odonate", rownames(PPmat)) == TRUE), 
  which(grepl("Larve", rownames(PPmat)) == TRUE)
)

PPmat[SpiderWonder_Preys, which(grepl("Araneae:Wandering", colnames(PPmat)) == TRUE )] <- 1

######for the araneae:weavers, they are more likely to adapt to catch flying insects 
##They are less likely feed on Larves 
SpiderWeaver_Preys <- c( ## Anton's paper,  # Barnes et al., 2020; Cedar Creek BEstudy, correlation matrix
  ## Hines et al., 2019 Jena interaction matrixes 
   which(grepl("Diplura", rownames(PPmat)) == TRUE),
   which(grepl("Collembola", rownames(PPmat)) == TRUE),
   which(grepl("Chilopoda", rownames(PPmat)) == TRUE),
   which(grepl("Isopoda", rownames(PPmat)) == TRUE),
  which(grepl("Diplopoda", rownames(PPmat)) == TRUE),
  which(grepl("Coleoptera:", rownames(PPmat)) == TRUE), 
  which(grepl("Hymenoptera:", rownames(PPmat)) == TRUE),
  which(grepl("Formicidae", rownames(PPmat)) == TRUE),
  which(grepl("Diptera:", rownames(PPmat)) == TRUE),
  which(grepl("Hemiptera:", rownames(PPmat)) == TRUE),
  which(grepl("Orthoptera", rownames(PPmat)) == TRUE),
  which(grepl("Thysanoptera", rownames(PPmat)) == TRUE),
  which(grepl("Psocoptera", rownames(PPmat)) == TRUE),
  which(grepl("Neuropterida:", rownames(PPmat)) == TRUE),
  which(grepl("Opiliones", rownames(PPmat)) == TRUE),
  which(grepl("Araneae", rownames(PPmat)) == TRUE),
  which(grepl("Dermaptera", rownames(PPmat)) == TRUE),
  which(grepl("Pseudoscorpiones", rownames(PPmat)) == TRUE),
  which(grepl("Odonate:", rownames(PPmat)) == TRUE)
)

PPmat[SpiderWeaver_Preys, which(grepl("Araneae:Weavers", colnames(PPmat)) == TRUE )] <- 1


####Opiliones
Opilion_Preys <- c( ## Potapov et al., 2022
  which(grepl("Diplura", rownames(PPmat)) == TRUE),
  which(grepl("Collembola", rownames(PPmat)) == TRUE),
  which(grepl("Mesostigmata", rownames(PPmat)) == TRUE),
  which(grepl("Diplopoda", rownames(PPmat)) == TRUE),
  which(grepl("Isopoda", rownames(PPmat)) == TRUE),
  which(grepl("Araneae", rownames(PPmat)) == TRUE),
  which(grepl("Coleoptera", rownames(PPmat)) == TRUE),
  which(grepl("Hymenoptera", rownames(PPmat)) == TRUE), ##suggested by schuldt
  which(grepl("Hemiptera", rownames(PPmat)) == TRUE),
  which(grepl("Thysanoptera", rownames(PPmat)) == TRUE),
  which(grepl("Gastropoda",rownames(PPmat)) == TRUE), 
  which(grepl("Diptera", rownames(PPmat)) == TRUE),
  #which(grepl("Raphidioptera", rownames(PPmat)) == TRUE),
  which(grepl("Neuropterida:", rownames(PPmat)) == TRUE),
  which(grepl("Opiliones", rownames(PPmat)) == TRUE),
  which(grepl("Pseudoscorpiones", rownames(PPmat)) == TRUE),
  which(grepl("Larve", rownames(PPmat)) == TRUE),
  which(grepl("Pupea", rownames(PPmat)) == TRUE),
  which(grepl("Lumbricina", rownames(PPmat)) == TRUE),
  which(grepl("Lepidoptera", rownames(PPmat) ) == TRUE)
)

PPmat[Opilion_Preys, which(grepl("Opiliones", colnames(PPmat)) == TRUE )] <- 1


##Dermaptera
Dermaptera_Preys <- c (
  which(grepl("Diplura", rownames(PPmat)) == TRUE),
  which(grepl("Collembola", rownames(PPmat)) == TRUE), ##Suggested by Olga
  which(grepl("Coleoptera", rownames(PPmat)) == TRUE),###suggested by Olga 
  which(grepl("Hemiptera", rownames(PPmat)) == TRUE),
  which(grepl("Diptera:", rownames(PPmat)) == TRUE), ##suggested by Olga 
  ##maybe they don't eat diptera:parasites 
  which(grepl("Diptera-Larve", rownames(PPmat)) == TRUE),
  which(grepl("Neuropterida:", rownames(PPmat)) == TRUE),
  which(grepl("Psocoptera", rownames(PPmat)) == TRUE), 
  which(grepl("Gastropoda",rownames(PPmat)) == TRUE), ##suggested by Olga 
  which(grepl("Larve", rownames(PPmat)) == TRUE), ##suggested by Olga
  which(grepl("Lepidoptera", rownames(PPmat) ) == TRUE)
)

PPmat[Dermaptera_Preys, which(grepl("Dermaptera", colnames(PPmat)) == TRUE )] <- 1


###All of the prey-predator interaction only validate if their diets include animals 
is.pred <- Feedgroup[,c("Group", "Animals")]  %>% unique %>% mutate(Animals = ifelse(is.na(Animals), 0, 1))

PPMAT <- matrix(NA, nrow = length(is.pred$Group), ncol = length(is.pred$Group))
for(i in 1:length(is.pred$Group)) {
  for(p in 1:length(PPmat)){
    PPMAT[p,i] <- PPmat[p,i]*is.pred$Animals[[i]]
  }
}
colnames(PPMAT) <- rownames(PPMAT) <- groups

# # ##Combine other resources into the prey-predators matrix
#  Otherfoods <- Feedgroup[,c("Group", "Plants", "Algea", "Detritus", "Bacteria", "Fungi")] %>% unique %>% 
#    mutate_at (c("Plants", "Algea", "Detritus", "Bacteria", "Fungi" ),~ifelse(is.na(.x), 0,1)) %>% select(!Group)
# #
#  PPMAT <- rbind(t(Otherfoods), PPMAT) %>% as.data.frame %>% mutate (Plants =0, Algea = 0, Detritus = 0, Bacteria = 0, Fungi = 0, .before = "Oribatida:Herbivores-Fungivores")
#  PPMAT["Detritus", "Bacteria"] <- 1
#  PPMAT[c("Detritus", "Plants"), "Fungi"] <- 1
# 
# ###output and double-check
# write.csv(PPMAT, file = "PPMAT_3rd.csv")
