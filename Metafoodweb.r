# Energyflux_Mydiv
Use for second project, to construct the food web and calculate the energy fluxes 
#####Check the species from belowground
## Group names 
SoilMeso <- c("Collembola:Herbivores-Microbivores", "Collembola:Microbivores", "Collembola:Herbivores-Detritivores-Microbivores","Collembola:Microbivores-Predators",
              "Mesostigmata:Predators", "Oribatida:Detritivores-Microbivores-Predators","Astigmata:Herbivores-Detritivores-Microbivores-Predators", "Prostigmata:Herbivores-Microbivores-Predators",
              "Nemotoda:Bacterivores", "Nemotoda:Predators", "Nemotoda:Fungivores" , "Nemotoda:Herbivores", "Nemotoda:Omnivores", 
              "Protura:Fungivores", "Pauropoda:Herbivores-Detritivores-Microbivores", "Symphyla:Herbivores-Detritivores-Fungivores")

SoilMacrofauna <- Feedgroup[,c("Parent_Group", "order", "family", "genus", "Group")]  %>%  ###Only live in soil or soil surface
  filter((Parent_Group %in% c("Isopoda", "Diplopoda", "Chilopoda",  "Haplotaxida"))) %>% select(Group) %>% unique %>% filter(!is.na(Group))
SoilMacrofauna <- c(SoilMacrofauna$Group, "Lumbricidae:Anecic", "Lumbricidae:Endogeic","Lumbricidae")


Macrofauna <- Feedgroup[,c("Parent_Group", "order", "family", "genus", "Group")]  %>% 
  filter((Parent_Group %in% c("Coleoptera", "Hymenoptera","Formicidae",
                             "Hymenoptera", "Diptera", "Hemiptera","Formicidae", "Orthoptera",  "Trichoptera", "Neuroptera", "Psocoptera", "Thysanoptera", 
                             "Araneae","Opiliones", "Dermaptera", "Gastropoda",  "Pseudoscorpiones", "Lepidoptera" ))) %>% select(Group) %>% unique %>% filter(!is.na(Group))
Macrofauna <- c(Macrofauna$Group)

  
mat = matrix(NA,
             nrow = length(SoilMeso) + length(SoilMacrofauna) + length(Macrofauna) +4,
             ncol =length(SoilMeso) + length(SoilMacrofauna) + length(Macrofauna) +4)
colnames(mat) <- c("Plants", "Detritus", "Bacteria", "Fungi", c(SoilMeso,SoilMacrofauna, Macrofauna))
rownames(mat) <- c("Plants", "Detritus", "Bacteria", "Fungi",c(SoilMeso,SoilMacrofauna, Macrofauna))

##Soil animals, collembola
mat[c("Plants","Bacteria","Fungi") ,   "Collembola:Herbivores-Microbivores"] = 1
mat[c("Bacteria","Fungi") ,   "Collembola:Microbivores"] = 1
mat[c("Plants","Detritus","Bacteria","Fungi") ,   "Collembola:Herbivores-Detritivores-Microbivores"] = 1
mat[c("Bacteria","Fungi", SoilMeso[which(grepl("Nemotoda", SoilMeso) == TRUE )]) , "Collembola:Microbivores-Predators"] =  c(1,1,rep(1/length(SoilMeso[which(grepl("Nemotoda", SoilMeso) == TRUE )]),
                                                                                                                                         length( SoilMeso[which(grepl("Nemotoda", SoilMeso) == TRUE )])))
### Mites
mat[SoilMeso,  "Mesostigmata:Predators"] = rep(1/length(SoilMeso), length(SoilMeso))
mat[c("Plants", "Detritus", "Bacteria", "Fungi", SoilMeso[which(grepl("Nemotoda", SoilMeso) == TRUE )]), "Astigmata:Herbivores-Detritivores-Microbivores-Predators" ] = 
  c(0.5, 0.5, 0.5, 1, rep(0.5/length(SoilMeso[which(grepl("Nemotoda", SoilMeso) == TRUE )]), length(SoilMeso[which(grepl("Nemotoda", SoilMeso) == TRUE )])))

mat[c( "Detritus", "Bacteria", "Fungi", SoilMeso[which(grepl("Nemotoda", SoilMeso) == TRUE )]), "Oribatida:Detritivores-Microbivores-Predators"] =
  c(1,0.5,1,rep(0.5/length( SoilMeso[which(grepl("Nemotoda", SoilMeso) == TRUE )]),length( SoilMeso[which(grepl("Nemotoda", SoilMeso) == TRUE )])))

mat[c("Plants","Bacteria", "Fungi",SoilMeso),"Prostigmata:Herbivores-Microbivores-Predators"] = c(0.5,0.5,0.5,rep(1/length(SoilMeso),
                                                                                    length(SoilMeso)))

###Nemotode
mat["Bacteria",  "Nemotoda:Bacterivores"] =1
mat[SoilMeso[which(grepl("Nemotoda", SoilMeso) == TRUE )],  "Nemotoda:Predators"] = rep(1/length(SoilMeso[which(grepl("Nemotoda", SoilMeso) == TRUE )]),
                                                                                       length(SoilMeso[which(grepl("Nemotoda", SoilMeso) == TRUE )]))
mat["Fungi",  "Nemotoda:Fungivores"] =1
mat["Plants", "Nemotoda:Herbivores"] = 1
mat[c("Plants", "Bacteria", SoilMeso[which(grepl("Nemotoda", SoilMeso) == TRUE )]), "Nemotoda:Omnivores"] = c(0.5, 1, rep(1/length(which(grepl("Nemotoda", SoilMeso) == TRUE )), 
                                                                                                                                         length(which(grepl("Nemotoda", SoilMeso) == TRUE ))))

###Symphyla, Pauropoda, Protura 
mat["Fungi","Protura:Fungivores"] <- 1
mat[c("Plants","Detritus", "Bacteria", "Fungi"),"Pauropoda:Herbivores-Detritivores-Microbivores" ] <- c(0.5, 1, 0.5, 1)
mat[c("Plants", "Detritus","Fungi", SoilMeso) , "Symphyla:Herbivores-Detritivores-Fungivores"] <- c(1, 0.5, 1, rep(0.5/length(SoilMeso), length(SoilMeso)))
 
###Diplopoda
mat["Detritus", "Diplopoda:Detritivores"] <- 1
mat["Fungi", "Diplopoda:Fungivores"] <- 1 

###isopoda 
mat[c( "Detritus","Bacteria","Fungi"), "Isopoda:Detritivores-Fungivores-Bacterivores" ] <- c (1, 0.5, 1)

##Gastropoda, what kinds of animals that they feed on ? 
mat[c("Plants", "Detritus", "Bacteria", "Fungi"),which(grepl("Gastropoda",rownames(mat)) == TRUE)] <- c(0.5,1,1,1)

##Psocoptera
mat[c("Plants","Bacteria", "Fungi"), "Psocoptera:Detritivores"] <- c(0.5, 0.5, 1)

##Worms
mat[c("Plants", "Detritus", "Bacteria", "Fungi", rownames(mat[which(grepl("Nemotoda", rownames(mat)) == TRUE),])),
    which(grepl("Lumbricidae",rownames(mat)) == TRUE)] <- c(0.5,1,0.5,0.5, rep(0.5/length(which(grepl("Nemotoda", rownames(mat)) == TRUE)), length(which(grepl("Nemotoda", rownames(mat)) == TRUE))))
mat[c("Plants", "Detritus", "Bacteria", "Fungi"), "Enchytraeidae:Detritivores"] <- c(0.5, 1, 0.5, 0.5)                                   


###Chilopoda --> Predators 
# "Chilopoda:Lithobiomorpha"  --> Fungal channels, Collembola, and earthworms                             
# "Chilopoda:Geophilomorpha" --> Gastropoda, Bacterial channels
Chilop_Preys <- c( which(grepl("Isopoda", rownames(mat)) == TRUE ),
                  which(grepl("Orthoptera", rownames(mat)) == TRUE), 
                  which(grepl("Hemiptera", rownames(mat)) == TRUE), 
                  which(grepl("Opiliones", rownames(mat)) == TRUE), 
                  which(grepl("Araneae", rownames(mat)) == TRUE), 
                  which(grepl("Lumbricidae", rownames(mat)) == TRUE), 
                  which(grepl("Enchytraeidae", rownames(mat)) == TRUE),
                  which(grepl("Dermaptera", rownames(mat)) == TRUE),
                  which(grepl("Gastropoda",rownames(mat)) == TRUE), 
                  which(grepl("Diptera-Larve", rownames(mat)) == TRUE))

mat[c(SoilMeso,rownames(mat[Chilop_Preys,]) ),"Chilopoda:Geophilomorpha"] <- 1
mat[c(SoilMeso,rownames(mat[Chilop_Preys,])[1:14] ),"Chilopoda:Lithobiomorpha"] <- 1

###Coleoptera 
mat["Detritus", "Coleoptera:Detritivores"] <- 1 
mat["Plants", "Coleoptera:Herbivores"] <- 1 
mat["Fungi", "Coleoptera:Fungivores" ] <- 1
mat[which(grepl("Nemotoda", SoilMeso) == TRUE ), "Coleoptera:Necrovores"] <- rep(1/length(which(grepl("Nemotoda", SoilMeso) == TRUE )), 
                                                                                 length(which(grepl("Nemotoda", SoilMeso) == TRUE )))
mat["Plants","Coleoptera:Herbivores:Granivores"] <- 1
mat[c("Plants", "Detritus", "Bacteria", "Fungi"), "Coleoptera:Herbivores-Detritivores-Microbivores" ] <- c(1, 1, 0.5,1)
mat[c("Detritus", "Fungi"), "Coleoptera:Detritivores-Fungivores"] <- c(1,1)

Carabi_Preys <- c(which(grepl("Collembola", rownames(mat)) == TRUE ),# 
                  which(grepl("Isopoda", rownames(mat)) == TRUE ),#
                  which(grepl("Lumbricidae", rownames(mat)) == TRUE), #
                  which(grepl("Enchytraeidae", rownames(mat)) == TRUE),#
                  which(grepl("Chilopoda",rownames(mat)) == TRUE),
                  which(grepl("Diplopoda",rownames(mat)) == TRUE),
                  which(grepl("Orthoptera", rownames(mat)) == TRUE), #
                  which(grepl("Hemiptera", rownames(mat)) == TRUE), #
                  which(grepl("Opiliones", rownames(mat)) == TRUE), 
                  which(grepl("Araneae", rownames(mat)) == TRUE), 
                  which(grepl("Gastropoda",rownames(mat)) == TRUE), #
                  which(grepl("Dermaptera",rownames(mat)) == TRUE),
                  which(grepl("Thysanoptera",rownames(mat)) == TRUE),
                  which(grepl("Coleoptera",rownames(mat)) == TRUE), 
                  which(grepl("Lepidoptera-Larve:Herbivores",rownames(mat)) == TRUE),
                  which(grepl("Diptera-Larve",rownames(mat)) == TRUE)) 
mat[Carabi_Preys, "Coleoptera:Predators:Carabidae" ] <- 1

RoveBeetles_Prey <- c( which(grepl("Isopoda", rownames(mat)) == TRUE),#
                  which(grepl("Hemiptera", rownames(mat)) == TRUE), #
                  which(grepl("Gastropoda",rownames(mat)) == TRUE), #
                  which(grepl("Coleoptera",rownames(mat)) == TRUE) ### Still need to confirm what they exactly eat 
                    )

mat["Fungi","Coleoptera:Omnivores:Staphylinidae"] <- 1
mat[RoveBeetles_Prey, "Coleoptera:Omnivores:Staphylinidae"] <- rep(1/length(RoveBeetles_Prey), length(RoveBeetles_Prey))

mat[c("Plants", "Detritus", "Bacteria", "Fungi"), "Coleoptera:Omnivores:Elateridae"] <- c(1,1,0.5,1)
mat[RoveBeetles_Prey, "Coleoptera:Omnivores:Elateridae"] <- rep(1/length(RoveBeetles_Prey), length(RoveBeetles_Prey))

mat[c('Plants', "Fungi"), "Coleoptera:Herbivores-Fungivores-Predators"] <- c(1,1)
mat["Plants", "Coleoptera:Predators-Herbivores" ] <- 1
mat[RoveBeetles_Prey, c( "Coleoptera:Herbivores-Fungivores-Predators","Coleoptera:Predators-Herbivores", "Coleoptera:Predators" )] <- rep(1/length(RoveBeetles_Prey), length(RoveBeetles_Prey))

Hemeno_Preys <- c(
  which(grepl("Thysanoptera",rownames(mat)) == TRUE),
  which(grepl("Coleoptera",rownames(mat)) == TRUE), 
  which(grepl("Hemiptera", rownames(mat)) == TRUE), #
  which(grepl("Diptera", rownames(mat)) == TRUE), #
  which(grepl("Araneae", rownames(mat)) == TRUE), 
  which(grepl("Lepidoptera", rownames(mat)) == TRUE), 
  which(grepl("Neuroptera", rownames(mat)) == TRUE), 
  which(grepl("Psocoptera", rownames(mat)) == TRUE), 
  which(grepl("Thysanoptera",rownames(mat)) == TRUE),
  which(grepl("Hymenoptera",rownames(mat)) == TRUE),
  which(grepl("Trichoptera",rownames(mat)) == TRUE) ,
  which(grepl("Dermaptera",rownames(mat)) == TRUE)
)
mat[Hemeno_Preys, "Hymenoptera:Predators"] <- 1

Hemeno_HerbiPreys <- c(
  which(grepl("Coleoptera:Detritivores",rownames(mat)) == TRUE),
  which(grepl("Diptera:Herbivores",rownames(mat)) == TRUE),
  which(grepl("Hemiptera:Herbivores",rownames(mat)) == TRUE),
  which(grepl("Hymenoptera:Herbivores",rownames(mat)) == TRUE),
  which(grepl("Hymenoptera:Predators",rownames(mat)) == TRUE),
  which(grepl("Lepidoptera", rownames(mat)) == TRUE), 
  which(grepl("Orthoptera", rownames(mat)) == TRUE), 
  which(grepl("Araneae", rownames(mat)) == TRUE)
)
mat[c("Plants", rownames(mat[Hemeno_HerbiPreys,])),"Hymenoptera:Herbivores-Predators" ] <- c(1, rep(1/length(Hemeno_HerbiPreys), length(Hemeno_HerbiPreys)))                            
mat["Plants", "Hymenoptera:Herbivores"] <- 1                                      
mat[c("Detritus", "Fungi"), "Hymenoptera:Detritivores-Fungivores"] <- c(1,1)
mat[c("Plants", "Fungi", SoilMeso,SoilMacrofauna, Macrofauna), "Formicidae:Herbivores-Detritivores-Microbivores-Predators"] <- c(1, 0.5,rep(1/(length(SoilMeso)+length(SoilMacrofauna) + length(Macrofauna)),
                                                                                                                                            (length(SoilMeso)+length(SoilMacrofauna) + length(Macrofauna))) )

###Diptera 
mat["Plants","Diptera:Herbivores" ]  <- 1
mat["Fungi", "Diptera:Fungivores" ] <- 1
mat[c("Plants", "Detritus"), "Diptera:Detrtivores-Herbivores" ] <- c(1,1)
mat[Macrofauna, "Diptera:Predators" ]  <- rep(1/length(Macrofauna), length(Macrofauna)) 
mat[c("Plants", Macrofauna), "Diptera:Herbivores-Predators" ] <- c(1, rep(1/length(Macrofauna), length(Macrofauna))) 

DipLarve_Prey <- c(
  which(grepl("Nemotoda", rownames(mat)) == TRUE), 
  which(grepl("Lumbricidae", rownames(mat)) == TRUE), 
  which(grepl("Enchytraeidae", rownames(mat)) == TRUE),
  which(grepl("Larve", rownames(mat)) == TRUE)
)
mat[c("Plants", "Detritus", "Fungi",rownames(mat[DipLarve_Prey,]) ),  "Diptera-Larve:Herbivores-Detritivores-Fungivores-Predators" ] <- c(1,1,1, 
                                                                                                                                          rep(1/length(DipLarve_Prey), length(DipLarve_Prey)))
###Hemiptera 
Hemi_Prey <- c(  which(grepl("Diplopoda", rownames(mat)) == TRUE), 
                    which(grepl("Orthoptera", rownames(mat)) == TRUE), #
                    which(grepl("Hemiptera:Herbivores", rownames(mat)) == TRUE), #
                    which(grepl("Opiliones", rownames(mat)) == TRUE), 
                    which(grepl("Araneae", rownames(mat)) == TRUE), 
                    which(grepl("Gastropoda",rownames(mat)) == TRUE), #
                    which(grepl("Dermaptera",rownames(mat)) == TRUE),
                    which(grepl("Thysanoptera",rownames(mat)) == TRUE),
                    which(grepl("Coleoptera:Herbivores",rownames(mat)) == TRUE), 
                    which(grepl("Lepidoptera-Larve:Herbivores",rownames(mat)) == TRUE),
                    which(grepl("Diptera-Larve",rownames(mat)) == TRUE)
               ) 

mat[Hemi_Prey, c("Hemiptera:Predators", "Hemiptera:Herbivores-Predators")]   <- rep(1/length(Hemi_Prey), length(Hemi_Prey))                                     
mat["Plants", c("Hemiptera:Herbivores","Hemiptera:Herbivores-Predators" )  ]  <- c( 1, 0.5) # Sternorrhyncha (scale insects, aphids, whiteflies,etc.)


Orth_Preys <- c ( ### Predate small insects ## Song 2018 
  which(grepl("Collembola", rownames(mat)) == TRUE ),# 
  which(grepl("Hemiptera", rownames(mat)) == TRUE), #
  which(grepl("Thysanoptera",rownames(mat)) == TRUE),
  which(grepl("Diptera",rownames(mat)) == TRUE),
  which(grepl("Psocoptera",rownames(mat)) == TRUE)
)
mat[c("Plants", "Detritus"),  "Orthoptera:Herbivores-Detritivores"] <- c(1,0.5)
mat[c("Plants", "Detritus"), "Orthoptera:Herbivores-Detritivores-Predators" ] <- c(1,0.5)
mat["Plants", "Orthoptera:Herbivores-Predators"] <- 1
mat[Orth_Preys, c("Orthoptera:Herbivores-Detritivores-Predators","Orthoptera:Herbivores-Predators" )] <- rep(1/length(Orth_Preys), length(Orth_Preys))



Dermaptera_Preys <- c (
  which(grepl("Hemiptera:Herbivores", rownames(mat)) == TRUE),
  which(grepl("Diptera-Larve", rownames(mat)) == TRUE),
  which(grepl("Psocoptera", rownames(mat)) == TRUE), 
  which(grepl("Lepidoptera-Larve:Herbivores", rownames(mat) ) == TRUE)
)
mat[c("Plants","Detritus", "Fungi", "Bacteria"), "Dermaptera:Herbivores-Detritivores-Predators"] <- c(0.5,1,0.5,0.5)
mat[Dermaptera_Preys,"Dermaptera:Herbivores-Detritivores-Predators" ]   <-    rep(1/length(Dermaptera_Preys), length(Dermaptera_Preys) )

##
mat["Fungi", "Thysanoptera:Fungivores"] <- 1

##
mat["Plants",  "Lepidoptera-Larve:Herbivores"  ] <- 1

Spider_Preys <- c(
  which(grepl("Collembola", rownames(mat)) == TRUE),
  which(grepl("Chilopoda", rownames(mat)) == TRUE),
  which(grepl("Diplopoda", rownames(mat)) == TRUE),
  which(grepl("Isopoda", rownames(mat)) == TRUE),
  which(grepl("Dermaptera", rownames(mat)) == TRUE),
  which(grepl("Orthoptera", rownames(mat)) == TRUE),
  which(grepl("Diptera", rownames(mat)) == TRUE),
  which(grepl("Hemiptera", rownames(mat)) == TRUE),
  which(grepl("Hymenoptera", rownames(mat)) == TRUE),
  which(grepl("Formicidae", rownames(mat)) == TRUE),
  which(grepl("Thysanoptera", rownames(mat)) == TRUE),
  which(grepl("Coleoptera", rownames(mat)) == TRUE),
  which(grepl("Opiliones", rownames(mat)) == TRUE),
  which(grepl("Araneae", rownames(mat)) == TRUE),
  which(grepl("Psocoptera", rownames(mat)) == TRUE),
  which(grepl("Trichoptera", rownames(mat)) == TRUE),
)

mat[Spider_Preys, c("Araneae:Wandering" , "\r\nAraneae:Weavers")] <- req(1/length(Spider_Preys), length(Spider_Preys))                                        
                                        
Opilion_Preys <- c(
  which(grepl("Collembola", rownames(mat)) == TRUE),
  which(grepl("Araneae", rownames(mat)) == TRUE),
  which(grepl("Diplopoda", rownames(mat)) == TRUE),
  which(grepl("Isopoda", rownames(mat)) == TRUE),
  which(grepl("Gastropoda",rownames(mat)) == TRUE), 
  which(grepl("Diptera", rownames(mat)) == TRUE),
  which(grepl("Diptera", rownames(mat)) == TRUE),
  which(grepl("Lepidoptera-Larve:Herbivores",rownames(mat)) == TRUE)
)
mat[c("Plants", rownames(mat[Opilion_Preys,])),  "Opiliones:Predators"] <- c(0.5, rep(1/length(Opilion_Preys),length(Opilion_Preys) ))

"Pseudoscorpiones:Predators" 
"Trichoptera:Predators"                                        
[31] "Neuroptera:Predators"


