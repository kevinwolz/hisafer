############# Les fonctions pour analyser les differentes simulations


ggplot_hisafe_ts<-function(datasource="trees", x = "Date", y, date_min="1994-01-01", date_max="2000-01-01"){

  data<-(hop[[datasource]])

  data = as.data.frame(data) # ggplot fonctionne avec des data frames si on fait data[,x] or il se peut qu'on soit avec des "tiddy" (et c'est MAL)
  min <- as.Date(date_min)
  max <- as.Date(date_max)
  ggplot(data = data) +
    scale_x_date(limits = c(min, max)) +
    geom_point(aes(data[,x], data[,y])) +
    xlab(x) +
    ylab(y) +
    theme_gray()+
    ggtitle(y)+
    theme(plot.title = element_text(face = "bold", colour ="black", size =16, hjust = 0.5))
}

## graphe permettant de representer les 3 cohortes de feuilles
ggplot_cohorte_feuille<-function(hop, date_min, date_max){

  data<-(hop[["trees"]])

  min <- as.Date(date_min)
  max <- as.Date(date_max)

  ggplot(data, aes(x = Date)) +
    scale_x_date(limits = c(min, max)) +
    geom_line(aes(y = leafArea_1, color = "cohort 1")) +
    geom_line(aes(y = leafArea_2, color = "cohort 2"))  +
    geom_line(aes(y = leafArea_3, color = "cohort 3"))  +
    scale_color_manual(values = c("darkgreen", "darkorange", "darkred"))+
    theme_gray()+
    ggtitle("Leaves cohorts of olive tree")+
    xlab("Date") + ylab("Leaf area (mÂ²)")+
    theme(plot.title = element_text(face = "bold", colour ="black", size =16, hjust = 0.5))

}

## Graphe : effet du stress carbon? sur l'initialisation du nombre de fleur
ggplot_flowers_initialisation_graphe1<- function(hop, date_min, date_max){

  data<-(hop[["trees"]])
  min <- as.Date(date_min)
  max <- as.Date(date_max)

  ggplot(data, aes(x = Date)) +
    scale_x_date(limits = c(min, max)) +
    geom_line(aes(y = flowerNbrPotential, color = "flowerNbrPotential")) +
    geom_line(aes(y = flowerNbrPotentialReducer, color = "flowerNbrPotentialReducer"))  +
    scale_color_manual(values = c("darkgreen", "darkorange"))+
    theme_gray()+
    ggtitle("Flowers potentials")+
    xlab("Date") + ylab("")+
    theme(plot.title = element_text(face = "bold", colour ="black", size =16, hjust = 0.5))
}
## Graphe : effet du stress carbon? sur l'initialisation du nombre de fleur 2
ggplot_flowers_initialisation_graphe2<- function(hop, date_min, date_max){

  data<-(hop[["trees"]])
  min <- as.Date(date_min)
  max <- as.Date(date_max)

  ggplot(data, aes(x = Date)) +
    scale_x_date(limits = c(min, max)) +
    geom_line(aes(y = flowerNbrPotential, color = "flowerNbrPotential")) +
    geom_line(aes(y = flowerNbrPotentialReducer, color = "flowerNbrPotentialReducer"))  +
    geom_line(aes(y = flowerNbrPotentialFromLeafArea, color = "flowerNbrPotentialFromLeafArea"))  +
    scale_color_manual(values = c("darkgreen", "darkorange","red"))+
    theme_gray()+
    ggtitle("Flowers potentials")+
    xlab("Date") + ylab("")+
    theme(plot.title = element_text(face = "bold", colour ="black", size =16, hjust = 0.5))
}

## Graphe : effet du stress carbon? sur l'initialisation du nombre de fleur
ggplot_fruits_initialisation<-function(hop, date_min, date_max){

  data<-(hop[["trees"]])
  min <- as.Date(date_min)
  max <- as.Date(date_max)

  ggplot(data, aes(x = Date)) +
    scale_x_date(limits = c(min, max)) +
    geom_line(aes(y = flowerNbrPotential, color = "flowerNbrPotential")) +
    geom_line(aes(y = flowerNbrPotentialReducer, color = "flowerNbrPotentialReducer"))  +
    geom_line(aes(y = fruitThinningDaily, color = "fruitThinningDaily"))  +
    geom_line(aes(y = flowerNbrPotentialFromLeafArea, color = "flowerNbrPotentialFromLeafArea"))  +
    geom_line(aes(y = fruitNbrDaily, color = "fruitNbrDaily"))  +
    scale_color_manual(values = c("darkgreen", "darkorange","green","blue","red"))+
    theme_gray()+
    ggtitle("Flowers potentials")+
    xlab("Date") + ylab("")+
    theme(plot.title = element_text(face = "bold", colour ="black", size =16, hjust = 0.5))
}

## Graphe : differents stades phenologiques des fruits
ggplot_fruit_phenological_stage<-function(hop, data, date_min, date_max){

  data_trees<-(hop[["trees"]])
  min <- as.Date(date_min)
  max <- as.Date(date_max)

  ggplot(data_trees, aes(x = Date)) +
    scale_x_date(limits = c(min, max)) +
    geom_line(aes(y = heatAccumulatedTemperature, color = "Temperature acccumulation")) +
    geom_line(aes(y = budburstDate, color = "Budburst"))  +
    geom_line(aes(y = floweringDate, color = "Flowering"))  +
    geom_line(aes(y = fruitSettingDate, color = "Fruit setting"))  +
    geom_line(aes(y = fruitGrowthDate, color = "Fruit growth"))  +
    geom_line(aes(y = veraisonDate, color = "Veraison"))  +
    scale_color_manual(values = c("black","darkblue", "chartreuse4", "darkorange", "darkorange4", "brown3"))+
    theme_gray()+
    ggtitle("Fruit phenological stage of olive tree")+
    xlab("Date") + ylab("temperature accumulation (degrees-days)")+
    theme(plot.title = element_text(face = "bold", colour ="black", size =16, hjust = 0.5))
}

## Graphe : compartiments d azote des feuilles
ggplot_nitrogen_compartement_foliage<-function(hop, data, date_min, date_max){

  data_trees<-(hop[["trees"]])

  min <- as.Date(date_min)
  max <- as.Date(date_max)

  nitrogen_foliage<-data.frame(
    "Date"=data_trees$Date,
    "nitrogenFoliage_1"=data_trees$nitrogenFoliage_1,
    "nitrogenFoliage_2"=data_trees$nitrogenFoliage_2,
    "nitrogenFoliage_3"=data_trees$nitrogenFoliage_3,
    "nitrogenFoliage_Sum"=(data_trees$nitrogenFoliage_1 + data_trees$nitrogenFoliage_2 + data_trees$nitrogenFoliage_3),
    "nitrogenFruit"=data_trees$nitrogenFruit,
    "nitrogenCoarseRoots"=data_trees$nitrogenCoarseRoots,
    "nitrogenFineRoots"=data_trees$nitrogenFineRoots,
    "nitrogenStem"=data_trees$nitrogenStem,
    "nitrogenStump"=data_trees$nitrogenStump,
    "nitrogenBranches"=data_trees$nitrogenBranches,
    "nitrogenLabile"=data_trees$nitrogenLabile)

  ggplot(nitrogen_foliage, aes(x = Date)) +
    scale_x_date(limits = c(min, max)) +
    geom_line(aes(y = nitrogenFoliage_Sum, color = "nitrogenFoliage_Sum")) +
    geom_line(aes(y = nitrogenFoliage_1, color = "nitrogenFoliage_1"))  +
    geom_line(aes(y = nitrogenFoliage_2, color = "nitrogenFoliage_2"))  +
    geom_line(aes(y = nitrogenFoliage_3, color = "nitrogenFoliage_3"))  +
    scale_color_manual(values = c("black","darkblue", "chartreuse4", "darkorange"))+
    theme_gray()+
    ggtitle("Nitrogen Foliage of olive tree")+
    theme(plot.title = element_text(face = "bold", colour ="black", size =16, hjust = 0.5))
}

## Graphe : cumul de la senescence d azote dans les differentes cohortes de feuilles

ggplot_nitrogen_senescence_foliage<-function(hop, data, date_min, date_max){

  data_trees<-(hop[["trees"]])

  min <- as.Date(date_min)
  max <- as.Date(date_max)

  nitrogen_foliage_senescence<-data.frame(
    "Date"=data_trees$Date,
    "nitrogenFoliageSen_1"=data_trees$nitrogenFoliageSen_1,
    "nitrogenFoliageSen_2"=data_trees$nitrogenFoliageSen_2,
    "nitrogenFoliageSen_3"=data_trees$nitrogenFoliageSen_3,
    "nitrogenFoliage_Sum"=data_trees$nitrogenFoliageSen_1 + data_trees$nitrogenFoliageSen_2 + data_trees$nitrogenFoliageSen_3)

  ggplot(data = nitrogen_foliage_senescence, aes(x = Date, y = nitrogenFoliage_Sum)) +
    scale_x_date(limits = c(min, max)) +
    geom_point()+
    theme_gray()+
    ggtitle("nitrogen foliage senescence Sum of olive tree")+
    theme(plot.title = element_text(face = "bold", colour ="black", size =16, hjust = 0.5))
}

## graphe : cycle du carbone et de l azote
## Par Justine Catel


ggplot_hisafe_cycle <- function(hop, annee_debut = 0, annee_fin = 5000, afficher_mois = FALSE, cycle = "carbon"){

  data_trees<-(hop[["trees"]])

  cycle_data<-data.frame(
    "Date"=data_trees$Date,
    "Year"=data_trees$Year,
    "Month"=data_trees$Month,
    "nitrogenBranches"=data_trees$nitrogenBranches,
    "nitrogenCoarseRoots"=data_trees$nitrogenCoarseRoots,
    "nitrogenFineRoots"=data_trees$nitrogenFineRoots,
    "nitrogenFruit"=data_trees$nitrogenFruit,
    "nitrogenLabile"=data_trees$nitrogenLabile,
    "nitrogenStem"=data_trees$nitrogenStem,
    "nitrogenStump"=data_trees$nitrogenStump,
    "nitrogenFoliage1"=data_trees$nitrogenFoliage_1,
    "nitrogenFoliage2"=data_trees$nitrogenFoliage_2,
    "nitrogenFoliage3"=data_trees$nitrogenFoliage_3,
    "nitrogenFoliageSum"=data_trees$nitrogenFoliage_1 + data_trees$nitrogenFoliage_2 + data_trees$nitrogenFoliage_3,
    "carbonBranches"=data_trees$carbonBranches,
    "carbonCoarseRoots"=data_trees$carbonCoarseRoots,
    "carbonFineRoots"=data_trees$carbonFineRoots,
    "carbonFruit"=data_trees$carbonFruit,
    "carbonLabile"=data_trees$carbonLabile,
    "carbonStem"=data_trees$carbonStem,
    "carbonStump"=data_trees$carbonStump,
    "carbonFoliage1"=data_trees$carbonFoliage_1,
    "carbonFoliage2"=data_trees$carbonFoliage_2,
    "carbonFoliage3"=data_trees$carbonFoliage_3,
    "carbonFoliageSum"=data_trees$carbonFoliage_1 + data_trees$carbonFoliage_2 + data_trees$carbonFoliage_3)

  df = as_tibble(cycle_data)


  # on decide quelles annees seront prises en compte
  data_used = df[annee_debut <= df$Year & df$Year <= annee_fin,]

  len_name_cycle = nchar(cycle)+1 # compte le nombre de caracteres des mots
  # pour la premiere variable
  variable = paste0(cycle,"Fruit") #paste0 = concatene sans espace "carbonFruit"
  Carbon_compartment = aggregate(simplify2array(data_used[,variable]) ~ Year + Month, FUN = sum, data=data_used)
  Carbon_compartment$Data = Carbon_compartment[,variable]#copie la valeur numÃ©rique dans la colonne data
  Carbon_compartment$Colour = str_sub(variable, len_name_cycle,50)#on rÃ©cupÃ¨re le noms de la variable sans le nom du cycle
  Carbon_compartment = Carbon_compartment[,-3]# on supprime la 3Ã¨me colonnne = carbonFruit
  # on reitere pour toutes les variables
  for(variable in c( paste0(cycle, "CoarseRoots"), paste0(cycle,"FineRoots"),
                     paste0(cycle,"Stem"), paste0(cycle,"Stump"), paste0(cycle,"Branches"),paste0(cycle,"FoliageSum"),
                     paste0(cycle,"Labile") )){
    Data_frame_2 = aggregate(simplify2array(data_used[,variable]) ~ Year + Month, FUN = sum, data=data_used)

    Data_frame_2$Data = Data_frame_2[,variable]
    Data_frame_2$Colour = str_sub(variable, len_name_cycle,50)
    Data_frame_2 = Data_frame_2[,-3]

    Carbon_compartment <- rbind(Carbon_compartment, Data_frame_2)
  }
  Carbon_compartment$Month[Carbon_compartment$Month<10] = paste0(0,Carbon_compartment$Month[Carbon_compartment$Month<10])

  Carbon_compartment$Year_Month <- as.Date(paste0(Carbon_compartment$Year, "-", Carbon_compartment$Month,"-","15"), format = "%Y-%m-%d")

  if(afficher_mois | annee_debut == annee_fin){
    ggplot(Carbon_compartment) + geom_col(aes(x = Year_Month, y = Data, fill = Colour))  +
      labs(title = cycle) + xlab("Date") + ylab(paste("Tree",cycle,"storage"))#  +# theme(axis.text.x = element_text(angle = 90))

  }
  else{
    ggplot(Carbon_compartment) + geom_col(aes(x = Year , y = Data, fill = Colour)) +
      labs(title = paste(cycle, "from", min(Carbon_compartment$Year), "to", max(Carbon_compartment$Year) ))+
      xlab("Date") + ylab(paste("Tree",cycle,"storage"))
    #paste seul implique qu'il y a un espace entre le mot
  }
}

## Graphe : dynamique d'increments de carbone pour l ensemble des compartiments
ggplot_carbon_increment<-function(hop, data, date_min, date_max){
  data_trees<-(hop[["trees"]])
  min <- as.Date(date_min)
  max <- as.Date(date_max)

  ggplot(data_trees, aes(x = Date)) +
    scale_x_date(limits = c(min, max)) +
    geom_line(aes(y = carbonFoliageIncrement, color = "Foliage")) +
    geom_line(aes(y = carbonFruitIncrement, color = "Fruits"))  +
    geom_line(aes(y = carbonCoarseRootsIncrement, color = "Coarse roots"))  +
    geom_line(aes(y = carbonFineRootsIncrement, color = "Fine roots"))  +
    geom_line(aes(y = carbonStemIncrement, color = "Stem"))  +
    geom_line(aes(y = carbonStumpIncrement, color = "Stump"))  +
    geom_line(aes(y = carbonBranchesIncrement, color = "Branches"))  +
    #geom_line(aes(y = carbonLabileIncrement, color = "Labile"))+
    scale_color_manual(values = c("black","darkblue", "chartreuse4", "darkorange","red","aquamarine4","darkorchid"))+
    theme_gray()+
    ggtitle("Carbon compartment Increment of olive tree")+
    theme(plot.title = element_text(face = "bold", colour ="black", size =16, hjust = 0.5))
}

## Graphe : cumul du carbon foliage senescence
ggplot_carbon_senescence_foliage<-function(hop, data, date_min, date_max){
  data_trees<-(hop[["trees"]])
  min <- as.Date(date_min)
  max <- as.Date(date_max)

carbonFoliage_data<-data.frame(
  "carbonFoliageSen_1"=data_trees$carbonFoliageSen_1,
  "carbonFoliageSen_2"=data_trees$carbonFoliageSen_2,
  "carbonFoliageSen_3"=data_trees$carbonFoliageSen_3,
  "carbonFoliageSen_Sum"=(data_trees$carbonFoliageSen_1+data_trees$carbonFoliageSen_2+data_trees$carbonFoliageSen_3),
  "Date"=data_trees$Date)

ggplot(carbonFoliage_data, aes(x = Date)) +
  scale_x_date(limits = c(min, max)) +
  geom_line(aes(y = carbonFoliageSen_1, color = "Foliage 1")) +
  geom_line(aes(y = carbonFoliageSen_2, color = "Foliage 2"))  +
  geom_line(aes(y = carbonFoliageSen_3, color = "Foliage 3"))  +
  geom_line(aes(y = carbonFoliageSen_Sum, color = "Foliage Sum"))  +
  scale_color_manual(values = c("black","darkblue", "chartreuse4", "darkorange"))+
  theme_gray()+
  ggtitle("Carbon Foliage senescence of olive tree")+
  theme(plot.title = element_text(face = "bold", colour ="black", size =16, hjust = 0.5))
}

########### fonctions pour le fichier plot ################################

## graphique PAR interceptée par les differentes plantes
ggplot_PAR_intercepted_typeline<-function(hop, date_min, date_max){

  data_plot<-(hop[["plot"]])

  min <- as.Date(date_min)
  max <- as.Date(date_max)

ggplot(data_plot, aes(x = Date)) +
  scale_x_date(limits = c(min, max)) +
  geom_line(aes(y = parInterceptedByTrees, color = "ByTree")) +
  geom_line(aes(y = parInterceptedByMainCrop, color = "ByMainCrop"))  +
  geom_line(aes(y = parInterceptedByInterCrop, color = "ByInterCrop"))  +
  scale_color_manual(values = c("red","darkblue", "darkorange"))+
  theme_gray()+
  ggtitle("PAR intercepted")+
  theme(plot.title = element_text(face = "bold", colour ="black", size =16, hjust = 0.5))
}


ggplot_PAR_intercepted_typehistogram <- function(hop, annee_debut = 0, annee_fin = 5000, afficher_mois = FALSE, cycle = "parIntercepted"){

  data_plot<-(hop[["plot"]])

  # on decide quelles annees seront prises en compte
  data_used = data_plot[annee_debut <= data_plot$Year & data_plot$Year <= annee_fin,]

  len_name_cycle = nchar(cycle)+1 # compte le nombre de caracteres des mots
  # pour la premiere variable
  variable = paste0(cycle,"ByInterCrop") #paste0 = concatene sans espace "carbonFruit"
  Carbon_compartment = aggregate(simplify2array(data_used[,variable]) ~ Year + Month, FUN = sum, data=data_used)
  Carbon_compartment$Data = Carbon_compartment[,variable]#copie la valeur numÃ©rique dans la colonne data
  Carbon_compartment$Colour = str_sub(variable, len_name_cycle,50)#on rÃ©cupÃ¨re le noms de la variable sans le nom du cycle
  Carbon_compartment = Carbon_compartment[,-3]# on supprime la 3Ã¨me colonnne = carbonFruit
  # on reitere pour toutes les variables
  for(variable in c( paste0(cycle, "ByMainCrop"), paste0(cycle,"ByTrees"))){
    Data_frame_2 = aggregate(simplify2array(data_used[,variable]) ~ Year + Month, FUN = sum, data=data_used)

    Data_frame_2$Data = Data_frame_2[,variable]
    Data_frame_2$Colour = str_sub(variable, len_name_cycle,50)
    Data_frame_2 = Data_frame_2[,-3]

    Carbon_compartment <- rbind(Carbon_compartment, Data_frame_2)
  }
  Carbon_compartment$Month[Carbon_compartment$Month<10] = paste0(0,Carbon_compartment$Month[Carbon_compartment$Month<10])

  Carbon_compartment$Year_Month <- as.Date(paste0(Carbon_compartment$Year, "-", Carbon_compartment$Month,"-","15"), format = "%Y-%m-%d")

  if(afficher_mois | annee_debut == annee_fin){
    ggplot(Carbon_compartment) + geom_col(aes(x = Year_Month, y = Data, fill = Colour))  +
      labs(title = cycle) + xlab("Date") + ylab(paste("Tree",cycle,"storage"))#  +# theme(axis.text.x = element_text(angle = 90))

  }
  else{
    ggplot(Carbon_compartment) + geom_col(aes(x = Year , y = Data, fill = Colour)) +
      labs(title = paste(cycle, "from", min(Carbon_compartment$Year), "to", max(Carbon_compartment$Year) ))+
      xlab("Date") + ylab(paste("Tree",cycle,"storage"))
    #paste seul implique qu'il y a un espace entre le mot
  }
}


ggplot_waterStock_available <- function(hop, annee_debut = 0, annee_fin = 5000, afficher_mois = FALSE, cycle = "waterStockAvailable"){

  data_plot<-(hop[["plot"]])

  # on decide quelles annees seront prises en compte
  data_used = data_plot[annee_debut <= data_plot$Year & data_plot$Year <= annee_fin,]

  len_name_cycle = nchar(cycle)+1 # compte le nombre de caracteres des mots
  # pour la premiere variable
  variable = paste0(cycle,"ForInterCrop") #paste0 = concatene sans espace "carbonFruit"
  Carbon_compartment = aggregate(simplify2array(data_used[,variable]) ~ Year + Month, FUN = sum, data=data_used)
  Carbon_compartment$Data = Carbon_compartment[,variable]#copie la valeur numÃ©rique dans la colonne data
  Carbon_compartment$Colour = str_sub(variable, len_name_cycle,50)#on rÃ©cupÃ¨re le noms de la variable sans le nom du cycle
  Carbon_compartment = Carbon_compartment[,-3]# on supprime la 3Ã¨me colonnne = carbonFruit
  # on reitere pour toutes les variables
  for(variable in c( paste0(cycle, "ForMainCrop"), paste0(cycle,"ForTrees"))){
    Data_frame_2 = aggregate(simplify2array(data_used[,variable]) ~ Year + Month, FUN = sum, data=data_used)

    Data_frame_2$Data = Data_frame_2[,variable]
    Data_frame_2$Colour = str_sub(variable, len_name_cycle,50)
    Data_frame_2 = Data_frame_2[,-3]

    Carbon_compartment <- rbind(Carbon_compartment, Data_frame_2)
  }
  Carbon_compartment$Month[Carbon_compartment$Month<10] = paste0(0,Carbon_compartment$Month[Carbon_compartment$Month<10])

  Carbon_compartment$Year_Month <- as.Date(paste0(Carbon_compartment$Year, "-", Carbon_compartment$Month,"-","15"), format = "%Y-%m-%d")

  if(afficher_mois | annee_debut == annee_fin){
    ggplot(Carbon_compartment) + geom_col(aes(x = Year_Month, y = Data, fill = Colour))  +
      labs(title = cycle) + xlab("Date") + ylab(paste("Tree",cycle,"storage"))#  +# theme(axis.text.x = element_text(angle = 90))

  }
  else{
    ggplot(Carbon_compartment) + geom_col(aes(x = Year , y = Data, fill = Colour)) +
      labs(title = paste(cycle, "from", min(Carbon_compartment$Year), "to", max(Carbon_compartment$Year) ))+
      xlab("Date") + ylab(paste("Tree",cycle,"storage"))
    #paste seul implique qu'il y a un espace entre le mot
  }
}


ggplot_grain_analyze_final<-function(hop, date_min, date_max){

  data_plot<-(hop[["plot"]])

  min <- as.Date(date_min)
  max <- as.Date(date_max)

  a<-ggplot(  data_plot<-(hop[["plot"]]), aes(x = Date)) +
    scale_x_date(limits = c(min, max)) +
    geom_line(aes(y = mainCropGrainNumber, color = "mainCropGrainNumber")) +
    scale_color_manual(values = c("black"))+
    theme_gray()+
    ggtitle("Grain number")+
    theme(plot.title = element_text(face = "bold", colour ="black", size =16, hjust = 0.5))

  b<-ggplot(  data_plot<-(hop[["plot"]]), aes(x = Date)) +
    scale_x_date(limits = c(min, max)) +
    geom_line(aes(y = mainCropGrainWeight, color = "mainCropGrainWeight")) +
    scale_color_manual(values = c("black"))+
    theme_gray()+
    ggtitle("Grain weight (g)")+
    theme(plot.title = element_text(face = "bold", colour ="black", size =16, hjust = 0.5))
  library(cowplot)
  plot_grid(a, b, labels=c("A", "B"), ncol = 2, nrow = 1)
}


ggplot_grain_analyze<-function(hop, date_min, date_max){

  data_plot<-(hop[["plot"]])

  min <- as.Date(date_min)
  max <- as.Date(date_max)

  ggplot(  data_plot<-(hop[["plot"]]), aes(x = Date)) +
    scale_x_date(limits = c(min, max)) +
    geom_line(aes(y = mainCropGrainNumber, color = "mainCropGrainNumber")) +
    scale_color_manual(values = c("black"))+
    theme_gray()+
    ggtitle("Grain number")+
    theme(plot.title = element_text(face = "bold", colour ="black", size =16, hjust = 0.5))
}


ggplot_grain_analyze2<-function(hop, date_min, date_max){

  data_plot<-(hop[["plot"]])

  min <- as.Date(date_min)
  max <- as.Date(date_max)

  ggplot(data_plot, aes(x = Date)) +
    scale_x_date(limits = c(min, max)) +
    geom_line(aes(y = mainCropGrainWeight, color = "mainCropGrainWeight")) +
    scale_color_manual(values = c("black"))+
    theme_gray()+
    ggtitle("Grain weight (g)")+
    theme(plot.title = element_text(face = "bold", colour ="black", size =16, hjust = 0.5))
}


########### fonctions pour le fichier climate ################################

# Graphes pour la PAR Photosynthetically active Radiation



ggplot_PAR_typeline<-function(hop, date_min, date_max){

  data_climate<-(hop[["climate"]])

  min <- as.Date(date_min)
  max <- as.Date(date_max)

ggplot(data_climate, aes(x = Date)) +
  scale_x_date(limits = c(min, max)) +
  geom_line(aes(y = globalPar, color = "globalPar")) +
  geom_line(aes(y = diffusePar, color = "diffusePar"))  +
  geom_line(aes(y = directPar, color = "directPar"))+
  scale_color_manual(values = c("blue", "red", "orange"))+
  theme_gray()+
  ggtitle("PAR Photosynthetically Active Radiation")+
  theme(plot.title = element_text(face = "bold", colour ="black", size =16, hjust = 0.5))
}



ggplot_PAR_typehistogram <- function(hop, annee_debut = 0, annee_fin = 5000, afficher_mois = FALSE, cycle = "globalPar"){

  data_climate<-(hop[["climate"]])

  data_climate_modif<-rename(data_climate, c("globalPardiffusePar" = "diffusePar", "globalPardirectPar" = "directPar", "globalParglobalPar" = "globalPar"))


  # on decide quelles annees seront prises en compte
  data_used = data_climate_modif[annee_debut <= data_climate_modif$Year & data_climate_modif$Year <= annee_fin,]

  len_name_cycle = nchar(cycle)+1 # compte le nombre de caracteres des mots
  # pour la premiere variable
  variable = paste0(cycle,"diffusePar") #paste0 = concatene sans espace "carbonFruit"
  Carbon_compartment = aggregate(simplify2array(data_used[,variable]) ~ Year + Month, FUN = sum, data=data_used)
  Carbon_compartment$Data = Carbon_compartment[,variable]#copie la valeur numÃ©rique dans la colonne data
  Carbon_compartment$Colour = str_sub(variable, len_name_cycle,50)#on rÃ©cupÃ¨re le noms de la variable sans le nom du cycle
  Carbon_compartment = Carbon_compartment[,-3]# on supprime la 3Ã¨me colonnne = carbonFruit
  # on reitere pour toutes les variables
  for(variable in c( paste0(cycle, "directPar"))){
    Data_frame_2 = aggregate(simplify2array(data_used[,variable]) ~ Year + Month, FUN = sum, data=data_used)

    Data_frame_2$Data = Data_frame_2[,variable]
    Data_frame_2$Colour = str_sub(variable, len_name_cycle,50)
    Data_frame_2 = Data_frame_2[,-3]

    Carbon_compartment <- rbind(Carbon_compartment, Data_frame_2)
  }
  Carbon_compartment$Month[Carbon_compartment$Month<10] = paste0(0,Carbon_compartment$Month[Carbon_compartment$Month<10])

  Carbon_compartment$Year_Month <- as.Date(paste0(Carbon_compartment$Year, "-", Carbon_compartment$Month,"-","15"), format = "%Y-%m-%d")

  if(afficher_mois | annee_debut == annee_fin){
    ggplot(Carbon_compartment) + geom_col(aes(x = Year_Month, y = Data, fill = Colour))  +
      labs(title = cycle) + xlab("Date") + ylab(paste("Tree",cycle,"storage"))#  +# theme(axis.text.x = element_text(angle = 90))

  }
  else{
    ggplot(Carbon_compartment) + geom_col(aes(x = Year , y = Data, fill = Colour)) +
      labs(title = paste(cycle, "from", min(Carbon_compartment$Year), "to", max(Carbon_compartment$Year) ))+
      xlab("Date") + ylab(paste("Tree",cycle,"storage"))
    #paste seul implique qu'il y a un espace entre le mot
  }
}


ggplot_Mean_Temperature<-function(hop){

  data_climate<-(hop[["climate"]])


data_climate_inter<-cbind(data_climate,Meantemperature = (data_climate$minTemperature + data_climate$maxTemperature)/2)
data_climate_inter

ggplot(data = data_climate_inter, aes(x = Date, y = Meantemperature)) +
  geom_line()+ theme_gray()+
  ggtitle("mean temperature per day")+
  theme(plot.title = element_text(face = "bold", colour ="black", size =16, hjust = 0.5))
}


ggplot_temperature<-function(hop, date_min, date_max){

  data_climate<-(hop[["climate"]])

  min <- as.Date(date_min)
  max <- as.Date(date_max)

  data_climate_inter<-cbind(data_climate,MeanTemperature = (data_climate$minTemperature + data_climate$maxTemperature)/2)




ggplot(data_climate_inter, aes(x = Date)) +
  scale_x_date(limits = c(min, max)) +
  geom_line(aes(y = MeanTemperature, color = "MeanTemperature")) +
  geom_line(aes(y = minTemperature, color = "minTemperature"))  +
  geom_line(aes(y = maxTemperature, color = "maxTemperature"))+
  scale_color_manual(values = c("blue", "red", "orange"))+
  theme_gray()+
  ggtitle("Temperature")+
  theme(plot.title = element_text(face = "bold", colour ="black", size =16, hjust = 0.5))
}

#climate description

ggplot_climate_description<-function(hop, date_min, date_max){

  data_climate<-(hop[["climate"]])

precipitation_cumulee = aggregate(precipitation ~ Year + Month, FUN = sum, data=data_climate)
precipitation_cumulee$Date = paste(precipitation_cumulee$Year,precipitation_cumulee$Month)

data_climate_precipitation = data_climate
data_climate_precipitation$Date1 = paste(data_climate_precipitation$Year,data_climate_precipitation$Month)

data_climate_precipitation = merge(data_climate_precipitation,precipitation_cumulee,by.x = "Date1", by.y = "Date")

# #Temperature minimale avec precipitation
# data_climate_precipitation$minTemperature_essai = data_climate_precipitation$minTemperature * 10
# ggplot(data_climate_precipitation) +
#   geom_col(aes(x = Date, y = precipitation.y)) +
#   geom_line(aes(x = Date, y = minTemperature_essai),size = 0.05, color = "red") +
#   scale_y_continuous(
#     "precipitation (mm)",
#     sec.axis = sec_axis(~. /10, name = "Temperature minimum (C?)"))

#temperature moyenne avec precipitation
data_climate_precipitation$minTemperature_essai = ((data_climate_precipitation$minTemperature+ data_climate_precipitation$maxTemperature)/2) * 10
min <- as.Date(date_min)
max <- as.Date(date_max)
ggplot(data_climate_precipitation) +
  scale_x_date(limits = c(min, max)) +
  geom_col(aes(x = Date, y = precipitation.y)) +
  geom_line(aes(x = Date, y = minTemperature_essai),size = 0.05, color = "red") +
  theme_update(plot.title = element_text(hjust = 0.5))+
  ggtitle("Climate description: precipitation (mm) and mean temperature (C?)") +
  scale_y_continuous(
    "precipitation (mm)",
    sec.axis = sec_axis(~. /10, name = "Mean temperature (C?)"))

}

#cumul des precipitations

ggplot_precipitation_cumulation<-function(hop)
{

  data_climate<-(hop[["climate"]])
  precipitation_cumulee = aggregate(precipitation ~ Year + Month, FUN = sum, data=data_climate)
  precipitation_cumulee$Date = paste(precipitation_cumulee$Year,precipitation_cumulee$Month)
  #
  # data_climate_precipitation = data_climate
  # data_climate_precipitation$Date1 = paste(data_climate_precipitation$Year,data_climate_precipitation$Month)
  #
  # data_climate_precipitation = merge(data_climate_precipitation,precipitation_cumulee,by.x = "Date1", by.y = "Date")
  #
  # # #Temperature minimale avec precipitation
  # # data_climate_precipitation$minTemperature_essai = data_climate_precipitation$minTemperature * 10
  # # ggplot(data_climate_precipitation) +
  # #   geom_col(aes(x = Date, y = precipitation.y)) +
  # #   geom_line(aes(x = Date, y = minTemperature_essai),size = 0.05, color = "red") +
  # #   scale_y_continuous(
  # #     "precipitation (mm)",
  # #     sec.axis = sec_axis(~. /10, name = "Temperature minimum (C?)"))
  #
  # #temperature moyenne avec precipitation
  # data_climate_precipitation$minTemperature_essai = ((data_climate_precipitation$minTemperature+ data_climate_precipitation$maxTemperature)/2) * 10
  # min <- as.Date(date_min)
  # max <- as.Date(date_max)


  ggplot(precipitation_cumulee) +
  geom_col(aes(x = Year, y = precipitation))+
  theme_update(plot.title = element_text(hjust = 0.5))+
  ggtitle("Precipitation cumulation (mm) per year")
}

ggplot_precipitation_cumulation_month<-function(hop)
{

  data_climate<-(hop[["climate"]])
  precipitation_cumulee = aggregate(precipitation ~ Year + Month, FUN = sum, data=data_climate)
  precipitation_cumulee$Date = paste(precipitation_cumulee$Year,precipitation_cumulee$Month)
  ggplot(precipitation_cumulee) +
    geom_col(aes(x = Date, y = precipitation))+
    theme_update(plot.title = element_text(hjust = 0.5))+
    ggtitle("Precipitation cumulation (mm)")
}

