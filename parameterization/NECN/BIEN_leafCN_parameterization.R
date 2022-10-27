library(BIEN)

Arcto_species <- c("Arctostaphylos canascens","Arctostaphylos columbiana","Arctostaphylos nevadensis","Arctostaphylos patula",
                   "Arctostaphylos viscida")
Arcto_traits_df <- BIEN_trait_species(Arcto_species)
Arcto_CN <- BIEN_trait_mean(Arcto_species, "leaf carbon content per leaf nitrogen content")
Arcto_mean_CN <- mean(as.numeric(Arcto_CN$mean_value))

rm(Arcto_CN, Arcto_species, Arcto_mean_CN, Arcto_traits_df)

##------------
ceano_species <- c("Ceanothus cuneatus","Ceanothus pumilus","Ceanothus velutinus")
ceano_traits_df <- BIEN_trait_species(ceano_species)
unique(ceano_traits_df$trait_name)
ceano_leaf_CN <- BIEN_trait_mean(ceano_species, "leaf carbon content per leaf nitrogen content")
ceano_mean_leaf_Cn <- mean(as.numeric(ceano_leaf_CN$mean_value))

rm(ceano_leaf_CN, ceano_mean_leaf_Cn, ceano_species, ceano_traits_df, ceano_leaf_longevity, ceano_leaf_long_yrs)

##------------
frangula_species <- c("Frangula californica","Frangula purshiana")
frangula_traits_df <- BIEN_trait_species(frangula_species)
unique(frangula_traits_df$trait_name)
frangula_leaf_CN <- BIEN_trait_mean(frangula_species, "leaf carbon content per leaf nitrogen content")
frangula_mean_leaf_Cn <- mean(as.numeric(frangula_leaf_CN$mean_value))

rm(frangula_species, frangula_traits_df, frangula_leaf_CN, frangula_leaf_CN)

##------------
Garrya_species <- c("Garrya buxifolia","Garrya fremontii")
Garrya_traits_df <- BIEN_trait_species(Garrya_species)
unique(Garrya_traits_df$trait_name)
Garrya_leaf_CN <- BIEN_trait_mean(Garrya_species, "leaf carbon content per leaf nitrogen content")
Garrya_mean_leaf_Cn <- mean(as.numeric(Garrya_leaf_CN$mean_value))

rm(Garrya_species, Garrya_traits_df, Garrya_leaf_CN, Garrya_mean_leaf_Cn)

##-----------
GaulShal_traits_df <- BIEN_trait_species("Gaultheria shallon")
GaulShal_leaf_CN <- BIEN_trait_mean("Gaultheria shallon", "leaf carbon content per leaf nitrogen content")
GaulShal_leaf_CN$mean_value

rm(GaulShal_leaf_CN, GaulShal_traits_df)

##-----------
Mahonia_traits_df <- BIEN_trait_genus("Mahonia")
unique(Mahonia_traits_df$trait_name) ## doesn't return anything useful ):

rm(MahoNerv_traits_df)

##-----------
Quercus_species <- c("Quercus sadleriana","Quercus vacciniifolia")
Quercus_traits_df <- BIEN_trait_species(Quercus_species)
unique(Quercus_traits_df$trait_name) # nope

Quercus_traits_df <- BIEN_trait_species("Quercus_chrysolepis")
unique(Quercus_traits_df$trait_name) # nada

##-----------
Rhodo_species <- c("Rhododendron macrophyllum","Rhododendron occidentale")
Rhodo_traits_df <- BIEN_trait_species(Rhodo_species)
unique(Rhodo_traits_df$trait_name) # doesn't return anything 

rm(Quercus_species, Quercus_traits_df, Rhodo_species, Rhodo_traits_df)

##----------
Rubus_species <- c("Rubus leucodermis", "Rubus parviflorus","Rubus ursinus")
Rubus_traits_df <- BIEN_trait_species(Rubus_species)
unique(Rubus_traits_df$trait_name)
Rubus_leaf_CN <- BIEN_trait_mean(Rubus_species, "leaf carbon content per leaf nitrogen content")
Rubus_mean_leaf_Cn <- mean(as.numeric(Rubus_leaf_CN$mean_value))

rm(Rubus_species, Rubus_traits_df, Rubus_leaf_CN, Rubus_mean_leaf_Cn)

##----------
Vacc_species <- c("Vaccinium ovatum", "Vaccinium parvifolium")
Vacc_traits_df <- BIEN_trait_species(Vacc_species)
unique(Vacc_traits_df$trait_name)
Vacc_leaf_CN <- BIEN_trait_mean(Vacc_species, "leaf carbon content per leaf nitrogen content")
Vacc_mean_leaf_Cn <- mean(as.numeric(Vacc_leaf_CN$mean_value))
  
rm(Vacc_leaf_CN, Vacc_mean_leaf_Cn, Vacc_species, Vacc_traits_df)

##-----------
WhipMode_traits_df <- BIEN_trait_species("Whipplea modesta")
unique(WhipMode_traits_df$trait_name) # doesn't return anything

Whipplea_traits <- BIEN_trait_genus("Whipplea")
unique(Whipplea_traits$trait_name) # NOTHING

rm(WhipMode_traits_df, Whipplea_traits)
  