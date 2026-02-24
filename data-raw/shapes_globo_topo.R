## code to prepare `shapes_globo_topo` dataset goes here

#########################################################################
############ shape (a.k.a. blue colored shapes) ####################
#########################################################################

#globo_topo_poly <- sf::st_read("subset_globo_topo_simp_filt.shp")

#topocat <- read.csv("data/topocat_Hylak_id_lev03_HYBAS_ID_v1c.csv")

# add the lev3_HYBAS_ID column to the globo_topo_poly from topocat where it matches Hylak_id
globo_topo_poly$lev3_HYBAS_ID <- topocat$lev3_HYBAS_ID[match(globo_topo_poly$Hylak_id, topocat$Hylak_id)]

globo_topo_poly <- sf::st_cast(globo_topo_poly, "POLYGON")
globo_topo_poly <- sf::st_make_valid(globo_topo_poly)

# how many uniqur Hylak_id in globo_topo_poly
length(unique(globo_topo_poly$Hylak_id))

globo_topo_poly <- sf::st_simplify(globo_topo_poly, dTolerance = 0.08)
# same code as above but  make it less simplified
globo_topo_poly <- sf::st_simplify(globo_topo_poly, dTolerance = 0.02)
globo_topo_poly <- globo_topo_poly[!sf::st_is_empty(globo_topo_poly), ]

usethis::use_data(globo_topo_poly, overwrite = TRUE)

#########################################################################
############ Outlines (borders of the shapes) ###########################
#########################################################################
globo_topo_lines <-  sf::st_cast(globo_topo_poly, to = "MULTILINESTRING")
globo_topo_lines <- sf::st_cast(globo_topo_lines, to = "LINESTRING")

usethis::use_data(globo_topo_lines, overwrite = TRUE)

