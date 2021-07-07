###############################################################################
##### DRFEWS: Parcel-Level Soil Composition
##### Scrape/Download Files Using RSelenium
##### Date: 6_21_21
##### Description: Navigate Soil Survey Website to download county level, highly
#####              detailed soil composition files using RSelenium
###############################################################################
### Note: Java Bullshittery
old_path <- Sys.getenv("PATH")
Sys.setenv(PATH = paste(old_path, 
                        "C:\\Users\\cultice.7\\Documents\\Java\\jdk-11.0.11+9\\bin", 
                        sep = ";"))
Sys.setenv(JAVA_HOME= "C:\\Users\\cultice.7\\Documents\\Java\\jdk-11.0.11+9\\jre")

### Note: Packages of Interest
libraries <- c("tidyverse",
               "tictoc",
               "tibble",
               "data.table",
               "terra",
               "sf",
               "here",
               "future", 
               "future.apply",
               "raster",
               "rgdal",
               "furrr", 
               "pryr",
               "reticulate",
               "RSelenium")
pacman::p_load(char = libraries)

### Note: Location/WebAddress
state <- "Ohio"
map(state_v, f_SoilDL)

### Note: Cleaning Soil Shapefiles for Data Extracts;
  #       For now, I grab solely the crop productivity index from these files, but we could include any number of variables
  #       that might affect conversion costs, forest productivity, etc such as soil types, bedrock depth, forest prod index,
  #       ect. These might become more useful with a parcel level model.
state_v <- "Ohio"
inp1_v  <- vector(mode = "character",
                  length = 0L)
inp2_v  <- vector(mode = "character",
                  length =0L)

for (i in 1:length(state_v)){
  cnty_v  <- list.files(here("Data", "Soils", state_v[i]),
                      full.names = FALSE)
  inp1_v <- c(inp1_v,
                rep(state_v[i],
                times = length(cnty_v)))
  inp2_v <- c(inp2_v,
                cnty_v)
}
inp_l <- list(inp1_v, inp2_v)
state <- inp1_v[1]
cnty  <- inp2_v[1]
pmap(inp_l, f_SoilClean)

### Note: Intersections of parcels and soil files
pmap(inp_l, f_SoilsInt)

### Note: Merge together all soils data and save
files_v    <- list.files(here("Data", "Soils", "CLU_Int"),
                         full.names = TRUE)
soils_m.dt <- lapply(files_v, fread) %>%
  rbindlist()
