##################################################################################
##### INFEWS Land Use: Soils Download and Intersection
##### Function File
##################################################################################

### Note: Raw download function; uses selenium to crawl through WSS and download county
  #       soil data
f_soilDL <- function(state){
  
  ### Note: Location/WebAddress
  url_start <- "https://websoilsurvey.sc.egov.usda.gov/App/HomePage.htm"
  
  ### Note: Delete zipped file location if exists
  if (dir.exists(here("Data", "Raw"))){
    unlink(here("Data", "Raw"))
    dir.create(here("Data", "Raw"))
  }else{
    dir.create(here("Data", "Raw"))
  }
  
  ### Note: RSelenium; navigate to site
  rD <- rsDriver(browser="firefox", port=4545L, verbose=F)
  remDr <- rD[["client"]]
  remDr$navigate(url_start)
  
  ### Note: Navigate to WSS portal tab
  wss_nav <- remDr$findElement(using = "id", value = "startContent")$clickElement()
  allwins <- unlist(remDr$getWindowHandles())
  newwin  <- allwins[!(allwins %in% unlist(remDr$getCurrentWindowHandle()))]
  remDr$switchToWindow(newwin)
  remDr$getCurrentWindowHandle()
  
  ### Note: Find and click download loc tabs
  remDr$findElement(using = "id",
                    value = "Download_Soils_Data")$clickElement()
  remDr$findElement(using = "id",
                    value = "Download_Soils_Data_for..._Soil_Survey_Area_.40.SSURGO.41._title")$clickElement()
  
  ### Note: Send state of interest to tab
  remDr$findElement(using = "id", 
                    value = "control-68914-state")$sendKeysToElement(list(state))
  
  Sys.sleep(5) # give the page time to fully load
  html <- remDr$getPageSource()[[1]]
  
  ### Note: Pull links from html; filter only those from state of interest
  Links <- read_html(html) %>%
    html_nodes(".last")    %>%
    html_nodes("a")        %>%
    html_attr("href")      %>%
    as.data.frame(.)       %>%
    rename(.,
           FullLink = .)   %>%
    rowwise()              %>%
    mutate(Templates = if_else(str_detect(FullLink,
                                          "Template"),
                               1,
                               0))                     %>%
    filter(Templates == 0)
  
  ### Note: Create save locations, save, unzip
  Links <- Links %>%
    rowwise()    %>%
    mutate(filename   = str_sub(FullLink,
                                start = str_locate_all(FullLink, "/")[[1]][7,1] + 1))   %>%
    mutate(filerename = str_sub(filename, 
                                start = str_locate_all(filename, "_")[[1]][2,1] + 1,
                                end   = str_locate_all(filename, "_")[[1]][3,1] - 1)) %>%
    mutate(dest       = paste0(here("Raw", filename)))
  
  for (i in 1:dim(Links)[1]){
    ### Delete existing zipped for space
    if (dir.exists(here("Data", "Raw"))){
      unlink(here("Data", "Raw"))
      dir.create(here("Data", "Raw"))
    }else{
      dir.create(here("Data", "Raw"))
    }
    
    ### Download file
    download.file(Links$FullLink[1],
                  destfile = Links$dest[1])
    
    ### Unzip to permanent location
    unzip(Links$dest[1],
          exdir = here("Data", "Soils", state))
  }
}

### Note: Import and clean soils data for each county; save resulting file as a geopackage
f_SoilClean <- function(state, cnty){
  
  soils.sf <- st_read(here("Data", "Soils", state, cnty, "spatial", paste0("soilmu_a_", str_to_lower(cnty), ".shp"))) %>%
    rename(mapunit_key = MUKEY)
  
  ### Note: Import Column heads
  col_names   <- read_delim(here("Data", "Soils", state, cnty, "tabular", "mstabcol.txt"), 
                            delim = "|",
                            col_names = FALSE)                                  %>%
    as.data.table()                                                             %>%
    .[X1 == "muaggatt" | X1 == "component" | X1 == "cointerp",]
  col_names_v  <- col_names[col_names$X1 == "muaggatt",]$X4
  col_names_v2 <- col_names[col_names$X1 == "component",]$X4
  col_names_v3 <- col_names[col_names$X1 == "cointerp",]$X4
  
  ### Note: Attributes
  mu_att.df <- read_delim(here("Data", "Soils", state, cnty, "tabular", "muaggatt.txt"),
                          delim = "|",
                          col_names = col_names_v)
  
  ### Note: Component
  mu_com.df <- read_delim(here("Data", "Soils", state, cnty, "tabular", "comp.txt"),
                          delim     = "|",
                          col_names = col_names_v2)
  
  ### Note: Crop Prod Index
  mu_cpi.df <- read_delim(here("Data", "Soils", state, cnty, "tabular", "cinterp.txt"),
                          delim     = "|",
                          col_names = col_names_v3) %>%
    filter(main_rule_key == 54955 & sequence_number == 0)
  
  
  mu_comcpi.df <- left_join(mu_com.df, mu_cpi.df, 
                            by = "component_key")
  mu.df        <- left_join(mu_att.df, mu_comcpi.df, 
                            by = "mapunit_key")         %>%
    mutate(component_percent_r = if_else(is.na(interp_high_rv),
                                         0,
                                         component_percent_r)) %>%
    group_by(mapunit_key)                                      %>%
    mutate(tot_nonna_share     = sum(component_percent_r))     %>%
    ungroup()                                                  %>%
    mutate(share = component_percent_r/tot_nonna_share)        %>%
    mutate(w_nccpi = share*interp_high_rv)                     %>%
    filter(!is.na(interp_high_rv))                             %>%
    group_by(mapunit_key)                                      %>%
    mutate(nccpi = sum(w_nccpi))                               %>%
    .[,c("mapunit_key", "nccpi")]                              %>%
    mutate(mapunit_key = as.character(mapunit_key))            %>%
    unique()
  
  ### Note: Merge components data table to spatial data file 
  soils.sf <- soils.sf            %>%
    left_join(mu.df,
              by = "mapunit_key")
  soils.sf[is.na(soils.sf$nccpi), "nccpi"] <- 0
  
  ### Note: Write merged county-level soils file with variables of interest
  st_write(soils.sf,
           here("Data", "Soils", paste0("SSURGO_", cnty, ".gpkg")))
}

### Note: Intersect the soils shapefile with parcels
f_SoilsInt <- function(state, cnty){
  cnty_ID <- str_sub(cnty, start = 3)
  ### Note: Load in appropriate field file
  clu.sf <- st_read(here("Data", "CLU", paste0("CLU_", state, "_Merged.gpkg"))) %>%
    .[clu.sf$County == cnty_ID,]
  
  ### Note: Load in county level soils file
  soils.sf <- st_read(here("Data", "Soils", paste0("SSURGO_", cnty, ".gpkg"))) %>%
    st_transform(.,st_crs(clu.sf))
  
  clu_soils_sf <- st_intersection(clu.sf, soils.sf) %>%
    mutate(clusoils_a = st_area(.))        %>%
    group_by(CLU_ID)                       %>%
    mutate(clusoils_max = max(clusoils_a)) %>%
    ungroup()                              %>%
    filter(clusoils_max == clusoils_a)     %>%
    dplyr::select(!one_of("clusoils_max", "clusoils_a", "AREASYMBOL", "SPATIALVER", "MUSYM")) 
  
  st_geometry(clu_soils_sf) <- NULL
  write_csv(clu_soils_sf,
            here("Data", "Soils", "CLU_Int", paste0("CLU_Soils_", cnty, ".csv")))
}
