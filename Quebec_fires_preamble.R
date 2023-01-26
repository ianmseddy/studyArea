defineModule(sim, list(
  name = "Quebec_fires_preamble",
  description = "create `studyArea`, `rasterToMatch`, `sppEquiv` objects for use with fireSense in Quebec, Canada.",
  keywords = "",
  authors = c(
    person("Ian", "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person("Alex M", "Chubaty", email = "achubaty@for-cast.ca", role = c("aut"))
  ),
  childModules = character(0),
  version = list(Quebec_fires_preamble = "0.1.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "Quebec_fires_preamble.Rmd"), ## same file
  reqdPkgs = list("geodata", "ggplot2", "nngeo", "sf", "spatialEco", "raster", "units",
                  "PredictiveEcology/climateData@development",
                  "PredictiveEcology/LandR@development",
                  "PredictiveEcology/SpaDES.core@development (>= 1.1.1)"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("studyAreaName", "character", NA, NA, NA,
                    paste("Should include 'QC_boreal' for managed boreal forests.",
                          "May also include the ecoprovince ID in which to run the model.",
                          "If `NA` (default) use Guillaume's custom study area.")),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput("rasterToMatch", "RasterLayer", desc = "Raster to match, based on study area."),
    expectsInput("rasterToMatchLarge", "RasterLayer", desc = "Raster to match (large) based on studyAreaLarge."),
    expectsInput("rasterToMatchReporting", "RasterLayer", desc = "Raster to match based on studyAreaReporting."),
    expectsInput("studyArea", "SpatialPolygons", desc = "Buffered study area in which to run simulations."),
    expectsInput("studyAreaLarge", "SpatialPolygons", desc = "Buffered study area used for parameterization/calibration."),
    expectsInput("studyAreaReporting", "SpatialPolygons", desc = "Unbuffered study area used for reporting/post-processing.")
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput("rasterToMatch", "RasterLayer", desc = "Raster to match, based on study area."),
    createsOutput("rasterToMatchLarge", "RasterLayer", desc = "Raster to match (large) based on studyAreaLarge."),
    createsOutput("rasterToMatchReporting", "RasterLayer", desc = "Raster to match based on studyAreaReporting."),
    createsOutput("sppColorVect", "character", desc = NA),
    createsOutput("sppEquiv", "data.table", desc = NA),
    createsOutput("sppEquivCol", "data.table", desc = NA),
    createsOutput("studyArea", "SpatialPolygons", desc = "Buffered study area in which to run simulations."),
    createsOutput("studyAreaLarge", "SpatialPolygons", desc = "Buffered study area used for parameterization/calibration."),
    createsOutput("studyAreaPSP", "SpatialPolygonsDataFrame",
                  paste("this area will be used to subset PSP plots before building the statistical model.",
                        "Currently PSP datasets with repeat measures exist only for Saskatchewan,",
                        "Alberta, Boreal British Columbia, and Ontario")),
    createsOutput("studyAreaReporting", "SpatialPolygons", desc = "Unbuffered study area used for reporting/post-processing.")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.Quebec_fires_preamble = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      #sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "Quebec_fires_preamble", "plot")
      #sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "Quebec_fires_preamble", "save")
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  sim$sppEquivCol <- "LandR"

  sim$sppEquiv <- LandR::sppEquivalencies_CA
  sim$sppEquiv <- sim$sppEquiv[LandR %in% c("Abie_bal",
                                            "Betu_pap",
                                            "Lari_lar",
                                            "Pice_gla", "Pice_mar",
                                            "Pinu_ban",
                                            "Popu_tre"), ]
  sim$sppColorVect <- LandR::sppColors(sppEquiv = sim$sppEquiv, sppEquivCol = sim$sppEquivCol,
                                       palette = "Accent")

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  mod$targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                         "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  chunks <- strsplit(P(sim)$studyAreaName, "_")[[1]]
  mod$ecoprov <- chunks[which(!is.na(suppressWarnings(as.integer(chunks))))] ## keep as character
  mod$ecoprov <- if (length(mod$ecoprov) > 0) mod$ecoprov else NULL

  if (!suppliedElsewhere("studyAreaReporting", sim)) {
    if (is.na(P(sim)$studyAreaName)) {
      sim$studyAreaReporting <- prepInputs(
        ## defaults to Guillaume's study area
        url = "https://drive.google.com/file/d/1OYWKXv3OciReK8PhQY0E7sFL5A-u3P6L/",
        destinationPath = dPath,
        fun = "sf::st_read",  ## use sf for now; convert to spdf at end
        overwrite = TRUE
      ) |>
        st_make_valid()
    } else if (grepl("QC_boreal", P(sim)$studyAreaName)) {
      QC <- geodata::gadm(country = "CAN", level = 1, path = dPath) |>
        st_as_sf() |>
        subset(NAME_1 == "Québec") |>
        st_transform(mod$targetCRS)

      QC_boreal <- Cache({
        prepInputs(
          url = "https://drive.google.com/file/d/1enlgSf4-EJKuHZL4J79TQthoktSRBarQ/",
          targetFile = "NABoreal.shp",
          archive = asPath("boreal.zip"),
          destinationPath = dPath,
          fun = "sf::read_sf",
          filename2 = NULL
        ) |>
          subset(TYPE %in% "BOREAL") |> ## only boreal forests for now; could include hemiboreal later
          st_transform(mod$targetCRS) |>
          st_intersection(QC) |>
          st_cast("POLYGON") |>
          spatialEco::remove.holes() |>
          `st_crs<-`(mod$targetCRS)
      }, userTags = c(currentModule(sim), "QC_boreal"))

      ## https://www.donneesquebec.ca/recherche/dataset/unite-d-amenagement
      ## fails to extract the zip due to 'invalid multibyte character' in filename of the metadata pdf
      tryCatch(preProcess(
        url = "https://diffusion.mffp.gouv.qc.ca/Diffusion/DonneeGratuite/Foret/LIM_TERRITOIRE_FOREST_PUBLIC/UA/UA_SHP.zip",
        targetFile = "STF_UA.shp",
        alsoExtract = "similar",
        destinationPath = dPath,
        filename2 = NULL,
        fun = "sf::read_sf"
      ), error = function(e) NULL)

      files <- unzip(file.path(dPath, "UA_SHP.zip"), list = TRUE)$Name
      files2extract <- grep("STF_UA", files, value = TRUE)
      unzip(file.path(dPath, "UA_SHP.zip"), files = files2extract, exdir = dPath)

      ## 1. read in data
      ## 2. remove most eastward polys + isalnds via subset
      ## 3. buffer 0 to get rid of 'lines'
      ## 4. buffer out, union the polys, they buffer back in (good enough approx of the study area)
      ## 5. remove holes
      ## 6. reproject the resulting single polygon
      d <- units::set_units(5, km) ## this might need to be tweaked; keep as small as possible
      ua <- Cache({
        st_read(file.path(dPath, "UA_SHP", "STF_UA.shp")) |>
          subset(!NO_UG_RESP %in% c("011", "012", "035", "051", "93", "094", "111", "112")) |>
          st_make_valid() |>
          st_transform(mod$targetCRS) |>
          st_buffer(0) |>
          st_buffer(d) |>
          st_union() |>
          st_buffer(-d) |>
          nngeo::st_remove_holes()
      }, userTags = c(currentModule(sim), "ua")) ## TODO: improve this

      ecoprov <- prepInputs(
        url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/province/ecoprovince_shp.zip",
        targetFile = "ecoprovinces.shp",
        alsoExtract = "similar",
        fun = "sf::st_read",
        destinationPath = dPath
      ) |>
        subset(ECOPROVINC %in% mod$ecoprov) |>
        st_transform(mod$targetCRS)

      studyAreaReporting <- st_intersection(QC_boreal, ua)
      if (!is.null(ecoprov)) {
        studyAreaReporting <- st_intersection(studyAreaReporting, ecoprov)
      }
      sim$studyAreaReporting <-  nngeo::st_remove_holes(studyAreaReporting)
    }
  }

  if (!suppliedElsewhere("studyArea", sim)) {
    sim$studyArea <- st_buffer(sim$studyAreaReporting, 20000) ## 20 km buffer
  }

  if (!suppliedElsewhere("studyAreaLarge", sim)) {
    sim$studyAreaLarge <- sim$studyArea
  }

  if (!suppliedElsewhere("rasterToMatchReporting", sim)) {
    sim$rasterToMatchReporting <- Cache(prepInputsLCC,
                                        year = 2010,
                                        destinationPath = dPath,
                                        studyArea = sim$studyAreaReporting)
    sim$rasterToMatchReporting <- Cache(projectRaster,
                                        sim$rasterToMatchReporting,
                                        crs = crs(sim$rasterToMatchReporting),
                                        res = c(250, 250), method = "ngb",
                                        filename = file.path(dPath, "RTMR_Quebec_studyArea.tif"),
                                        overwrite = TRUE)
  }

  if (!suppliedElsewhere("rasterToMatch", sim)) {
    sim$rasterToMatch <- Cache(prepInputsLCC,
                               year = 2010,
                               destinationPath = dPath,
                               studyArea = sim$studyArea)
    sim$rasterToMatch <- Cache(projectRaster,
                               sim$rasterToMatch,
                               crs = crs(sim$rasterToMatch),
                               res = c(250, 250), method = "ngb",
                               filename = file.path(dPath, "RTM_Quebec_studyArea.tif"),
                               overwrite = TRUE)
  }

  if (!suppliedElsewhere("rasterToMatchLarge", sim)) {
    sim$rasterToMatchLarge <- Cache(prepInputsLCC,
                                    year = 2010,
                                    destinationPath = dPath,
                                    studyArea = sim$studyAreaLarge)
    sim$rasterToMatchLarge <- Cache(projectRaster,
                                    sim$rasterToMatchLarge,
                                    crs = crs(sim$rasterToMatchLarge),
                                    res = c(250, 250), method = "ngb",
                                    filename = file.path(dPath, "RTML_Quebec_studyArea.tif"),
                                    overwrite = TRUE)
  }

  sim$studyArea <- st_transform(sim$studyArea, crs = crs(sim$rasterToMatch)) |> as_Spatial()
  sim$studyAreaLarge <- st_transform(sim$studyAreaLarge, crs = crs(sim$rasterToMatchLarge)) |> as_Spatial()
  sim$studyAreaReporting <- st_transform(sim$studyAreaReporting, crs = crs(sim$rasterToMatchReporting)) |> as_Spatial()

  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
