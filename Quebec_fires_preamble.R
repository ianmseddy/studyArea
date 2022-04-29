## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "Quebec_fires_preamble",
  description = "",
  keywords = "",
  authors = c(
    person("Ian", "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person("Alex M", "Chubaty", email = "achubaty@for-cast.ca", role = c("aut"))
  ),
  childModules = character(0),
  version = list(Quebec_fires_preamble = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "Quebec_fires_preamble.Rmd"), ## same file
  reqdPkgs = list("PredictiveEcology/SpaDES.core@development (>= 1.0.10.9002)",
                  "fasterize", "sf" ,"raster", "ggplot2", "PredictiveEcology/climateData",
                  "PredictiveEcology/LandR@development"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
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
    createsOutput("rasterToMatch", "RasterLayer", desc = "Raster to match, based on study area."),
    createsOutput("rasterToMatchLarge", "RasterLayer", desc = "Raster to match (large) based on studyAreaLarge."),
    createsOutput("rasterToMatchReporting", "RasterLayer", desc = "Raster to match based on studyAreaReporting."),
    createsOutput("studyArea", "SpatialPolygons", desc = "Buffered study area in which to run simulations."),
    createsOutput("studyAreaLarge", "SpatialPolygons", desc = "Buffered study area used for parameterization/calibration."),
    createsOutput("studyAreaReporting", "SpatialPolygons", desc = "Unbuffered study area used for reporting/post-processing.")
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "sppEquiv", objectClass = "data.table", desc = NA),
    createsOutput(objectName = "sppColors", objectClass = "character", desc = NA)
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
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "Quebec_fires_preamble", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "Quebec_fires_preamble", "save")
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
  sim$sppEquiv <- LandR::sppEquivalencies_CA
  sim$sppEquiv <- sim$sppEquiv[LandR %in% c("Abie_bal", "Pice_mar",
                                            "Pinu_ban", "Lari_lar",
                                            "Pice_gla", "Betu_pap",
                                            "Popu_tre"), ]
  sim$sppColors <- LandR::sppColors(sppEquiv = sim$sppEquiv, sppEquivCol = "LandR", palette = "Accent")
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("studyAreaReporting", sim)) {
    sim$studyAreaReporting <- prepInputs(
      url = "https://drive.google.com/file/d/1OYWKXv3OciReK8PhQY0E7sFL5A-u3P6L/",
      destinationPath = dPath,
      fun = "sf::st_read",
      overwrite = TRUE
    ) ## use sf for now; convert to spdf at end
  }

  if (!suppliedElsewhere("studyArea", sim)) {
    sim$studyArea <- st_buffer(studyAreaReporting, 20000) ## 20 km buffer
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

  sim$studyArea <- st_transform(sim$studyArea, crs = crs(sim$rasterToMatch)) %>% as_Spatial()
  sim$studyAreaLarge <- st_transform(sim$studyAreaLarge, crs = crs(sim$rasterToMatchLarge)) %>% as_Spatial()
  sim$studyAreaReporting <- st_transform(sim$studyAreaReporting, crs = crs(sim$rasterToMatchReporting)) %>% as_Spatial()

  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot(data, aes(TheSample)) +
    geom_histogram(...)
}

### add additional events as needed by copy/pasting from above
