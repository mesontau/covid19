library(covid19)


maxDetected = 10000

dataPath = "data"
plotsPath = file.path("data", "plots")


## World

fileWorld   = file.path(dataPath, "data_covid19_World.txt")

covid19W  <- readDataCovid(dataFile = fileWorld)


covid19WFiltered <- covid19W %>%
  filter((max.detected > maxDetected |
           country == "Schweden" |
           country == "Kuba") & country != "USA")

covid19::plotTimeToDouble( covid19WFiltered
                           ,plotPath = file.path(plotsPath, "cv19_time2double_World.png")
                           ,plotType = c("smooth")
                           ,plotCurve = c("detected", "deceased", "active")
                           ,ylims = c(0,40)
                           ,ncols = 5)

covid19::plotDeltaDetected( covid19WFiltered
                           ,plotPath = file.path(plotsPath, "cv19_deltaDetected_World.png")
                           ,plotType = c("smooth")
                           ,plotCurve = c("detected", "deceased", "active")
                           ,ylims = c(-3000,5000)
                           ,ncols = 5)

covid19::plotTotalCases(covid19Filtered = covid19WFiltered
                        ,plotPath = file.path(plotsPath, "cv19_totalCases_World.png")
                        ,ncols = 5)

covid19::plotNewCasesVsTotalCases(covid19WFiltered %>% filter(max.detected > 20000)
                                  ,plotPath = file.path(plotsPath, "cv19_NewCasesVsTotalCases_World.png")
                                  ,plotType = "smooth"
                                  ,plotCurve = "detected")


## Germany

fileGermany = file.path(dataPath, "data_covid19_Germany.txt")

covid19GE <- readDataCovid(dataFile = fileGermany)

covid19::plotTimeToDouble( covid19GE
                           ,plotPath = file.path(plotsPath, "cv19_time2double_Germany.png")
                           ,plotType = c("smooth")
                           ,plotCurve = c("detected", "deceased")
                           ,ylims = c(0,30)
                           ,ncols = 4)

covid19::plotDeltaDetected( covid19GE
                           ,plotPath = file.path(plotsPath, "cv19_deltaDetected_Germany.png")
                           ,plotType = c("smooth")
                           ,plotCurve = c("detected", "deceased")
                           ,ylims = c(0,2000)
                           ,ncols = 4)


covid19::plotTotalCases(covid19GE
                        ,plotPath = file.path(plotsPath, "cv19_totalCases_Germany.png")
                        ,ncols = 4)

covid19::plotNewCasesVsTotalCases(covid19GE %>% filter(max.detected > 5000)
                                  ,plotPath = file.path(plotsPath, "cv19_NewCasesVsTotalCases_Germany.png")
                                  ,plotType = "smooth"
                                  ,plotCurve = "detected")


## Spain

fileSpain = file.path(dataPath, "data_covid19_Spain.txt")

covid19SP <- readDataCovid(dataFile = fileSpain, col_types = NULL)

countryDict = list("AN" = "Andalucía"
                   ,"AR" = "Aragón"
                   ,"AS" = "Asturias"
                   ,"IB" = "Baleares"
                   ,"CN" = "Canarias"
                   ,"CB" = "Cantabria"
                   ,"CM" = "Castilla La Mancha"
                   ,"CL" = "Castilla y León"
                   ,"CT" = "Cataluña"
                   ,"CE" = "Ceuta"
                   ,"VC" = "C.Valenciana"
                   ,"EX" = "Extremadura"
                   ,"GA" = "Galicia"
                   ,"MD" = "Madrid"
                   ,"ME" = "Melilla"
                   ,"ML" = "Melilla"
                   ,"MC" = "Murcia"
                   ,"NC" = "Navarra"
                   ,"PV" = "Euskadi"
                   ,"RI" = "Rioja")

covid19SP$country <- unlist(countryDict[covid19SP$country])


covid19::plotTimeToDouble( covid19SP
                           ,plotPath = file.path(plotsPath, "cv19_time2double_Spain.png")
                           ,plotType = c("smooth")
                           ,plotCurve = c("detected", "deceased")
                           ,ylims = c(0,30)
                           ,ncols = 5)

covid19::plotDeltaDetected( covid19SP
                           ,plotPath = file.path(plotsPath, "cv19_deltaDetected_Spain.png")
                           ,plotType = c("smooth")
                           ,plotCurve = c("detected", "deceased")
                           # ,ylims = c(0,20)
                           ,ncols = 5)


covid19::plotTotalCases(covid19SP
                        ,plotPath = file.path(plotsPath, "cv19_totalCases_Spain.png")
                        ,ncols = 5)


covid19::plotNewCasesVsTotalCases(covid19SP %>% filter(max.detected > 5000)
                                  ,plotPath = file.path(plotsPath, "cv19_NewCasesVsTotalCases_Spain.png")
                                  ,plotType = "smooth"
                                  ,plotCurve = "detected")







Bremen <- covid19GE %>% filter(country == "Bremen")
Germany <- covid19W %>% filter(country == "Deutschland")
Spain <- covid19W %>% filter(country == "Spanien")
Italy <- covid19W %>% filter(country == "Italien")
China <- covid19W %>% filter(country == "China")
UK <- covid19W %>% filter(country == "Großbritannien")
USA <- covid19W %>% filter(country == "USA")
#
# unique(covid19W$country)

indivCountries <- list( "Bremen"  = Bremen
                       ,"Germany" = Germany
                       ,"Spain"   = Spain
                       ,"Italy"   = Italy
                       ,"China"   = China
                       ,"UK"      = UK)
countryName = "Bremen"
lapply(names(indivCountries), function(countryName){
  covid19::plotDeltaDetected( indivCountries[[countryName]]
                              ,plotPath = file.path(plotsPath, paste0("cv19_deltaDetected_", countryName, ".png"))
                              ,plotType = c("smooth", "line")
                              ,plotCurve = c("detected", "deceased", "active")
                              ,ncols = 1)
})

covid19::plotDeltaDetected( Bremen
                            ,plotPath = file.path(plotsPath, "cv19_deltaDetected_Bremen.png")
                            ,plotType = c("smooth", "line")
                            ,plotCurve = c("detected", "deceased")
                            # ,ylims = c(0,20)
                            ,ncols = 1)



covid19::plotDeltaDetected( USA
                            ,plotPath = file.path(plotsPath, "cv19_deltaDetected_USA.png")
                            ,plotType = c("smooth", "line")
                            ,plotCurve = c("detected", "deceased")
                            # ,ylims = c(0,20)
                            ,ncols = 1)

