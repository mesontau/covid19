library(covid19)


maxDetected = 5000

dataPath = "data"
plotsPath = file.path("data", "plots")


## World

fileWorld   = file.path(dataPath, "data_covid19_World.txt")

covid19W  <- readDataCovid(dataFile = fileWorld)


covid19WFiltered <- covid19W %>%
  filter(max.detected > maxDetected |
           country == "Schweden" |
           country == "Kuba")

covid19::plotTimeToDouble( covid19WFiltered
                           ,plotPath = file.path(plotsPath, "cv19_time2double_World.png")
                           ,plotType = c("smooth")
                           ,plotCurve = c("detected", "deceased")
                           ,ylims = c(0,20)
                           ,ncols = 5)

covid19::plotTotalCases(covid19WFiltered
                        ,plotPath = file.path(plotsPath, "cv19_totalCases_World.png")
                        ,ncols = 5)


## Germany

fileGermany = file.path(dataPath, "data_covid19_Germany.txt")

covid19GE <- readDataCovid(dataFile = fileGermany)

covid19::plotTimeToDouble( covid19GE
                           ,plotPath = file.path(plotsPath, "cv19_time2double_Germany.png")
                           ,plotType = c("smooth")
                           ,plotCurve = c("detected", "deceased")
                           ,ylims = c(0,20)
                           ,ncols = 4)



covid19::plotTotalCases(covid19GE
                        ,plotPath = file.path(plotsPath, "cv19_totalCases_Germany.png")
                        ,ncols = 4)

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
                   ,"MC" = "Murcia"
                   ,"NC" = "Navarra"
                   ,"PV" = "Euskadi"
                   ,"RI" = "Rioja")

covid19SP$country <- unlist(countryDict[covid19SP$country])


covid19::plotTimeToDouble( covid19SP
                           ,plotPath = file.path(plotsPath, "cv19_time2double_Spain.png")
                           ,plotType = c("smooth")
                           ,plotCurve = c("detected", "deceased")
                           ,ylims = c(0,20)
                           ,ncols = 5)



covid19::plotTotalCases(covid19SP
                        ,plotPath = file.path(plotsPath, "cv19_totalCases_Spain.png")
                        ,ncols = 5)


