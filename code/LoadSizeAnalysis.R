#
# LoadSizeAnalysis.R
#   - visualise the load size performance of Shovel Operators
#   - requires:
#      - load size data (via LoadFactors.sql)
#
#   - produces:
#      - Load size distribution graph by Shovel (LS_Shovel.png)
#      - Load size distribution graph by Shovel LP Type (LS_ShovelType.png)
#      - Load size distribution graph by Truck LP Type (LS_TruckType.png)
#      - Load size distribution graph by Material (LS_Material.png)
#      - Load size distribution graph by Load Group (LS_LoadGroup.png)
#      - Load size distribution graph by Shovel Operator (LS_ShovelOperator.png)
#      - Load size distribution graph by Shovel Operator & Load Group (LS_LoadGroup_*.png)
#      - Load size distribution graph by Shovel Operator & Material (LS_Material_*.png)
#      - Load size distribution graph by Shovel Operator & Shovel Lp Type (LS_ExcavType_*.png)
#      - Various CSV files summarising the above data for further analysis
#
# Client-specific value(s) & function(s):
#   - Locale specific version of 'Regular' in LoadFactors.sql
#   - Change the naming convention for files to suit local preference
#

# Setup
library(ggplot2)
library(plyr)
library(stringr)

# Functions
ReadLoads <- function(x) {
  tempLoads <- read.csv(paste0("./data/", x), header = FALSE, stringsAsFactors = FALSE)
  colnames(tempLoads) <- c("Excav", "ExcavLP", "Operator", "Truck", "TruckLP", "Material", "Group", "Tons", "LoadFactor", "LoadProp")
  tempLoads <- tempLoads[tempLoads$Tons > 50,]
  return(tempLoads)
}

# Get the data
message("Reading data ...")
loadFiles <- dir(path = "./data", pattern = "LoadProp.*csv$")
theLoads <- data.frame()
for(f in loadFiles) {
    theLoads <- rbind(theLoads, ReadLoads(f))
}

# Start with the easy ones
# plot the density distribution by Shovel - ignores Operator, Material, etc.
message("\nPlotting 'Shovels' ...")
png("./output/LS_Shovel.png", width=1800, height=900)
p <- ggplot(theLoads[theLoads$LoadProp >= 0.5 & theLoads$LoadProp <= 1.5,], aes(x=LoadProp)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  xlab("Normalised load size") +
  ylab("Density") +
  geom_vline(xintercept = 1.0) +
  geom_vline(xintercept = 1.2, color = "red") +
  geom_vline(xintercept = 0.8, color = "red") +
  ggtitle("Normalised Load Size Distribution by Shovel 2018 YTD") +
  geom_density(alpha = 0.2, fill = "#FF6666") +
  facet_wrap(~ Excav)
print(p)
dev.off()

excavSummary <- ddply(theLoads[theLoads$LoadProp >= 0.5 & theLoads$LoadProp <= 1.5,], c("Excav"), summarise,
                      n = length(Operator),
                      stddev    = sd(LoadProp),
                      mn        = mean(LoadProp),
                      lt80      = sum(LoadProp < 0.8),
                      lt80Rate  = lt80/n,
                      lt90      = sum(LoadProp < 0.9 & LoadProp >= 0.8),
                      lt90Rate  = lt90/n,
                      gt110     = sum(LoadProp >= 1.1 & LoadProp < 1.2),
                      gt110Rate = gt110/n,
                      gt120     = sum(LoadProp >= 1.2),
                      gt120Rate = gt120/n)
write.csv(excavSummary, file = "./output/ExcavLoadsizeSummary.csv", row.names = FALSE)

# plot the density distribution by Shovel type
message("\nPlotting 'Shovel type' ...")
png("./output/LS_ShovelType.png", width=1800, height=900)
p <- ggplot(theLoads[theLoads$LoadProp >= 0.5 & theLoads$LoadProp <= 1.5,], aes(x=LoadProp)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  xlab("Normalised load size") +
  ylab("Density") +
  geom_vline(xintercept = 1.0) +
  geom_vline(xintercept = 1.2, color = "red") +
  geom_vline(xintercept = 0.8, color = "red") +
  ggtitle("Normalised Load Size Distribution by Shovel Type 2018 YTD ") +
  geom_density(alpha = 0.2, fill = "#FF6666") +
  facet_wrap(~ ExcavLP)
print(p)
dev.off()

excavTypeSummary <- ddply(theLoads[theLoads$LoadProp >= 0.5 & theLoads$LoadProp <= 1.5,], c("ExcavLP"), summarise,
                          n = length(Operator),
                          stddev    = sd(LoadProp),
                          mn        = mean(LoadProp),
                          lt80      = sum(LoadProp < 0.8),
                          lt80Rate  = lt80/n,
                          lt90      = sum(LoadProp < 0.9 & LoadProp >= 0.8),
                          lt90Rate  = lt90/n,
                          gt110     = sum(LoadProp >= 1.1 & LoadProp < 1.2),
                          gt110Rate = gt110/n,
                          gt120     = sum(LoadProp >= 1.2),
                          gt120Rate = gt120/n)
write.csv(excavTypeSummary, file = "./output/ExcavTypeLoadsizeSummary.csv", row.names = FALSE)

# plot the density distribution by Truck type
message("\nPlotting 'Truck type' ...")
png("./output/LS_TruckType.png", width=1800, height=900)
p <- ggplot(theLoads[theLoads$LoadProp >= 0.5 & theLoads$LoadProp <= 1.5,], aes(x=LoadProp)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  xlab("Normalised load size") +
  ylab("Density") +
  geom_vline(xintercept = 1.0) +
  geom_vline(xintercept = 1.2, color = "red") +
  geom_vline(xintercept = 0.8, color = "red") +
  ggtitle("Normalised Load Size Distribution by Truck Type 2018 YTD") +
  geom_density(alpha = 0.2, fill = "#FF6666") +
  facet_wrap(~ TruckLP)
print(p)
dev.off()

truckTypeSummary <- ddply(theLoads[theLoads$LoadProp >= 0.5 & theLoads$LoadProp <= 1.5,], c("TruckLP"), summarise,
                          n = length(Operator),
                          stddev    = sd(LoadProp),
                          mn        = mean(LoadProp),
                          lt80      = sum(LoadProp < 0.8),
                          lt80Rate  = lt80/n,
                          lt90      = sum(LoadProp < 0.9 & LoadProp >= 0.8),
                          lt90Rate  = lt90/n,
                          gt110     = sum(LoadProp >= 1.1 & LoadProp < 1.2),
                          gt110Rate = gt110/n,
                          gt120     = sum(LoadProp >= 1.2),
                          gt120Rate = gt120/n)
write.csv(truckTypeSummary, file = "./output/TruckTypeLoadsizeSummary.csv", row.names = FALSE)

# Plot by Material - again, this ignores the operator, shovel type, truck type, etc.
# Sometimes a material is not used much, so filter out materials with < 20 loads
message("\nPlotting 'Material' ...")
LoadSizeDat <- ddply(theLoads, "Material", function(d) {if(nrow(d)>20) d else NULL})
png("./output/LS_Material.png", width=1800, height=900)
p <- ggplot(LoadSizeDat[LoadSizeDat$LoadProp >= 0.5 & LoadSizeDat$LoadProp <= 1.5,], aes(x=LoadProp)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  xlab("Normalised load size") +
  ylab("Density") +
  geom_vline(xintercept = 1.0) +
  geom_vline(xintercept = 1.2, color = "red") +
  geom_vline(xintercept = 0.8, color = "red") +
  ggtitle("Normalised Load Size Distribution by Material 2018 YTD") +
  geom_density(alpha = 0.2, fill = "#FF6666") +
  facet_wrap(~ Material)
print(p)
dev.off()

materialSummary <- ddply(theLoads[theLoads$LoadProp >= 0.5 & theLoads$LoadProp <= 1.5,], c("Material"), summarise,
                         n = length(Operator),
                         stddev    = sd(LoadProp),
                         mn        = mean(LoadProp),
                         lt80      = sum(LoadProp < 0.8),
                         lt80Rate  = lt80/n,
                         lt90      = sum(LoadProp < 0.9 & LoadProp >= 0.8),
                         lt90Rate  = lt90/n,
                         gt110     = sum(LoadProp >= 1.1 & LoadProp < 1.2),
                         gt110Rate = gt110/n,
                         gt120     = sum(LoadProp >= 1.2),
                         gt120Rate = gt120/n)
write.csv(materialSummary, file = "./output/MaterialLoadsizeSummary.csv", row.names = FALSE)

# Now by load group
message("\nPlotting 'Load group' ...")
LoadSizeDat <- ddply(theLoads, "Group", function(d) {if(nrow(d)>20) d else NULL})
png("./output/LS_LoadGroup.png", width=1800, height=900)
p <- ggplot(LoadSizeDat[LoadSizeDat$LoadProp >= 0.5 & LoadSizeDat$LoadProp <= 1.5,], aes(x=LoadProp)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  xlab("Normalised load size") +
  ylab("Density") +
  geom_vline(xintercept = 1.0) +
  geom_vline(xintercept = 1.2, color = "red") +
  geom_vline(xintercept = 0.8, color = "red") +
  ggtitle("Normalised Load Size Distribution by LoadGroup 2018 YTD") +
  geom_density(alpha = 0.2, fill = "#FF6666") +
  facet_wrap(~ Group)
print(p)
dev.off()

GroupSummary <- ddply(theLoads[theLoads$LoadProp >= 0.5 & theLoads$LoadProp <= 1.5,], c("Group"), summarise,
                      n = length(Operator),
                      stddev    = sd(LoadProp),
                      mn        = mean(LoadProp),
                      lt80      = sum(LoadProp < 0.8),
                      lt80Rate  = lt80/n,
                      lt90      = sum(LoadProp < 0.9 & LoadProp >= 0.8),
                      lt90Rate  = lt90/n,
                      gt110     = sum(LoadProp >= 1.1 & LoadProp < 1.2),
                      gt110Rate = gt110/n,
                      gt120     = sum(LoadProp >= 1.2),
                      gt120Rate = gt120/n)
write.csv(GroupSummary, file = "./output/GroupLoadsizeSummary.csv", row.names = FALSE)

# Now by Shovel Operator - ignores material, etc.
message("\nPlotting 'Shovel operator' ...")
LoadSizeDat <- ddply(theLoads, "Operator", function(d) {if(nrow(d)>20) d else NULL})
png("./output/LS_ShovelOperator.png", width=1800, height=900)
p <- ggplot(LoadSizeDat[LoadSizeDat$LoadProp >= 0.5 & LoadSizeDat$LoadProp <= 1.5 & LoadSizeDat$Operator != "",], aes(x=LoadProp)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  ggtitle("Normalised Load Size Distribution by Shovel Operator (>20 Loads) 2018 YTD") +
  xlab("Normalised load size") +
  ylab("Density") +
  geom_vline(xintercept = 1.0) +
  geom_vline(xintercept = 1.2, color = "red") +
  geom_vline(xintercept = 0.8, color = "red") +
  geom_density(alpha = 0.2, fill = "#FF6666") +
  facet_wrap(~ Operator)
print(p)
dev.off()

excavOperatorSummary <- ddply(theLoads[theLoads$LoadProp >= 0.5 & theLoads$LoadProp <= 1.5,], c("Operator"), summarise,
                              n = length(Operator),
                              stddev    = sd(LoadProp),
                              mn        = mean(LoadProp),
                              lt80      = sum(LoadProp < 0.8),
                              lt80Rate  = lt80/n,
                              lt90      = sum(LoadProp < 0.9 & LoadProp >= 0.8),
                              lt90Rate  = lt90/n,
                              gt110     = sum(LoadProp >= 1.1 & LoadProp < 1.2),
                              gt110Rate = gt110/n,
                              gt120     = sum(LoadProp >= 1.2),
                              gt120Rate = gt120/n)
write.csv(excavOperatorSummary, file = "./output/ExcavOperatorLoadSizeSummary.csv", row.names = FALSE)

# Shovel Operator by Load group
message("\nPlotting 'Shovel oper by load group' ...")
LoadSizeDat <- ddply(theLoads, c("Operator", "Group"), function(d) {if(nrow(d)>20) d else NULL})
loadGroups <- unique(theLoads$Group)
for(lg in seq_along(loadGroups)){
  theGroup <- loadGroups[lg]
  if(length(unique(LoadSizeDat[LoadSizeDat$Group == theGroup,]$Operator)) > 1) {
    message(paste0("\t", theGroup))
    png(paste0("./output/LS_LoadGroup_", theGroup, ".png"), width=1800, height=900)
    p <- ggplot(LoadSizeDat[LoadSizeDat$LoadProp >= 0.5 & LoadSizeDat$LoadProp <= 1.5 & !is.na(LoadSizeDat$Operator) & LoadSizeDat$Group == theGroup,], aes(x=LoadProp)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white") +
      ggtitle(paste0('Normalised "', theGroup, '" Group Load Size Distribution by Shovel Operator (>20 Loads) 2018 YTD')) +
      xlab("Normalised load size") +
      ylab("Density") +
      geom_vline(xintercept = 1.0) +
      geom_vline(xintercept = 1.2, color = "red") +
      geom_vline(xintercept = 0.8, color = "red") +
      geom_density(alpha = 0.2, fill = "#FF6666") +
      facet_wrap(~ Operator)
    print(p)
    dev.off()
  }
}

excavOperLoadGroupSummary <- ddply(theLoads[theLoads$LoadProp >= 0.5 & theLoads$LoadProp <= 1.5,], c("Operator", "Group"), summarise,
                                   n = length(Operator),
                                   stddev    = sd(LoadProp),
                                   mn        = mean(LoadProp),
                                   lt80      = sum(LoadProp < 0.8),
                                   lt80Rate  = lt80/n,
                                   lt90      = sum(LoadProp < 0.9 & LoadProp >= 0.8),
                                   lt90Rate  = lt90/n,
                                   gt110     = sum(LoadProp >= 1.1 & LoadProp < 1.2),
                                   gt110Rate = gt110/n,
                                   gt120     = sum(LoadProp >= 1.2),
                                   gt120Rate = gt120/n)
write.csv(excavOperLoadGroupSummary, file = "./output/ExcavOperatorLoadGroupLoadSizeSummary.csv", row.names = FALSE)

# Shovel Operator by Material
message("\nPlotting 'Shovel oper by material' ...")
LoadSizeDat <- ddply(theLoads, c("Operator", "Material"), function(d) {if(nrow(d)>20) d else NULL})
loadMaterials <- unique(theLoads$Material)
for(lm in seq_along(loadMaterials)){
  theMaterial <- loadMaterials[lm]
  if(length(unique(LoadSizeDat[LoadSizeDat$Material == theMaterial,]$Operator)) > 1) {
    message(paste0("\t", theMaterial))
    png(paste0("./output/LS_Material_", theMaterial, ".png"), width=1800, height=900)
    p <- ggplot(LoadSizeDat[LoadSizeDat$LoadProp >= 0.5 & LoadSizeDat$LoadProp <= 1.5 & !is.na(LoadSizeDat$Operator) & LoadSizeDat$Material == theMaterial,], aes(x=LoadProp)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white") +
      ggtitle(paste0('Normalised "', theMaterial, '" Material Load Size Distribution by Shovel Operator (>20 Loads) 2018 YTD')) +
      xlab("Normalised load size") +
      ylab("Density") +
      geom_vline(xintercept = 1.0) +
      geom_vline(xintercept = 1.2, color = "red") +
      geom_vline(xintercept = 0.8, color = "red") +
      geom_density(alpha = 0.2, fill = "#FF6666") +
      facet_wrap(~ Operator)
    print(p)
    dev.off()
  }
}

excavOperMaterialSummary <- ddply(theLoads[theLoads$LoadProp >= 0.5 & theLoads$LoadProp <= 1.5,], c("Operator", "Material"), summarise,
                                  n = length(Operator),
                                  stddev    = sd(LoadProp),
                                  mn        = mean(LoadProp),
                                  lt80      = sum(LoadProp < 0.8),
                                  lt80Rate  = lt80/n,
                                  lt90      = sum(LoadProp < 0.9 & LoadProp >= 0.8),
                                  lt90Rate  = lt90/n,
                                  gt110     = sum(LoadProp >= 1.1 & LoadProp < 1.2),
                                  gt110Rate = gt110/n,
                                  gt120     = sum(LoadProp >= 1.2),
                                  gt120Rate = gt120/n)
write.csv(excavOperMaterialSummary, file = "./output/ExcavOperatorMaterialLoadSizeSummary.csv", row.names = FALSE)

# Shovel Operator by Excav Type
message("\nPlotting 'Shovel oper by excav type' ...")
LoadSizeDat <- ddply(theLoads, c("Operator", "ExcavLP"), function(d) {if(nrow(d)>20) d else NULL})
excavTypes <- unique(theLoads$ExcavLP)
for(et in seq_along(excavTypes)){
  theExcavType <- excavTypes[et]
  if(length(unique(LoadSizeDat[LoadSizeDat$ExcavLP == theExcavType,]$Operator)) > 1) {
    message(paste0("\t", theExcavType))
    png(paste0("./output/LS_ExcavType_", theExcavType, ".png"), width=1800, height=900)
    p <- ggplot(LoadSizeDat[LoadSizeDat$LoadProp >= 0.5 & LoadSizeDat$LoadProp <= 1.5 & !is.na(LoadSizeDat$Operator) & LoadSizeDat$ExcavLP == theExcavType,], aes(x=LoadProp)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white") +
      ggtitle(paste0('Normalised "', theExcavType, '" Shovel Type Load Size Distribution by Shovel Operator (>20 Loads) 2018 YTD')) +
      xlab("Normalised load size") +
      ylab("Density") +
      geom_vline(xintercept = 1.0) +
      geom_vline(xintercept = 1.2, color = "red") +
      geom_vline(xintercept = 0.8, color = "red") +
      geom_density(alpha = 0.2, fill = "#FF6666") +
      facet_wrap(~ Operator)
    print(p)
    dev.off()
  }
}

excavOperTypeSummary <- ddply(theLoads[theLoads$LoadProp >= 0.5 & theLoads$LoadProp <= 1.5,], c("Operator", "ExcavLP"), summarise,
                              n = length(Operator),
                              stddev    = sd(LoadProp),
                              mn        = mean(LoadProp),
                              lt80      = sum(LoadProp < 0.8),
                              lt80Rate  = lt80/n,
                              lt90      = sum(LoadProp < 0.9 & LoadProp >= 0.8),
                              lt90Rate  = lt90/n,
                              gt110     = sum(LoadProp >= 1.1 & LoadProp < 1.2),
                              gt110Rate = gt110/n,
                              gt120     = sum(LoadProp >= 1.2),
                              gt120Rate = gt120/n)
write.csv(excavOperTypeSummary, file = "./output/ExcavOperatorExcavTypeLoadSizeSummary.csv", row.names = FALSE)

message("Done!")