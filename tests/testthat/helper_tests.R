source("../../R/util.R")
source("../../R/vista.R")

source("../../R/locations.R")
distanceMatrixFile <- "../expected/1.setup/locDistanceMatrix.rds"
distanceMatrixIndexFile <- "../expected/1.setup/locDistanceMatrixIndex.rds"
sa1AttributedFile <- "../expected/1.setup/locSa1Aattributed.rds"
sa1CentroidsFile <- "../expected/1.setup/locSa1Centroids.rds"
addressesFile <- "../expected/1.setup/locAddresses.rds"
distancesFile <- "../expected/1.setup/expectedDistances.rds"
loadLocationsData(distanceMatrixFile, distanceMatrixIndexFile,
                  sa1AttributedFile, sa1CentroidsFile, addressesFile, distancesFile)

