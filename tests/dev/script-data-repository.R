library(ospsuite)

# Load the obs data from a pkml file
obsData <- loadDataRepositoryFromPKML("tests/data/obs_data.pkml")

# get the meta data properties as a list
metaData <- obsData$metaData

columns <- obsData$columns


