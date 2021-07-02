library(ospsuite)

# Load the obs data from a pkml file
obsData <- loadDataRepositoryFromPKML("tests/data/obs_data.pkml")

# Create an empty data set
dataSet <- DataSet$new(obsData)

# get the values
xValues <- dataSet$xValues

xUnit <- dataSet$xUnit


emptyDataSet <- DataSet$new()
#this will be Time in min
emptyDataSet$xValues <- c(1, 2, 3, 4, 5)
emptyDataSet$xDimension <- ospDimensions$Amount

