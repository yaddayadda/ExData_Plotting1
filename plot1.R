# readTable reads the household power consumption table's for dates 2007-02-01 and 2007-02-02 
readTable<-function(fileName="household_power_consumption.txt")
{
	# Get the class of each column in the file (by reading a snippet of the data and using it as the class to read through data)

	tab5rows <- read.table(fileName, header = TRUE, sep = ";", dec = ".", na.strings = "?", nrows = 5)
	classes <- sapply(tab5rows, class)
	
	dates<- read.table(fileName , header=TRUE , sep=";" , na.strings="? " , as.is=TRUE , colClasses=c(NA,rep("NULL",ncol(tab5rows)-1)))
	newDates<-as.Date(dates$Date,format="%d/%m/%Y")

	# Get the index for the start of 2007-02-01 and the number of rows until 2007-02-02 from 2007-02-01
	# Assuming the dates are sorted in ascending order

	startRow=which.max(newDates >= "2007-02-01")
	stopRow=which.max(newDates > "2007-02-02")
	nosRowsToRead=stopRow-startRow

	# Read data by skipping lines until start of 2007-02-01 and until rows with 2007-02-02
	hpc <- read.table(fileName, header = FALSE, sep = ";", dec = ".", na.strings = "?", col.names=colnames(tab5rows), colClasses = classes,skip=startRow-1,nrows=nosRowsToRead)

	# Add Data and Time Column to the data set
	hpc$DateTime <- strptime(paste(hpc$Date , hpc$Time , sep=" ") , format="%d/%m/%Y %H:%M:%S")
	hpc

}

# Function to plot the histogram (plot 1)
plot1<-function()
{
	# Read the household_power_consumption.txt data
	hpc<-readTable()

	# Start the PNG graphic device of 480 x 480 pixels with while background
	png(filename="plot1.png" , height=480 , width=480 , units="px", bg = "white")

	# Set the number of figures to be drawn
	par(mfrow = c(1, 1))
	
	# Plot the histogram
	hist(hpc$Global_active_power,xlab="Globel Active Power (kilowatts)",ylab="Frequency",main="Global Active Power",col="red")

	# Close the device
	dev.off()
}

plot1()
message(paste0("figure plot1.png written in directory : ",getwd()))



