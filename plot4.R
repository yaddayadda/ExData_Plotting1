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


#Function to plot the fourth figures (plot 4)
plot4<-function()
{
	# Read the household_power_consumption.txt data
	hpc<-readTable()

	# Start the PNG graphic device of 480 x 480 pixels with while background
	png(filename="plot4.png" , height=480 , width=480 , units="px", bg = "white")

	# Set the number of figures to be drawn
	par(mfrow = c(2, 2))

	# Plot all the four figures
	plot(x=hpc$DateTime,y=hpc$Global_active_power,type='l',ylab="Globel Active Power",xlab="")
	plot(x=hpc$DateTime,y=hpc$Voltage,type='l',ylab="Voltage",xlab="datetime")
	plot(x=hpc$DateTime,hpc$Sub_metering_1, xlab="", ylab="Energy Sub Metering",type="l",col="black")
	points(x=hpc$DateTime,y=hpc$Sub_metering_2, xlab="", ylab="",type="l",col="red")
	points(x=hpc$DateTime,y=hpc$Sub_metering_3, xlab="", ylab="",type="l",col="blue")
	legend("topright", lwd=1, col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
	plot(x=hpc$DateTime,y=hpc$Global_reactive_power,type='l',ylab="Global_reactive_power",xlab="datetime")

	# Close the device
	dev.off()
}

# Plot figure 4
plot4()

message(paste0("figure plot4.png written in directory : ",getwd()))