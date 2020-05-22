
## Reading data and altering it according to the need
setwd("~/Desktop/Prakash/R Prog/Project2")
library(ggplot2)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

graph <- NEI[,c(4,6)]
graph <- split(graph, graph$year)
graph <- lapply(graph, sum)
graph <- as.numeric(graph)
years <- c('1999', '2002', '2005', '2008')
m <- matrix(rep(0,4),nrow = 1, ncol = 4)
m[1,] <- graph
colnames(m) <- years
rm(graph,years)


#plot1.png
png(filename = "plot1.png", width = 480, height = 480, units = "px", bg = "white")
par(mfrow = c(1,1), mar = c(4,5,2.5,2))
barplot(m, col = 'wheat', xlab = "Year", ylab = "Total PM2.5 Emission", main = 

"Variation in PM2.5 Emission in US over the years")
dev.off()


#plot2.png
graph <- NEI[NEI$fips == "24510",]
graph <- aggregate(Emissions ~ year, graph, sum)

png(filename = "plot2.png", width = 480, height = 480, units = "px", bg = "white")
par(mfrow = c(1,1), mar = c(4,5,2.5,2))
barplot(height=graph$Emissions, col = 'wheat', xlab = "Year", ylab = "Total PM2.5 

Emission in Baltimore City", main = "Variation in PM2.5 Emission in Baltimore City over 

the years")
dev.off()



#Plot 3
graph <- NEI[NEI$fips == "24510",]
graph <- aggregate(Emissions ~ year + type, graph, sum) 

png(filename = "plot3.png", width = 600, height = 480, units = "px", bg = "white")
g <- ggplot(graph, aes(year, Emissions, col = type))
g <- g + geom_line() + xlab("Year") + ylab("Total PM2.5 Emission in Baltimore City") + 

ggtitle("Variation in PM2.5 Emission in Baltimore City over the years")
print(g)
dev.off()

#plot4
graph <- merge(NEI, SCC, by = "SCC")
index <- grep("coal", graph$Short.Name, ignore.case = TRUE)
graph <- graph[index,]
graph <- aggregate(Emissions ~ year, graph, sum) 

png(filename = "plot4.png", width = 480, height = 480, units = "px", bg = "white")
g <- ggplot(graph, aes(factor(year), Emissions))
g <- g + geom_bar(stat = "identity") + xlab("Year") + ylab("Total Emissions from Coal 

Combustion") + ggtitle("Variation in Coal Combustion Related Emissions over the years")
print(g)
dev.off()


#plot5
graph <- NEI[NEI$fips == "24510" & NEI$type == "ON-ROAD",]
graph <- aggregate(Emissions ~ year, graph, sum) 

png(filename = "plot5.png", width = 480, height = 480, units = "px", bg = "white")
g <- ggplot(graph, aes(factor(year), Emissions))
g <- g + geom_bar(stat = "identity") + xlab("Year") + ylab("Total Emissions from Motor 

Sources in Baltimore City") + ggtitle("Variation in Motor Source Related Emissions in 

Baltimore City over the years")
print(g)
dev.off()




#plot6

graph1 <- NEI[NEI$fips == "24510" & NEI$type == "ON-ROAD",]
graph1 <- aggregate(Emissions ~ year, graph1, sum) 
graph1 <- cbind(Counties = rep("Baltimore City", 4),graph1)
graph2 <- NEI[NEI$fips == "06037" & NEI$type == "ON-ROAD",]
graph2 <- aggregate(Emissions ~ year, graph2, sum) 
graph2 <- cbind(Counties = rep("Los Angeles", 4),graph2)
graph <- rbind(graph1, graph2)
rm(graph1, graph2)

png(filename = "plot6.png", width = 480, height = 480, units = "px", bg = "white")
g <- ggplot(graph, aes(year, Emissions, col = Counties))
g <- g + geom_line() + xlab("Year") + ylab("Total Emissions from Motor Sources") + 

ggtitle("rison of Motor Source Related Emissions between two Counties over the years")
print(g)
dev.off()



