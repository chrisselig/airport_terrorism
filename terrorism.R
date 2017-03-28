#National Consortium for the Study of Terrorism and Responses to Terrorism
#START: A Center of Excellence of the U.S. Department of Homeland Security 
#University of Maryland, College Park, MD 20740, USA 301.405.6600
#Accessed March 1, 2017
#http://www.start.umd.edu/gtd/
02

Sys.setenv(R_ZIPCMD= "C:/Rtools/bin/zip")

setwd("D:/Personal/R coding/RScripts")

require(tidyverse); require(openxlsx); require(leaflet); require(ggplot2);require(stringr)
require(lubridate)

file1 <- "D:/Personal/R coding/Data/Terrorism Data/globalterrorismdb_0616dist.xlsx"
df1 <- read.xlsx(file1,sheet = 1, na.strings = c(".","","-99","-9","-9.0"))
terrorraw <- df1[,c(2:4,6:7,9,11:15,19,27:28,30,32,
                    34,36,38:40,42,59,68:69,72:73,85,
                    87,89,91,93,95,97,99:107,109:127)]

#further reduce variables
terrorraw <- terrorraw[,c(1:3,6:11,13:15,18,22:23,26,28,37,39:40,42,44)]

##################################################################
###################### Data Wrangling ############################
##################################################################

#Clean up values that are unknown in the dataset, and make them NA
terror_clean <- terrorraw %>%
                    mutate(nperps = replace(nperps,nperps ==-99,NA)) %>%
                    mutate(nperps = replace(nperps,nperps ==-9,NA)) %>%
                    as_tibble()

#Rename variables
names(terror_clean) <- c("year",'month','day','incidentCountry','incidentRegion',
                         'incidentProvState','incidentCity', 'latitude', 'longitude','success',
                         'suicide', 'attackType', 'targetType', 'targetNationality',
                         'groupName','numPerps', 'weaponType', 'numDeaths', 'numPerpDeaths',
                         'numWounded','numPerpWounded','propValue')

terror_clean <- terror_clean %>%
    mutate(numDeaths = ifelse(is.na(numDeaths),0,numDeaths)) %>%
    mutate(numPerpDeaths = ifelse(is.na(numPerpDeaths),0,numPerpDeaths)) %>%
    mutate(numWounded = ifelse(is.na(numWounded),0,numWounded)) %>%
    mutate(numPerpWounded = ifelse(is.na(numPerpWounded),0,numPerpWounded)) %>%
    mutate(month = ifelse(month==0,1,month)) %>%   #translate unknown month to Jan
    mutate(day = ifelse(day ==0,1,day)) %>%  #translate unknown day to the 1st
    mutate(incidentDate = make_date(year,month,day)) %>%
    mutate(weekDay = wday(incidentDate,label = TRUE)) %>%
    mutate(incidentMonth = month(incidentDate, label = TRUE))


#Export the file to be used for the shinyapp
#write.xlsx(terror_clean, file = "D:/Personal/R coding/ShinyApps/Terrorism/Terrorism_Analysis/terrordata.xlsx",
#          colNames = TRUE)

##################################################################
################## Exploratory Data Analysis #####################
##################################################################


airport_tbl <- terror_clean %>%
                filter(targetType == "Airports & Aircraft") %>%
                mutate(
                    Casualties = (numDeaths - numPerpDeaths) + (numWounded - numPerpWounded))

airport_stats <- airport_tbl %>%
                    group_by(attackType) %>%
                    summarise(
                        Total = n(),
                        Successful = sum(success),
                        'Success%' = round((Successful/Total)*100,1)
                    ) %>%
                    arrange(desc(Total))


#Bar chart of attack type count
airport_stats %>%
    ggplot(mapping = aes(x = reorder(attackType,Total), y = Total)) +
    geom_bar(stat = "identity", position = "identity", color = "black", fill = "blue") +
    coord_flip()+
    labs(x = "Attack Type", y = "", 
         title = "Attempted Attacks on Airports and Aircraft by Type")+
    geom_text(aes(label = Total),hjust = 1.2, color = "white") +
    theme_classic()

#Success rate by attack type
airport_stats %>%
    ggplot(mapping = aes(x = reorder(attackType,`Success%`), y = `Success%`)) +
    geom_bar(stat = "identity", position = "identity", color = "black", fill = "blue") +
    coord_flip() +
    labs(x = "Attack Type", y = "", 
         title = "Successful Attacks on Airports and Aircraft by Type")+
    geom_text(aes(label = `Success%`),hjust = 1.2, color = "white")+
    theme_classic()


#Create Dataframe that holds lat/long coordinates
latlongdf <- airport_tbl[,8:9]
latlongdf <- latlongdf[complete.cases(latlongdf),]

#map the locations of attacks
leaflet() %>%
    addTiles() %>%
    addMarkers(lng = latlongdf$longitude, lat = latlongdf$latitude,
               clusterOptions = markerClusterOptions())

#What weapon type is the most common
weapon_tbl <- airport_tbl %>%
    group_by(weaponType) %>%
    summarise(
        `Total # of Attacks` = n(),
        `Total # Successful Attacks` = sum(success),
        `Total # Wounded` = round(sum(numWounded - numPerpWounded),0),
        `Total # Killed` = round(sum(numDeaths - numPerpDeaths),0),
        `Total Casualties` = round(sum(Casualties),0)) %>%
    arrange(desc(`Total # of Attacks`))

#Total Attacks by weapon type and year
weaponTypeYear_tbl <- airport_tbl %>%
    group_by(weaponType,year) %>%
    summarise(
        Total = n())
weaponTypeYear_tbl$weaponType <- as.factor(weaponTypeYear_tbl$weaponType)
names(weaponTypeYear_tbl) <- c("Weapon Type","Year", "Total")

#Plot total attacks per year
weaponTypeYear_tbl %>%
    ggplot(mapping = aes(x = Year, y = Total)) +
    geom_line(aes(colour = `Weapon Type`)) + 
    ggtitle('Number of Attacks on Airports Each Year by Type')+
    labs(x = "", y = "Total Attacks by Weapon") +
    theme_classic()

#Casualities by weapon type
casualities_tbl <- airport_tbl %>%
    group_by(weaponType) %>%
    summarise(
        Total = round(sum(Casualties),0)) %>%
    arrange(desc(Total))

#Casualities PER YEAR by weapon type
casualtiesYear_tbl <- airport_tbl %>%
    group_by(weaponType, year) %>%
    summarise(
        Total = round(sum(Casualties),0)) %>%
    arrange(desc(Total))
casualtiesYear_tbl$weaponType <- as.factor(casualtiesYear_tbl$weaponType)
names(casualtiesYear_tbl) <- c("Weapon Type","Year", "Total")

#Plot total attacks per year
casualtiesYear_tbl %>%
    ggplot(mapping = aes(x = Year, y = Total)) +
    geom_line() + 
    ggtitle('Total Casualties by Year and Weapon Type')+
    labs(x = "", y = "Total Casualties by Weapon Type") +
    theme_classic()+
    facet_wrap(~`Weapon Type`)

#Which countries airports are targeted the most
targetNationality_tbl <- airport_tbl %>%
    group_by(targetNationality) %>%
    summarise(
        Total = n()) %>%
    arrange(desc(Total))

#Which group claimed responsibility the most
terrorGroup_tbl <- airport_tbl %>%
    group_by(groupName) %>%
    summarise(
        Total = n()) %>%
    arrange(desc(Total))

#Day of the week do most attacks appear on? Monday and friday are busiest for business travellers
weekday_plot <- airport_tbl %>%
    group_by(weekDay)%>%
    ggplot(aes(x = weekDay)) + 
    geom_bar()+
    labs(x = "Weekday", y = "") +
    theme_classic()

#Total Attacks by year
attacksYear_tbl <- airport_tbl %>%
    group_by(year) %>%
    summarise(
        Total = n(),
        Successful = sum(success),
        `Success%` = (Successful/Total))

#Plot total attacks per year
attacksYear_tbl %>%
    ggplot(mapping = aes(x = year, y = Total)) +
    geom_line() + 
    geom_point(size = 2, shape = 22, fill = "darkred", color = "black") +
    ggtitle('Number of Attacks on Airports Each Year')+
    labs(x = "", y = "Total Attacks") +
    scale_x_continuous(limits=c(1970, 2018))+
    geom_smooth(method = 'lm',aes(colour="Linear Line"),se = FALSE)+
    geom_smooth(aes(colour= "LOESS Curve"))+
    scale_colour_manual(name="Legend", values=c("orange", "darkgreen"))+
    theme_classic()

#1980 - 1985 Analysis
years1 <- c(1980:1985)
eighties_tbl <- airport_tbl%>%
    subset(year %in% years1) %>%
    group_by(year)

eightiesgrp_tbl <- eighties_tbl %>%
    group_by(groupName) %>%
    summarise(
        Total = n(),
        Successful = sum(success),
        `Successful%` = (Successful/Total)*100) %>%
    arrange(desc(Total))

##################################################################
###################### Data Analysis #############################
##################################################################


#Question, are total terrorist attacks at airports trending down?
#Looks at first 10 years of data vs last 10 years
#Ho muf10 = mul10
#Ha muf10 < mul10
x <- attacksYear_tbl
group1 <- c(1970:1980)
group2 <- c(2005:2015)
x1 <- subset(x,year %in% group1)
x2 <- subset(x, year %in% group2)

tt <- t.test(x1$Total, x2$Total, lower.tail = TRUE, paired = FALSE, 
             var.equal = FALSE, data = x)
tt$conf.int
tt$p.value
#Confidence Interval:  Contains 0
#P-value 0.17  Cannot reject the null hypothesis

#Question, with improved technology, we should see successful terrorist attacks at airports trending down?
#Looks at first 10 years of data vs last 10 years
#Ho mufs10 = muls10
#Ha mufs10 < muls10
ts <- t.test(x1$Successful,x2$Successful, lower.tail = TRUE, paired = FALSE,
             var.equal = FALSE, data = x)
ts$conf.int
ts$p.value
#Conf into contains 0
#p value = 0.11, so cannot reject the null hypothesis.  On a global scale, technology didn't decrease
#the amount of successful attacks on airports

##################################################################
###################### Weapon Analysis ###########################
##################################################################


#What if we repeat the previous analysis, but only include attacks that involve explosives, because 
#IED detection has been more sophisticated?
#Subset data on explosives
explosives_tbl <- airport_tbl %>%
                    filter(str_detect(weaponType,"^[Ee]xplosiv")) %>%
                    group_by(year) %>%
                    summarise(
                        Total = n(),
                        Successful = sum(success),
                        `Successful%` = (Successful/Total)*100)

#Explosives chart
explosives_tbl %>%
    ggplot(mapping = aes(x = year, y = Total)) +
    geom_line() + 
    geom_point(size = 2, shape = 22, fill = "darkred", color = "black") +
    ggtitle('Total Attacks with Explosives')+
    labs(x = "", y = "Total Attacks") +
    scale_x_continuous(limits=c(1970, 2018))+
    geom_smooth(method = 'lm',aes(colour="Linear Line"),se = FALSE)+
    geom_smooth(aes(colour= "LOESS Curve"))+
    scale_colour_manual(name="Legend", values=c("orange", "darkgreen"))+
    theme_classic()

x2 <- explosives_tbl
group3 <- c(1970:1980)
group4 <- c(2005:2015)
x3 <- subset(x2,year %in% group1)
x4 <- subset(x2, year %in% group2)

te <- t.test(x3$Total, x4$Total, lower.tail = TRUE, paired = FALSE, 
             var.equal = FALSE, data = x)
te$conf.int
te$p.value
#Conf interval contains 0 and p-value is 0.6.  Cannot reject the null hypothesis

