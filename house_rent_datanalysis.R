

##3.0 DATA IMPORT

#3.1 Library Import

library(tidyverse)
library(skimr)
library(janitor)
library(plotrix)
library(wordcloud2)
library(DescTools)


#3.2 Dataset Import
house_rent_data <- read_csv('./House_Rent_Dataset.csv')



##4.0 DATA CLEANING

#4.1 Detecting and Remvoing Duplicate Records

#before starting the cleaning process, if there are any duplicated records, they are removed using the distinct() function 

#Detecting and identifying duplicate records
house_rent_data %>% 
  duplicated()

#Removing duplicate records
house_rent_data <- house_rent_data %>% 
  distinct()

#4.2 Dropping rows containing missing values
house_rent_data <- drop_na(house_rent_data)

#4.3 Cleaning column names
house_rent_data <- house_rent_data %>% 
  clean_names()
colnames(house_rent_data)






##5.0 DATA PRE-PROCESSING

#5.1 Chaning Column Data Type


#Chaning the data type of 'posted_on' column

house_rent_data <- house_rent_data %>% 
  mutate(posted_on=as.Date(posted_on, '%m/%d/%Y'))

class(house_rent_data$posted_on)



#5.2 Renaming Columns


#posted_on column name is renamed as posted_date as posted_date makes more sense and makes it easier to do analysis later on
house_rent_data <- house_rent_data %>% 
  rename(posted_date=posted_on)




#5.3Adding Columns

#Issues that were found during the previous steps made it essential to create two additional columns named num_of_floors, and floor_level which are derived from the floor column
#This is done using the mutate function

house_rent_data <- house_rent_data %>% 
  mutate(num_of_floors = ifelse(grepl('out of', floor), str_sub(floor, nchar(floor)-1, nchar(floor)), floor))

house_rent_data <- house_rent_data %>% 
  mutate(num_of_floors = ifelse(num_of_floors=="Ground", 1, num_of_floors))

house_rent_data <- house_rent_data %>% 
  mutate(floor_level = ifelse(grepl('out of', floor), str_sub(floor, 0, nchar(floor)-9), floor))



#After the conversion, it necessary to remove any unwanted whitespaces in the values in num_of_floor and floor_level variables

house_rent_data$num_of_floors <- trimws(house_rent_data$num_of_floors, which = c("both"))


house_rent_data$floor_level <- trimws(house_rent_data$floor_level, which = c("both"))


#One issue that was identified is the data type of num_of_floors column which was in character data type. It would be more approrpriate for it to be in numeric data type. Thus, it was converted to numeric data type 


house_rent_data <- house_rent_data %>% 
  mutate(num_of_floors=as.numeric(num_of_floors))





#Creating the floor_level_type column to identify whether that floor level is upper or lower level. Those in the upper half are considered upper level houses and those in the lower half are considered as lower level houses

house_rent_data <- house_rent_data %>% 
  mutate(floor_level_type= ifelse(floor_level<num_of_floors%/%2, 'lower', 'upper'))
house_rent_data$floor_level_type



#creating the month column
house_rent_data <- house_rent_data %>% 
  mutate(posted_month = months(posted_date))

#creating the weekday column
house_rent_data <- house_rent_data %>% 
  mutate(posted_weekday = weekdays(posted_date))

colnames(house_rent_data)





##6.0 DATA EXPLORATION


#6.1 Getting an overview of the data set
skim_without_charts(house_rent_data)

#6.2 Determining the structure of the data set
#dim() function to identify the dimension of dataset
dim(house_rent_data)

#6.3 Determining column names
colnames(house_rent_data)


#6.4 Determining data type of columns
str(house_rent_data)


#6.5 Identitfying the Levels of Categorical Variables

#identifying the categorical levels of bhk
factor(house_rent_data$bhk)

#identifying the categorical levels of area type
factor(house_rent_data$area_type)

#identifying the categorical levels of city
factor(house_rent_data$city)

#identifying the categorical levels of furnishing status
factor(house_rent_data$furnishing_status)

#identifying the categorical levels of tenant preferred
factor(house_rent_data$tenant_preferred)

#identifying the categorical levels of point of contact
factor(house_rent_data$point_of_contact)

#identifying the categorical levels of floor level type
factor(house_rent_data$floor_level_type)


#6.6 Viewing and Styding the Values Present in the data set

#to view the first few rows of the dataset. This helps to get an overview of values in every column of the dataset
head(house_rent_data)

#viewing the last few rows of the dataset
tail(house_rent_data)


#viewing particular set of rows
house_rent_data[2000:2200,]


#Viewing values of a particular column
house_rent_data %>% 
  select(floor)





#6.7 Basic General Analysis


#highest rental price
max(house_rent_data$rent)

#average rental price
mean(house_rent_data$rent)

#minimum rental housing size
min(house_rent_data$size)

#average number of bathrooms
mean(house_rent_data$bathroom)

#most frequent bhk value
Mode(house_rent_data$bhk)







##7.0 QUESTIONS & ANALYSIS




#7.1. Question 1: Which city has the highest number of rentals?

#7.1.1 Analysis 1.1: Relationship between city and number of rentals
#total number of rentals per city
num_of_rentals_per_city <- house_rent_data %>% 
  group_by(city) %>% 
  summarise(num_of_rental=n())
num_of_rentals_per_city

#bar chart for the relationship between the city and number of rentals per city
ggplot(data=num_of_rentals_per_city, aes(x=city, y=num_of_rental, fill=city)) +
  geom_bar(stat="identity", color="blue") +
  geom_text(aes(label=num_of_rental)) +
  labs(title="Number of Rentals Per City", x="City", y="Number of Rentals", fill="City") +
  theme(plot.title.position = "panel")

#Mumbai has the highest number of rentals which is around 972 rentals


# 7.1.2 Analysis 1.2 Number of rentals per area locality in Mumbai

rental_per_locality <- house_rent_data %>% 
  filter(city=="Mumbai") %>% 
  group_by(area_locality) %>% 
  summarise(num_of_rentals = n()) %>% 
  arrange(-num_of_rentals)

wordcloud2(rental_per_locality, size=0.5, color=rep_len( c("blue","red"), nrow(rental_per_locality) ) )


#Bandra West in Mumbai has the highest number of rentals. Tenants prefer Bandra West in Mumbai compared to other area localities


#7.1.3 Analysis 1.3: Number of rentals for each area type in Mumbai

#Number of rentals for each area type in Mumbai
rental_per_areatype <- house_rent_data %>% 
  filter(city=="Mumbai") %>% 
  group_by(area_type) %>% 
  summarise(num_of_rentals = n()) 
rental_per_areatype

#pie chart representing number of rentals per area type
pie(rental_per_areatype$num_of_rentals, rental_per_areatype$area_type, radius=1.0, col=c("red", "blue", "green"), main="Number of Rentals per Area Type in Mumbai")

#Houses with carpet area has the highest number of rentals in Mumbai


#7.1.4 Analysis 1.4: Among the rentals made in Mumbai, who has made more rentals,  family tenants or bachelor tenants? 

house_rent_data %>% 
  filter(city=="Mumbai" & (tenant_preferred=="Bachelors" | tenant_preferred=="Family")) %>% 
  group_by(tenant_preferred) %>% 
  summarise(num_of_rentals=n()) %>% 
  ggplot(aes(x=tenant_preferred, y=num_of_rentals, fill=tenant_preferred)) +
  geom_bar(stat="identity", color="black") +
  geom_text(aes(label=num_of_rentals)) +
  labs(title="Number of Rentals Per Tenants Type in Mumbai", x="Tenant Type", y="Number of Rentals", fill="Tenant Type")


#Among the rentals made in Mumbai, family tenants has made more rentals than bachelors


#7.1.5 Analysis 1.5: In Mumbai, most tenants prefer upper level floors or lower level floors?

house_rent_data %>% 
  filter(city=="Mumbai") %>% 
  ggplot(aes(x=floor_level_type, fill=floor_level_type)) +
  geom_bar() +
  labs(title="Upper Vs Lower Level Floors in Mumbai", subtitle="Number of Rentals Per Floor Level Type in Mumbai", x="Floor Level", y="Number of Rentals", fill="Floor Level")

#Most of the rentals made in Mumbai were of upper level floors which is more than 600 rentals


#7.1.6 Analysis 1.6: Among the rentals made in Mumbai, how many were made for furnished houses, semi-furnished houses, and unfurnished houses respectively?

house_rent_data %>% 
  filter(city=="Mumbai") %>% 
  ggplot(aes(x=furnishing_status, fill=furnishing_status)) + 
  geom_bar(color="black") +
  labs(title="Number of Rentals per Furnshing Status in Mumbai", x="Furnishing Status", y="Number of Rentals", fill="Furnishing Status")


#Among the rentals made in Mumbai, Semi-Furnished houses have teh highest number of rentals


#7.1.7 Analysis 1.7: In Mumbai, how number of rentals differs among different bhk values?

#number of rentals per bhk in Mumbai
rental_per_bhk <-house_rent_data %>% 
  filter(city=="Mumbai") %>% 
  group_by(bhk) %>% 
  summarise(num_of_rentals = n())
rental_per_bhk

ggplot(data=rental_per_bhk, aes(x=bhk, y=num_of_rentals)) +
  geom_bar(stat="identity", fill="red", color="blue") +
  geom_text(aes(label=num_of_rentals), nudge_y=8) +
  scale_fill_gradient(low="gray", high="purple") +
  labs(title="Number of Rentals per BHK Value in Mumbai", x="BHK", y="Number of Rentals", fill="BHK")


#Most of the rentals made in Mumbai were of 2 bedroom houses




#7.1.8 Analysis 1.8:  Total Number of rentals per weekday in Mumbai

#num of rental per weekday in Mumbai
num_rental_weekday <- house_rent_data %>% 
  filter(city=="Mumbai") %>% 
  group_by(posted_weekday) %>% 
  summarise(num_of_rentals = n())
num_rental_weekday

ggplot(num_rental_weekday, aes(x=reorder(posted_weekday, num_of_rentals), y=num_of_rentals)) +
  geom_point() +
  geom_text(aes(label=num_of_rentals), nudge_y = 8) +
  geom_segment(aes(x=posted_weekday,xend=posted_weekday,y=0,yend=num_of_rentals))+
  theme(axis.text.x=element_text(angle=90)) +
  labs(title="Total Number of Rentals per Weekday in Mumbai", x="Weekday", y="Number of Rentals")

#Among the rentals made in Mumbai, most of them were made on Thursdays



#7.1.9 Analysis 1.9: Among the rentals in Mumbai, whom do most tenants contact when renting a house?

house_rent_data %>% 
  filter(city=="Mumbai") %>% 
  ggplot(aes(x=point_of_contact, fill=point_of_contact)) +
  geom_bar(color="black") +
  labs(title="Total Number of Rentals per Point of Contact in Mumbai", x="Point of Contact", y="Number of Rentals", fill="Point of Contact")


#Most of the tenants in Mumbai has contacted agents when they rented the houses


#7.1.10 Analysis 1.10: In Mumbai, how does rental price in Mumbai change over time for each furnishing status?

house_rent_data %>% 
  filter(city=="Mumbai") %>% 
  ggplot(aes(x=posted_date, y=rent, color=furnishing_status, group=furnishing_status)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(se=FALSE, color="yellow")+
  facet_wrap(~furnishing_status) +
  labs(title="Rental Price Change for each Furnishing Status Over Time in Mumbai", x="Time", y="Rental Price", color="Furnishing Status") +
  theme(legend.position="bottom", plot.title.position="plot")




#Conclusion for Question 1
#conclusion: Mumbai has the highest number of rentals compared to other cities. 
#The area type with the highest number of rentals in Mumbai is Bandra West.
#Among the rentals in Mumbai, more rentals were made by family tenants compared to bachelor tenants
#In conclusion, most of tenants in Mumbai prefer houses which are semi furnished, those with carpet area, and with 2 bedrooms
#Most of the rentals in Mumbai were made in Thursday and in the month of June
#In Mumbai, Most of them prefer to contact agents when they rent a house






#7.2 Question 2. Whom do most tenants contact when they rent a house and how it affects their rental decision?

#7.2.1 Analysis 2.1: Relationship between number of rentals and point of contact

#number of rentals per contact
num_of_rentals_per_contact <- house_rent_data %>% 
  group_by(point_of_contact) %>% 
  summarise(num_of_rental=n())
num_of_rentals_per_contact

ggplot(data=num_of_rentals_per_contact, aes(x=point_of_contact, y=num_of_rental)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=num_of_rental)) +
  labs(title="Number of Rentals Per Point of Contact", x="Point of Contact", y="Number of Rentals", fill="Point of Contact") +
  theme(plot.title.position = "panel")


#Most of the tenants contact owners when they rent a house


#7.2.2 Analysis 2.2 Which city has the highest recorded number of rentals made through house owners?


#number of rentals made through owners per city
owner_rental_city <- house_rent_data %>% 
  filter(point_of_contact=="Contact Owner") %>%
  group_by(city) %>% 
  summarise(num_of_rentals=n())
owner_rental_city


ggplot(owner_rental_city,aes(x=num_of_rentals,y=reorder(city,num_of_rentals)))+######reorder in descending
  geom_point(size=5)+
  geom_text(aes(label=num_of_rentals), nudge_x = 30)+
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(linetype="dashed",color="blue"))+
  annotate("rect", xmin=750, xmax=800, ymin="Chennai", ymax="Hyderabad", alpha=.2, fill="red") +
  labs(title="Number of Rentals Made Through Owners Per City", x="City", y="Number of Rentals")



#Chennai and Hyderabad have the highest record number of rentals made through house owners which is 757 rentals.


#7.2.3 Analysis 2.3 Which furnishing status is the most common among rentals made through house owners

house_rent_data %>% 
  filter(point_of_contact == "Contact Owner") %>% 
  group_by(furnishing_status) %>% 
  summarise(num_of_rentals=n()) %>% 
  ggplot(aes(x=furnishing_status, y=num_of_rentals, fill=furnishing_status)) +
  geom_bar(stat="identity", color="black") +
  geom_text(aes(label=num_of_rentals)) +
  labs(title="Number of Rentals Made Through Owners Per Furnishing Status", x="Furnishing Status", y="Number of Rentals", fill="Furnishing Status")


#Most of the houses rented through houses owners were semi-furnished ones



#7.2.4 Analysis 2.4 Among those contacted owners how many are bachelor tenants and how many are family tenants?


bachelor_family_owners <- house_rent_data %>% 
  filter(point_of_contact=="Contact Owner" & (tenant_preferred=="Bachelors" | tenant_preferred=="Family")) %>% 
  group_by(tenant_preferred) %>% 
  summarise(num_of_rentals=n())
bachelor_family_owners

ggplot(bachelor_family_owners, aes(x=tenant_preferred, y=num_of_rentals, fill=tenant_preferred)) +
  geom_bar(stat="identity", color="black") +
  geom_text(aes(label=num_of_rentals), nudge_y = 8) +
  labs(title="Number of Rentals made Through Owners per Tenant Type", x="Tenant Type", y="Number of Rentals", fill="Tenant Type")


#Among the rentals made through agents, most of them were made by bachelor tenants


#7.2.5 Analysis 2.5 Average housing size among those rented through owners

owners_rent_data <- house_rent_data %>% 
  filter(point_of_contact=="Contact Owner")

mean(owners_rent_data$size)
#873.8335


#7.2.6 Analysis 2.6 Most common area type among houses rented through house owners?

owner_rental_areatype <- house_rent_data %>% 
  filter(point_of_contact=="Contact Owner") %>% 
  group_by(area_type) %>% 
  summarise(num_of_rental=n())


owner_rental_areatype$fraction = owner_rental_areatype$num_of_rental / sum(owner_rental_areatype$num_of_rental)

# Compute the cumulative percentages (top of each rectangle)
owner_rental_areatype$ymax = cumsum(owner_rental_areatype$fraction)

# Compute the bottom of each rectangle
owner_rental_areatype$ymin = c(0, head(owner_rental_areatype$ymax, n=-1))

# Compute label position
owner_rental_areatype$labelPosition <- (owner_rental_areatype$ymax + owner_rental_areatype$ymin) / 2

# Compute a good label
owner_rental_areatype$label <- paste0(owner_rental_areatype$area_type, "\n value: ", owner_rental_areatype$num_of_rental)


ggplot(owner_rental_areatype, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=area_type)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=4) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title="Number of Rentals Made Through House Owners per Area Type")



#Super Area type is the most common area type among houses rented through house owners




#7.2.7 Analysis 2.7 Which weekday has the most number of rentals made through house owners?

weekday_owner_rentals <- house_rent_data %>% 
  filter(point_of_contact=="Contact Owner") %>% 
  group_by(posted_weekday) %>% 
  summarise(num_of_rentals=n())
weekday_owner_rentals

ggplot(weekday_owner_rentals, aes(x=reorder(posted_weekday, num_of_rentals), y=num_of_rentals, color=posted_weekday)) +
  geom_point()+
  geom_text(aes(label=num_of_rentals), nudge_y = 13) +
  geom_segment(aes(x=posted_weekday,xend=posted_weekday,y=0,yend=num_of_rentals))+
  labs(title="Total Number of House Rentals through Owners per Weekday", x="Weekday", y="Number of Rentals", color="Weekday")

#Friday has the most number of rentals made through house owners


#7.2.8 Analysis 2.8  Among the rentals made through house owners what is the most common bhk value?

house_rent_data %>% 
  filter(point_of_contact=="Contact Owner") %>% 
  ggplot(aes(x=bhk)) +
  geom_bar(fill="red", color="black")+
  labs(title="Number of Rentals through Owners Per BHK", x="BHK", y="Number of Rentals")

#Houses with 2 bedrooms are the most common ones in the rental made through house owners


#7.2.9 Analysis 2.9 On average, Who pays more rent when houses are rented through house owners?

house_rent_data %>% 
  filter(point_of_contact=="Contact Owner" & (tenant_preferred=="Bachelors" | tenant_preferred=="Family")) %>% 
  group_by(tenant_preferred) %>% 
  summarise(average_rental_price=mean(rent)) %>% 
  ggplot(aes(x=tenant_preferred, y=average_rental_price, fill=tenant_preferred)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=average_rental_price), nudge_y = 8) +
  labs(title="Average rental price per Tenant Type", x="Tenant Type", y="Average Rental Price", fill="Tenant Type")

#Among those rentals made through house owners bachelor tenants pay slightly higher rental


#7.2.10 Analysis 2.10 Which area type has the most number of rentals made through house owners

#Number of rentals made through owenrs per area locality
owner_rental_per_locality <- house_rent_data %>% 
  filter(point_of_contact=="Contact Owner") %>% 
  group_by(area_locality) %>% 
  summarise(num_of_rentals = n()) %>% 
  arrange(-num_of_rentals)
owner_rental_per_locality

wordcloud2(owner_rental_per_locality, size=0.2, color=rep_len( c("red","blue"), nrow(owner_rental_per_locality) ) )

#Bandra West has the highest number of rentals made through house owners









#7.3 Question 3. What is the most common area type among the rentals?

#7.3.1 Analysis 3.1: Relationship between area type and number of rentals

area_type_rentals <- house_rent_data %>% 
  group_by(area_type) %>% 
  summarise(num_of_rental = n())
area_type_rentals


ggplot(data=area_type_rentals, aes(x=area_type, y=num_of_rental, fill=area_type)) +
  geom_bar(stat="identity", color="black") +
  geom_text(aes(label=num_of_rental)) +
  labs(title="Number of Rentals per Area Type", x="Area Type", y="Number of Rentals", fill="Area Type")

#Super Area type is the most common area type among tenants


#7.3.2 Analysis 3.2:  Number of super-area typed house renals per city

super_rentals_per_city <- house_rent_data %>% 
  filter(area_type=="Super Area") %>% 
  group_by(city) %>% 
  summarise(num_of_rentals=n())
super_rentals_per_city

pie3D(super_rentals_per_city$num_of_rentals, radius=1.5, labels=super_rentals_per_city$city, border="black", explode=0.2)


#Hyderabad has the most number of rentals involving houses with super area type


#7.3.3 Analysis 3.3: Among bachelor and family tenants, who rents more super area typed houses?
house_rent_data %>% 
  filter(area_type=="Super Area" & (tenant_preferred=="Bachelors" | tenant_preferred=="Family")) %>% 
  ggplot(aes(x=tenant_preferred, fill=tenant_preferred)) +
  geom_bar() +
  labs(title="Bachelors Vs Family Tenants for Super Area Houses", subtitle="Number of Rentals for Bachelors and Family Tenants", x="Tenant Type", y="Number of Rentals", fill="Tenant Type")

#Family tenants rents more super area typed houses than bachelor tenants



#7.3.4 Analysis 3.4: What is the average rental price of houses with super area?

super_area_data <- house_rent_data %>% 
  filter(area_type=="Super Area")

mean(super_area_data$rent)


#7.3.5 Analysis 3.5: Among family and bachelor tenants, who pays more rent for super area typed houses?
house_rent_data %>% 
  filter(area_type=="Super Area" & (tenant_preferred=="Bachelors" | tenant_preferred=="Family")) %>% 
  group_by(tenant_preferred) %>% 
  summarise(average_rental_price=mean(rent)) %>% 
  ggplot(aes(x=tenant_preferred, y=average_rental_price, fill=tenant_preferred)) +
  geom_bar(stat="identity", color="black") +
  geom_text(aes(label=average_rental_price)) +
  labs(title="Average Rental Price per Tenant Type", x="Tenant Type", y="Average Rental Price", fill="Tenant Type")

#Family tenants pay more rent for super area typed houses on average



#7.3.6 Analysis 3.6: Whom do most tenants contact when they rent super area typed houses?

super_tenant_contact <- house_rent_data %>% 
  filter(area_type=="Super Area") %>% 
  group_by(point_of_contact) %>% 
  summarise(num_of_rentals=n())
super_tenant_contact

ggplot(super_tenant_contact, aes(x=point_of_contact, y=num_of_rentals)) +
  geom_point() +
  geom_text(aes(label=num_of_rentals), nudge_y = 30) +
  geom_segment(aes(x=point_of_contact,xend=point_of_contact,y=0,yend=num_of_rentals))+
  labs(title="Total Number of Super Area House Rentals per Point Of Contact", x="Point of Contact", y="Number of Rentals")

#Most of them contact house owners when they rent super area typed houses



#7.3.7 Analysis 3.7: What is the most common number of bathrooms for super area typed houses?
house_rent_data %>% 
  filter(area_type=="Super Area") %>% 
  ggplot(aes(x=bathroom)) +
  geom_bar(fill="red", color="black") +
  labs(title="Number of Super Area Typed houses per Bathroom Count", x="Number of Bathrooms", y="Number of Rentals")


#Most super area typed houses have 2 bathrooms in general


#7.3.8 Analysis 3.8: Furnishing status of super area typed houses

furnishing_rental_areatype <- house_rent_data %>% 
  filter(area_type=="Super Area") %>% 
  group_by(furnishing_status) %>% 
  summarise(num_of_rental=n())


furnishing_rental_areatype$fraction = furnishing_rental_areatype$num_of_rental / sum(furnishing_rental_areatype$num_of_rental)

# Compute the cumulative percentages (top of each rectangle)
furnishing_rental_areatype$ymax = cumsum(furnishing_rental_areatype$fraction)

# Compute the bottom of each rectangle
furnishing_rental_areatype$ymin = c(0, head(furnishing_rental_areatype$ymax, n=-1))

# Compute label position
furnishing_rental_areatype$labelPosition <- (furnishing_rental_areatype$ymax + furnishing_rental_areatype$ymin) / 2

# Compute a good label
furnishing_rental_areatype$label <- paste0(furnishing_rental_areatype$furnishing_status, "\n value: ", furnishing_rental_areatype$num_of_rental)


ggplot(furnishing_rental_areatype, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=furnishing_status)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=4) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title="Number of Super Area Typed House Rentals Per Furnishing Status")


#Among super area typed houses, semi-furnished houses are the most common ones






#7.3.9 Analysis 3.9: Change in rental price of super area typed houses as time progresses

house_rent_data %>% 
  filter(area_type=="Super Area") %>% 
  ggplot(aes(x=posted_date, y=rent)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(se=FALSE) +
  labs(title="Change in Price of Super Area Typed houses as time Progresses", x="Furnishing Status", y="Rental Price")

#We can see that the rental price of super area typed houses slowly increases as time progresses



#7.3.10 Analysis 3.10: Change in price of super area typed houses as house size increases for each city
house_rent_data %>% 
  filter(area_type=="Super Area") %>% 
  ggplot(aes(x=size, y=rent, group=city)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(se=FALSE) +
  facet_wrap(~city) +
  labs(title="Change in Price of Super Area Typed Houses as Size Increases", x="Size", y="Rental Price")



#rental price of super area typed houses increases as house size increases in each ciy





#7.4 Question 4. Which furnishing status do most tenants prefer on average?

#7.4.1 Analysis 4.1 Number of rentals per furnishing status

furnishing_status_rentals <- house_rent_data %>% 
  group_by(furnishing_status) %>% 
  summarise(num_of_rental = n())
furnishing_status_rentals


ggplot(data=furnishing_status_rentals, aes(x=furnishing_status, y=num_of_rental, fill=furnishing_status)) +
  geom_bar(stat="identity", color="black") +
  geom_text(aes(label=num_of_rental)) +
  labs(title="Number of Rentals per Furnishing Status", x="Furnishing Status", y="Number of Rentals", fill="Furnishing Status")

#Most of the tenants prefer semi-furnished houses



#7.4.2 Analysis 4.2 Who rents more semi-furnished houses, bachelor tenants or family tenants?

house_rent_data %>% 
  filter(furnishing_status=="Semi-Furnished" & (tenant_preferred=="Bachelors" | tenant_preferred=="Family")) %>% 
  ggplot(aes(x=tenant_preferred, fill=tenant_preferred)) +
  geom_bar()+
  labs(title="Number of Semi-Furnished House Rentals per Tenant Type", x="Tenant Type", y="Number of Rentals", fill="Tenant Type")

#Bachelors rent more semi-furnished houses than family tenants



#7.4.3 Analysis 4.3 Which city has the most number of semi-furnished houses?

semifurnished_rental_city <- house_rent_data %>% 
  filter(furnishing_status=="Semi-Furnished") %>%
  group_by(city) %>% 
  summarise(num_of_rentals=n())
semifurnished_rental_city


ggplot(semifurnished_rental_city,aes(x=num_of_rentals,y=reorder(city,num_of_rentals)))+######reorder in descending
  geom_point(size=5)+
  geom_text(aes(label=num_of_rentals), nudge_x = 30)+
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(linetype="dashed",color="blue"))+
  labs(title="Number of Semi-Furnished House Rentals Made Per City", x="Number of Rentals", y="City")

#Bangalore has the highest number of semi-furnished house rentals




#7.4.4 Analysis 4.4 Which local area has the most number of semi-furnished houses?

semifurnished_per_locality <- house_rent_data %>% 
  filter(furnishing_status=="Semi-Furnished") %>% 
  group_by(area_locality) %>% 
  summarise(num_of_houses = n()) %>% 
  arrange(-num_of_houses)

semifurnished_per_locality


wordcloud2(semifurnished_per_locality, size=0.3, color=rep_len( c("green","blue"), nrow(semifurnished_per_locality) ) )


#Bandra West has the most number of semi-furnished rental houses



#7.4.5 Analysis 4.5 How the price of semi furnished houses changes over house size in every city?
house_rent_data %>%
  filter(furnishing_status=="Semi-Furnished") %>% 
  ggplot(aes(x=size, y=rent, group=city)) +
  geom_point()+
  geom_jitter()+
  geom_smooth(se=FALSE) +
  facet_wrap(~city)+
  labs(title="Semi-Furnished Rental Housing Price Change Over Size in Every City", x="Size", y="Rental Price")

#Semi-furnished rental housing price in Mumbai, Delhi, Hyderabad increases proportionally as size increases, wherease semi-furnished housing rental price stays constant in Bangalore, Chennai, and Kolkata.


#7.4.6 Analsyis 4.6 What is the most common area type for semi furnished houses

house_rent_data %>% 
  filter(furnishing_status=="Semi-Furnished") %>% 
  group_by(area_type) %>% 
  summarise(num_of_rentals = n()) %>% 
  ggplot(aes(x=area_type, y=num_of_rentals, fill=area_type)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=num_of_rentals), nudge_y = 8) +
  labs(title="Number of Semi-Furnished House Rentals Per Area Type", x="Area Type", y="Number of Rentals", fill="Area Type")


#Super Area is the most common area type for semi-furnished houses


#7.4.7 Analysis 4.7 What is the most common number of bathrooms for semi-furnished houses?

house_rent_data %>% 
  filter(furnishing_status=="Semi-Furnished") %>% 
  group_by(bathroom) %>% 
  summarise(num_of_rentals=n()) %>% 
  ggplot(aes(x=bathroom, y=num_of_rentals)) +
  geom_line() +
  geom_point()+
  geom_text(aes(label=num_of_rentals), nudge_y = 20) +
  labs(title="Number of Semi-Furnished House Rentals Per Bathroom Count", x="Bathroom Count", y="Number of Rentals")

#Most of the Semi-furnished rental houses have 2 bathrooms


#7.4.8 Analysis 4.8 What is the most common BHK value for semi-furnished houses?

house_rent_data %>% 
  filter(furnishing_status=="Semi-Furnished") %>%
  group_by(bhk) %>% 
  summarise(num_of_rentals=n()) %>% 
  ggplot(aes(x=bhk, y=num_of_rentals)) +
  geom_bar(stat="identity", fill="yellow", color="blue") +
  geom_text(aes(label=num_of_rentals)) +
  labs(title="Number of Semi-Furnished House Rentals Per BHK Value", x="BHK", y="Number of Rentals")
#Most of the semif=furnished rental houses have 2 bedrooms



#7.4.9 Analysis 4.9 Whom do most tenants contact when they rent semi-furnished houses?

house_rent_data %>% 
  filter(furnishing_status=="Semi-Furnished") %>%
  group_by(point_of_contact) %>% 
  summarise(num_of_rentals=n()) %>% 
  ggplot(aes(x=point_of_contact, y=num_of_rentals)) +
  geom_point() +
  geom_text(aes(label=num_of_rentals), nudge_y = 25) +
  geom_segment(aes(x=point_of_contact,xend=point_of_contact,y=0,yend=num_of_rentals))+
  labs(title="Number of Semi-Furnished House Rentals Per Point of Contact", x="Point Of Contact", y="Number of Rentals")

#Most the tenants contact owners when they rent semi-furnished houses


#7.4.10 Analysis 4.10 On average, among bachelor and family tenants who pays more rent for semi-furnished houses?

tenant_furnished_rent <- house_rent_data %>% 
  filter(furnishing_status=="Semi-Furnished" & (tenant_preferred=="Bachelors" | tenant_preferred=="Family")) %>% 
  group_by(tenant_preferred) %>% 
  summarise(average_rental_price = round(mean(rent, na.rm=TRUE), 3))


tenant_furnished_rent$fraction = tenant_furnished_rent$average_rental_price / sum(tenant_furnished_rent$average_rental_price)

# Compute the cumulative percentages (top of each rectangle)
tenant_furnished_rent$ymax = cumsum(tenant_furnished_rent$fraction)

# Compute the bottom of each rectangle
tenant_furnished_rent$ymin = c(0, head(tenant_furnished_rent$ymax, n=-1))

# Compute label position
tenant_furnished_rent$labelPosition <- (tenant_furnished_rent$ymax + tenant_furnished_rent$ymin) / 2

# Compute a good label
tenant_furnished_rent$label <- paste0(tenant_furnished_rent$tenant_preferred, "\n Average Rent: ", tenant_furnished_rent$average_rental_price)


ggplot(tenant_furnished_rent, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=tenant_preferred)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=2.9) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title="Average Rental Price for Semi-Furnished Houses for Bachelor and Family Tenants")




#7.5 Question 5. How does bathroom coutn of a house affect the rental decision of tenants?



#7.5.1 Analysis 5.1 Number of Rentals per Bathroom Count

bathroom_rentals <- house_rent_data %>% 
  group_by(bathroom) %>% 
  summarise(num_of_rental = n())
bathroom_rentals

pie3D(bathroom_rentals$num_of_rental, radius=1.5, labels=bathroom_rentals$bathroom, explode=0.2)

#Most tenants prefer houses with 2 bathrooms



#7.5.2 Analysis 5.2 Which city has the most number of rental houses with 2 bathrooms?

twobathroom_rental_city <- house_rent_data %>% 
  filter(bathroom==2) %>%
  group_by(city) %>% 
  summarise(num_of_rentals=n())
twobathroom_rental_city

ggplot(twobathroom_rental_city,aes(x=num_of_rentals,y=reorder(city,num_of_rentals)))+######reorder in descending
  geom_point(size=5)+
  geom_text(aes(label=num_of_rentals), nudge_x = 30)+
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(linetype="dashed",color="blue"))+
  labs(title="Number of Rental Houses with 2 Bathrooms Per City", x="Number of Rentals", y="City")


#Chennai has the highest number of houses with 2 bathrooms


#7.5.3 Analysis 5.3 Which area locality has the most number of rental houses with 2 bathrooms?

twobathroom_per_locality <- house_rent_data %>% 
  filter(bathroom==2) %>% 
  group_by(area_locality) %>% 
  summarise(num_of_houses = n()) %>% 
  arrange(-num_of_houses)
twobathroom_per_locality



wordcloud2(twobathroom_per_locality, size=0.2, color=rep_len( c("green","blue"), nrow(demoFreq) ) )



#Madipakkam, Electronic City and Velacery all have the most number of rental houses with 2 bathrooms




#7.5.4 Analysis 5.4 Among houses with 2 bathrooms, who has made more rentals, bachelors or family tenants?

house_rent_data %>% 
  filter(bathroom==2 & (tenant_preferred=="Bachelors" | tenant_preferred=="Family")) %>% 
  ggplot(aes(x=tenant_preferred, fill=tenant_preferred)) +
  geom_bar() +
  labs(title="Number of 2 bathroom House Rentals Per Tenant Type", x="Tenant Type", y="Number of Rentals", fill="Tenant Type")

#Bachelor tenants have rented more houses with 2 bathrooms than family tenants


#7.5.5 Analysis 5.5 Average rental price of houses with 2 bathrooms
twobathroom_data <- house_rent_data %>% 
  filter(bathroom==2)

avg_rent_price <- mean(twobathroom_data$rent)
avg_rent_price



#7.5.6 Analysis 5.6 Average size of houses with 2 bathrooms

twobathroom_data <- house_rent_data %>% 
  filter(bathroom==2)

avg_size <- mean(twobathroom_data$size)
avg_size



#7.5.7 Analysis 5.7 How rental price of houses with 2 bathrooms changes over house size in each city?
house_rent_data %>% 
  filter(bathroom==2) %>% 
  ggplot(aes(x=size, y=rent, group=city)) +
  geom_point() +
  geom_jitter() +
  geom_smooth() +
  facet_wrap(~city) +
  labs(title="Rental Price Change of Houses with 2 Bathrooms over Size", x="Size", y="Rent")


#Rental price of houses with 2 bathrooms in every city increases proportionally as time progresses




#7.5.8 Analysis 5.8 What is the most common area type for houses with 2 bathrooms?

house_rent_data %>% 
  filter(bathroom==2) %>% 
  group_by(area_type) %>% 
  summarise(num_of_rentals=n()) %>% 
  ggplot(aes(x=area_type, y=num_of_rentals)) +
  geom_point() +
  geom_text(aes(label=num_of_rentals), nudge_y = 15) +
  geom_segment(aes(x=area_type,xend=area_type,y=0,yend=num_of_rentals))+
  labs(title="Number of 2 bathroom House Rentals Per Area Type", x="Area Type", y="Number of Rentals")

#Most of the houses with 2 bathrooms are of super area types



#7.5.9 Analysis 5.9 What is the most common furnishing status for houses with 2 bathrooms? 


house_rent_data %>% 
  filter(bathroom==2) %>% 
  group_by(furnishing_status) %>% 
  summarise(num_of_rentals=n()) %>% 
  ggplot(aes(x=furnishing_status, y=num_of_rentals)) +
  geom_point() +
  geom_text(aes(label=num_of_rentals), nudge_y = 15) +
  geom_segment(aes(x=furnishing_status,xend=furnishing_status,y=0,yend=num_of_rentals))+
  labs(title="Number of 2 bathroom House Rentals Per Furnishing Status", x="Furnishing Status", y="Number of Rentals")

#Most of the houses with 2 bathrooms are semi-furnished


#7.5.10 Analysis 5.10 Whom do most tenants contact when they rent houses with 2 bathrooms?

twobathroom_contact <- house_rent_data %>% 
  filter(bathroom==2) %>% 
  group_by(point_of_contact) %>% 
  summarise(num_of_rentals=n())
twobathroom_contact

ggplot(twobathroom_contact, aes(x=point_of_contact, y=num_of_rentals, fill=point_of_contact)) +
  geom_bar(stat="identity") +
  labs(title="Number of House Rentals per Point of Cotact", x="Point of Contact"G, y="Number of Rentals", fill="Point of Contact")

#Most of the tenants contact owners when they rent houses with 2 bathrooms

#7.5.11 Analysis 5.11 How does the size of 2-bathroom rental houses differ among cities?

house_rent_data %>%
  filter(bathroom==2) %>% 
  ggplot( aes(x=city, y=size, fill=city)) +
  geom_boxplot() +
  
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Box plot to Study the House Size of Rental Houses in Each City") +
  xlab("City")


#Median size of 2-bathrooom rental houses in Hyderabad is the highest among all




#7.6 Question 6: How does BHK value(bedroom count) affect the rental decision of tenants?

#7.6.1 Analysis 6.1 Number of rentals per BHK Value

bedroom_rentals <- house_rent_data %>% 
  group_by(bhk) %>% 
  summarise(num_of_rental = n())
bedroom_rentals

pie3D(bedroom_rentals$num_of_rental, radius=1.5, labels=bathroom_rentals$bhk, explode=0.2)


#Most of the tenants prefer houses with 2 bedrooms




#7.6.2 Analysis 6.2 Among bachelor and family tenants, who has made more 2-bedroom house rentals?

house_rent_data %>% 
  filter(bhk==2 & (tenant_preferred=="Bachelors" | tenant_preferred=="Family")) %>% 
  ggplot(aes(x=tenant_preferred, fill=tenant_preferred)) +
  geom_bar() +
  labs(title="Number of 2 Bedroom House Rentals Per Tenant Type", x="Tenant Type", y="Number of Rentals", fill="Tenant Type")
#Bachelor tenants rent more 2 bedroom houses than family tenants


#7.6.3 Analysis 6.3 Whom do most tenants contact in general when they rent 2 bedroom houses?

twobhk_contact <- house_rent_data %>% 
  filter(bhk==2) %>% 
  group_by(point_of_contact) %>% 
  summarise(num_of_rentals=n())
twobhk_contact

ggplot(twobhk_contact, aes(x=point_of_contact, y=num_of_rentals, fill=point_of_contact)) +
  geom_bar(stat="identity") +
  labs(title="Number of 2 Bedroom House Rentals per Point of Cotact", x="Point of Contact", y="Number of Rentals", fill="Point of Contact")


#Most of the tenants contact house owners when they rent 2 bedroom houses



#7.6.4 Analysis 6.4 Average rental price of 2 bedroom housesy?

twobedroom_data <- house_rent_data %>% 
  filter(bhk==2)

mean(twobedroom_data$rent)




#7.6.5 Analysis 6.5 What is the most common area type of 2 bedroom houses?


house_rent_data %>% 
  filter(bhk==2) %>% 
  group_by(area_type) %>% 
  summarise(num_of_rentals=n()) %>% 
  ggplot(aes(x=area_type, y=num_of_rentals)) +
  geom_point() +
  geom_text(aes(label=num_of_rentals), nudge_y = 15) +
  geom_segment(aes(x=area_type,xend=area_type,y=0,yend=num_of_rentals))+
  labs(title="Number of 2 Bedroom House Rentals Per Area Type", x="Area Type", y="Number of Rentals")



#Most of the 2 bedroom houses are of super area types




#7.6.6 Analysis 6.6 Which city has the most number of 2 bedroom houses?

twobhk_rental_city <- house_rent_data %>% 
  filter(bhk==2) %>%
  group_by(city) %>% 
  summarise(num_of_rentals=n())
twobhk_rental_city


ggplot(twobhk_rental_city,aes(x=num_of_rentals,y=reorder(city,num_of_rentals)))+######reorder in descending
  geom_point(size=5)+
  geom_text(aes(label=num_of_rentals), nudge_x = 30)+
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(linetype="dashed",color="blue"))+
  labs(title="Number of Rental Houses with 2 Bedrooms Per City", x="Number of Rentals", y="City")

#Chennai has the highest number of rental houses with 2 bedrooms



#7.6.7 Anlaysis 6.7 Which area locality has the most number of 2 bedroom houses?


twobhk_per_locality <- house_rent_data %>% 
  filter(bhk==2) %>% 
  group_by(area_locality) %>% 
  summarise(num_of_houses = n()) %>% 
  arrange(-num_of_houses)
twobhk_per_locality


wordcloud2(twobhk_per_locality, size=0.2, color=rep_len( c("green","blue"), nrow(demoFreq) ) )


#Electronic city and Velachery has the highest number of rental houses with 2 bedrooms




#7.6.8 Analysis 6.8: On average, Who pays more rent for 2 bedroom houses, bachelors or families?

house_rent_data %>% 
  filter(bhk==2 & (tenant_preferred=="Bachelors" | tenant_preferred=="Family")) %>% 
  group_by(tenant_preferred) %>% 
  summarise(average_rental_price=mean(rent)) %>% 
  ggplot(aes(x=tenant_preferred, y=average_rental_price, fill=tenant_preferred)) +
  geom_bar(stat='identity') +
  geom_text(aes(label=average_rental_price)) +
  labs(title="Average Rental Fee Per Tenant Type", x="Tenant Type", y="Average Rental Fee", fill="Tenant Type")

#On average, bachelor tenants pay more rent for 2 bedroom houses



#7.6.9 Analysis 6.9 How does the rental price of 2 bedroom houses change over house size in each city?
house_rent_data %>% 
  filter(bhk==2) %>% 
  ggplot(aes(x=size, y=rent, group=city)) +
  geom_point() +
  geom_jitter() +
  geom_smooth() +
  facet_wrap(~city) +
  labs(title="Rental Price Change of Houses with 2 Bedrooms over Size", x="Size", y="Rent")

#In Bangalore, Hyderabad, Kolkata, and Chenani,  rental price of houses with 2 bedrooms stays constant as house size increases, whereas in Delhi and Mumbai the housing rental price increases has houses size increases


#7.6.10 Analysis 6.10 What is the most common furnishing status for 2 bedroom houses?


house_rent_data %>% 
  filter(bhk==2) %>% 
  group_by(furnishing_status) %>% 
  summarise(num_of_rentals=n()) %>% 
  ggplot(aes(x=furnishing_status, y=num_of_rentals)) +
  geom_point() +
  geom_text(aes(label=num_of_rentals), nudge_y = 15) +
  geom_segment(aes(x=furnishing_status,xend=furnishing_status,y=0,yend=num_of_rentals))+
  labs(title="Number of 2 Bedroom House Rentals Per Furnishing Status", x="Furnishing Status", y="Number of Rentals")


#Most of the houses with 2 bedroom houses are semi-furnished





##7.7 Question 7. what kind of houses do bachelor tenants specifically prefer?



#7.7.1 Analysis 7.1 Houses in which location do most bachelor tenants prefer to rent?

bachelor_rental_location <- house_rent_data %>% 
  filter(tenant_preferred=="Bachelors") %>% 
  group_by(city) %>% 
  summarise(num_of_rentals=n())
bachelor_rental_location


ggplot(data=bachelor_rental_location, aes(x=city, y=num_of_rentals, fill=city)) +
  geom_bar(stat="identity", color="blue") +
  geom_text(aes(label=num_of_rentals)) +
  labs(title="Number of Rentals by Bachelor Tenants per City", x="City", y="Number of Rentals", fill="City") +
  theme(plot.title.position = "panel")


#Most bachelor tenants prefer rental houses in Mumbai which has about 172 rentals




#7.7.2 Analysis 7.2 Which furnishing status do bachelor tenants prefer when renting house?

furnishing_bachelor_tenant <- house_rent_data %>% 
  filter(tenant_preferred=="Bachelors") %>%  
  group_by(furnishing_status) %>% 
  summarise(num_of_rentals=n())
furnishing_bachelor_tenant


ggplot(data=furnishing_bachelor_tenant, aes(x=furnishing_status, y=num_of_rentals, color=furnishing_status)) +
  geom_point() +
  geom_text(aes(label=num_of_rentals), nudge_y = 15) +
  geom_segment(aes(x=furnishing_status,xend=furnishing_status,y=0,yend=num_of_rentals))+
  labs(title="Number of Rentals Made by Bachelor Tenants Per Furnishing Status", x="Furnishing Status", y="Number of Rentals", color="Furnishing Status")


#Bachelor tenants prefer unfurnished houses which has about 409 rentals




#7.7.3 Analysis 7.3 Which area type do bachelor tenants prefer when renting house?

house_rent_data %>% 
  filter(tenant_preferred=="Bachelors") %>%  
  group_by(area_type) %>% 
  summarise(num_of_rentals=n()) %>% 
  ggplot(aes(x=num_of_rentals, y=area_type, fill=area_type)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=num_of_rentals)) +
  labs(title="Number of Rentals by Bachelor Tenants per Area Type", x="Number of Rentals", y="Area Type", fill="City")


#Bachelor tenants prefer carpet area typed houses



#7.7.4 Analysis 7.4 How many bathrooms do bachelor tenants prefer when renting house?

house_rent_data %>% 
  filter(tenant_preferred=="Bachelors") %>% 
  group_by(bathroom) %>% 
  summarise(num_of_rentals=n()) %>% 
  ggplot(aes(x=bathroom, y=num_of_rentals, fill=bathroom)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=num_of_rentals), nudge_y=6)+
  labs(title="Number of Rentals by Bachelor Tenants per Bathroom Count", x="Number of Rentals", y="Bathroom Count", fill="Bathroom Count")

#Bachelor tenants prefer houses with 2 bathrooms




#7.7.5 Anlaysis 7.5 Whom do bachelor tenants prefer to contact when renting houses?


house_rent_data %>% 
  filter(tenant_preferred=="Bachelors") %>% 
  group_by(point_of_contact) %>% 
  summarise(num_of_rentals=n()) %>% 
  ggplot(aes(x=point_of_contact, y=num_of_rentals, fill=point_of_contact)) +
  geom_bar(stat="identity")+
  coord_polar()+
  labs(title="Number of Rentals by Bachelor Tenants per Point of Contact",  x="Point of Contact", y="Number of Rentals", fill="Point of Contact")


#Bacher tenants prefer to contact agents when renting houses



#7.7.6 Analysis 7.6 what is the average rental price of houses rented by bachelor tenants in each city?

house_rent_data %>% 
  filter(tenant_preferred=="Bachelors") %>%
  group_by(city) %>% 
  summarise(average_rental_price=mean(rent))


#7.7.7 Analysis 7.7 What is the average size of houses rented by bachelor tenants?


bachelor_house = house_rent_data %>% 
  filter(tenant_preferred=="Bachelors") 

mean(bachelor_house$size)
#1015.11sqft


#7.7.8 Analysis 7.8 On which weekday do most bachelor tenants prefer to rent houses?

weekday_bachelor_rental = house_rent_data %>% 
  filter(tenant_preferred=="Bachelors") %>% 
  group_by(posted_weekday) %>% 
  summarise(num_of_rentals=n())
weekday_bachelor_rental

ggplot(weekday_bachelor_rental,aes(x=num_of_rentals,y=reorder(posted_weekday,num_of_rentals)))+######reorder in descending
  geom_point(size=5)+
  geom_text(aes(label=num_of_rentals), nudge_x = 3)+
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(linetype="dashed",color="blue"))+
  labs(title="Number of Rental Houses by Bachelor Tenants per Weekday", x="Number of Rentals", y="Weekday")


#Bachelor tenants prefer to rent houses on Wednesdays



#7.7.9 Analysis 7.9 How many bedrooms do bachelor tenants prefer when renting house?

house_rent_data %>% 
  filter(tenant_preferred=="Bachelors") %>% 
  group_by(bhk) %>% 
  summarise(num_of_rentals=n()) %>% 
  ggplot(aes(x=bhk, y=num_of_rentals, fill=bhk)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=num_of_rentals)) +
  labs(title="Number of Rental Houses by Bachelor Tenants per BHK Value", x="BHK", y="Number of Rentals", fill="BHK")


#Bachelor tenants prefer houses with 2 bedrooms


#7.7.10 Analysis 7.10 How does rental housing price change over time for bachelor tenants?

house_rent_data %>% 
  filter(tenant_preferred=="Bachelors") %>% 
  ggplot(aes(x=posted_date, y=rent))+
  geom_point()+
  geom_jitter() +
  geom_smooth() +
  labs(title="Rental Price Change Over Time of Houses Rented by Bachelor Tenants", x="Time", y="Rental Price")


#Rental price of houses rented by bachelor tenants remain constant over time.





##7.8 Question 8. what kind of houses do family tenants specifically prefer?



#7.8.1 Analysis 8.1 Houses in which location do most family tenants prefer to rent?

family_rental_location <- house_rent_data %>% 
  filter(tenant_preferred=="Family") %>% 
  group_by(city) %>% 
  summarise(num_of_rentals=n())
family_rental_location


ggplot(data=family_rental_location, aes(x=city, y=num_of_rentals, fill=city)) +
  geom_bar(stat="identity", color="blue") +
  geom_text(aes(label=num_of_rentals)) +
  labs(title="Number of Rentals by Family Tenants per City", x="City", y="Number of Rentals", fill="City") +
  theme(plot.title.position = "panel")


#Family tenants prefer to rent houses in Mumbai which has about 186 rentals



#7.8.2 Analysis 8.2 Which furnishing status do family tenants prefer when renting house?

furnishing_family_tenant <- house_rent_data %>% 
  filter(tenant_preferred=="Family") %>%  
  group_by(furnishing_status) %>% 
  summarise(num_of_rentals=n())
furnishing_family_tenant


ggplot(data=furnishing_family_tenant, aes(x=furnishing_status, y=num_of_rentals, color=furnishing_status)) +
  geom_point() +
  geom_text(aes(label=num_of_rentals), nudge_y = 15) +
  geom_segment(aes(x=furnishing_status,xend=furnishing_status,y=0,yend=num_of_rentals))+
  labs(title="Number of Rentals Made by Family Tenants Per Furnishing Status", x="Furnishing Status", y="Number of Rentals", color="Furnishing Status")


#Family tenants prefer semi-furnished houses



#7.8.3 Analysis 8.3 Which area type do family tenants prefer when renting house?

house_rent_data %>% 
  filter(tenant_preferred=="Family") %>%  
  group_by(area_type) %>% 
  summarise(num_of_rentals=n()) %>% 
  ggplot(aes(x=num_of_rentals, y=area_type, fill=area_type)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=num_of_rentals)) +
  labs(title="Number of Rentals by Family Tenants per Area Type", x="Number of Rentals", y="Area Type", fill="City")


#Family tenants prefer carpet area typed houses



#7.8.4 Analysis 8.4 How many bathrooms do family tenants prefer when renting house?

house_rent_data %>% 
  filter(tenant_preferred=="Family") %>% 
  group_by(bathroom) %>% 
  summarise(num_of_rentals=n()) %>% 
  ggplot(aes(x=bathroom, y=num_of_rentals, fill=bathroom)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=num_of_rentals), nudge_y=6)+
  labs(title="Number of Rentals by Family Tenants per Bathroom Count", x="Number of Rentals", y="Bathroom Count", fill="Bathroom Count")

#Family tenants prefer houses with 2 bathrooms




#7.8.5 Analysis 8.5 Whom do family tenants prefer to contact when renting houses?


house_rent_data %>% 
  filter(tenant_preferred=="Family") %>% 
  group_by(point_of_contact) %>% 
  summarise(num_of_rentals=n()) %>% 
  ggplot(aes(x=point_of_contact, y=num_of_rentals, fill=point_of_contact)) +
  geom_bar(stat="identity")+
  coord_polar()+
  labs(title="Number of Rentals by Family Tenants per Point of Contact",  x="Point of Contact", y="Number of Rentals", fill="Point of Contact")

#Family tenants prefer to contact agents when renting houses



#7.8.6 Analysis 8.6 what is the average rental price of houses rented by family tenants in each city?

house_rent_data %>% 
  filter(tenant_preferred=="Family") %>%
  group_by(city) %>% 
  summarise(average_rental_price=mean(rent))


#7.8.7 Analysis 8.7 What is the average size of houses rented by family tenants?


bachelor_house = house_rent_data %>% 
  filter(tenant_preferred=="Family") 

mean(bachelor_house$size)
#1155.331 sqft


#7.8.8 Analysis 8.8 On which weekday do most family tenants prefer to rent houses?

weekday_family_rental = house_rent_data %>% 
  filter(tenant_preferred=="Family") %>% 
  group_by(posted_weekday) %>% 
  summarise(num_of_rentals=n())
weekday_family_rental

ggplot(weekday_family_rental,aes(x=num_of_rentals,y=reorder(posted_weekday,num_of_rentals)))+######reorder in descending
  geom_point(size=5)+
  geom_text(aes(label=num_of_rentals), nudge_x = 3)+
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(linetype="dashed",color="blue"))+
  labs(title="Number of Rental Houses by Family Tenants per Weekday", x="Number of Rentals", y="Weekday")


#Family tenants prefer to rent houses on Wednesdays


#7.8.9 Analysis 8.9 How many bedrooms do family tenants prefer when renting house?

house_rent_data %>% 
  filter(tenant_preferred=="Family") %>% 
  group_by(bhk) %>% 
  summarise(num_of_rentals=n()) %>% 
  ggplot(aes(x=bhk, y=num_of_rentals, fill=bhk)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=num_of_rentals), nudge_y=5) +
  labs(title="Number of Rental Houses by Family Tenants per BHK Value", x="BHK", y="Number of Rentals", fill="BHK")

#Family tenans prefer houses with 2 bathrooms


#7.8.10 Analysis 8.10 How does rental housing price change over time for family tenants?

house_rent_data %>% 
  filter(tenant_preferred=="Family") %>% 
  ggplot(aes(x=posted_date, y=rent))+
  geom_point()+
  geom_jitter() +
  geom_smooth() +
  labs(title="Rental Price Change Over Time of Houses Rented by Family Tenants", x="Time", y="Rental Price")



#rental price of houses rented by family tenants remains constant over time


