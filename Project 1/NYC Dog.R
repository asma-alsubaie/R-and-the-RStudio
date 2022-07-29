library(tidyverse)
library(ggplot2)

data=read.csv('NYC_Dog_Licensing_Dataset.csv') 

#Exploratory Data Analysis
head(data)
summary(data)

#Animal Genders
n_distinct(data$AnimalGender)
levels(data$AnimalGender) #Look at the levels
data=data[! data$AnimalGender %in% c('', ' '),] #Dropping the extra levels

#Now you should be seeing only two levels using n_distinct.
n_distinct(data$AnimalGender)

n_distinct(data$Borough)
unique(data$Borough)

data$Borough=tolower(data$Borough) #convert all names to lowercase

#STEP 2: 
#------------------Remove values outside the NYC area
#data=data[!(data$Borough=="long island city"),]
#data=data[!(data$Borough=="albany"),]
#data=data[!(data$Borough=="Jersey City"),]
#data=data[!(data$Borough=="b"),]

#change all the misspelled Boroughs to correct spellings or empty spaces!
data$Borough [data$Borough=="staten is"] <- "staten island"
data$Borough [data$Borough=="quens"] <- "queens"
data$Borough [data$Borough=="brooklyn "] <- "brooklyn"
data$Borough [data$Borough=="nyc"] <- "NYC"
data$Borough [data$Borough=="ny"] <- "NYC"
data$Borough [data$Borough=="new york "] <- "NYC"
data$Borough [data$Borough=="new york  "] <- "NYC"
data$Borough [data$Borough=="new york city"] <- "NYC"
data$Borough [data$Borough=="new york"] <- "NYC"
data$Borough [data$Borough=="bronx "] <- "bronx"
data$Borough [data$Borough=="manhattan "] <- "manhattan"

# select value in NYC area 
New_data<- data %>%
                 filter(Borough %in% c("manhattan", "brooklyn","queens","bronx","staten island"))
#check
unique(New_data$Borough)
n_distinct(New_data$Borough)
na.omit(New_data)

#---------------- insight 1----------------------------------

table(New_data$AnimalGender, New_data$Borough)

#naming the conversion to dataframe as s for easier convention
s=as.data.frame(table(New_data$AnimalGender, New_data$Borough)) 
s
head(s)
#Let's remove the factor level values such as Bronx [0], Brooklyn [2]..etc. 
s=s[ s$Freq!=0, ]
s=s[ s$Freq!=2, ]
s=s[ s$Freq!=1, ]

#Rename the column names below
colnames(s)[colnames(s)=="Var1"]<-"Gender"
colnames(s)[colnames(s)=="Var2"]<-"Borough"
colnames(s)[colnames(s)=="Freq"]<-"Count"

#Plotting with ggplot
ggplot(s, aes(x=Gender, y=Count, fill=Borough)) + #aes aka asthetics
  geom_bar(stat="identity") +#barplot
  xlab("Gender")+ylab("Count")+ #Renaming x-label and y-label 
  facet_grid(.~Borough) #Split it based on Borough

#Top 10 breeds in NYC
breeds=as.data.frame(table(data$BreedName)) 
colnames(breeds)[colnames(breeds)=="Var1"] <- "BreedName"
colnames(breeds)[colnames(breeds)=="Freq"] <- "Count"

#Ordering in descending order to figure out top breeds
breeds=breeds[order(breeds$Count, decreasing = TRUE),]

#Making a seperate dataframe with top 11 breeds. 
top_breeds=breeds[1:11,]

#converting top_breeds to a factor 
top_breeds$BreedName=factor(top_breeds$BreedName, 
                            levels=c("Beagle",
                                     "Pomeranian",
                                     "Labrador Retriever Crossbreed",
                                     "American Pit Bull Terrier/Pit Bull",
                                     "American Pit Bull Mix / Pit Bull Mix",
                                     "Labrador Retriever",
                                     "Maltese",
                                     "Chihuahua",
                                     "Shih Tzu",
                                     "Yorkshire Terrier",
                                     "Unknown")) 
top_breeds


#------------------insight 2------------------
ggplot(top_breeds, aes(x=BreedName, y=Count, fill=Count))+ 
  #barplot
  geom_bar(stat="identity")+ 
  #writing the value of count on top of each bar
  geom_text(stat="identity", aes(label=Count), vjust=-0.5)+
  #putting color across the barplot
  scale_fill_continuous(low="blue", high="red")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#------------------insight 3------------------

City=as.data.frame(table(New_data$Borough)) 
City
head(City)

#Rename the column names below
colnames(City)[colnames(City)=="Var1"]<-"City"
colnames(City)[colnames(City)=="Freq"]<-"Count"
piepercent<- round(100 * City$Count / sum(City$Count), 1)

pie(City$Count, labels=piepercent, main = "Total numbers of dogs by city", col = rainbow(length(City$City)))
legend("topright",legend=c(City$City),cex = 0.5, fill = rainbow(length(City$City)))

#--------------------insight 4----------------

DOF=as.data.frame(table(New_data$AnimalBirthMonth))
DOF

colnames(DOF)[colnames(DOF)=="Var1"]<-"date"
colnames(DOF)[colnames(DOF)=="Freq"]<-"Count"

DOF$year <-substring(DOF$date,7,10)

insight<-aggregate(DOF["Count"],by=DOF["year"],sum)

ggplot(insight)  +
  geom_line(aes(x=year, y=Count, group =1),stat="identity", colour="sienna3")+
  labs(title= "Total number of dogs by years",
       x="Year",y="Number of dogs")
