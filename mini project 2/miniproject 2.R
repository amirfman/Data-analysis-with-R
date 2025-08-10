library(lubridate)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggmap)
CPUE_Balitic_21_22_23_24_WBC <- read.csv("CPUE Balitic 21 22 23 24 WBC.csv",na ="0")
CPUE_Balitic_21_22_23_24_WBC_Age <- read_csv("CPUE Balitic 21 22 23 24 WBC. Age.csv")

###################################  DATA WRANGELING  #################################################
View(CPUE_Balitic_21_22_23_24_WBC)
is.na(CPUE_Balitic_21_22_23_24_WBC)
summary(CPUE_Balitic_21_22_23_24_WBC)
df <-drop_na(CPUE_Balitic_21_22_23_24_WBC)
summary(df)
View(df)
dff <- filter(df,Area<25,Area>20)
View(dff)
#dffa<-arrange(dff,Area)
#View(dffa)
DF <- filter(CPUE_Balitic_21_22_23_24_WBC_Age,Area<25,Area>20)
View(DF)
DFAA<-arrange(DF,Area)
View(DFAA)
DFA<-gather(DFAA,Age0,Age1,Age2,Age3,Age4,Age5,Age6,Age7,Age8,Age9,Age10,key = Age_Class,value = CPUE_per_Age)
View(DFA)

#deviation of Areas per Year
A21 <- filter(dff,Area==21)
A22 <- filter(dff,Area==22)
A23 <- filter(dff,Area==23)
A24 <- filter(dff,Area==24)
View(A24)

#deviation of Areas per Year
A21A <- filter(DFA,Area==21)
A22A <- filter(DFA,Area==22)
A23A <- filter(DFA,Area==23)
A24A <- filter(DFA,Area==24)
View(A21A)

#deviation of Area (Length class,Quarters)
LCPUE<-select(dff,LngtClass,CPUE_number_per_hour)
View(LCPUE)

A21LQ<-select(A21,Quarter,LngtClass,CPUE_number_per_hour)
A22LQ<-select(A22,Quarter,LngtClass,CPUE_number_per_hour)
A23LQ<-select(A23,Quarter,LngtClass,CPUE_number_per_hour)
A24LQ<-select(A24,Quarter,LngtClass,CPUE_number_per_hour)
#testing the data frame
View(A21LQ)
A21LQ1<-filter(A21,Quarter==1)
A21LQ4<-filter(A21,Quarter==4)
View(A21LQ1)

#deviation of Area (Age ,Quarters)

A21AQ<-select(A21A,Quarter,Age_Class,CPUE_per_Age)
A22AQ<-select(A22A,Quarter,Age_Class,CPUE_per_Age)
A23AQ<-select(A23A,Quarter,Age_Class,CPUE_per_Age)
A24AQ<-select(A24A,Quarter,Age_Class,CPUE_per_Age)
#testing the data frame
View(A21AQ)
A21AQ1<-filter(A21AQ,Quarter==1)
A21AQ4<-filter(A21AQ,Quarter==4)
View(A21AQ1)

###################################  DATA Visualization  ##############################################
#Length
ggplot(dffa, aes(x = Year , y = CPUE_number_per_hour)) +geom_point()

ggplot(dffa, aes(x = Year, 
                 y = LngtClass)) +
  geom_col()

ggplot(dffa, aes(x = Year, 
                 y = CPUE_number_per_hour)) +
  geom_col()

ggplot(dff, aes(x = Year, 
                y = CPUE_number_per_hour)) +
  geom_col() +
  facet_wrap(~Area, nrow=4)

#Age
ggplot(DFA, aes(x = Year , y = CPUE_per_Age)) +geom_point()
ggplot(DFA, aes(x = Year, 
                 y = CPUE_per_Age)) +
  geom_col()
ggplot(DFA, aes(x = Year, 
                y = CPUE_per_Age)) +
  geom_col() +
  facet_wrap(~Age_Class, nrow=11)


###################################  CPUE per Year divided by Area  ##############################################
ggplot(A21, aes(x = Year, 
                 y = CPUE_number_per_hour)) +
  geom_col()
ggplot(A22, aes(x = Year, 
                 y = CPUE_number_per_hour)) +
  geom_col()
ggplot(A23, aes(x = Year, 
                 y = CPUE_number_per_hour)) +
  geom_col()
ggplot(A24, aes(x = Year, 
                 y = CPUE_number_per_hour)) +
  geom_col()


################################### CPUE per Length class divided by Area  ##############################################

ggplot(A21LQ, aes(x = LngtClass, 
                y = CPUE_number_per_hour)) +
  geom_col()
ggplot(A22LQ, aes(x = LngtClass, 
                y = CPUE_number_per_hour)) +
  geom_col()
ggplot(A23LQ, aes(x = LngtClass, 
                y = CPUE_number_per_hour)) +
  geom_col()
ggplot(A24LQ, aes(x = LngtClass, 
                y = CPUE_number_per_hour)) +
  geom_col()

## per Quarter

ggplot(A21LQ, aes(x = LngtClass, 
                y = CPUE_number_per_hour)) +
  geom_col() +
  facet_wrap(~Quarter, nrow=4)

########################### testing the plot (data frame)
ggplot(A21LQ1, aes(x = LngtClass, 
                  y = CPUE_number_per_hour)) +
  geom_col()
ggplot(A21LQ4, aes(x = LngtClass, 
                  y = CPUE_number_per_hour)) +
  geom_col()
####################

ggplot(A22LQ, aes(x = LngtClass, 
                  y = CPUE_number_per_hour)) +
  geom_col() +
  facet_wrap(~Quarter, nrow=4)
ggplot(A23LQ, aes(x = LngtClass, 
                  y = CPUE_number_per_hour)) +
  geom_col() +
  facet_wrap(~Quarter, nrow=4)
ggplot(A24LQ, aes(x = LngtClass, 
                  y = CPUE_number_per_hour)) +
  geom_col() +
  facet_wrap(~Quarter, nrow=4)

################################### CPUE per Age divided by Area  ##############################################

ggplot(A21AQ, aes(x = Age_Class, 
                  y = CPUE_per_Age)) +
  geom_col()
ggplot(A22AQ, aes(x = Age_Class, 
                  y = CPUE_per_Age)) +
  geom_col()
ggplot(A23AQ, aes(x = Age_Class, 
                  y = CPUE_per_Age)) +
  geom_col()
ggplot(A24AQ, aes(x = Age_Class, 
                  y = CPUE_per_Age)) +
  geom_col()

## per Quarter

ggplot(A21AQ, aes(x = Age_Class, 
                  y = CPUE_per_Age)) +
  geom_col() +
  facet_wrap(~Quarter, nrow=4)

########################### testing the plot (data frame)
ggplot(A21AQ1, aes(x = Age_Class, 
                   y = CPUE_per_Age)) +
  geom_col()
ggplot(A21AQ4, aes(x = Age_Class, 
                   y = CPUE_per_Age)) +
  geom_col()
####################

ggplot(A22AQ, aes(x = Age_Class, 
                  y = CPUE_per_Age)) +
  geom_col() +
  facet_wrap(~Quarter, nrow=4)
ggplot(A23AQ, aes(x = Age_Class, 
                  y = CPUE_per_Age)) +
  geom_col() +
  facet_wrap(~Quarter, nrow=4)
ggplot(A24AQ, aes(x = Age_Class, 
                  y = CPUE_per_Age)) +
  geom_col() +
  facet_wrap(~Quarter, nrow=4)


















ggplot(dffa, aes(x = LngtClass, 
                 y = CPUE_number_per_hour)) +
  geom_col() 

ks.test(dff$CPUE_number_per_hour, "pnorm", mean(dff$CPUE_number_per_hour),
        + sd(dff$CPUE_number_per_hour))
model1<-lm(formula =CPUE_number_per_hour~LngtClass,LCPUE)
summary(model1)
plot(model1)

cpue <- CPUE_number_per_hour %>% 
  filter(Species == "Gadus morhua", Year == 2015, Quarter == 1) %>%
  select(Area, contains("Age")) %>%
  mutate(total_cpue = rowSums(select(., contains("Age")))) 

ggplot(A21, aes(x = Year, 
                y = CPUE_number_per_hour)) +
  geom_histogram()




world <- map_data("world")
worldmap <- ggplot(world, aes(
  x=long, y=lat, group=group)) +
  geom_polygon(fill = "white", 
               colour = "black") 
map<-worldmap + coord_map("ortho", 
                     orientation = c(55, 20, 0),
                     xlim = c(10, 30), ylim = c(54,66))

map

map + geom_point(data=, 
                    aes(long,lat), size=0.5, 
                    color="red")

  group_by(fmonth, station, date_time, cruise) 
ggplot(dff, aes(x = Year, y = CPUE_number_per_hour)) +
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.2)


p1 <- ggplot(aes(y = CPUE_number_per_hour, x = Area), data = Y20) + geom_boxplot()+
theme_bw()+ xlab("Area")+ylab( "Cpue")

p1


