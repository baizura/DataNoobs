#install.packages("r2d3")


library(dplyr)
library(ggplot2)
library(rworldmap)
library(viridis)
library(leaflet)
library(r2d3) #d3 visualization

# Import and understand the dataset
dataset <- read.delim("data.csv", header = TRUE, sep = "\t")

a <- read.csv("Data/summaryData.csv")

#replace gender=0 to be 3
dataset$gender[dataset$gender < 1] <- 3

# Add new calculated column
# Calculate TotalMark
dataset <- dataset %>% mutate(TM=Q1 + Q2 + Q3 + Q4 + Q5 + Q6 +Q7 + Q8 + 
                                    Q9 + Q10 + Q11 + Q12 +Q13 + Q14 + Q15 + Q16 + 
                                    Q17 + Q18 +Q19 + Q20 + Q21 + Q22 + Q23 + Q24 +
                                    Q25 + Q26 + Q27 + Q28 + Q29 + Q30 +Q31 + Q32 + 
                                    Q33 + Q34 + Q35 + Q36 +Q37 + Q38 + Q39 + Q40 + 
                                    Q41 + Q42 +Q43 + Q44  )

# Calculate Total masculinity mark (MM)
dataset <- dataset %>% mutate(MM=Q1 + Q3 + Q5 + Q7 +  Q9 + Q11 + Q13 + Q15 + 
                                    Q17 + Q19 + Q21 + Q23 + Q25 + Q27 + Q29 + Q31 + Q33 + 
                                    Q35 + Q37 + Q39 + Q41 + Q43)



# Calculate Total femininity mark (FM)
dataset <- dataset %>% mutate(FM=Q2 + Q4 + Q6 + Q8 +  Q10 + Q12 + Q14 + Q16 + 
                                    Q18 + Q20 + Q22 + Q24 + Q26 + Q28 + Q30 + Q32 + Q34 + 
                                    Q36 + Q38 + Q40 + Q42 + Q44)

# Calculate masculinity score (MS)
dataset <- dataset %>% mutate(MS=MM/22)

# Calculate femininity score (FS)
dataset <- dataset %>% mutate(FS=FM/22)

# Calculate Androgyny score (AS)
dataset <- dataset %>% mutate(AS=FS - MS)

# Classification
#Feminine: AS >=1
dataset1 <- dataset %>% filter(AS >=1) %>% mutate(cls=1)

#Near Feminine:AS > -0.5 & AS <= 0.5
dataset2 <- dataset %>% filter(AS >= 0.5 & AS < 1) %>% mutate(cls=2)

#Androgynous: AS > -0.5 & AS <= 0.5
dataset3 <- dataset %>% filter(AS >= -0.5 & AS < 0.5) %>% mutate(cls=3)

# Near Masculine: AS > -1 & AS <= -0.5
dataset4 <- dataset %>% filter(AS >= -1 & AS < -0.5) %>% mutate(cls=4)

#Masculine: AS < -1
dataset5 <- dataset %>% filter(AS < -1) %>% mutate(cls=5)

# combine back the data
dataset <- rbind(dataset1,dataset2,dataset3,dataset4,dataset5)

#remove unused dataset
rm(dataset1,dataset2,dataset3,dataset4,dataset5)
names(dataset)

allData <- dataset %>% select(IP,gender, TM, MM, FM, MS,  FS, AS,  cls) 

# Write CSV in R
write.csv(allData, file = "Data/allData.csv",row.names=FALSE)
#===================================================================================================
#     ValueBox Summary Tab
#===================================================================================================
## Total Participant (TP)
TP <- tatally(dataset) %>% mutate(AttrVar="TP") 
names(TP) <- c("TotalVal","AttrVar")


## convert to integer and format it to have thousand seperator
TP <- formatC(as.integer(TP), format="f", big.mark=",", digits=0)

## Total variables (TV)
TV <- data.frame(length(dataset)) %>% mutate(AttrVar="TV") 
names(TV) <- c("TotalVal","AttrVar")

## convert to integer and format it to have thousand seperator
TV <- formatC(as.integer(TV), format="f", big.mark=",", digits=0)

## Total Country (TC)
TC <- data.frame(length(levels(factor(dataset$IP)))) %>% mutate(AttrVar="TC") 
names(TC) <- c("TotalVal","AttrVar")

## convert to integer and format it to have thousand seperator
TC <- formatC(as.integer(TC), format="f", big.mark=",", digits=0)

## Average time elapse(ATE) in seconds
ATE <- data.frame(mean(dataset$testelapse)) %>% mutate(AttrVar="ATE") 
names(ATE) <- c("TotalVal","AttrVar")

summaryData <- rbind(TP,TV,TC,ATE)


# Write CSV in R
write.csv(summaryData, file = "Data/summaryData.csv",row.names=FALSE)

#===================================================================================================
#     Map Data distribution Summary Tab
#===================================================================================================

## Country Participant Distribution (CPD)
CPD <- dataset %>% group_by(IP) %>% count()

# assign column name
names(CPD) <- c("Country","Total")

#convert data to data.frame
CPD <- as.data.frame(CPD)
#country.dat

# Write CSV in R
write.csv(CPD, file = "Data/countryData.csv",row.names=FALSE)


#joinCountryData2Map to plot the country base on joinCode
SPDF <- joinCountryData2Map( CPD
                             ,joinCode = "ISO2"
                             ,nameJoinColumn = "Country")

# spdf is a sp::SpatialPolygonsDataFrame
qpal <- colorNumeric(rev(viridis::plasma(15)),
                     SPDF$Total, n=15)

CPDMap <- leaflet(SPDF, options =
               leafletOptions(attributionControl = FALSE, minzoom=1.5)) %>%
  addPolygons(
    label=~stringr::str_c(
      NAME, ' ',
      formatC(Total, big.mark = ',', format='d')),
    labelOptions= labelOptions(direction = 'auto'),
    weight=1,color='#333333', opacity=1,
    fillColor = ~qpal(Total), fillOpacity = 1,
    highlightOptions = highlightOptions(
      color='#000000', weight = 2,
      bringToFront = TRUE, sendToBack = TRUE)
  ) %>%
  addLegend(
    "topright", pal = qpal, values = ~Total,
    title = htmltools::HTML("User Density"),
    opacity = 1 )



#===================================================================================================
#     Gender
#===================================================================================================
genderData <- table(dataset$gender)
names(genderData) <- c("Male","Female","Others")
genderData <- as.data.frame(genderData)
names(genderData) <- c("Gender","Total")

genderData <- genderData %>%
  group_by(Gender) %>%
  summarise(volume = sum(Total)) %>%
  mutate(share=volume/sum(volume)*100.0) %>%
  arrange(desc(volume))

# Write CSV in R
write.csv(genderData, file = "Data/genderData.csv",row.names=FALSE)

#===================================================================================================
#     OSRI finding
#===================================================================================================
osriData <- table(dataset$gender,dataset$cls)
names(osriData) <- c("")
osriData


#===================================================================================================
#     question distribution
#===================================================================================================
Q1 <- data.frame(table(dataset$Q1)) %>% mutate(Qstn=1)
Q2 <- data.frame(table(dataset$Q2)) %>% mutate(Qstn=2)
Q3 <- data.frame(table(dataset$Q3)) %>% mutate(Qstn=3)
Q4 <- data.frame(table(dataset$Q4)) %>% mutate(Qstn=4)
Q5 <- data.frame(table(dataset$Q5)) %>% mutate(Qstn=5)
Q6 <- data.frame(table(dataset$Q6)) %>% mutate(Qstn=6)
Q7 <- data.frame(table(dataset$Q7)) %>% mutate(Qstn=7)
Q8 <- data.frame(table(dataset$Q8)) %>% mutate(Qstn=8)
Q9 <- data.frame(table(dataset$Q9)) %>% mutate(Qstn=9)
Q10 <- data.frame(table(dataset$Q10)) %>% mutate(Qstn=10)
Q11 <- data.frame(table(dataset$Q11)) %>% mutate(Qstn=11)
Q12 <- data.frame(table(dataset$Q12)) %>% mutate(Qstn=12)
Q13 <- data.frame(table(dataset$Q13)) %>% mutate(Qstn=13)
Q14 <- data.frame(table(dataset$Q14)) %>% mutate(Qstn=14)
Q15 <- data.frame(table(dataset$Q15)) %>% mutate(Qstn=15)
Q16 <- data.frame(table(dataset$Q16)) %>% mutate(Qstn=16)
Q17 <- data.frame(table(dataset$Q17)) %>% mutate(Qstn=17)
Q18 <- data.frame(table(dataset$Q18)) %>% mutate(Qstn=18)
Q19 <- data.frame(table(dataset$Q19)) %>% mutate(Qstn=19)
Q20 <- data.frame(table(dataset$Q20)) %>% mutate(Qstn=20)
Q21 <- data.frame(table(dataset$Q21)) %>% mutate(Qstn=21)
Q22 <- data.frame(table(dataset$Q22)) %>% mutate(Qstn=22)
Q23 <- data.frame(table(dataset$Q23)) %>% mutate(Qstn=23)
Q24 <- data.frame(table(dataset$Q24)) %>% mutate(Qstn=24)
Q25 <- data.frame(table(dataset$Q25)) %>% mutate(Qstn=25)
Q26 <- data.frame(table(dataset$Q26)) %>% mutate(Qstn=26)
Q27 <- data.frame(table(dataset$Q27)) %>% mutate(Qstn=27)
Q28 <- data.frame(table(dataset$Q28)) %>% mutate(Qstn=28)
Q29 <- data.frame(table(dataset$Q29)) %>% mutate(Qstn=29)
Q30 <- data.frame(table(dataset$Q30)) %>% mutate(Qstn=30)
Q31 <- data.frame(table(dataset$Q31)) %>% mutate(Qstn=31)
Q32 <- data.frame(table(dataset$Q32)) %>% mutate(Qstn=32)
Q33 <- data.frame(table(dataset$Q33)) %>% mutate(Qstn=33)
Q34 <- data.frame(table(dataset$Q34)) %>% mutate(Qstn=34)
Q35 <- data.frame(table(dataset$Q35)) %>% mutate(Qstn=35)
Q36 <- data.frame(table(dataset$Q36)) %>% mutate(Qstn=36)
Q37 <- data.frame(table(dataset$Q37)) %>% mutate(Qstn=37)
Q38 <- data.frame(table(dataset$Q38)) %>% mutate(Qstn=38)
Q39 <- data.frame(table(dataset$Q39)) %>% mutate(Qstn=39)
Q40 <- data.frame(table(dataset$Q40)) %>% mutate(Qstn=40)
Q41 <- data.frame(table(dataset$Q41)) %>% mutate(Qstn=41)
Q42 <- data.frame(table(dataset$Q42)) %>% mutate(Qstn=42)
Q43 <- data.frame(table(dataset$Q43)) %>% mutate(Qstn=43)
Q44 <- data.frame(table(dataset$Q44)) %>% mutate(Qstn=44)

# combine the data
QSTN <- rbind(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18,Q19,Q20,Q21,Q22,Q23,Q24,Q25,Q26,Q27,Q28,Q29,Q30,Q31,Q32,Q33,Q34,Q35,Q36,Q37,Q38,Q39,Q40,Q41,Q42,Q43,Q44)

# remove unused dataset
rm(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18,Q19,Q20,Q21,Q22,Q23,Q24,Q25,Q26,Q27,Q28,Q29,Q30,Q31,Q32,Q33,Q34,Q35,Q36,Q37,Q38,Q39,Q40,Q41,Q42,Q43,Q44)

# Rename column
names(QSTN) <- c("Score","Total","Qstn")



# Write CSV in R
write.csv(QSTN, file = "Data/qstnData.csv",row.names=FALSE)


