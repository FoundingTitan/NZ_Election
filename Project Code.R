# Import Libraries

library(tidyverse)
library(readr)
library(ggplot2)
library(ggrepel)  
library(DataCombine)


# read files

files <- list.files(path = "/Users/aj/Documents/UoA/STATS 765 Statistical Learning for Computer Science/Project/Data/Main-Counts", pattern = "*.csv", full.names = T)
tbl <- sapply(files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")

f1 = read_csv(files[1])
age.file = read_csv(files[2])
area.file = read_csv(files[3])
ethnic.file = read_csv(files[4])
sex.file = read_csv(files[5])
year.file = read_csv(files[6])


# Separate 2006, 2013 and 2018 data
f1_2006 = as.data.frame(f1[f1$Year == 2006,])
f1_2013 = as.data.frame(f1[f1$Year == 2013,])
f1_2018 = as.data.frame(f1[f1$Year == 2018,])


# Check NULL (or "..C") values and remove those rows
f1_2006 = f1_2006[!(f1_2006$count == '..C'),]
f1_2013 = f1_2013[!(f1_2013$count == '..C'),]
f1_2018 = f1_2018[!(f1_2018$count == '..C'),]

#f1_2018$Num = seq(1:length(f1_2018$Year))
#f1_2018$Cval = apply(f1_2018, 1, function(r) any(r %in% c("..C")))

#ggplot(f1_2018, aes(x = f1_2018$Year, y = f1_2018$Num )) +
#  geom_tile(aes(fill= Cval)) +
#  geom_point(size = 3) +
#  theme(legend.position="bottom")
     


# Specify 'Age Code' from age.file that should be included in the data frame.
age.codes = c('01','02','03','04','05','06','07','08','09','10',
              '11','12','13','14','15','16','17','18','19','20','21')

f1_2006 = f1_2006 %>% filter(Age %in% age.codes )
f1_2013 = f1_2013 %>% filter(Age %in% age.codes )
f1_2018 = f1_2018 %>% filter(Age %in% age.codes )


# Specify Area from area.file that should be included in the data frame.
area.of.interest = c('Auckland|Mangere|Papakura|Mount Roskill|Manurewa|Manukau')

electorates.file = area.file %>%
  filter(grepl(area.of.interest,Description))



# Find and Replace values into the 3 data frames and then filter via Area.

# 2006 Data
f1_2006$Ethnic <- as.character(f1_2006$Ethnic)
f1_2006$Area<- as.character(f1_2006$Area)
f1_2006$Sex <- as.character(f1_2006$Sex)
f1_2006$Ethnic <- as.character(f1_2006$Ethnic)
f1_2006$count <- as.numeric(f1_2006$count)

f1_2006 = f1_2006 %>%
  FindReplace(Var = 'Ethnic', replaceData = ethnic.file,
              from = 'Code', to = 'Description', exact = TRUE) %>%
  FindReplace(Var = 'Sex', replaceData = sex.file,
              from = 'Code', to = 'Description', exact = TRUE) %>%
  FindReplace(Var = 'Age', replaceData = age.file,
              from = 'Code', to = 'Description', exact = TRUE) %>%
  FindReplace(Var = 'Area', replaceData = electorates.file,
              from = 'Code', to = 'Description', exact = TRUE)

f1_2006 = f1_2006 %>%
  filter(grepl(area.of.interest, Area))

f1_2006 = f1_2006[!(f1_2006$Ethnic == 'Total people stated'),]


# 2013 Data
f1_2013$Ethnic <- as.character(f1_2013$Ethnic)
f1_2013$Area<- as.character(f1_2013$Area)
f1_2013$Sex <- as.character(f1_2013$Sex)
f1_2013$Ethnic <- as.character(f1_2013$Ethnic)
f1_2013$count <- as.numeric(f1_2013$count)

f1_2013 = f1_2013 %>%
  FindReplace(Var = 'Ethnic', replaceData = ethnic.file,
                             from = 'Code', to = 'Description', exact = TRUE) %>%
  FindReplace(Var = 'Sex', replaceData = sex.file,
                             from = 'Code', to = 'Description', exact = TRUE) %>%
  FindReplace(Var = 'Age', replaceData = age.file,
                             from = 'Code', to = 'Description', exact = TRUE) %>%
  FindReplace(Var = 'Area', replaceData = electorates.file,
                             from = 'Code', to = 'Description', exact = TRUE)

f1_2013 = f1_2013 %>%
  filter(grepl(area.of.interest, Area))

f1_2013 = f1_2013[!(f1_2013$Ethnic == 'Total people stated'),]


# 2018 Data
f1_2018$Ethnic <- as.character(f1_2018$Ethnic)
f1_2018$Area<- as.character(f1_2018$Area)
f1_2018$Sex <- as.character(f1_2018$Sex)
f1_2018$Ethnic <- as.character(f1_2018$Ethnic)
f1_2018$count <- as.numeric(f1_2018$count)

f1_2018 = f1_2018 %>%
  FindReplace(Var = 'Ethnic', replaceData = ethnic.file,
              from = 'Code', to = 'Description', exact = TRUE) %>%
  FindReplace(Var = 'Sex', replaceData = sex.file,
              from = 'Code', to = 'Description', exact = TRUE) %>%
  FindReplace(Var = 'Age', replaceData = age.file,
              from = 'Code', to = 'Description', exact = TRUE) %>%
  FindReplace(Var = 'Area', replaceData = electorates.file,
              from = 'Code', to = 'Description', exact = TRUE)

f1_2018 = f1_2018 %>%
  filter(grepl(area.of.interest, Area))

f1_2018 = f1_2018 %>%
  filter(!(Ethnic == 'Total people stated'))


#Plot Individual Electorates

#f1_2018 = read.csv(file.choose())

mangere = f1_2018 %>%
  filter(grepl('Mangere', Area), Age != "other")

mangere_east = mangere %>%
  filter(Area == 'Mangere East')

mangere_east %>%
  filter(Ethnic == 'Asian') %>% #View()
  ggplot(aes(x = Age, y = count, color = Sex, shape = Sex)) +
  geom_point(size = 3) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.position="bottom") 
  #ylim(0,200)

papakura = f1_2013 %>%
  filter

# Write file(s)
write_csv(f1,file = "/Users/aj/Documents/UoA/STATS 765 Statistical Learning for Computer Science/Project/f1.csv")



