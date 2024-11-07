# INSTALL LIBRARIES
install.packages("haven")
install.packages("dplyr")

# LOAD LIBRARIES
library(haven)
library(arules)
library(dplyr)


# LOAD ALL DATA BY YEAR
data_2022 <- read_sav('C:\\files\\2022.sav')
data_2021 <- read_sav('C:\\files\\2021.sav')
data_2020 <- read_sav('C:\\files\\2020.sav')
data_2019 <- read_sav('C:\\files\\2019.sav')
data_2018 <- read_sav('C:\\files\\2018.sav')
data_2017 <- read_sav('C:\\files\\2017.sav')
data_2016 <- read_sav('C:\\files\\2016.sav')
data_2015 <- read_sav('C:\\files\\2015.sav')
data_2014 <- read_sav('C:\\files\\2014.sav')
data_2013 <- read_sav('C:\\files\\2013.sav')
data_2012 <- read_sav('C:\\files\\2012.sav')
data_2011 <- read_sav('C:\\files\\2011.sav')
data_2010 <- read_sav('C:\\files\\2010.sav')
data_2009 <- read_sav('C:\\files\\2009.sav')


# SOLVE CONFLICTS WITH LABELS ON COLUMNS: PPERTENENCIA, DEPTORESIDEN, MUNIRESIDEN
data_2019$PPERTENENCIA <- labelled(data_2019$PPERTENENCIA, labels = attr(data_2022$PPERTENENCIA, "labels"))
data_2019$DEPTORESIDEN <- labelled(data_2019$DEPTORESIDEN, labels = attr(data_2022$DEPTORESIDEN, "labels"))
data_2019$MUNIRESIDEN <- labelled(data_2019$MUNIRESIDEN, labels = attr(data_2022$MUNIRESIDEN, "labels"))

data_2018$PPERTENENCIA <- labelled(data_2018$PPERTENENCIA, labels = attr(data_2022$PPERTENENCIA, "labels"))
data_2018$DEPTORESIDEN <- labelled(data_2018$DEPTORESIDEN, labels = attr(data_2022$DEPTORESIDEN, "labels"))
data_2018$MUNIRESIDEN <- labelled(data_2018$MUNIRESIDEN, labels = attr(data_2022$MUNIRESIDEN, "labels"))


# APPLY APRIORI ALGORITHM FOR EACH DATASET.

data_2022_2018 <- bind_rows(data_2022, data_2021, data_2020, data_2019, data_2018)
apriori_rules_2022_2018 <- apriori(data_2022_2018, parameter = list(support=0.01, confidence=0.5))
df_2022_2018 <- as(apriori_rules_2022_2018, "data.frame")
write.csv(df_2022_2018, "C:\\files\\results\\result_2022_2018.csv")


data_2022_2020 <- bind_rows(data_2022, data_2021, data_2020)
apriori_rules_2022_2020 <- apriori(data_2022_2020, parameter = list(support=0.03, confidence=0.5))
df_2022_2020 <- as(apriori_rules_2022_2020, "data.frame")
write.csv(df_2022_2020, "C:\\files\\results\\result_2022_2020.csv")


data_2019_2018 <- bind_rows(data_2019, data_2018)
apriori_rules_2019_2018 <- apriori(data_2019_2018, parameter = list(support=0.03, confidence=0.5))
df_2019_2018 <- as(apriori_rules_2019_2018, "data.frame")
write.csv(df_2019_2018, "C:\\files\\results\\result_2019_2018.csv")


data_2016_2015 <- bind_rows(data_2016, data_2015)
apriori_rules_2016_2015 <- apriori(data_2016_2015, parameter = list(support=0.03, confidence=0.5))
df_2016_2015 <- as(apriori_rules_2016_2015, "data.frame")
write.csv(df_2016_2015, "C:\\files\\results\\result_2016_2015.csv")


data_2014_2009 <- bind_rows(data_2014, data_2013, data_2012, data_2011, data_2010, data_2009)
apriori_rules_2014_2009 <- apriori(data_2014_2009, parameter = list(support=0.03, confidence=0.5))
df_2014_2009 <- as(apriori_rules_2014_2009, "data.frame")
write.csv(df_2014_2009, "C:\\files\\results\\result_2014_2009.csv")

