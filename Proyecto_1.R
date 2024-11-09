# INSTALL LIBRARIES
install.packages("arules")
install.packages("haven")
install.packages("dplyr")
install.packages("ggplot2")

# LOAD LIBRARIES
library(haven)
library(arules)
library(dplyr)
library(ggplot2)


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

names(data_2017)[names(data_2017) == "GRUPETNICO"] <- "PPERTENENCIA"

data_2017$PPERTENENCIA <- labelled(data_2017$PPERTENENCIA, labels = attr(data_2022$PPERTENENCIA, "labels"))
data_2017$DEPTORESIDEN <- labelled(data_2017$DEPTORESIDEN, labels = attr(data_2022$DEPTORESIDEN, "labels"))
data_2017$MUNIRESIDEN <- labelled(data_2017$MUNIRESIDEN, labels = attr(data_2022$MUNIRESIDEN, "labels"))

names(data_2016)[names(data_2016) == "GRUPETNICO"] <- "PPERTENENCIA"

data_2016$PPERTENENCIA <- labelled(data_2016$PPERTENENCIA, labels = attr(data_2022$PPERTENENCIA, "labels"))
data_2016$DEPTORESIDEN <- labelled(data_2016$DEPTORESIDEN, labels = attr(data_2022$DEPTORESIDEN, "labels"))
data_2016$MUNIRESIDEN <- labelled(data_2016$MUNIRESIDEN, labels = attr(data_2022$MUNIRESIDEN, "labels"))

names(data_2015)[names(data_2015) == "GRUPETNICO"] <- "PPERTENENCIA"

data_2015$PPERTENENCIA <- labelled(data_2015$PPERTENENCIA, labels = attr(data_2022$PPERTENENCIA, "labels"))
data_2015$DEPTORESIDEN <- labelled(data_2015$DEPTORESIDEN, labels = attr(data_2022$DEPTORESIDEN, "labels"))
data_2015$MUNIRESIDEN <- labelled(data_2015$MUNIRESIDEN, labels = attr(data_2022$MUNIRESIDEN, "labels"))


data_2022_2015 <- bind_rows(data_2022, data_2021, data_2020, data_2019, data_2018, data_2017, data_2016, data_2015)
data_2014_2009 <- bind_rows(data_2014, data_2013, data_2012, data_2011, data_2010, data_2009)

# APPLY APRIORI ALGORITHM FOR EACH DATASET.

# RULE NO.1

apriori_rules_2022_2015 <- apriori(data_2022_2015, parameter = list(support=0.0025, confidence=0.5))
df_2022_2015 <- as(apriori_rules_2022_2015, "data.frame")
write.csv(df_2022_2015, "C:\\files\\results\\result_apriori_r1.csv")

# RULE NO.2 - RULES FOR DEPTORESIDEN EQUAL TO GUATEMALA - región occidente
data_2022_2015_r2 <- subset(data_2022_2015, DEPTORESIDEN %in% c(9, 7, 13, 12, 14, 8))

apriori_rules_r2 <- apriori(data_2022_2015_r2, parameter = list(support=0.0025, confidence=0.3))
df_r2 <- as(apriori_rules_r2, "data.frame")
write.csv(df_r2, "C:\\files\\results\\result_apriori_r2.csv")


# RULE NO.3 - lepra
data_2022_2015_r3 <- subset(data_2022_2015, CAUFIN %in% c("A30","A300","A301","A302","A303","A304","A305","A308","A309"))

apriori_rules_r3 <- apriori(data_2022_2015_r3, parameter = list(support=0.05, confidence=0.3))
df_r3 <- as(apriori_rules_r3, "data.frame")
write.csv(df_r3, "C:\\files\\results\\result_apriori_r3.csv")

# RULE NO.4 - general
apriori_rules_2014_2009 <- apriori(data_2014_2009, parameter = list(support=0.0025, confidence=0.3))
df_2014_2009 <- as(apriori_rules_2014_2009, "data.frame")
write.csv(df_2014_2009, "C:\\files\\results\\result_apriori_r4.csv")


# RULE NO.5 - Departamento de Guatemala tipo de consulta emergencia
data_2014_2009_r5 <- subset(data_2014_2009, DEPTORESIDEN == 1)
data_2014_2009_r5 <- data_2014_2009_r5[, !(names(data_2014_2009_r5) %in% c("DEPTORESIDEN"))]

apriori_rules_r5 <- apriori(data_2014_2009_r5, parameter = list(support=0.0025, confidence=0.2))
df_r5 <- as(apriori_rules_r5, "data.frame")
write.csv(df_r5, "C:\\files\\results\\result_apriori_r5.csv")


# APPLY FP-GROWTH ALGORITHM FOR EACH DATASET.

# RULE NO.1

data_fp_growth_r1 <- subset(data_2022_2015, TC %in% c(3,4))
data_fp_growth_r1 <- subset(data_fp_growth_r1, MUNIRESIDEN = 0101)
data_fp_growth_r1 <- data_fp_growth_r1[, !(names(data_fp_growth_r1) %in% c("DEPTORESIDEN"))]
data_fp_growth_r1 <- data_fp_growth_r1[, !(names(data_fp_growth_r1) %in% c("MUNIRESIDEN"))]


fp_growth_rules_2022_2015 <- fim4r(data_fp_growth_r1, method = "fpgrowth", target="rules", supp = 0.0025, conf = 0.3)
df_fp_growth_2022_2015 <- as(fp_growth_rules_2022_2015, "data.frame")
write.csv(df_fp_growth_2022_2015, "C:\\files\\results\\fp-growth_result_r1.csv")


# RULE NO.2 - RULES FOR DEPTORESIDEN EQUAL - departamento de guatemala 
data_fp_r2 <- subset(data_2022_2015, MUNIRESIDEN %in% c("0101","0102","0103","0104","0105","0106","0107","0108","0109","0110","0111","0112","0113","0114","0115","0116","0117"))
data_fp_r2 <- data_fp_r2[, !(names(data_fp_r2) %in% c("DEPTORESIDEN"))]

fp_growth_rules_r2 <- fim4r(data_fp_r2, method = "fpgrowth", target="rules", supp = 0.0025, conf = 0.3)
df_fp_growth_r2<- as(fp_growth_rules_r2, "data.frame")
write.csv(df_fp_growth_r2, "C:\\files\\results\\fp-growth_result_r2.csv")

# RULE NO.3 - Municipio de Villa Nueva

data_fp_growth_r3 <- subset(data_2022_2015, MUNIRESIDEN = 0115)
data_fp_growth_r3 <- data_fp_growth_r3[, !(names(data_fp_growth_r3) %in% c("MUNIRESIDEN"))]
data_fp_growth_r3 <- data_fp_r2[, !(names(data_fp_growth_r3) %in% c("DEPTORESIDEN"))]

fp_growth_rules_r3 <- fim4r(data_fp_growth_r3, method = "fpgrowth", target="rules", supp = 0.0025, conf = 0.2)
df_fp_growth_r3 <- as(fp_growth_rules_r3, "data.frame")
write.csv(df_fp_growth_r3, "C:\\files\\results\\fp-growth_result_r3.csv")


# RULE NO.4 - General
fp_growth_rules_r4 <- fim4r(data_2014_2009, method = "fpgrowth", target="rules", supp = 0.0025, conf = 0.2)
df_fp_growth_r4 <- as(fp_growth_rules_r4, "data.frame")
write.csv(df_fp_growth_r4, "C:\\files\\results\\fp-growth_result_r4.csv")


# RULE NO.5 - Región occidente

data_2014_2009_r5 <- subset(data_2014_2009, DEPTORESIDEN %in% c(15, 19, 12, 21, 20, 22, 6))
data_2014_2009_r5 <- data_2014_2009_r5[, !(names(data_2014_2009_r5) %in% c("MUNIRESIDEN"))]

fp_growth_rules_r5 <- fim4r(data_2014_2009_r5, method = "fpgrowth", target="rules", supp = 0.0025, conf = 0.2)
df_fp_growth_r5<- as(fp_growth_rules_r5, "data.frame")
write.csv(df_fp_growth_r5, "C:\\files\\results\\fp-growth_result_r5.csv")

# K-MEANS

# No.1

data_2022_2015$EDAD[data_2022_2015$EDAD == 999] <- 99

data_km1 <- subset(data_2022_2015, CAUFIN %in% c("G20X"))
data_km1 <- data_km1[, !(names(data_km1) %in% c("CAUFIN"))]

data_km1_apriori <- data_km1

cluster1 <- kmeans(data_km1_apriori, centers=4)

ggplot(data_km1_apriori, aes(x = DEPTORESIDEN, y = EDAD, color = as.factor(cluster1$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(cluster1$centers), aes(x=DEPTORESIDEN, y = EDAD), color = "black", size=4, shape=17)+
  labs(title = "Departamento vs Edad")+
  theme_minimal()

# No.2

data_km2 <- subset(data_2022_2015, CAUFIN %in% 
    c(
        "A30",
        "A300",
        "A301",
        "A302",
        "A303",
        "A304",
        "A305",
        "A308",
        "A309"
    )
)
data_km2 <- data_km2[, !(names(data_km2) %in% c("CAUFIN"))]

data_km2_apriori <- data_km2

cluster2 <- kmeans(data_km2_apriori, centers=4)

ggplot(data_km2_apriori, aes(x = AÑO, y = EDAD, color = as.factor(cluster2$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(cluster2$centers), aes(x=AÑO, y = EDAD), color = "black", size=4, shape=17)+
  labs(title = "AÑO vs Edad")+
  theme_minimal()



# No.3 - Miopia y Astigmatismo

data_km3 <- subset(data_2022_2015, CAUFIN %in% 
                     c(
                       "H521",
                       "H522"
                     )
                   & DEPTORESIDEN != 99
)


data_km3 <- data_km3[, !(names(data_km3) %in% c("CAUFIN"))]

data_km3_apriori <- data_km3

cluster3 <- kmeans(data_km3_apriori, centers=4)

ggplot(data_km3_apriori, aes(x = AÑO, y = DEPTORESIDEN, color = as.factor(cluster3$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(cluster3$centers), aes(x=AÑO, y = DEPTORESIDEN), color = "black", size=4, shape=17)+
  labs(title = "Año vs Departamento")+
  theme_minimal()

# No.4 - Insomnio

data_km4 <- subset(data_2022_2015, CAUFIN %in% 
                     c(
                       "G470",
                       "F510"
                     )
)


data_km4 <- data_km4[, !(names(data_km4) %in% c("CAUFIN"))]

data_km4_apriori <- data_km4

cluster4 <- kmeans(data_km4_apriori, centers=4)

ggplot(data_km4_apriori, aes(x = DEPTORESIDEN, y = EDAD, color = as.factor(cluster4$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(cluster4$centers), aes(x=DEPTORESIDEN, y = EDAD), color = "black", size=4, shape=17)+
  labs(title = "Departamento vs Edad")+
  theme_minimal()


