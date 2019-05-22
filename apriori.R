# The required libraries
# Here is the libraries you need to apply an apriori algorithm
#1. arules
#Provides the infrastructure for representing, manipulating and analyzing transaction
#data and patterns (frequent itemsets and association rules).
#
#2. arulesViz
#Extends package 'arules' with various visualization techniques for association rules
#and item-sets. The package also includes several interactive visualizations 
#for rule exploration.
#
#3. tidyverse
#The tidyverse is an opinionated collection of R packages designed for data science
#
#4. readxl
#Read Excel Files in R
#
#5. plyr
#Tools for Splitting, Applying and Combining Data
#
#6. ggplot2
#Create graphics and charts
#
#7. knitr
#Dynamic Report generation in R
#
#8. lubridate
#Lubridate is an R package that makes it easier to work with dates and times.
#
#9. dplyr
#provides a flexible grammar of data manipulation. It's the next iteration of plyr,
#focused on tools for working with data frames (hence the d in the name).

#------------------------------------------------------------------------------------
#-----------------------------DATA PREPARATION---------------------------------------
#------------------------------------------------------------------------------------

#1. Install the required packages and libraries
#install and load package arules
#install.packages("arules")
library(arules)
#install and load arulesViz
#install.packages("arulesViz")
library(arulesViz)
#install and load tidyverse
#install.packages("tidyverse")
library(tidyverse)
#install and load readxml
#install.packages("readxml")
library(readxl)
#install and load knitr
#install.packages("knitr")
library(knitr)
#load ggplot2 as it comes in tidyverse
library(ggplot2)
#install and load lubridate
#install.packages("lubridate")
library(lubridate)
#install and load plyr
#install.packages("plyr")
library(plyr)
library(dplyr)

#2. Load xlsx dataset
#read excel into R dataframe
toko<-read_excel("~/R Files/Uji Coba/Apriori/Dataset/TokoATK.xlsx")
View(toko)
glimpse(toko)
#checking if value is missing
is.na(toko)

#3. Complete.cases(data) will return a logical vector indicating which rows 
#have no missing values. Then use the vector to get only rows that are complete 
#using retail[,].
#Return a logical vector indicating which cases are complete
#i.e., have no missing values.
toko<-toko[complete.cases(toko),]
glimpse(toko)

#4. Mutate function is from dplyr package. 
#It is used to edit or add new columns to dataframe. Here tkket column is being
#converted to factor column. as.factor function converts column to factor column.
#%>% is an operator with which you may pipe values to another function or expression
toko<-toko %>% mutate(tkket = as.factor(tkket))
glimpse(toko)

#5. The dataset will split into a column based on tkno and ttgl
#and separated by ";"
DataTransaksi <- ddply(toko,c("tkno","ttgl"),
                       function(df1)paste(df1$tkket,
                                          collapse = ";"))
View(DataTransaksi)
glimpse(DataTransaksi)

#6. Next, as tkkode, tkqty1, tkhjual, and tkTotal will not be of 
#any use in the rule mining, you can set them to NULL
DataTransaksi$tkno <- NULL
DataTransaksi$ttgl <- NULL
colnames(DataTransaksi)<- c("Barang")
glimpse(DataTransaksi)
View(DataTransaksi)

#7. Next, you have to store this transaction data into a .csv 
#(Comma Separated Values) file.  For this, write.csv().
write.csv(DataTransaksi,"market_basket_DataTransaksi.csv", quote = FALSE, row.names = FALSE)
#DataTransaksi: Data to be written in DataTransaksi variable
#market_basket_DataTransaksi.csv: The file name to be written
#quote: If TRUE it will surround character or factor column with double quotes. If FALSE nothing will be quoted.
#row.names: either a logical value indicating whether the row names of x are to be written along with x, 
#or a character vector of row names to be written.

#------------------------------------------------------------------------------------
#-----------------------------APRIORI PROCESS----------------------------------------
#------------------------------------------------------------------------------------

#8. Next, you have to load this transaction data into an object of the transaction class. 
#This is done by using the R function read.transactions of the arules package.
#The following line of code will take transaksi data file market_basket_DataTransaksi.csv
#which is in basket format and convert it into an object of the transaction class.
transaksi <- read.transactions('market_basket_DataTransaksi.csv', format = 'basket', sep = ';')
#summary is a generic function used to produce result summaries of the results of various model fitting functions
summary(transaksi)

#9. You can generate an itemFrequencyPlot to create an item Frequency Bar Plot
#to view the distribution of objects based on itemMatrix
#Create an item frequency plot for the top 10 frequent items
if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}
itemFrequencyPlot(transaksi,topN=10,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

#10. Next step is to mine the rules using the APRIORI algorithm.
#The function apriori() is from package arules.
#Treshold: Support as 0.001, confidence as 0.8.
aturan_asosiasi <- apriori(transaksi, parameter = list(supp=0.001, conf=0.8, maxlen=10))
summary(aturan_asosiasi)
#Sort the variable by "lift"
aturan_asosiasi <- sort(aturan_asosiasi, by="lift")
inspect(aturan_asosiasi)

#11. Plotting
plot(aturan_asosiasi, method="graph", engine = "htmlwidget")
