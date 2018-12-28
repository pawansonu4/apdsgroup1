###########################################################################
#                                                                         #
# This is the main program to be executed, select the sites from which    #
# data is needed, select page numbers and then execute                    #
#                                                                         #
###########################################################################


# Below packages are required to run the program
library('openxlsx')
library('rvest')
library('lubridate')
library('tcltk')
library('dplyr')

# Select the sites from which we need news articles along with page number

########### Money Control ######################
moneycontrol<-'X'     # Keep the value as 'X' if we want data from money control else keep it blank
money_page_from<-1    # Page number from
money_page_to<-1      # Page number to [Max pages available on the site is 1200+]

########### Economies.com ######################
economies<-'X'       # Keep the value as 'X' if we want data from economies.com else keep it blank
econ_page_from<-1    # Page number from
econ_page_to<-5      # Page number to [Max pages available on the site is 517]

########### Economic time ######################
economictimes <- 'X' # Keep the value as 'X' if we want data from economic times else keep it blank
et_pg_from<-1        # Page number from
et_pg_to<-5          # Page number to ----- Keep max 50 as only 50 pages are available on the site

########### Enter filename where we want to save the data
filename<-'D://WebData.xlsx'

options(timeout= 9000000) 

########## Data frames
scraped_data <- data.frame(Source = '', Commodity = '' , Date = '', Title = '', Shortstory = '', stringsAsFactors = FALSE)
scraped_data_eco<-scraped_data
scraped_data_et<-scraped_data

######### If money control is selected then call the R script to get the data from moneycontrol.com
if(moneycontrol == 'X'){
  source('moneycontrol.R')
}

########## If economies is selected the call the R script to get the data from economies.com
if(economies == 'X'){
  source('Economies.R')
  scraped_data<-rbind(scraped_data,scraped_data_eco)
}

######### If economic times is selected then call the  R script to get the data from economic times
if(economictimes == 'X'){
  source('EconomicTimes.R')
  scraped_data<-rbind(scraped_data,scraped_data_et)  
}

######### Write the data to the file name specified
write.xlsx(scraped_data,filename,sheetName = "Data", row.names = F)
