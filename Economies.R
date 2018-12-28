#############################################################################
#                                                                           #
# Do not execute this script directly, execute it from Main.R               #
# To understand the working, read the comments in moneycontrol.R            #
#                                                                           #
#############################################################################



# Below packages are required for running this program
# install.packages('xlsx')
# install.packages('rvest')

library('rvest')
library('lubridate')
library('tcltk')

options(timeout= 9000000)


# Specify here pages for which scraping needs to be done
# News articles related to commodoties is arranged page wise in website
# URL - https://www.moneycontrol.com/news/commodities-news-94.html/page-1/ 
page_from<-econ_page_from
page_to<-econ_page_to
pages <- page_from:page_to


# In this function, we will parse the webpage tags to get the news sections
read_data <- function(newsitem = '',page_url = '', page = '' ,srno = '',webpage)
{
  #  webpage <- read_html(page_url)
  datefield <- paste(newsitem,'.item-date')  # html tag span has the date 
  titlefield <- paste(newsitem,'.brief')   # html tag h2 has the news title
  shorttextfield <- paste(newsitem,'.arial_12') # html tag p has the news story
  
  titles_html <- html_nodes(webpage,titlefield)
  titles_data <- html_text(titles_html)
  titles_data
  
  date_html <- html_nodes(webpage,datefield)
  date_data <- html_text(date_html)
  date_data  
  
  body_html <- html_nodes(webpage,shorttextfield)
  body_data <- html_text(body_html)
  body_data   
  cdate<- ''
  ctitle <- ''
  ctext <- ''
  cdate <- date_data[1]
  date_data<-unlist(strsplit(cdate, "\\("))
  cdate<-date_data[1]
  ctitle <- body_data[1]
  ctext <- body_data[2]
  
  # Here we specify the commodities for which we want the news articles
  commodities <- c('gold','silver','oil','natural gas')
  as.character(commodities)
  
  as.character(ctitle)
  
  for (i in 1:length(commodities)){
    return2 <- grepl(commodities[i],ctitle,ignore.case = TRUE)
    if ( return2 == TRUE ) {
      cdate<-format(as.Date(ymd_hms(cdate)),"%d.%m.%Y")
      return_data_eco <- data.frame( Source = 'Economies.com',Commodity = commodities[i], Date = cdate, Title = ctitle, Shortstory = ctext)    
      return(return_data_eco)
      break()
    }
  }
}

# This function will read the news item of the web page specified in url
read_list<-function(url,page)
{
  list_items <- 1:30
  # This will read the html page and store it in variable webpage
  webpage <- read_html(url)
  for(i in list_items)
  {
    newsitem <- paste('.list-row:nth-child(',i,')', sep = '')
    trimws(newsitem)
    # Pass the html page to read_data to parse the data as per tags
    de <- read_data(newsitem, url,page,i,webpage)
    if (!is.null(de))
    {
      scraped_data_eco<-rbind( scraped_data_eco,de) 
    }
  }
  return(scraped_data_eco)
}

read_page_mc<- function(page_no,total)
{
  done<-0
  pb <- tkProgressBar(title = "Economies.com", min = 0,
                      max = total, width = 300)
  # One by one below code will execute for all the pages which we had specified
  for(i in page_no){
    # In the below variable url, each time value of i will change and accordingly url of each page
    # will be formed as https://www.moneycontrol.com/news/commodities-news-94.html/page-1/
    # https://www.moneycontrol.com/news/commodities-news-94.html/page-2/ and so on
    url <- paste('https://www.economies.com/commodities/news?page=',i,sep = '')
    # Here we will pass the page URL to function read_list which will read the page contents
    page_data<-read_list(url,i) # data returned from the page will be stored in page_data and then we will bind it to scrapped_data data frame
    scraped_data_eco<-rbind( scraped_data_eco,page_data) 
    Sys.sleep(0.1)
    done<-done + 1
    setTkProgressBar(pb, i, label=paste( round(i/total*100, 0),
                                         "% done"))
  }  
  close(pb)
  return(scraped_data_eco)
}

total <- page_to - page_from + 1.

# This is where the execution will start
# We are passing here pages which needs to be scraped to the function
scraped_data_eco <- read_page_mc(pages, total)

#scraped_data<-scraped_data[!is.null(scraped_data$Title),]
scraped_data_eco<-scraped_data_eco[scraped_data_eco$Title != '',]
