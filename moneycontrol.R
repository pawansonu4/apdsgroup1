#############################################################################
#                                                                           #
# Do not execute this script directly, execute it from Main.R               #
#                                                                           #
#############################################################################


# Specify here pages for which scraping needs to be done -- Page number range is 
#      as specified as Main.R
# News articles related to commodoties is arranged page wise in website
# URL - https://www.moneycontrol.com/news/commodities-news-94.html/page-1/ 
page_from<-money_page_from    
page_to<-money_page_to
pages <- page_from:page_to
total <- page_to - page_from + 1.

# In this function, we will parse the webpage tags to get the news sections
read_data <- function(newsitem = '',page_url = '', page = '' ,srno = '',webpage)
{
  #  webpage <- read_html(page_url)
  datefield <- paste(newsitem,'span')  # html tag span has the date 
  titlefield <- paste(newsitem,'h2')   # html tag h2 has the news title
  shorttextfield <- paste(newsitem,'p') # html tag p has the news story
  
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
  ctitle <- titles_data[1]
  ctext <- body_data[1]
  
  # Here we specify the commodities for which we want the news articles
  commodities <- c('gold','silver','oil','natural gas')
  as.character(commodities)
  
  as.character(ctitle)
  
  for (i in 1:length(commodities)){
    return2 <- grepl(commodities[i],ctitle,ignore.case = TRUE)
    if ( return2 == TRUE ) {
      cdate<-format(as.Date(mdy_hm(cdate)),"%d.%m.%Y")
      return_data <- data.frame(Source = 'Money Control',Commodity = commodities[i], Date = cdate, Title = ctitle, Shortstory = ctext)    
      return(return_data)
      break()
    }
  }
}

# This function will read the news item of the web page specified in url
read_list<-function(url,page)
{
  # Each page has 25 news items as per the html page
  list_items <- 0:25
  # This will read the html page and store it in variable webpage
  webpage <- read_html(url)
  for(i in list_items)
  {
    newsitem <- paste('#newslist-',i, sep = '')   # As per the html page, news item list is arranged as #newlist-1 and so on
    trimws(newsitem)
    # Pass the html page to read_data to parse the data as per tags
    de <- read_data(newsitem, url,page,i,webpage)
    if (!is.null(de))
    {
      scraped_data<-rbind( scraped_data,de) 
    }
  }
  return(scraped_data)
}

read_page_mc<- function(page_no)
{
  pb <- tkProgressBar(title = "Money Control", min = 0,
                      max = total, width = 300)
  done<-0
  # One by one below code will execute for all the pages which we had specified
  for(i in page_no){
    # In the below variable url, each time value of i will change and accordingly url of each page
    # will be formed as https://www.moneycontrol.com/news/commodities-news-94.html/page-1/
    # https://www.moneycontrol.com/news/commodities-news-94.html/page-2/ and so on
    url <- paste('https://www.moneycontrol.com/news/commodities-news-94.html/page-',i,'/',sep = '')
    # Here we will pass the page URL to function read_list which will read the page contents
    page_data<-read_list(url,i) # data returned from the page will be stored in page_data and then we will bind it to scrapped_data data frame
    scraped_data<-rbind( scraped_data,page_data) 
    done<-done + 1
    ## For the progress bar
    setTkProgressBar(pb, done, label=paste( round(done/total*100, 0),
                                            "% done"))
  }  
  close(pb)  ### Close the progress bar once done
  return(scraped_data)
}


# This is where the execution will start
# We are passing here pages which needs to be scraped to the function
scraped_data <- read_page_mc(pages)

# Clean the data
scraped_data<-scraped_data[scraped_data$Commodity != '',]
