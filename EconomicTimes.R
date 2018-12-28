#############################################################################
#                                                                           #
# Do not execute this script directly, execute it from Main.R               #
# To understand the working, read the comments in moneycontrol.R            #
#                                                                           #
#############################################################################


page_from<-et_pg_from
page_to<-et_pg_to
pages <- page_from:page_to

# In this function, we will parse the webpage tags to get the new sections
read_data <- function(newsitem = '',page_url = '', page = '' ,srno = '',webpage)
{
  #  webpage <- read_html(page_url)
  datefield <- paste(newsitem,'.date-format')  # html tag date-format has the date 
  titlefield <- paste(newsitem,'h3')   # html tag h3 has the news title
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
      return_data_et <- data.frame(Source = 'Economic Times' , Commodity = commodities[i],Date = cdate, Title = ctitle, Shortstory = ctext)    
      return(return_data_et)
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
    newsitem <- paste('.eachStory:nth-child(',i,')', sep = '')
    trimws(newsitem)
    # Pass the html page to read_data to parse the data as per tags
    de <- read_data(newsitem, url,page,i,webpage)
    if (!is.null(de))
    {
      scraped_data_et<-rbind( scraped_data_et,de) 
    }
  }
  return(scraped_data_et)
}

read_page_mc<- function(page_no)
{
  done<-0
  pb <- tkProgressBar(title = "Economic Times", min = 0,
                      max = total, width = 300)
  # One by one below code will execute for all the pages which we had specified
  for(i in page_no){
    # In the below variable url, each time value of i will change and accordingly url of each page
    # will be formed as https://www.moneycontrol.com/news/commodities-news-94.html/page-1/
    # https://www.moneycontrol.com/news/commodities-news-94.html/page-2/ and so on
    url <- paste('https://economictimes.indiatimes.com/lazyloadlistnew.cms?msid=50991753&curpg=',i,'&img=0',sep = '')
    # Here we will pass the page URL to function read_list which will read the page contents
    page_data<-read_list(url,i) # data returned from the page will be stored in page_data and then we will bind it to scrapped_data data frame
    scraped_data_et<-rbind( scraped_data_et,page_data) 
    Sys.sleep(0.1)
    done<-done + 1
    setTkProgressBar(pb, i, label=paste( round(done/total*100, 0),
                                         "% done"))
  }  
  close(pb)
  return(scraped_data_et)
}

total <- page_to - page_from + 1.
# This is where the execution will start
# We are passing here pages which needs to be scraped to the function
scraped_data_et <- read_page_mc(pages)

#scraped_data<-scraped_data[!is.null(scraped_data$Title),]
scraped_data_et<-scraped_data_et[scraped_data_et$Title != '',!is.null(scraped_data_et$Title)]