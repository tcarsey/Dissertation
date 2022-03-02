#simulated data stuff
install.packages('simstudy')
install.packages("RedditExtractoR")
library(RedditExtractoR)
install.packages('xlsx')
library(xlsx)

## Not run:
example_attr = reddit_content(URL="https://www.reddit.com/r/transgamers/", wait_time=2)
example_data = get_reddit(example_attr, search_terms="identity")
## End(Not run)
View(example_data)



#Return relevant reddit URL
example = reddit_urls(search_terms = 'OCD', regex_filter = "", subreddit = '',
            cn_threshold = 1, page_threshold = 1, sort_by = "relevance",
            wait_time = 2)


#Runs start here
searchterms <- read.csv('searchtermsaskhr8.csv', header=FALSE)

searchterms <- as.matrix(searchterms)

datalist = list()

for (i in 1:length(searchterms)[1]){
  dat <- get_reddit(search_terms= searchterms[i], subreddit="ROCD", page_threshold= 99, wait_time= 2)
  dat[[i]] <- dat
  datalist[[i]] <- dat[[i]]
}


askhr8 = do.call(rbind, datalist)

write.csv(askhr8, file="askhr8.csv")


#Making comment graphics

my_url = "https://www.reddit.com/r/OCD/"
url_data = reddit_content(my_url)
graph_object = construct_graph(url_data)


#Getting data attributes from search query


get_reddit(search_terms = NA, regex_filter = "", subreddit = NA,
           cn_threshold = 1, page_threshold = 1, sort_by = "comments",
           wait_time = 2)

reddit_data = get_reddit(search_terms = "trans",subreddit = "video game",cn_threshold=10)





