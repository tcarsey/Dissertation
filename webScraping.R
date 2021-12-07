#simulated data stuff
install.packages('simstudy')
install.packages("RedditExtractoR")
library(RedditExtractoR)


install.packages('xlsx')
library(xlsx)


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



## Not run:
example_attr = reddit_content(URL="https://www.reddit.com/r/transgamers/", wait_time=2)
example_data = get_reddit(example_attr, search_terms="identity")
## End(Not run)
View(example_data)



#Return relevant reddit URL
example = reddit_urls(search_terms = 'OCD', regex_filter = "", subreddit = '',
            cn_threshold = 1, page_threshold = 1, sort_by = "relevance",
            wait_time = 2)


#Pulling relevant URLs
example_urls = reddit_urls(search_terms="trans")
example_urls


#Pulling data
example_data3 = get_reddit(search_terms="transgender", subreddit = 'game')
View(example_data3)

#web-scrape for term WORK
DataSet_work <- get_reddit(search_terms= 'work', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_work, file="dataset_work.csv")

#web-scrape for term JOB
DataSet_job <- get_reddit(search_terms= 'job', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_job, file="dataset_job.csv")

#web-scrape for term EMPLOY
DataSet_employ <- get_reddit(search_terms= 'employ', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_employ, file="dataset_cowork.csv")

#web-scrape for term COWORK
DataSet_cowork <- get_reddit(search_terms= 'cowork', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_cowork, file="dataset_cowork.csv")

#web-scrape for term MANAGER
DataSet_manager <- get_reddit(search_terms= 'manager', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_manager, file="dataset_manager.csv")

#web-scrape for term EXPOSE
DataSet_expose <- get_reddit(search_terms= 'expose', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_expose, file="dataset_expose.csv")

#web-scrape for term BOSS
DataSet_boss2 <- get_reddit(search_terms= 'boss', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_boss2, file="dataset_boss2.csv")

#web-scrape for term DISCLOSE
DataSet_disclose <- get_reddit(search_terms= 'disclose', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_disclose, file="dataset_disclose.csv")

#web-scrape for term MONITOR
DataSet_monitor <- get_reddit(search_terms= 'monitor', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_monitor, file="dataset_monitor.csv")

#web-scrape for term LOOKOUT
DataSet_lookout <- get_reddit(search_terms= 'lookout', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_lookout, file="dataset_lookout.csv")

#web-scrape for term PEER
DataSet_peer <- get_reddit(search_terms= 'peer', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_peer, file="dataset_peer.csv")

#web-scrape for term CAREER
DataSet_career <- get_reddit(search_terms= 'career', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_career, file="dataset_career.csv")

#web-scrape for term TalkAboutMe
DataSet_talkaboutme <- get_reddit(search_terms= 'talk about me', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_talkaboutme, file="dataset_talkaboutme.csv")

#web-scrape for term REVEAL
DataSet_reveal <- get_reddit(search_terms= 'reveal', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_reveal, file="dataset_reveal.csv")

#web-scrape for term CONFESS
DataSet_confess <- get_reddit(search_terms= 'confess', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_confess, file="dataset_confess.csv")

#web-scrape for term LEAK
DataSet_leak <- get_reddit(search_terms= 'leak', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_leak, file="dataset_leak.csv")

#web-scrape for term CONCEAL
DataSet_conceal <- get_reddit(search_terms= 'conceal', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_conceal, file="dataset_conceal.csv")

#web-scrape for term HIDE
DataSet_hide <- get_reddit(search_terms= 'hide', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_hide, file="dataset_hide.csv")

#web-scrape for term PUBLIC
DataSet_public <- get_reddit(search_terms= 'public', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_public, file="dataset_public.csv")

#web-scrape for term ADMIT
DataSet_admit <- get_reddit(search_terms= 'admit', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_admit, file="dataset_admit.csv")

#web-scrape for term DIVULGE
DataSet_divulge <- get_reddit(search_terms= 'divulge', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_divulge, file="dataset_divulge.csv")

#web-scrape for term BLAB
DataSet_blab <- get_reddit(search_terms= 'blab', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_blab, file="dataset_blab.csv")

#web-scrape for term DISCOVER
DataSet_discover <- get_reddit(search_terms= 'discover', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_discover, file="dataset_discover.csv")

#web-scrape for term EXHIBIT
DataSet_exhibit <- get_reddit(search_terms= 'exhibit', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_exhibit, file="dataset_exhibit.csv")

#web-scrape for term LetSlip
DataSet_letslip <- get_reddit(search_terms= 'let slip', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_letslip, file="dataset_letslip.csv")

#web-scrape for term WORKER
DataSet_worker <- get_reddit(search_terms= 'worker', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_worker, file="dataset_worker.csv")

#web-scrape for term STAFF
DataSet_staff <- get_reddit(search_terms= 'staff', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_staff, file="dataset_staff.csv")

#web-scrape for term WORKPLACE
DataSet_workplace <- get_reddit(search_terms= 'workplace', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_workplace, file="dataset_workplace.csv")

#web-scrape for term WORKFORCE
DataSet_workforce <- get_reddit(search_terms= 'workforce', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_workforce, file="dataset_workforce.csv")

#web-scrape for term PERSONNEL
DataSet_personnel <- get_reddit(search_terms= 'personnel', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_personnel, file="dataset_personnel.csv")

#web-scrape for term ASSOCIATE
DataSet_associate <- get_reddit(search_terms= 'associate', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_associate, file="dataset_associate.csv")

#web-scrape for term COLLEAGUE
DataSet_colleague <- get_reddit(search_terms= 'colleague', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_colleague, file="dataset_colleague.csv")

#web-scrape for term TEAM
DataSet_team <- get_reddit(search_terms= 'team', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_team, file="dataset_team.csv")

#web-scrape for term FELLOW EMPLOYEE
DataSet_fellowemployee <- get_reddit(search_terms= 'fellow employee', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_fellowemployee, file="dataset_fellowemployee.csv")

#web-scrape for term FIRED
DataSet_fired <- get_reddit(search_terms= 'fired', subreddit="OCD", page_threshold= 99, wait_time= 4)

write.csv(DataSet_fired, file="dataset_fired.csv")