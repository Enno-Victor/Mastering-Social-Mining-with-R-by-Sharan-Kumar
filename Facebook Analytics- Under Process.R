#install.packages("devtools")
library(devtools)
#install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")
library(Rfacebook)

token<- "EAACEdEose0cBAMhYHhKGu86XYCjEwOHR4ZCRMqs6lfAQouZAQhRSTDZCF04bztNw1hyUmtH7DATTt5nV6Ub3efmLXDjdXT0X9pMwC4HhOfw5AEL67QrVmpZCEPmygFuJxHXEQDmvncUJqZCiB7bLEnOMUaJrfHjfrTP0RUCwnRAZDZD"
me<- getUsers("10204869798723332", token, private_info = TRUE)

me$name
me$hometown

library("Rfacebook")
fb_oauth<- fbOAuth(app_id="1111208928950839",app_secret="ca5d07038262c3881c67f49be8fd16ad",extended_permissions = TRUE)

save(fb_oauth, file="fb_oauth")
load("fb_oauth")


# Most trending posts
page<- getPage("TED", token, n = 500)
head(page, n=20)

#Let's get the detail about the post which had the maximum number of likes.
# Page with maximum likes
page[which.max(page$likes_count), ]

#Trending Topics
pageRecent<- page[which(page$created_time> "2015-05-01"), ]
top<- pageRecent[order(- pageRecent$likes),]
head(top, n=10)

#Influencers 
post_id<- head(page$id, n = 1)  ## ID of most recent post
post<- getPost(post_id, token, n = 1000, likes = TRUE, comments = TRUE)
head(post$comments, n=2)

samppost<- post$comments

##Top user information of any post 

#install.packages("sqldf")
library(sqldf)
comments <- post$comments
influentialusers<- sqldf("select from_name, sum(likes_count) as totlikes from comments group by from_name")
head(influentialusers)
influentialusers$totlikes<- as.numeric(influentialusers$totlikes)
# Sorting the users based on the number of likes they received
top<- influentialusers[order(- influentialusers$totlikes),]
head(top, n=10)

#Based on multiple posts
post_id<- head(page$id, n = 100)
head(post_id, n=10)
post_id<- as.matrix(post_id)
allcomments<- ""

# Collecting all the commments from all the 100 posts
for (i in 1:nrow(post_id))
{
  # Get upto 1000 comments for each post
  post<- getPost(post_id[i,], token, n = 1000, likes = TRUE, comments = TRUE)
  comments<- post$comments
  # Append the comments to a single data frame
  allcomments<- rbind(allcomments, comments)
}

# Consolidating the like for each user.
influentialusers<- sqldf("select from_name, sum(likes_count) as totlikes from allcomments group by from_name")
influentialusers$totlikes<- as.numeric(influentialusers$totlikes)
top<- influentialusers[order(- influentialusers$totlikes),]
head(top, n=20)

