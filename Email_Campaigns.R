##### BL Assignment #####
### Eirini Dernika ###

# Step 1: Loading the dataset.
# The initial dataset was given in SQL dump format.
# It is observed that email_data.sql contained three tables (email_activity, email_campaigns and email_lists).
# Each table started with the following format: the command "COPY" followed by table's name and the variables' names in parentheses.
# e.x. COPY email_activity (action, campaign_id, email_address, email_id, list_id, "timestamp", type) FROM stdin;
# Therefore in order these lines to be imported into R, they were carefully copied into three different excel spreadsheets and saved as csv files.

setwd("C:/Users/Eirini/Desktop/Blendo Assignment")

# Load table email_activity
email_activity <- read.csv("C:/Users/Eirini/Desktop/Blendo Assignment/email_activity.csv", sep= ";", header=TRUE)
dim(email_activity)
names(email_activity)
head(email_activity, 5)

# Load table email_campaigns
email_campaigns <- read.csv("C:/Users/Eirini/Desktop/Blendo Assignment/email_campaigns.csv", sep= ";", header=TRUE)
dim(email_campaigns)
names(email_campaigns)
head(email_campaigns, 5)

# Load table email_lists
email_lists <- read.csv("C:/Users/Eirini/Desktop/Blendo Assignment/email_lists.csv", sep= ";", header=TRUE)
dim(email_lists)
names(email_lists)
head(email_lists, 5)


# Step 2: Check which variables are useful for our analysis.

# The table email_activity includes 10376 observations and 7 variables (action, campaign_id, email_address, email_id, list_id, timestamp, type).
# It is noticed that email_id is part of email_address (email_address = email_id@acme.com).
# There is no need to keep both variables. We decide to keep email_id.
email_activity <- email_activity[,-3]

# The table email_campaigns includes 49 observations and 12 variables (create_time, delivery_status_enabled, emails_sent, id, recipients_list_id, recipients_recipient_count, report_summary_click_rate, report_summary_clicks, report_summary_open_rate, report_summary_opens, report_summary_unique_opens, send_time).
# It is noticed that delivery_status_enabled takes the value 'f' for every record.
# We decide to remove this variable as it does not provide any useful information.
email_campaigns$delivery_status_enabled=='f'
# The variable create_time will also be removed, as it is not helpful for the specified analysis.
# The variables emails_sent and recipients_recipient_count are exactly equal.
email_campaigns$emails_sent==email_campaigns$recipients_recipient_count
# We decide to keep only the variable emails_sent.
# The variables report_summary_clicks, report_summary_opens and report_summary_unique_opens will be
# removed, as their rates are included in the dataset.
email_campaigns <- email_campaigns[,-c(1, 2, 6, 8, 10, 11)]

# The table email_lists includes 1649 observations and 13 variables (email_address, email_client, email_type, id, language, last_changed, list_id, location_country_code, location_timezone, stats_avg_click_rate, stats_avg_open_rate, status, timestamp_signup).
# It is noticed that id is part of email_address (email_address = id@acme.com).
# There is no need to keep both variables. We decide to keep id.
# The variable email_type takes the value 'html' for every record.
# We decide to remove this variable as it does not provide any useful information.
length(which((email_lists$email_type=='html')==FALSE))
# The variable last_changed will also be removed as it does not provide any useful information.
email_lists <- email_lists[,-c(1, 3, 6)]


# Step 3: Check each table for any missing values.

# There are not any missing values in email_activity.
indexNA <- complete.cases(email_activity)
length(which(indexNA==FALSE))

# There are not any missing values in email_campaigns.
indexNA <- complete.cases(email_campaigns)
length(which(indexNA==FALSE))

# In email_lists the variables email_client, language, location_country_code and location_timezone have missing values.


# Step 4: View summary statistics and descriptive measures about the variables.
# Please note that due to small size of the datasets, some results could be also easily extracted by simple manipulation in excel.

summary(email_activity)
summary(email_campaigns)
summary(email_lists)

# Response/Action Rate
# Summary of email_activity$action
(summary(email_activity$action)/length(email_activity$action))*100

# Bounce Rate
# Summary soft and hard Bounces
(summary(email_activity$type)/length(which(email_activity$action=="bounce")==TRUE))*100

# Summary of email_activity$list_id
(summary(email_activity$list_id)/length(email_activity$list_id))*100

# Subscribe Rate
# Summary of email_lists$status
(summary(email_lists$status)/length(email_lists$status))*100

# Lists and campaigns
levels(email_campaigns$recipients_list_id)
id_list_rates <- email_campaigns[,c(2,3,4,5)]
order(id_list_rates, partial = as.vector(id_list_rates$report_summary_open_rate), decreasing = FALSE)

# View email clients
sort(summary(email_lists$email_client), decreasing = TRUE)
sort(summary(email_lists$email_client)/length(email_lists$email_client)*100, decreasing = TRUE)

# View locations
sort(summary(email_lists$location_country_code), decreasing = TRUE)
sort(summary(email_lists$location_timezone), decreasing = TRUE)


# Step 5: Plotting interesting results.

# graphical representation of email_activity$action in percentages
barplot(((summary(email_activity$action)/10376)*100), ylim = c(0,100), main = "Email Actions", col = c("gray", "gray50", "gray32"))
legend("topleft",c("bounce 1.88%", "click 11.65%", "open 86.46%"), col = c("gray", "gray50", "gray32"), pch=15)

# graphical representation of email_activity$list_id in percentages
barplot(((summary(email_activity$list_id)/10376)*100), ylim = c(0,100), main = "Email Activity by Recipients list", col = c("gray", "gray50", "gray32"))
legend("topright",c("74.17% List1 with id=180b7eeb41", "1.35% List2 with id=9375e3c354", "24.48% List3 with id=cd055c6fe3"), col = c("gray", "gray50", "gray32"), pch=15)

# graphical representation of email_lists$status in percentages
barplot(((summary(email_lists$status)/1649)*100), ylim = c(0,100), main = "Status of Email lists", col = c("gray", "gray50", "gray32", "gray 15"))
legend("topleft",c("3.15% cleaned", "2.48% pending", "81.68% subscribed ", "12.67% unsubscribed"), col = c("gray", "gray50", "gray32", "gray 15"), pch=15)
