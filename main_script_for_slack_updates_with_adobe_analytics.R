#Slack API Setup with R and Adobe Analytics Anomaly Detection API and Graph 

#Credit goes out to the maintainers of the packages- Randy Zwitch, Bob Rudis & Hadley Wickham - Thank you all!


#More information in links below
#pdf - https://cran.r-project.org/web/packages/slackr/slackr.pdf
#github - https://github.com/hrbrmstr/slackr

#Note on slack:
#there is a setup needing to take place before you run the code below - you NEED to either declare this function below at beginning of 
#this project and run it every time - or you need to store it in .slackr in your home directory / c:/users/username folder under ."slackr"
#and in that folder you need to store this information (per the instructions in the github repo and pdf above):

#go to setup section in github repo link above

#in the .slackr file store just this text and save the file - without the # symbols of course!

#api_token: YOUR_FULL_API_TOKEN
#channel: #general
#username: slackr
#incoming_webhook_url: https://hooks.slack.com/services/XXXXX/XXXXX/XXXXX

#or install dev version  (I recommend it but leaving it optional)
#install.packages("devtools")
#devtools::install_github("hrbrmstr/slackr")

#I dont know if you need any other packages like curl, httr, etc.  It will throw some errors and you'll have to google it if you do need
#other packages, so just a forewarning.  Enjoy!

install.packages("slackr")
install.packages("RSiteCatalyst")
install.packages("ggploto2")

library(slackr)

slackr_setup(channel = '#insert_channel_name_you_setup_with_api_config', username = 'insertusername_defaults_to_slackr', incoming_webhook_url = 'get_from_api_setup_configuration_via_slack_api_settings_for_channel', api_token = 'get_from_api_setup_use_api_tester_if_unsure_of_api_key', config_file = '~/.slackr', echo = FALSE)

library("RSiteCatalyst")

#I have my API keys stored in my .Renviron (In a file called .Renviron under my C:/Users/Username folder as that's where it typically lives in windows - or Home directory for your user in Linux/Ubuntu fyi)

#API Authentication -using last 35 days w/ variables as well - you can just put your web client credentials instead of ADOBE_KEY and ADOBE_SECRET
#But I recommend storing in .Renviron to save you time in the future

SCAuth(Sys.getenv("ADOBE_KEY"), Sys.getenv("ADOBE_SECRET"))
dateFrom <- Sys.Date()-35
dateTo <- Sys.Date()-1

#API function call
visits_w_forecast <- QueueOvertime("insert_your_report_suite_id", date.from = dateFrom, date.to= dateTo, metrics = "visits", date.granularity ="day", anomaly.detection = TRUE)

#Plot data using ggplot2
library(ggplot2)

#Combine year/month/day together into POSIX
visits_w_forecast$date <- ISOdate(visits_w_forecast$year, visits_w_forecast$month, visits_w_forecast$day)

#Convert columns to numeric
visits_w_forecast$visits <- as.numeric(visits_w_forecast$visits)
visits_w_forecast$upperBound.visits <- as.numeric(visits_w_forecast$upperBound.visits)
visits_w_forecast$lowerBound.visits <- as.numeric(visits_w_forecast$lowerBound.visits)

#Calculate points crossing UCL or LCL
visits_w_forecast$outliers <- 
  ifelse(visits_w_forecast$visits > visits_w_forecast$upperBound.visits, visits_w_forecast$visits,
         ifelse(visits_w_forecast$visits < visits_w_forecast$lowerBound.visits, visits_w_forecast$visits, NA))

#Add LCL and UCL labels
LCL <- vector(mode = "character", nrow(visits_w_forecast))
LCL[nrow(visits_w_forecast)] <- "LCL"
UCL <- vector(mode = "character", nrow(visits_w_forecast))
UCL[nrow(visits_w_forecast)] <- "UCL"
visits_w_forecast <- cbind(visits_w_forecast, LCL)
visits_w_forecast <- cbind(visits_w_forecast, UCL)

#Create ggplot with actual, UCL, LCL, outliers - make sure to leave \n after your title in ggtitle function
ggplot(visits_w_forecast, aes(date)) +
  theme_bw(base_family="Garamond") + 
  theme(text = element_text(size=20)) + 
  ggtitle("Visits for www.nameofyoursite.com\n") + 
  geom_line(aes(y = visits), colour = "grey40") + 
  geom_point(aes(y = visits), colour = "grey40", size=3) +
  geom_point(aes(y = outliers), colour = "red", size=3) + 
  geom_line(aes(y = visits_w_forecast$upperBound.visits), colour = "green4", linetype = "dashed") + 
  geom_line(aes(y = visits_w_forecast$lowerBound.visits), colour = "green4", linetype = "dashed") +
  xlab("\nDate\n\n Upper/Lower Control Limits: Adobe Analytics") + 
  
  #change visits to whatever metric you are pulling or just ctrl+f the whole thing and replace "visits" with metric used in api 
  #call to fastrack setup for your use case
  
  ylab("Visits\n") +
  geom_text(aes(label=UCL, family = "Garamond"), y = visits_w_forecast$upperBound.visits, size=4.5, hjust = -.1) +
  geom_text(aes(label=LCL, family = "Garamond"), y = visits_w_forecast$lowerBound.visits, size=4.5, hjust = -.1)


ggslackr(plot = last_plot(), channel = "#same_channel_you_declared_in_slack_setup_at_top_of_this_script")

#optional - to add title and create a report to reference if an anomaly is detected
text_slackr("Click this link to go to Report: https://sc5.omniture.com/insert_custom_url_link_to_report_here", channel = "#same_channel_you_declared_in_slack_setup_at_top_of_this_script")


#For Prophet Forecasting/Predicting Future Visits#convert each dataframe to the variable name data

data <- visits_w_forecast


#qplot(date, visits, data = data)

ds <- data$datetime

y <- log(data$visits)

df <- data.frame(ds,y)

m <- prophet(df, daily.seasonality = TRUE) 

View(m)

make_future_dataframe(m, periods = 35)

tail(future)

future <- make_future_dataframe(m, periods = 7)

tail(future)

predict(m, future)

forecast <- predict(m, future)

tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, forecast)
ggslackr(plot = last_plot(), channel = "#random")
text_slackr("Prophet Forecasting and Daily Seasonality Reports: Pageviews", channel = "#random")

prophet_plot_components(m, forecast)
ggslackr(plot = last_plot(), channel = "#INSERT_CHANNEL_NAME_HERE")

text_slackr("Prophet Forecasting Trending Components: Visits", channel = "#INSERT_CHANNEL_NAME_HERE")


