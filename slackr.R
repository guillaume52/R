
#Crap
Token=
webhook=


    
library(jsonlite)
library(httr)
#send message    
POST(url="https://slack.com/api/chat.postMessage",
     body=list(token=Token,
               incoming_webhook_url=webhook,
               channel="@gman",
               username="gman",
               icon_emoji="",
               
               attachments = '[
               {    "fallback": "Required plain-text summary of the attachment.",
               "color": "#36a64f",
               "title": "169 - Extra Stems Image",
               "title_link": "",
               
               "fields": [
               {
               "title": "Image",
               "value": "Available"
               }
               ],
               
               
               }
               ]'
               ,as_user=TRUE
     ))

#Retrieve message
URL="https://slack.com/api/channels.history"
Channel="#testtest"
DD<-GET(url=URL,
     body=list(token=Token,
               #incoming_webhook_url=webhook,
               channel="@gman",
               count=10
               
     ))

DD<-GET(paste0(URL,"?token=",Token,"&channel=",Channel,"&count=5"))
data.frame(DD)

library(tidyjson)
jsonlite::fromJSON(DD, simplifyDataFrame = TRUE)
str(DD)


