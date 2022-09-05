#PBX
Token="xoxp-26461059348-144877054676-220966048544-b4041bf09760576c8ec9fc218c99db30"
webhook="https://hooks.slack.com/services/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"
#Crap
Token="xoxp-223958811877-225027056727-224797867059-71d520716277fda4453b50f1068725e0"
webhook="https://hooks.slack.com/services/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"


    
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
               "title_link": "http://confluence.photobox.com/display/WM/169+-+Extra+Stems+Image",
               
               "fields": [
               {
               "title": "Analysis of 169 - Extra Stems Image",
               "value": "Available in Confluence"
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


