#Getting data on posts of 2 more pages from Facebook

library(Rfacebook)
token <- "token"
ServerCloud <- getPage("ServerCloud", token, n = 5000)
WindowsServer <- getPage("WindowsServer", token, n = 5000)