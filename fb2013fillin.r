#Getting data on posts of 2 more pages from Facebook

library(Rfacebook)
token <- "CAACEdEose0cBAFy8C5JjZA1GNJdB94ol9owoY4Qp8jTzH26AZBNyX73BmPTAzQB5E2ROHhAabhBELwTR3gwD2rDSmuYGeYpjhTcJclblXSM0y0LBkaTMGNxqUe3HaXoP6JnLmuf59QeNHkOlZB56ZC0RzW539AkAQLg1QynkZCcO6PJ77exrbbd4Uw4ilnlaQWNZBCG2cAZBwZDZD"
ServerCloud <- getPage("ServerCloud", token, n = 5000)
WindowsServer <- getPage("WindowsServer", token, n = 5000)