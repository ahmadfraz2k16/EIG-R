# library(gmailr)
# # path_old <- "C:/Users/python/Downloads/gmailr/client_secret_569058314845-3jjikvp2k3f0ao7v8dkfg36botomlln3.apps.googleusercontent.com.json"
# # d <- fs::dir_create(rappdirs::user_data_dir("gmailr"), recurse = TRUE)
# # fs::file_move(path_old, d)
# gm_auth_configure()
# 
# gm_oauth_client()
# # Set up your Gmail account credentials
# email_address <- "ahmadfraz2k16@gmail.com"
# email_password <- "password12345@#$"
# 
# # Log in to your Gmail account
# gmailr::gm_auth(token = NULL)
# gm_auth(login = email_address, password = email_password)
# 
# # Search for emails from the specific sender
# search_query <- sprintf("from:%s", "ahmad.fraz@lums.edu.pk")
# emails <- gm_messages(search_query)
# 
# # Download Excel attachments from the matching emails
# download_dir <- "C:/Users/python/Documents/projR/MAR-2022"
# for (email in emails) {
#   attachments <- gm_get_attachments(email)
#   for (attachment in attachments) {
#     if (endsWith(attachment$name, ".xlsm")) {
#       download_path <- file.path(download_dir, attachment$name)
#       gm_download_attachment(attachment, download_path)
#     }
#   }
# }
# 
# # Log out
# gm_auth(token = NULL)






# # Load the gmailr package
# library(gmailr)
# 
# # Configure your OAuth client
# gm_auth_configure(path = "C:/Users/python/AppData/Local/gmailr/gmailr/client_secret_569058314845-3jjikvp2k3f0ao7v8dkfg36botomlln3.apps.googleusercontent.com.json")
# # Log in to your Gmail account
# gm_auth()
# 
# # Set up your Gmail account credentials
# email_address <- "ahmadfraz2k16@gmail.com"
# 
# # Search for emails from the specific sender
# search_query <- sprintf("from:%s", "ahmad.fraz@lums.edu.pk")
# emails <- gm_messages(search_query)
# 
# # Download Excel attachments from the matching emails
# download_dir <- "C:/Users/python/Documents/projR/MAR-2022"
# for (email in emails) {
#   attachments <- gm_attachments(email)
#   for (attachment in attachments) {
#     if (endsWith(attachment$name, ".xlsm")) {
#       download_path <- file.path(download_dir, attachment$name)
#       gm_save_attachment(email, attachment_id = attachment$id, path = download_path)
#     }
#   }
# }
# 
# # Log out
# gm_deauth()


# # used thread to fetch and download files, but email fiter isn't working
# # Load the gmailr package
# library(gmailr)
# 
# # Set the path to your OAuth JSON file
# json_path <- "C:/Users/python/AppData/Local/gmailr/gmailr/client_secret_569058314845-3jjikvp2k3f0ao7v8dkfg36botomlln3.apps.googleusercontent.com.json"
# 
# # Configure your OAuth client
# gm_auth_configure(path = json_path)
# 
# # Manually trigger the OAuth2 process
# gm_auth(email = "ahmadfraz2k16@gmail.com")
# gm_profile()
# my_threads <- gm_threads(search = 'from:ahmad.fraz@lums.edu.pk')
# my_threads <- gm_threads(num_results = 15)
# thread_id <- gm_id(my_threads)[[1]]
# # retrieve the latest thread by retrieving the first ID
# latest_thread <- gm_thread(gm_id(my_threads)[[1]])
# my_msg <- latest_thread$messages[[1]]
# gm_from(my_msg)
# gm_date(my_msg)
# gm_subject(my_msg)
# gm_body(my_msg)
# gm_save_attachments(my_msg, path = "C:/Users/python/Documents/projR/MAR-2022")
# # Log out
# gm_deauth()

# Load the gmailr package
library(gmailr)

# Set the path to your OAuth JSON file
json_path <- "C:/Users/python/AppData/Local/gmailr/gmailr/client_secret_569058314845-3jjikvp2k3f0ao7v8dkfg36botomlln3.apps.googleusercontent.com.json"

# Configure your OAuth client
gm_auth_configure(path = json_path)

# Manually trigger the OAuth2 process
gm_auth(email = "ahmadfraz2k16@gmail.com")
gm_profile()
messagez <- gm_messages(search = 'from:ahmad.fraz@lums.edu.pk', num_results = 15)
number_of_messagez <- length(messagez[[1]]$messages)
print(number_of_messagez)
for (i in 1:number_of_messagez) {
  latest_message <- gm_message(gm_id(messagez)[[i]])
  filenameMsg <- latest_message$payload$parts[[2]]$filename
  print(filenameMsg)
  gm_save_attachments(latest_message, path = "C:/Users/python/Documents/projR/MAR-2022")
}

# # for latest email only
# latest_message <- gm_message(gm_id(messagez)[[1]])
# filenameMsg <- latest_message$payload$parts[[2]]$filename
# print(filenameMsg)
# gm_save_attachments(latest_message, path = "C:/Users/python/Documents/projR/MAR-2022")
# Log out
gm_deauth()

# my_message <- latest_message$messages[[1]]
# gm_from(my_message)
# gm_date(my_message)
# gm_subject(my_message)


# my_threads <- gm_threads(search = 'from:ahmad.fraz@lums.edu.pk')
# my_threads <- gm_threads(num_results = 15)
# thread_id <- gm_id(my_threads)[[1]]
# # retrieve the latest thread by retrieving the first ID
# latest_thread <- gm_thread(gm_id(my_threads)[[1]])
# my_msg <- latest_thread$messages[[1]]
# gm_from(my_msg)
# gm_date(my_msg)
# gm_subject(my_msg)
# gm_body(my_msg)
# gm_save_attachments(my_msg, path = "C:/Users/python/Documents/projR/MAR-2022")
# # Log out
# gm_deauth()