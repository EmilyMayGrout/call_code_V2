#this script is calculating the time between the rhythmic calls

wd <- "C:/Users/egrout/Dropbox/calls/Galaxy_labels/completed_labels/"
plot_dir <- "C:/Users/egrout/Dropbox/calls/results/"

setwd <- wd

#LIBRARIES
library(stringr)
library(hms)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(ggplot2)
library(gridExtra)
library(lubridate)

# get a list of all the CSV files in the folder
files <- list.files(wd, pattern = "*.csv")

#create data frame with 0 rows and 3 columns
all_data <- data.frame(matrix(ncol = 8, nrow = 0))
#provide column names
colnames(all_data) <- c("label","Start","Duration","Time","Format","Type","Description","file_name")

# loop through each CSV file and add it to the dataframe
for (i in 1:length(files)) {
  # read in the CSV data as a tibble
  # using header = TRUE assumes the first row of each CSV file is a header with column names
  file_data <- read.csv(paste0(wd, files[i]), header = T, sep="\t")
  
  # add a column with the row names (i.e. the name of the CSV file)
  file_data$file_name <- files[i]
  #only keeping necessary info of the file name
  file_data$file <- str_sub(file_data$file_name,end=11)
  file_data$date <- str_sub(file_data$file_name, start = 13, end = 20)
  colnames(file_data)[colnames(file_data) == "Name"] <- "label"
  
  # add the data to the all_data dataframe
  all_data <- rbind(all_data, file_data)
}


#remove rows which contain the date 
all_data <- all_data[!grepl(":", all_data$label),]

#add ID column
all_data$id <- str_sub(all_data$file_name, end=5)

#make date colum in POSIXct
all_data$date <- as.Date(sub("(..)$", "20\\1", all_data$date), "%d.%m.%Y")

#make time column
all_data$Start <- all_data$Start
table(str_length(all_data$Start))

#remove rows which contain the date 
all_data <- all_data[!grepl(":", all_data$label), ]

#remove unnecessary rows
all_data <- all_data[, -c(4:6)]



#because the length of the time column is different due to some times less than an hour, need to split the data to get the times and then rbind them
t <- filter(all_data, nchar(Start) == 8)
t$time <- paste0("6:", t$Start)
t$time <- as_hms(t$time)

s <- filter(all_data, nchar(Start) == 9)
s$time <- paste0("6:", s$Start)
s$time <- as_hms(s$time)

v <- filter(all_data, nchar(Start) == 11)
v$time <- as_hms(as_hms(v$Start) + as_hms('6:00:00.000'))

all_data_hms <- rbind(t,s,v)

#add as.POSIXct
all_data_hms$datetime <- as.POSIXct(paste(all_data_hms$date, all_data_hms$time), format = "%Y-%m-%d %H:%M:%OS")


#---------------------------------------------------------------------
#now cleaning labels and removing labels which are not calls
unique(all_data$label)
#remove labels
all_data_cleaned <- all_data_hms

#removing labels not interested in:
all_data_cleaned <- all_data_cleaned[!grepl("unk", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("scratch", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("shake", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("firework", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("wood", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("mov", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("monkey", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("sniff", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("walk", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("dig", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("train", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("drink", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("breath", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("hale", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("run", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("dolphin", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("synch", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("bird", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("manakin", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("pant", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("vibrate", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("forag", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("fart", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("buzz", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("howler", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("fission", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("fusion", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("start", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("stop", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("insect", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("rain", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("eating", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("collar", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("scatch", all_data_cleaned$label),]


#cleaning call labels
all_data_cleaned$label[all_data_cleaned$label == "chirp "] <- "chirp"
all_data_cleaned$label[all_data_cleaned$label == "chrip"] <- "chirp"
all_data_cleaned$label[all_data_cleaned$label == "chirp x"] <- "chirp"
all_data_cleaned$label[all_data_cleaned$label == "grnt"] <- "grunt"
all_data_cleaned$label[all_data_cleaned$label == "gunt"] <- "grunt"
all_data_cleaned$label[all_data_cleaned$label == "spueal"] <- "squeal"
all_data_cleaned$label[all_data_cleaned$label == "nf chitter "] <- "nf chitter"
all_data_cleaned$label[all_data_cleaned$label == "nf chitter x"] <- "nf chitter"
all_data_cleaned$label[all_data_cleaned$label == "chitter x "] <- "chitter"
all_data_cleaned$label[all_data_cleaned$label == "chitter x"] <- "chitter"
all_data_cleaned$label[all_data_cleaned$label == "chitter "] <- "chitter"
all_data_cleaned$label[all_data_cleaned$label == "dc x"] <- "dc"
all_data_cleaned$label[all_data_cleaned$label == "bob"] <- "bop"
all_data_cleaned$label[all_data_cleaned$label == "chirpgr x"] <- "chirp grunt"
all_data_cleaned$label[all_data_cleaned$label == "chirpgr "] <- "chirp grunt"
all_data_cleaned$label[all_data_cleaned$label == "chirpgr"] <- "chirp grunt"
all_data_cleaned$label[all_data_cleaned$label == "low peep"] <- "peep"
all_data_cleaned$label[all_data_cleaned$label == "chirp click "] <- "chirp click"
all_data_cleaned$label[all_data_cleaned$label == "chirp cick"] <- "chirp click"
all_data_cleaned$label[all_data_cleaned$label == "chirpr"] <- "chirp grunt"
all_data_cleaned$label[all_data_cleaned$label == "chirgpr"] <- "chirp grunt"
all_data_cleaned$label[all_data_cleaned$label == "click grnut"] <- "click grunt"
all_data_cleaned$label[all_data_cleaned$label == "squeal chitters"] <- "squeal chittering"
all_data_cleaned$label[all_data_cleaned$label == "squeal chitter"] <- "squeal chittering"
all_data_cleaned$label[all_data_cleaned$label == "squeal chitter x"] <- "squeal chittering"
all_data_cleaned$label[all_data_cleaned$label == "squeal chitter x"] <- "squeal chittering"
all_data_cleaned$label[all_data_cleaned$label == "chitter squeal"] <- "squeal chittering"
all_data_cleaned$label[all_data_cleaned$label == "low squeal"] <- "squeal"
all_data_cleaned$label[all_data_cleaned$label == "squeal "] <- "squeal"


#remove nf calls
all_data_cleaned <- all_data_cleaned[!grepl("nf", all_data_cleaned$label),]

unique(all_data_cleaned$label)


#look at counts for each call type 
table(all_data_cleaned$label)

cleaned <- all_data_cleaned

#remove calls which are not described in the repertoire paper where the sample size is also super small

cleaned <- cleaned[!grepl("chop chop", cleaned$label),]
cleaned <- cleaned[!grepl("peep", cleaned$label),]
cleaned <- cleaned[!grepl("purr", cleaned$label),]
cleaned <- cleaned[!grepl("quack", cleaned$label),]
cleaned <- cleaned[!grepl("snarl", cleaned$label),]
cleaned <- cleaned[!grepl("cackle", cleaned$label),]
cleaned <- cleaned[!grepl("cackling", cleaned$label),]
cleaned <- cleaned[!grepl("quack", cleaned$label),]
cleaned <- cleaned[!grepl("whistle", cleaned$label),]
cleaned <- cleaned[!grepl("snort", cleaned$label),]
cleaned <- cleaned[!grepl("squeak", cleaned$label),]
cleaned <- cleaned[!grepl("chuckle", cleaned$label),]
cleaned <- cleaned[!grepl("low squeal", cleaned$label),]


#look at counts for each call type 
table(cleaned$label)


#want to look at each individuals to get average number of calls for each call type

cleaned_diff_time <- cleaned %>%
  arrange(file_name, label, time) %>%
  group_by(file, label) %>%
  mutate(Start = as_hms(time),  # Convert "Start" to hms format
         diff_time_s = as.numeric(round(difftime(time, lag(time, default = first(time)), units = "secs"), 4))) %>%
  ungroup()


# Create a violin plot for all calls
ggplot(cleaned_diff_time, aes(y = label, x = diff_time_s)) +
  geom_violin() +
  labs(
    title = "Distribution of diff_time for Call Types (diff_time < 60 seconds)",
    y = "Call Type",
    x = "Time Difference (seconds)"
  )


# Filter the data for rows where diff_time is less than 1 second
filtered_data <- cleaned_diff_time[cleaned_diff_time$diff_time_s < 1, ]

#removing calls not interested in as they're not rhythmic
excluded_call_types <- c("growl", "snort", "chuckle", "click", "grunt", "chirp grunt", "chirp click", "chirp", "bop", "hum", "snore", "squeal chittering")

filtered_data$label[filtered_data$label == "dc"] <- "dolphin call"

#remove points where time difference is 0 because it's impossible to call 2 calls at the same time, so likely a nf caller

filtered_data <- filtered_data[!filtered_data$diff_time_s == 0, ]


filtered_data <- filtered_data %>%
  filter(!label %in% excluded_call_types)

# Create a violin plot for filtered calls

png(height = 600, width = 1000, units = 'px', filename = paste0(plot_dir, "intercall_timediff.png"))
ggplot(filtered_data, aes(y = label, x = diff_time_s)) +
  geom_violin(fill = "plum") +
  labs(
    #title = "Distribution of diff_time for Call Types (diff_time < 1 seconds)",
    y = "Call Type",
    x = "Time Difference (seconds)") + geom_jitter(height = 0.15, width = 0, size = 0.01, alpha = 0.1, color = "black")+
  theme(legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.text=element_text(size=20), axis.title = element_text(size = 20), legend.text = element_text(size = 20))

dev.off()



#now want to look at which call follows which call - sequential data

#just use the cleaned dataframe
#first need to split calls which have 2 components e.g. chirp grunt - to make the grunt follow the chirp

# Split "chirp grunt" into two rows - have to run this manually for chirp grunt, chirp click, and click grunt
chirp_click_split <- cleaned %>%
  filter(label == "chirp click") %>%
  rowwise() %>%
  do(data.frame(
    id = .$id,
    label = c("chirp", "click"),
    datetime = c(.$datetime, .$datetime + seconds(0.1)),
    #other_col1 = c(.$other_col1[1], .$other_col1[1]),   # Fill in NA with original values
    #other_col2 = c(.$other_col2[1], .$other_col2[1]),   # Fill in NA with original values
    stringsAsFactors = FALSE
  ))

# Combine with the original dataframe excluding the original "chirp grunt" rows
cleaned <- cleaned %>%
  filter(label != "chirp click") %>%
  bind_rows(chirp_click_split) %>%
  arrange(id, datetime)

unique(cleaned$label)


# # Define function to split calls
# split_calls <- function(df, call_type, split_time) {
#   call_split <- df %>%
#     filter(label == call_type) %>%
#     rowwise() %>%
#     do(data.frame(
#       id = .$id,
#       label = c(call_type %>% strsplit(" ")[[1]] %>% unlist(), call_type %>% strsplit(" ")[[2]] %>% unlist()),
#       datetime = c(.$datetime, .$datetime + seconds(split_time)),
#       stringsAsFactors = FALSE
#     ))
#   return(call_split)
# }
# #not working yet.... 
# 
# # Split "chirp grunt", "chirp click", and "click grunt" into two rows each
# chirp_grunt_split <- split_calls(cleaned, "chirp grunt", 0.1)
# chirp_click_split <- split_calls(cleaned, "chirp click", 0.1)
# click_grunt_split <- split_calls(cleaned, "click grunt", 0.1)
# 
# # Combine with the original dataframe excluding the original "chirp grunt", "chirp click", and "click grunt" rows
# cleaned <- cleaned %>%
#   filter(!label %in% c("chirp grunt", "chirp click", "click grunt")) %>%
#   bind_rows(chirp_grunt_split, chirp_click_split, click_grunt_split) %>%
#   arrange(id, datetime)


#now making the dataframe to get the order of calls
# Add a date column if it's not already present
cleaned$date <- as.Date(cleaned$datetime)

# Prepare the data
calls <- cleaned %>%
  arrange(id, datetime) %>%  # Arrange by individual and time
  group_by(id) %>%  # Group by individual
  mutate(next_call_type = if_else(lead(date) == date, lead(label), NA_character_)) %>%  # Create a column for the next call type
  filter(!is.na(label))  # Remove rows where the next call type is NA

# Create a list to store matrices for each individual
transition_matrices <- list()

# Get the unique individuals
individuals <- unique(calls$id)

# Generate transition matrices for each individual
for (ind in individuals) {
  # Filter data for the individual
  individual_calls <- calls %>%
    filter(id == ind)
  
  # Create a contingency table (transition matrix)
  transition_matrix <- table(individual_calls$label, individual_calls$next_call_type)
  # Normalize the transition matrix by row sums to get probabilities (if needed)
  transition_matrix <- prop.table(transition_matrix, 1)
  # Store the transition matrix in the list
  transition_matrices[[ind]] <- transition_matrix
}

# Example of accessing and printing the transition matrix for a specific individual
print(transition_matrices[[1]])  # Replace 1 with the index of the desired individual


for (i in seq_along(transition_matrices)) {
  transition_matrix <- transition_matrices[[i]]
  transition_df <- as.data.frame(as.table(transition_matrix))
  colnames(transition_df) <- c("From", "To", "Frequency")
  
  p <- ggplot(transition_df, aes(x = From, y = To, fill = Frequency)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(title = paste("Transition Matrix for Individual", names(transition_matrices)[i]),
         x = "From Call Type",
         y = "To Call Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

}

p

#NEXT: is there a way of combining all the matrices of each ind into one?













