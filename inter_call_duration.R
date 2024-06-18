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
#which files have bops
#bops <- all_data[all_data$label == "bop",]
#table(bops$file_name)

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
excluded_call_types <- c("growl", "chuckle", "click", "grunt", "chirp grunt", "chirp click", "chirp","bop", "hum", "snore", "squeal chittering")

filtered_data$label[filtered_data$label == "dc"] <- "dolphin call"

#remove points where time difference is 0 because it's impossible to call 2 calls at the same time, so likely a nf caller

filtered_data <- filtered_data[!filtered_data$diff_time_s == 0, ]

filtered_data <- filtered_data %>%
  filter(!label %in% excluded_call_types)

# Define the desired order of the labels
desired_order <- c("chitter", "squeal", "dolphin call", "bark", "snort")

# Convert the label variable to a factor with the desired order of levels
filtered_data$label <- factor(filtered_data$label, levels = desired_order)

mean(filtered_data$diff_time_s[filtered_data$label == "chitter"])
sd(filtered_data$diff_time_s[filtered_data$label == "chitter"])

mean(filtered_data$diff_time_s[filtered_data$label == "squeal"])
sd(filtered_data$diff_time_s[filtered_data$label == "squeal"])

mean(filtered_data$diff_time_s[filtered_data$label == "bark"])
sd(filtered_data$diff_time_s[filtered_data$label == "bark"])

mean(filtered_data$diff_time_s[filtered_data$label == "dolphin call"])
sd(filtered_data$diff_time_s[filtered_data$label == "dolphin call"])



png(height = 600, width = 800, units = 'px', filename = paste0(plot_dir, "intercall_timediff.png"))
# Plot the reordered data
ggplot(filtered_data, aes(y = label, x = diff_time_s)) +
  geom_violin(fill = "plum2") +
  labs(
    #title = "Distribution of diff_time for Call Types (diff_time < 1 seconds)",
    y = "Call Type",
    x = "Time Difference (seconds)") + 
  geom_jitter(height = 0.15, width = 0, size = 0.01, alpha = 0.1, color = "black") +
  theme(legend.title = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.text = element_text(size = 24), 
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 20))
dev.off()


#now want to look at which call follows which call - sequential data
unique(cleaned$label)

call_to_split <- c("chirp grunt", "chirp click", "click grunt")

#double_call <- "chirp grunt"

for (double_call in call_to_split){
  
call_split <- cleaned %>%
  filter(label == double_call) %>%
  rowwise() %>%
  do(data.frame(
    id = .$id,
    label = strsplit(double_call, split = " ")[[1]],
    datetime = c(.$datetime, .$datetime + seconds(0.1)),
    Start = c(.$Start[1], .$Start[1]),  
    Duration = c(.$Duration[1], .$Duration[1]),   
    file_name = c(.$file_name[1], .$filename[1]), 
    date = c(.$date[1], .$date[1]),
    time = c(.$time[1], .$time[1]), 
    
    #other_col2 = c(.$other_col2[1], .$other_col2[1]),   # Fill in NA with original values
    stringsAsFactors = FALSE
  ))

# Combine with the original dataframe excluding the original "chirp grunt" rows
cleaned <- cleaned %>%
  filter(label != double_call) %>%
  bind_rows(call_split) %>%
  arrange(id, datetime)

}

cleaned$date <- as.Date(cleaned$datetime)
#replace dc with dolphin call
cleaned$label[cleaned$label == "dc"] <- "dolphin call"


# Add a date column if it's not already present
# Prepare the data
calls <- cleaned %>%
  arrange(id, datetime) %>%  # Arrange by individual and time
  group_by(id) %>%  # Group by individual
  mutate(next_call_type = if_else(lead(date) == date, lead(label), NA_character_)) %>%  # Create a column for the next call type
  filter(!is.na(label) & !is.na(next_call_type))  # Remove rows where the label or next call type is NA

# Get all unique call types
all_call_types <- unique(c(calls$label, calls$next_call_type))

# Create a complete transition matrix template
empty_transition_matrix <- matrix(0, nrow = length(all_call_types), ncol = length(all_call_types),
                                  dimnames = list(all_call_types, all_call_types))

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
  
  # Convert the table to a matrix and ensure it matches the template dimensions
  transition_matrix <- as.matrix(transition_matrix)
  full_transition_matrix <- empty_transition_matrix
  full_transition_matrix[rownames(transition_matrix), colnames(transition_matrix)] <- transition_matrix
  
  # Store the transition matrix in the list
  transition_matrices[[ind]] <- full_transition_matrix
}

# Sum transition matrices across individuals
combined_transition_matrix <- Reduce("+", transition_matrices)

# Normalize the combined transition matrix by row sums to get probabilities
combined_transition_matrix <- prop.table(combined_transition_matrix, 1)

# Convert the combined transition matrix to a dataframe for plotting
combined_transition_df <- as.data.frame(as.table(combined_transition_matrix))
colnames(combined_transition_df) <- c("From", "To", "Frequency")

# Remove rows with NA values in the From or To columns
combined_transition_df <- combined_transition_df %>%
  filter(!is.na(From) & !is.na(To))

#also filter calls not interested in having in the plot such as snore
combined_transition_df <- combined_transition_df %>%
  filter(From != "snore" & To != "snore")

#only plot for rhythmic calls
combined_transition_df <- combined_transition_df %>%
  filter(From != "squeal chittering" & To != "squeal chittering")
combined_transition_df <- combined_transition_df %>%
  filter(From != "hum" & To != "hum")
combined_transition_df <- combined_transition_df %>%
  filter(From != "growl" & To != "growl")
combined_transition_df <- combined_transition_df %>%
  filter(From != "bop" & To != "bop")
combined_transition_df <- combined_transition_df %>%
  filter(From != "grunt" & To != "grunt")
combined_transition_df <- combined_transition_df %>%
  filter(From != "click" & To != "click")
combined_transition_df <- combined_transition_df %>%
  filter(From != "chirp" & To != "chirp")


# Plot the combined transition matrix
p <- ggplot(combined_transition_df, aes(x = From, y = To, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "magenta4") +
  labs(title = "Combined Transition Matrix for All Individuals",
       x = "From Call Type",
       y = "To Call Type") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 22),  # Increase x-axis text size
    axis.text.y = element_text(size = 22),  # Increase y-axis text size
    axis.title.x = element_text(size = 20),  # Increase x-axis title size
    axis.title.y = element_text(size = 20),  # Increase y-axis title size
    plot.title = element_text(size = 24),  # Increase plot title size
    legend.title = element_text(size = 18),  # Increase legend title size
    legend.text = element_text(size = 16)  # Increase legend text size
  )

print(p)

ggsave(filename = paste0(plot_dir, 'rhythmic_transition+matrix.png'), plot = p, width = 9, height = 8, dpi = 300)


#get the mean values for each transistion being the same call


same_transition_means <- combined_transition_df %>%
  filter(From == To)%>%
  summarise(
    mean_Frequency = mean(Frequency),
    sd_Frequency = sd(Frequency)
  )



#--------------------------------------------
# #normalising the data and transition call has to be within 1min of previous call to count
# # Prepare the data
# calls <- cleaned %>%
#   filter(label != "snore") %>%  # Exclude rows where the label is "snore"
#   arrange(id, datetime) %>%  # Arrange by individual and time
#   group_by(id) %>%  # Group by individual
#   mutate(next_call_type = if_else(lead(date) == date & (lead(datetime) - datetime <= minutes(1)), lead(label), NA_character_)) %>%  # Create a column for the next call type within 1 minute
#   filter(!is.na(label) & !is.na(next_call_type))  # Remove rows where the label or next call type is NA
# 
# # Get all unique call types
# all_call_types <- unique(c(calls$label, calls$next_call_type))
# 
# # Create a complete transition matrix template
# empty_transition_matrix <- matrix(0, nrow = length(all_call_types), ncol = length(all_call_types),
#                                   dimnames = list(all_call_types, all_call_types))
# 
# # Create a list to store matrices for each individual
# transition_matrices <- list()
# 
# # Get the unique individuals
# individuals <- unique(calls$id)
# 
# # Generate transition matrices for each individual
# for (ind in individuals) {
#   # Filter data for the individual
#   individual_calls <- calls %>%
#     filter(id == ind)
#   
#   # Create a contingency table (transition matrix)
#   transition_matrix <- table(individual_calls$label, individual_calls$next_call_type)
#   
#   # Convert the table to a matrix and ensure it matches the template dimensions
#   transition_matrix <- as.matrix(transition_matrix)
#   full_transition_matrix <- empty_transition_matrix
#   full_transition_matrix[rownames(transition_matrix), colnames(transition_matrix)] <- transition_matrix
#   
#   # Normalize the transition matrix by the counts of the "From" call types
#   from_call_counts <- rowSums(full_transition_matrix)
#   normalized_transition_matrix <- sweep(full_transition_matrix, 1, from_call_counts, FUN = "/")
#   normalized_transition_matrix[is.nan(normalized_transition_matrix)] <- 0  # Replace NaN with 0
#   
#   # Store the normalized transition matrix in the list
#   transition_matrices[[ind]] <- normalized_transition_matrix
# }
# 
# # Sum normalized transition matrices across individuals
# combined_normalized_transition_matrix <- Reduce("+", transition_matrices)
# 
# # Convert the combined normalized transition matrix to a dataframe for plotting
# combined_transition_df <- as.data.frame(as.table(combined_normalized_transition_matrix))
# colnames(combined_transition_df) <- c("From", "To", "Frequency")
# 
# # Remove rows with NA values in the From or To columns
# combined_transition_df <- combined_transition_df %>%
#   filter(!is.na(From) & !is.na(To))
# 
# # Remove rows and columns where the call type is "snore"
# combined_transition_df <- combined_transition_df %>%
#   filter(From != "snore" & To != "snore")
# 
# # Plot the combined normalized transition matrix
# p <- ggplot(combined_transition_df, aes(x = From, y = To, fill = Frequency)) +
#   geom_tile(color = "white") +
#   scale_fill_gradient(low = "white", high = "steelblue") +
#   labs(title = "Combined Normalized Transition Matrix for All Individuals",
#        x = "From Call Type (Initial Call)",
#        y = "To Call Type (Subsequent Call)") +  # Explicitly state which call is first and which is second
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# print(p)







