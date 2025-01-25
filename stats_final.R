attach(booking)

#Exploring summary of the dataset amd checking for missing values
print(summary(booking))

#getting a size of dataset
dim(booking)

#Exploring numerical variables

distinct_values <- unique(num_passengers) #dictinct values of num_of passengers

# Print the distinct values
length(distinct_values)


#distribution of num_passengers
hist(num_passengers, c(1,2, 3,4, 5,6, 7,8, 9), xlab="Number of Passengers", ylab="Frequency",
     main="Distribution of Number of Passengers", col="#AED6F1", border="white")



#exploring length_of_stay
hist(length_of_stay, breaks = 10,xlab="Length of stay", ylab="Frequency",
     main="Distribution of Length of Stay", col="#AED6F1", border="white")

#building boxplot to see outliers and how much data is skewed
boxplot(length_of_stay, 
        main = "Boxplot of Length of Stay", 
        ylab = "Length of Stay")
#building histogram for len<400
hist(length_of_stay, 
     breaks = 20, 
     xlab = "Length of stay", 
     ylab = "Frequency",
     main = "Distribution of Length of Stay",
     xlim = c(0, 400),
     col="#AED6F1", 
     border="white",
     xaxt = "n")  

axis(side = 1, at = seq(0, 400, by = 50), labels = seq(0, 400, by = 50))#adding axis

# Exploring flight_duration distribution
dens <- density(booking$flight_duration) 

# Create histogram for flight_duration
hist(flight_duration, freq = FALSE,
     breaks = 10, 
     xlab = "Flight Duration", 
     ylab = "Frequency",
     main = "Distribution of Flight Duration",
     col = "#3E90CC", 
     border = "#105DAE",
     ylim = c(0, max(dens$y)))  # Set y-axis limits to match density plot

# Add density plot
lines(dens, col = "red")

# Pie chart for chanel mix
proportions <- round(100 * channel_mix / sum(channel_mix), 1)

channel_mix <- table(sales_channel)
pie(channel_mix, 
    labels = paste(c("Internet", "Mobile"), "(", proportions, "%)"), 
    col = c( "#AED6F1","#FFC300"),  # Nice shades of blue
    border = "white",
    main = "Sales Channel Mix",
    cex.main = 1.5)  # Increase main title font size

#fligt day frequencies
days_freq <- table(booking$flight_day)

# Sort days_freq by values in descending order
days_freq <- days_freq[order(-days_freq)]

# Create horizontal bar chart
barplot(days_freq, horiz = TRUE, main = "Count of Flights by Flight Day",
        xlab = "Count", ylab = "Flight Day", col = "#FFC300", border = 'white',las = 1, cex.axis = 0.8)


# Pie chart for trip type
trip_mix <- table(trip_type)
proportions <- round(100 * trip_mix / sum(trip_mix), 1)


pie(trip_mix, 
    labels = paste(c("CircleTrip", "OneWay",'Roundtrip'), "(", proportions, "%)"), 
    col = c( '#105DAE',"#FFC300","#AED6F1"),  
    border = "white",
    main = "Trip Type Mix",
    cex.main = 1.5)  # Increase main title font size



library(ISLR)
summary(booking)

# Define point symbols based on "wants_extra_baggage"
#symbols <- ifelse(wants_extra_baggage == 1, "+", "-")

# Create scatterplot with enhanced visuals
'''plot(num_passengers,length_of_stay, 
     xlab = "", 
     ylab = "",
     main = "Scatterplot of Length of Stay vs Flight Duration",
     col = colors, 
     pch = symbols,  # Use different point symbols based on the value of "wants_extra_baggage"
     cex = 1.5)  # Increase point size'''

# Add legend
'''legend("topright", 
       legend = c("Wants Extra Baggage", "No Extra Baggage"), 
       col = c("#4CAF50", "#F44336"), 
       pch = c("+", "-"),  # Corresponding symbols for legend
       bty = "n", 
       cex = 0.8)  # Adjust legend font size'''




# Create boxplot for wants_extra_baggage=1 and wants_extra_baggage = 0 with length_of_stay < 50
boxplot(length_of_stay[booking$length_of_stay < 50] ~ wants_extra_baggage[booking$length_of_stay < 50], 
        data = booking,
        xlab = "Wants Extra Baggage",
        ylab = "Length of Stay",
        main = "Boxplot of Length of Stay by Wants Extra Baggage (Length < 50)",
        col = c("#FF7D33", "#DAF7A6"),  # Colors for boxplots
        border = "black",  # Border color for boxplots
        notch = TRUE)  # Add notches to boxplots for comparing medians



# Create boxplot
boxplot(length_of_stay ~ trip_type, data = booking,
        main = "Boxplot of Length of Stay by Trip Type",
        xlab = "Trip Type", ylab = "Length of Stay",
        col = "#1E90FF")  # Set boxplot color

# Create boxplot for wants_extra_baggage and wants_extra_baggage = 0 with flight_duration
boxplot(flight_duration ~ wants_extra_baggage, 
        data = booking,
        xlab = "Wants Extra Baggage",
        ylab = "Flight Duration",
        main = "Boxplot of Flight Duration by Wants Extra Baggage",
        col = c("#FF7D33", "#DAF7A6"),  # Colors for boxplots
        border = "black",  # Border color for boxplots
        notch = TRUE)  # Add notches to boxplots for comparing medians

# Pie chart for wants extra baggage
# Count the frequencies of "wants_extra_baggage"
frequency <- table(booking$wants_extra_baggage)

# Calculate proportions
proportions <- round(100 * frequency / sum(frequency), 1)

# Create pie chart with custom colors and percentages
pie(frequency, 
    labels = paste(c("No Extra Baggage", "Wants Extra Baggage"), "(", proportions, "%)"), 
    col = c("#AED6F1", "#FFC300"),  # Nice shades of blue
    border = "white",
    main = "Pie Chart of Wants Extra Baggage",
    cex.main = 1.5) 

# Add legend
legend("bottomright", 
       legend = c("No Extra Baggage", "Wants Extra Baggage"), 
       fill = c("#AED6F1", "#FFC300"),  # Colors for legend
       cex = 0.8,  # Adjust legend font size
       title = "Baggage Preference")  # Add legend title



# Get unique sales channels
sales_channels <- unique(booking$sales_channel)

# Create pie charts for each sales channel
for (channel in sales_channels) {
  # Subset data for the current sales channel
  subset_data <- booking[booking$sales_channel == channel, ]
  
  # Count the frequencies of "wants_extra_baggage" for the current sales channel
  frequency <- table(subset_data$wants_extra_baggage)
  
  # Calculate proportions
  proportions <- round(100 * frequency / sum(frequency), 1)
  
  # Create pie chart with custom colors and percentages
  pie(frequency, 
      labels = paste(c("No Extra Baggage", "Wants Extra Baggage"), "(", proportions, "%)"), 
      col = c("#AED6F1", "#FFC300"),  # Nice shades of blue
      border = "white",  # Change border color to white
      main = paste("Pie Chart of Wants Extra Baggage -", channel),
      cex.main = 1.5)  # Increase main title font size
  
  # Add legend
  legend("bottomright", 
         legend = c("No Extra Baggage", "Wants Extra Baggage"), 
         fill = c("#AED6F1", "#FFC300"),  # Colors for legend
         cex = 0.8,  # Adjust legend font size
         title = "Baggage Preference")  # Add legend title
}



#Modelling
# bucketing length of stay bc it is too much skewed
booking$length_of_stay_bucket <- as.factor(
  ifelse(booking$length_of_stay<=7,"<7",
  ifelse(booking$length_of_stay<=14,"8-14",
  ifelse(booking$length_of_stay<=21,"15-21",
  ifelse(booking$length_of_stay<=28,"22-28",
  ifelse(booking$length_of_stay<=35,"29-35",
  ifelse(booking$length_of_stay<=42,"36-42","42+")))))))


#booking$sales_channel_f <- as.factor(booking$sales_channel)
#booking$trip_type_f <- as.factor(booking$trip_type)
#booking$flight_day_f <- as.factor(booking$flight_day)
#booking$route_f <- as.factor(booking$route)
#booking$booking_origin_f <- as.factor(booking$booking_origin)



df <- booking[,c("num_passengers","sales_channel","trip_type","purchase_lead","length_of_stay_bucket","flight_hour",
                 "flight_day","route","booking_origin",'wants_preferred_seat','wants_in_flight_meals', 'flight_duration',"booking_complete")]

logit_1 <- glm(wants_extra_baggage~., family = binomial,data = df)
summary(logit_1)
library(MASS)

# method for variable selection based on `AIC`
logit_2 <- stepAIC(logit_1)
summary(logit_2)








