#IE2
#1.  Kartik Donwade                   TYCOA62
#2.   Sahil Dhumane                     TYCOA60
#3.  Vedant Ghumade                  TYCOA68
#4.Sangharsh Devtale                TYCOA56
 #5.  Raghavendra Kalkutaki            TYCOA286
demo1=data.frame(Crop_production)
str(demo1)

#1
crop_yield_per_hectare <- demo1 %>%
  group_by(Crop_Type, Crop) %>%
  summarise(
    Mean_Production_per_Hec = mean(Production_in_tons / Area_in_hectares)
  )


ggplot(crop_yield_per_hectare, aes(x = Crop, y = Mean_Production_per_Hec, fill = Crop_Type)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Crop Type",
    y = "Mean Production per Hectare",
    fill = "Crop Type"
  ) +
  ggtitle("Mean Production per Hectare by Crop Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

#2
demo1$Region <- NA



south_zone <- c("andhra pradesh", "karnataka", "kerala", "tamil nadu", "puducherry", "andaman & nicobar islands")
west_zone <- c("gujarat", "dadra and nagar haveli", "goa", "maharashtra")
north_zone <- c("jammu and kashmir", "himachal pradesh", "punjab", "chandigarh", "uttarakhand", "haryana", "delhi", "uttar pradesh")
east_zone <- c("bihar", "sikkim", "arunachal pradesh", "nagaland", "manipur", "mizoram", "tripura", "meghalaya", "assam", "west bengal", "jharkhand", "odisha", "chhattisgarh")

demo1$Region <- ifelse(demo1$State_Name %in% south_zone, "South",
                       ifelse(demo1$State_Name %in% west_zone, "West",
                              ifelse(demo1$State_Name %in% north_zone, "North",
                                     ifelse(demo1$State_Name %in% east_zone, "East", "Other"))))




str(demo1)


library(dplyr)
library(ggplot2)


selected_region <- "South" 

region_crop_production <- demo1 %>%
  filter(Region == selected_region) %>%
  group_by(Crop_Type) %>%
  summarise(Total_Production_in_tons = sum(Production_in_tons))

ggplot(region_crop_production, aes(x = "", y = Total_Production_in_tons, fill = Crop_Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = paste("Crop Type Production in", selected_region, "Region"),
       fill = "Crop Type")  


selected_region <- "North"  

region_crop_production <- demo1 %>%
  filter(Region == selected_region) %>%
  group_by(Crop_Type) %>%
  summarise(Total_Production_in_tons = sum(Production_in_tons))


ggplot(region_crop_production, aes(x = "", y = Total_Production_in_tons, fill = Crop_Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = paste("Crop Type Production in", selected_region, "Region"),
       fill = "Crop Type")

selected_region <- "East"  

region_crop_production <- demo1 %>%
  filter(Region == selected_region) %>%
  group_by(Crop_Type) %>%
  summarise(Total_Production_in_tons = sum(Production_in_tons))

ggplot(region_crop_production, aes(x = "", y = Total_Production_in_tons, fill = Crop_Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = paste("Crop Type Production in", selected_region, "Region"),
       fill = "Crop Type") 


selected_region <- "West"  

region_crop_production <- demo1 %>%
  filter(Region == selected_region) %>%
  group_by(Crop_Type) %>%
  summarise(Total_Production_in_tons = sum(Production_in_tons))


ggplot(region_crop_production, aes(x = "", y = Total_Production_in_tons, fill = Crop_Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = paste("Crop Type Production in", selected_region, "Region"),
       fill = "Crop Type") 









#3

summary(demo1)
descr(demo1,headings = FALSE)
ggplot(demo1,aes(Production_in_tons))+geom_boxplot()
ggplot(demo1,aes(Production_in_tons))+geom_density()
ggplot(demo1,aes(Yield_ton_per_hec))+geom_density()

#4

library(corrplot)


d1 <- cor(demo1[sapply(demo1, is.numeric)])


corrplot(d1,method="number")
#5
state_production <- demo1 %>%
  group_by(State_Name) %>%
  summarize(Total_Production_in_tons = sum(Production_in_tons))

unique_crops <- demo1 %>%
  group_by(Crop) %>%
  summarize(num_states = n_distinct(State_Name)) 

state_production <- demo1 %>%
  group_by(State_Name) %>%
  summarize(Total_Production = sum(Yield_ton_per_hec))
ggplot(state_production, aes(x = reorder(State_Name, -Total_Production), y = Total_Production)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Production Across States",
       x = "State",
       y = "Total Production (tons)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability

