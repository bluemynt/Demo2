
library(RSQLite)
library(dplyr)
library(ggplot2)
library(DBI)


# Selecting Product ID, Category, and Order Date

connection <- RSQLite::dbConnect(RSQLite::SQLite(),"ecomm.db")

# Join "Customers", "Orders", and "Order_items" tables using inner join
joint_cust_query <- "
SELECT 
    c.cust_id,
    c.cust_dob,
    o.order_id,
    o.order_date,
    oi.order_item_quantity,
    oi.order_item_unit_price
FROM 
    order_items AS oi
    JOIN orders AS o ON oi.order_id = o.order_id
    JOIN customers AS c ON o.cust_id = c.cust_id;"


cus_o_oi <- DBI::dbGetQuery(connection, joint_cust_query)

# Maintain "cust_dob" and "order_date" columns to the same data type
cus_o_oi$cust_dob <- as.Date(cus_o_oi$cust_dob)
cus_o_oi$order_date <- as.POSIXct(cus_o_oi$order_date)

# To analyse the platform growth, we can directly use the data from cus_o_oi data frame
order_analysis <- cus_o_oi %>%
  mutate(
    year = lubridate::year(order_date),  # Extract year
    month = lubridate::month(order_date), # Extract month
    order_value = order_item_quantity * order_item_unit_price # to get the order value
  )

# Convert year and month to a date format
order_analysis$date <- as.Date(paste(order_analysis$year, order_analysis$month, "01", sep = "-"), "%Y-%m-%d")

# Generate a line graph to the growth
order_growth_plot <- ggplot(order_analysis, aes(x = date, y = order_value)) +
  geom_line(color = "lightblue") +
  geom_smooth(method = "loess", se = FALSE, color = "skyblue") +  # Add smoother
  geom_area(fill = "lightgrey", alpha = 0.3) +
  labs(x = "Date", y = "Order Value", title = "Order Value Growth per Quarter") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the line plot
print(order_growth_plot)


#Save as png
this_filename_date <- as.character(Sys.Date())
# format the Sys.time() to show only hours and minutes 
this_filename_time <- as.character(format(Sys.time(), format = "%H_%M"))
ggsave(paste0("figures/order_growth_plot",
              this_filename_date,"_",
              this_filename_time,".png"), plot = order_growth_plot)
