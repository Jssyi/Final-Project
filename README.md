# Data Science Salaries

#### Jesse Dolan, Bela Banegas, Jennifer Godbersen

## Introduction
With the advent of the internet, the ubiquity of technology, and the growth in companys' interest in utilizing consumer data, the fairly recent field of data science has become a hot topic. Data science is an attractive field for analytical thinkers who can enjoy problem solving, finding insights, presenting findings, and enjoying the benefits of the field (flexibility in modality, growing job market). As students hoping to pursue careers in data science, we were interested in learning recent trends in data science salaries in order to be fully informed when looking for our first full-time jobs. 

The main question we were looking to answer is: What variables affect data science salaries?

To attempt to answer this, we explored six different research questions: 
RQ1: How have data science salaries overall changed over time?
RQ2: How does company size affect salary? 
RQ3: How does experience level affect salary?
RQ4: How does company location affect salary?
RQ5: How does job title affect salary?
RQ6: How does remote ratio (in-person, hybrid, or remote) affect salary? Does this relationship appear to be different for companies of different sizes?

## Data 
This data set includes information about data science salaries. We received this data from Kaggle and has 13732 rows <https://www.kaggle.com/datasets/mexwell/data-science-salary-data>. It was originally retrieved (by Kaggle user Mexwell) from <https://ai-jobs.net/>. This site acts as a job board for data science positions, and it collects various information on these positions, employers, and employees. 

Variables include: year, experience level, employment type (full-time, part-time, contract, freelance), job title, salary, salary currency, salary in usd, employee residence, remote ratio (amount of work done remotely), and company location.

```{r}
file_path <- "salaries.csv"

df <- read.csv(file_path)

head(df)

```

```{r}
empty_rows <- df[!complete.cases(df), ]
if (any(is.na(df))) {
  print("There are NA values in the dataframe.")
} else {
  print("There are no NA values in the dataframe.")
}
```

Data cleaning steps:

There are no empty rows, columns, or NA values at all. We didn't need to sort anything out because we included all necessary sorting inside of our visualization steps. 

## Variables

Our data includes 10 different variables listed below. All are functionally categorical except for the salary and salary_in_usd variables. For our work, we chose to use the salary_in_usd variable and ignore the original unconverted salary variable.

work_year -\> (2020-2024)

experience_level -\> (This includes values such as Entry-Level and Senior)

job_title -\> (Includes a range of different job titles in the field)

salary -\> (Salary in currency of the country the job is in)

salary_currency -\> (The type of currency the salary is in)

salary_in_usd -\> (The salary number in USD)

remote_ratio -\> (Either 0, 50, or 100, for In-person, Hybrid, and Remote respectively)

company_location -\> (Company location by country)

company_size -\> (Company size, either S, M or L)

There was a lack of information on the Kaggle site and <https://ai-jobs.net/> about what qualifies as a small, medium, or large company. 


## Results

###   Salary Over Time

```{r echo=TRUE}
library(ggplot2)
library(dplyr)
# Group by year and calculate the mean salary
salary_over_time <- df %>%
  group_by(work_year) %>%
  summarize(
    mean_salary = mean(salary_in_usd),
    sd_salary = sd(salary_in_usd)
  )

# Plotting
ggplot(salary_over_time, aes(x = work_year, y = mean_salary)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = mean_salary - sd_salary, ymax = mean_salary + sd_salary), width = 0.2) +
  labs(
    title = "Average Salary Over Time",
    x = "Year",
    y = "Mean Salary ($)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)  # for readability use comma format
```

###   Salary by Company Size

```{r echo=TRUE}
library(ggplot2)
library(dplyr)
# Group by year and company size, and calculate the mean salary
salary_over_time <- df %>%
  group_by(work_year, company_size) %>%
  summarize(
    mean_salary = mean(salary_in_usd),
    sd_salary = sd(salary_in_usd)
  )

# Define colors and labels for each company size
company_size_labels <- c("small" = "Small", "medium" = "Medium", "large" = "Large")
company_size_colors <- c("small" = "blue", "medium" = "red", "large" = "yellow")


ggplot(salary_over_time, aes(x = work_year, y = mean_salary, color = company_size, linetype = company_size)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_salary - sd_salary, ymax = mean_salary + sd_salary), width = 0.2) +
  labs(
    title = "Average Salary Over Time by Company Size",
    x = "Year",
    y = "Mean Salary ($)",
    color = "Company Size",
    linetype = "Company Size"
  ) +
  scale_color_manual(values = company_size_colors, labels = company_size_labels) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), labels = company_size_labels) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)  
```
###   Salary by Company Location

```{r echo=TRUE, fig.height=6, fig.width=12}
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Group by unique company_location and calculate the median salary, sorted by median_salary
salary_summary <- df %>%
  group_by(company_location) %>%
  summarize(
    median_salary = median(salary_in_usd),
    iqr_salary = IQR(salary_in_usd)  # Using IQR for error bars
  ) %>%
  arrange(median_salary)  # Sort by median salary

# Create the plot with sorted median salary and IQR error bars
ggplot(salary_summary, aes(x = reorder(company_location, median_salary), y = median_salary)) +
  geom_bar(stat = "identity", fill = "green", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = median_salary - iqr_salary/2, ymax = median_salary + iqr_salary/2), width = 0.2) +
  labs(
    title = "Median Salary by Unique Company Location (Sorted)",
    x = "Company Location",
    y = "Median Salary ($)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)  # Rotate labels for better spacing
  ) +
  scale_y_continuous(labels = scales::comma)  # Prevent scientific notation by using comma format
```
![image](https://github.com/Jssyi/Final-Project/assets/158086989/d0c67af1-359a-4cf5-bcde-f1607844cb30)

The graph illustrates the median salary in USD for data science roles across various countries, sorted from lowest to highest. Qatar stands out with the highest median salary, reaching approximately \$300,000. This significant salary suggests that data science roles in Qatar are among the most lucrative globally. The disparity between Qatar and other locations could be due to several factors, including the high demand for specialized skills, cost of living, or market dynamics in the region.

Meanwhile, the median salary in the United States, which is often considered a hub for technology and data science, is about \$150,000. Although this value is significantly lower than Qatar's, it is still relatively high compared to other countries on the graph. This distribution shows a wide range of salary levels for data science roles worldwide, indicating that geographic location is a significant factor in salary determination. Factors such as cost of living, market maturity, and regional demand for data science talent likely play critical roles in shaping these differences.

###   Salary by Experience

```{r echo=TRUE, fig.height=6, fig.width=12}
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Create consistent experience level naming
df <- df %>%
  mutate(experience_level = case_when(
    experience_level == "EN" ~ "Entry-Level",
    experience_level == "EX" ~ "Executive",
    experience_level == "SE" ~ "Senior",
    experience_level == "MI" ~ "Mid-Level",
    TRUE ~ experience_level  # Default if no match
  ))

# Calculate the median salary by work year and experience level
median_salary <- df %>%
  group_by(work_year, experience_level) %>%
  summarize(
    median_salary = median(salary_in_usd, na.rm = TRUE)  # Using median
  )

# Create a line plot with median salary over time by experience level
ggplot(median_salary, aes(x = work_year, y = median_salary, group = experience_level, color = experience_level)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Median Salary by Experience Level Over Time",
    x = "Year",
    y = "Median Salary (USD)",
    color = "Experience Level"
  ) +
  scale_color_manual(values = c("red", "green", "skyblue", "yellow")) +
  theme_minimal()


```
![image](https://github.com/Jssyi/Final-Project/assets/158086989/a1e29a3a-08b6-4162-b7b6-614cdcff354c)


The graph displays the median salary for data science roles across four experience levels—Entry-Level, Mid-Level, Senior, and Executive—from 2020 to 2024. As expected, the salaries increase with experience, with Entry-Level consistently earning the least and Executive earning the most. This trend remains consistent throughout the years plotted, indicating a clear progression in compensation as professionals advance in their careers. The gap between the different levels is significant.

Over the years, the graph shows a general upward trend in median salary across all experience levels, although the rate of increase varies. The difference between Mid-Level and Senior is notable, with Senior positions commanding a much higher salary, reflecting the added responsibility and expertise required at that level. The consistency in salary growth over the period indicates a robust market for data science roles, with experienced professionals enjoying significant compensation advantages. Despite minor fluctuations, the overall upward trajectory suggests a positive outlook for career progression in the data science field.


### Salary by Job Title

```{r echo=TRUE, message=TRUE, warning=TRUE}
# Reorder job titles from greatest to least based on median salary
df_filtered$job_title <- factor(df_filtered$job_title, levels = median_salaries$job_title[order(-median_salaries$median_salary)])

# Create the plot using the filtered dataframe
ggplot(df_filtered, aes(x = job_title, y = salary_in_usd)) +
  geom_boxplot(fill = "skyblue", color = "black", outlier.shape = NA) +  # Use geom_boxplot to create boxplots
  geom_jitter(data = df_filtered %>% filter(salary_in_usd < quantile(salary_in_usd, 0.25) - 1.5*IQR(salary_in_usd) | salary_in_usd > quantile(salary_in_usd, 0.75) + 1.5*IQR(salary_in_usd)), width = 0.2, color = "black", alpha = 0.5) +
  geom_hline(yintercept = median(df_filtered$salary_in_usd), color = "red", linetype = "dashed") +  # Add a dashed line for median
  labs(
    title = "Salary Distribution by Job Title",
    x = "Job Title",
    y = "Salary ($)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)  # Rotate labels for better spacing
  ) +
  scale_y_continuous(labels = scales::comma)  # Prevent scientific notation by using comma format

```
[![image](https://github.com/Jssyi/Final-Project/blob/7e65f941357eaf18f7d1fc8b9d029383bdac6494/salarybyjobtitle.png)


### Salary by Remote Ratio and Company Size

```{r echo=TRUE}
# Convert remote_ratio to factor with specified levels
df$remote_ratio <- factor(df$remote_ratio, levels = c(0, 50, 100))
df$company_size <- factor(df$company_size, levels = c("S", "M", "L"))

# Print summary of counts for each level of company_size
cat("Counts for each level of company_size:\n")
print(summary(df$company_size))

# Print summary of counts for each level of remote_ratio
cat("\nCounts for each level of remote_ratio:\n")
print(summary(df$remote_ratio))

```
[![image](https://github.com/Jssyi/Final-Project/blob/7e65f941357eaf18f7d1fc8b9d029383bdac6494/table.png)

```{r echo=TRUE}
# Load necessary library
library(ggplot2)
library(scales)
library(dplyr)

# Summarize data by company_size and remote_ratio
summary_df <- df %>%
  group_by(company_size, remote_ratio) %>%
  summarize(count = n()) %>%
  mutate(prop = count / sum(count))

# Create the plot
ggplot(summary_df, aes(x = company_size, y = prop, fill = remote_ratio)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Distribution of Remote Ratios by Company Size",
    x = "Company Size",
    y = "Proportion of Entries"
  ) +
  scale_fill_manual(values = c("skyblue", "lightgreen", "pink")) +  # Custom color palette
  theme_minimal()

```
[![image](https://github.com/Jssyi/Final-Project/blob/7e65f941357eaf18f7d1fc8b9d029383bdac6494/stackedbarchart.png)

```{r echo=TRUE}
# Calculate count of jobs for each combination of time, remote_ratio, and company_size
job_counts <- aggregate(job_title ~ work_year + remote_ratio + company_size, data = df, FUN = length)

# Plot with faceting by company_size and free y-axis scales
ggplot(job_counts, aes(x = work_year, y = job_title, group = remote_ratio, color = remote_ratio)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Number of Jobs by Remote Ratio Over Time (by Company Size)",
    x = "Year",
    y = "Number of Jobs",
    color = "Remote Ratio"
  ) +
  scale_color_manual(values = c("pink", "green", "skyblue")) +
  theme_minimal() +
  facet_wrap(~ company_size, scales = "free_y") +  # Facet by company_size variable with free y-axis scales
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))  # Rotate y-axis labels vertically

```
[![image](https://github.com/Jssyi/Final-Project/blob/7e65f941357eaf18f7d1fc8b9d029383bdac6494/jobsovertime.png)

```{r echo=TRUE}
# Convert time to factor if it's not already
df$work_year <- as.factor(df$work_year)
df$work_year <- factor(df$work_year, levels = c("2020", "2021", "2022", "2023", "2024"))

# Calculate average salary for each combination of time, remote_ratio, and company_size
average_salary <- aggregate(salary_in_usd ~ work_year + remote_ratio + company_size, data = df, FUN = median)

# Plot with faceting by company_size and adjusted facet width
ggplot(average_salary, aes(x = work_year, y = salary_in_usd, group = remote_ratio, color = remote_ratio)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Median Salary by Remote Ratio Over Time (by Company Size)",
    x = "Year",
    y = "Median Salary (USD)",
    color = "Remote Ratio"
  ) +
  scale_color_manual(values = c("pink", "green", "skyblue")) +
  theme_minimal() +
  facet_wrap(~ company_size, scales = "free_y") +  # Facet by company_size variable with free y-axis scales
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

```
[![image](https://github.com/Jssyi/Final-Project/blob/7e65f941357eaf18f7d1fc8b9d029383bdac6494/salaryovertime.png)

```{r echo=TRUE}
# Create the horizontally rotated box plot with angled x-axis labels, no legend, and custom colors
ggplot(df, aes(x = salary_in_usd, y = "", fill = remote_ratio)) +
  geom_boxplot() +
  facet_grid(company_size ~ remote_ratio, scales = "free_y") +
  labs(
    title = "Salary Distribution by Remote Ratio and Company Size",
    x = "Salary (USD)",
    y = "Remote Ratio (%)"
  ) +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_fill_manual(values = c("skyblue", "skyblue", "skyblue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # Remove legend

```
[![image](https://github.com/Jssyi/Final-Project/blob/7e65f941357eaf18f7d1fc8b9d029383bdac6494/remoteratiosizedistribution.png)

## Conclusion
