# Data Science Salaries

#### Jesse Dolan, Bela Banegas, Jennifer Godbersen

## Introduction
With the ubiquity of technology in our everyday lives and the growing ability to extract large amounts of data from it, companies' and governments' interest in utilizing consumer data has grown. As a result, the fairly recent field of data science has become a hot topic. Data science is an attractive field for analytical thinkers who enjoy problem-solving, discovering insights, presenting findings, and enjoying the touted benefits of the field (flexibility in modality, growing job market). As students hoping to pursue careers in data science, we were interested in learning recent trends in data science salaries in order to be fully informed when looking for our first full-time jobs. 

The main question we were looking to answer is: What variables affect data science salaries?

To attempt to answer this, we explored six different research questions: 
-   RQ1: How have data science salaries overall changed over time?
-   RQ2: How does company size affect salary? 
-   RQ3: How does company location affect salary?
-   RQ4: How does experience level affect salary?
-   RQ5: How does job title affect salary?
-   RQ6: How does remote ratio (in-person, hybrid, or remote) affect salary? Does this relationship appear to be different for companies of different sizes?

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

###   RQ1: Salary Over Time

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
[![image](https://github.com/Jssyi/Final-Project/blob/e741e872a7b804b7767c0a677429938fa56984ed/avgsalary(1).png)

###   RQ2: Salary by Company Size

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
[![image](https://github.com/Jssyi/Final-Project/blob/e741e872a7b804b7767c0a677429938fa56984ed/avgsalarycompany(1).png)

After discussing the previous graphs in our final presentation I decided to make changes to the graphs
to include median salary rather than mean. This will address issues of extreme variance and outliers while 
maintaining the same relationships. Additionally, we received feedback that displaying a standard deviation
of one may not truly add to the information of the graph. Keeping in mind the importance of variability, 
I instead included IQR rather than standard deviation. 

###  RQ1 : Attempt 2 

```{r}
# Group by year and calculate the median salary and IQR
salary_over_time <- df %>%
  group_by(work_year) %>%
  summarize(
    median_salary = median(salary_in_usd),
    q1 = quantile(salary_in_usd, 0.25),
    q3 = quantile(salary_in_usd, 0.75)
  )

# Calculate IQR
salary_over_time$IQR <- salary_over_time$q3 - salary_over_time$q1

# Plotting
ggplot(salary_over_time, aes(x = work_year, y = median_salary)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = median_salary - IQR, ymax = median_salary + IQR), width = 0.2) +
  labs(
    title = "Median Salary Over Time with IQR",
    x = "Year",
    y = "Median Salary ($)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)  # for readability use comma format
```

[![image](https://github.com/Jssyi/Final-Project/blob/e741e872a7b804b7767c0a677429938fa56984ed/mediansalaryovertime.png)

#### Discussion: 

The line graph illustrates the median salary in USD across five years, from 2020 to 2024. From 2020 to
2022, there is a consistent upward trend in median salaries, suggesting a period of salary growth likely
influenced by factors such as industry expansion and heightened demand. However, around 2023 and 2024, the
median salary appears to stabilize, indicating a potential saturation point or market equilibrium. This
stabilization prompts questions for data scientists regarding the underlying causes, whether they stem from
market dynamics, policy changes, or other factors. Additionally, the error bars at each data point
represent the interquartile range (IQR), showcasing the variability in salaries around the median. Larger
IQRs imply greater dispersion in salaries, offering valuable insights for further exploration by data
science professionals. Overall, this graph provides a comprehensive view of salary trends over time,
enabling analysis, prediction, and understanding of compensation dynamics within the dataset's context.

###  RQ2 : Attempt 2 

```{r}
# Group by year and company size, and calculate the median salary and IQR
salary_over_time <- df %>%
  group_by(work_year, company_size) %>%
  summarize(
    median_salary = median(salary_in_usd),
    q1_salary = quantile(salary_in_usd, 0.25),
    q3_salary = quantile(salary_in_usd, 0.75)
  )

# Define colors and labels for each company size
company_size_labels <- c("small" = "Small", "medium" = "Medium", "large" = "Large")
company_size_colors <- c("small" = "blue", "medium" = "red", "large" = "yellow")

# Create the plot
ggplot(salary_over_time, aes(x = work_year, y = median_salary, color = company_size, linetype = company_size)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = q1_salary, ymax = q3_salary), width = 0.2) +  # Use IQR for error bars
  labs(
    title = "Median Salary Over Time by Company Size",
    x = "Year",
    y = "Median Salary ($)",
    color = "Company Size",
    linetype = "Company Size"
  ) +
  scale_color_manual(values = company_size_colors, labels = company_size_labels) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), labels = company_size_labels) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)
```
[![image](https://github.com/Jssyi/Final-Project/blob/e741e872a7b804b7767c0a677429938fa56984ed/mediansalarycompany.png)

#### Discussion: 

The graph presents how median salaries fluctuate according to company size over the course of five years,
spanning from 2020 to 2024. There are three distinct categories: large (L), medium (M), and small (S)
companies, each represented by solid, dashed, and dotted lines. Large companies exhibit a steady upward
trajectory in median salary over the entire duration, indicating consistent growth for employees within
this category. Conversely, medium-sized companies display a similar overall trend of increasing median
salaries, with noticeable fluctuations. These fluctuations likely stem from various factors such as market
dynamics, industry-specific shifts, or internal company policies. Meanwhile, small companies portray a more
unstable pattern, with median salaries oscillating throughout the years. Such variability may reflect the
challenges and constraints of smaller businesses. The graph displays significant disparities in median \
salaries across company sizes, with larger enterprises consistently offering higher compensation. 
Ultimately, the graph serves as a valuable resource for job seekers, HR professionals, and economists
alike, offering critical insights into the multifaceted dynamics of compensation trends relative to company
size.


###   RQ3: Salary by Company Location

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

#### Discussion: 
The graph illustrates the median salary in USD for data science roles across various countries, sorted from lowest to highest. Qatar stands out with the highest median salary, reaching approximately \$300,000. This significant salary suggests that data science roles in Qatar are among the most lucrative globally. The disparity between Qatar and other locations could be due to several factors, including the high demand for specialized skills, cost of living, or market dynamics in the region.

Meanwhile, the median salary in the United States, which is often considered a hub for technology and data science, is about \$150,000. Although this value is significantly lower than Qatar's, it is still relatively high compared to other countries on the graph. This distribution shows a wide range of salary levels for data science roles worldwide, indicating that geographic location is a significant factor in salary determination. Factors such as cost of living, market maturity, and regional demand for data science talent likely play critical roles in shaping these differences.

###   RQ4: Salary by Experience

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

#### Discussion: 

The graph displays the median salary for data science roles across four experience levels—Entry-Level, Mid-Level, Senior, and Executive—from 2020 to 2024. As expected, the salaries increase with experience, with Entry-Level consistently earning the least and Executive earning the most. This trend remains consistent throughout the years plotted, indicating a clear progression in compensation as professionals advance in their careers. The gap between the different levels is significant.

Over the years, the graph shows a general upward trend in median salary across all experience levels, although the rate of increase varies. The difference between Mid-Level and Senior is notable, with Senior positions commanding a much higher salary, reflecting the added responsibility and expertise required at that level. The consistency in salary growth over the period indicates a robust market for data science roles, with experienced professionals enjoying significant compensation advantages. Despite minor fluctuations, the overall upward trajectory suggests a positive outlook for career progression in the data science field.


### RQ5: Salary by Job Title

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

#### Discussion: 


### RQ6: Salary by Remote Ratio and Company Size

Initially, we looked solely are remote ratio and its relationship to salary, but upon receiving feedback from the professor, we realized that company size may have an influence on remote ratio policies. Because of this, we decided to examine the interaction of remote ratio, company size, and salary.

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

#### Discussion: 

As shown in the table above, there were a significantly larger number of medium-sized companies in the database. We were interested in looking at the count of in-person, hybrid, and remote jobs for each company size, but due to the very different scales we decided to display the data in a 100% stacked bar chart, as shown in the plot below. 

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

#### Discussion: 

In the 100% stacked bar chart, we can observe slightly different patterns by company size. Small and large companies had large ratios of hybrid jobs, and small companies had a higher ratio of totally remote jobs (nearly twice as many as large companies). This may be due to a data scientist working independently for a small company as opposed to on a collaborative team for a medium or large company. Still, all company sizes had a considerable proportion of remote and hybrid positions (all were at least 40% combined remote/hybrid). 

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

#### Discussion: 

The next relationship we wanted to look at was the number of jobs by remote ratio over the last 5 years. It should be noted that the dataset was updated two months ago, so data from 2024 may not be complete. With that in mind, it appears that the number of jobs in the dataset is generally increasing over time. It appears that there is not a large growth in the number of hybrid jobs in any three category, but in-person and remote jobs do seem to be growing (at least by the end of 2023, the last full-year the data was updated). 

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

#### Discussion: 

Next, we wanted to look at the median salary over time for the different remote ratios in different company sizes. We can see that salaries for hybrid positions appear to be decreasing in medium and large companies, but increasing in small companies. In-person and remote salaries tended to have similar relationships in small and medium companies, in large companies they appear to be inverted (with median salary of remote positions decreasing while in-person median salary increased from 2022-2023). For the incomplete year of 2024, the median salary of remote jobs is rising, while the median salary of in-person jobs is declining. It would be very interesting to see if this trend continues, or if it is due to incomplete data.

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

#### Discussion: 

Finally, we wanted to look at the individual distributions of salary by remote ratio and company size. There are some striking differences between the intersections, with in-person and remote positions at medium companies having very high outliers (potentially due to the much larger sample size). Surprisingly, the hybrid positions at large companies also had a notable amount of outliers. The hybrid positions at small and medium companies had very small distributions with no outliers, which may very well be caused by the small sample sizes in these categories. The greatest mean salary belonged to the in-person positions at medium-sized companies, followed by the remote positions at large companies and the in-person positions at large companies. 

## Conclusion


In conclusion, our examination of median salary trends in the data science domain has revealed noteworthy patterns and areas for further exploration. The consistent upward trajectory in median salaries from 2020 to 2022 suggests a period of substantial growth, potentially fueled by factors such as expanding industries and heightened demand for specialized skills. However, the stabilization observed around 2023 and 2024 prompts inquiries into potential saturation points or shifts in market dynamics. Furthermore, the disparities in median salaries across different company sizes and experience levels underscore the nuanced nature of compensation dynamics within the field.

Moving forward, future research endeavors could focus on several key areas. Firstly, delving deeper into the changes in salary trends over time, particularly during pivotal periods such as the pandemic years, could provide valuable insights into evolving compensation dynamics and their underlying drivers. Additionally, exploring the impact of additional factors, such as geographical location or educational background, on salary variations could offer a more comprehensive understanding of the factors influencing compensation levels in data science roles.

Moreover, investigating the interaction between remote work policies, company size, and salary could shed light on emerging trends in remote work practices and their implications for compensation strategies. By examining how these factors intersect and influence each other, we can better understand the evolving landscape of work arrangements and their impact on salary structures.

Overall, our findings highlight the need for ongoing research to deepen our understanding of salary trends in the data science field. By addressing these questions, we can inform strategic decision-making and contribute to the development of more equitable and competitive compensation practices within the industry.
