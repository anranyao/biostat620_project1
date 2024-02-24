# How Social Apps Relate to Wake-Up Times in College Life, A Federated Learning Study

## Authors
- [Lufeiya Liu](https://github.com/feiyaLL)
- [Anran Yao](https://github.com/anranyao)

## Description
Smartphones are an essential aspect of college students' daily lives, sparking concerns about their effects on mental and physical health. This study examines the impact of social application screen time on college students' sleep patterns, specifically focusing on wake-up times. Utilizing federated learning and linear regression methods to analyze phone usage data while maintaining data privacy, we investigated the relationship between social application usage and wake-up times. The data, collected from two University of Michigan students, indicated a significant positive correlation between the amount of time spent on social apps and later wake-up times. This was after considering variables like daily phone pickups and whether it was a weekday or weekend. Our findings demonstrate the effectiveness of federated learning in achieving results that align with those from traditional linear regression.

## Files
[R code](codes_v1.R)

[Result outputs](codes_v1.html)
## Data collection and Description Plots
Data were collected from two students at the University of Michigan who were taking the Biostatistics 620 course during the Winter 2024 semester. Each student recorded data every day from January 1, 2024, to February 13, 2024. The data from each student (n1=44, n2=44) were combined into one dataset (n=88) for further analysis (confirmation analysis). Since there was no missing data in this dataset, no records were removed from the analysis.

<img src="correlation.png" title="Correlation, distribution and scatter plots">
<img src="density.first.pickup.png" title = "Scatter plot and density plot for the first pickup on a 24-hour clock circle">
<img src="boxplot_v1.png" title="Boxplots for time series data">

## Federated Learning
<img src="Distributed%20Computing.png" alt="The distribution computing platform design to implement our federated learning machinery" title="Distribution computing platform design">

## Acknowledgment
In this project, Lufeiya Liu and Anran Yao contributed equally. Both were responsible for data cleaning, federated learning, confirmation analysis, model diagnosis, and drafting the report. 

