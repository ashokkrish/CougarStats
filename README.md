# _CougarStats_: An R Shiny App for Statistical Data Analysis

## Overview

<img align="right" src="https://github.com/ashokkrish/CougarStats/blob/master/www/CougarStats.png" alt="rank" width="200" style="margin-top: 20px" />

**CougarStats** is an open-source platform-independent browser-based interface for statistical data analysis. 

You can use the app on our website: <https://www.cougarstats.ca/>

or using our backup on Shinyapps.io server by clicking <https://cougarstats.shinyapps.io/CougarStats/>

The name _CougarStats_ is inspired by Mount Royal University's athletics mascot, the cougar, symbolizing strength and agility, and by the app's focus on statistics.

<!-- Alternatively you can send a pull request to download all the files in this repository and run the app by loading `global.R`, `ui.R`, and `server.R` and clicking `Run App`. Note that the ``CougarStats`` project is not on CRAN, just on github.

CougarStats has also been Dockerized. You can pull the Docker image <a href="https://hub.docker.com/repository/docker/mmyer/cougarstats/general">here</a> by running:

    docker pull mmyer/cougarstats:latest

then start the container:

    docker run -dp 127.0.0.1:3838:3838 mmyer/cougarstats

and navigate to 127.0.0.1:3838 using a browser of your choice to access a copy of CougarStats running locally on your machine.

Read more about [Getting Started with Docker](https://www.docker.com/get-started/).   -->

## Key features

- Calculate the descriptive statistics (ex. mean, median, mode, quartiles, IQR, standard deviation, check for potential outliers etc.)
- Construct a Boxplot, Histogram, Stem and Leaf plot, and Scatterplot
- Calculate the marginal, joint, union, and conditional probabilities for a contingency table 
- Calculate exact and cumulative probabilities for Binomial, Poisson, Hypergeometric and Negative Binomial distributions
- Calculate cumulative probability for Normal distribution
- Sample size estimation
- Confidence interval and Hypothesis test for
    - one population mean
    - difference between two population means (independent samples)
    - population mean difference of paired populations (dependent samples)
    - one population standard deviation
    - one population proportion
    - difference between two population proportions
    - ratio of two population variances
- Wilcoxon rank sum test (or the Mann-Whitney U test)
- One-Way Analysis of Variance (ANOVA)
- Kruskal-Wallis Test
- Chi-Square test of independence
- Fisher's Exact Test
- Simple Linear Regression and Correlation Analysis
     - Pearson's correlation coefficient
     - Spearman's rank correlation coefficient
     - Kendall's tau
- Multiple Linear Regression
- Binary Logistic Regression

## Who is _CougarStats_ for?

- Students enrolled in an introductory/intermediate statistics class
- Instructors wanting to use an open-source tool in their labs

## Developers
### Project supervisor
- [Ashok Krishnamurthy Ph.D.,](https://github.com/ashokkrish), Associate Professor, Mount Royal University: Template and model designer, copyright holder. See the [faculty biography page of Prof. Krishnamurthy](https://www.mtroyal.ca/ProgramsCourses/FacultiesSchoolsCentres/ScienceTechnology/Departments/MathematicsComputing/Faculty/akrishnamurthy.htm).

### Current project members
- [Jacie Bennett](https://github.com/jacie-b): Senior Developer
- [Maryam Abou El Nasr](https://github.com/Maryaen): Senior Developer

### Citation
- Krishnamurthy, A., et al. (2026). _CougarStats_: An R Shiny app for statistical data analysis [Web application]. https://www.cougarstats.ca/

## Acknowledgements
| Funding Agency / Coursework | Recipient | Date(s) |
|--------|--------|--------|
| MATH 4199: Directed Readings | Maryam Abou El Nasr | January through April 2026 |
| Alberta Innovates Summer Research Studentships | Darren Law Yan Lun | May 2025 through August 2025 |
| Faculty of Science and Technology (FST) Student Research Award | Jacie Bennett | May 2025 through August 2025 |
| Faculty of Science and Technology (FST) Student Research Award | Diana Vi | May 2025 through August 2025 |
| Provost's Teaching and Learning Enhancement Grant (TLEG) | Samantha Brian | January 2025 through May 2025 |
| Provost's Teaching and Learning Enhancement Grant (TLEG) | Bryce Carson | November 2024 through January 2025 |
| Provost's Teaching and Learning Enhancement Grant (TLEG) | Michael Myer | September 2023 through April 2024 |
| Faculty of Science and Technology (FST) Student Research Award | Michael Myer | June through August 2023 |
| COMP 5690: Senior Computer Science Project | Crystal Wai | September through December 2022 |