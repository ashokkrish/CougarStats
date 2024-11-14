<!---
![CougarStats logo](https://github.com/cwai097/COMP5690/blob/master/www/CougarStats.png)
-->

# _CougarStats_: An R Shiny App for Statistical Data Analysis

## Overview

<img align="right" src="https://github.com/cwai097/COMP5690/blob/master/www/CougarStats.png" alt="rank" width="200" style="margin-top: 20px" />

**CougarStats** is an open-source platform-independent browser-based interface for statistical data analysis. 

You can use the app on our website: <https://www.cougarstats.ca/>

or using our backup on Shinyapp.io server by clicking <https://cougarstats.shinyapps.io/CougarStats/>

Alternatively you can send a pull request to download all the files in this repository and run the app by loading `global.R`, `ui.R`, and `server.R` and clicking `Run App`. Note that the ``CougarStats`` project is not on CRAN, just on github.

CougarStats has also been Dockerized. You can pull the Docker image <a href="https://hub.docker.com/repository/docker/mmyer/cougarstats/general">here</a> by running:

    docker pull mmyer/cougarstats:latest

then start the container:

    docker run -dp 127.0.0.1:3838:3838 mmyer/cougarstats

and navigate to 127.0.0.1:3838 using a browser of your choice to access a copy of CougarStats running locally on your machine.

Read more about [Getting Started with Docker](https://www.docker.com/get-started/).

## Key features

- Calculate the descriptive statistics (ex. mean, median, mode, quartiles, IQR, standard deviation, check for potential outliers etc.)
- Construct a Boxplot, Histogram, Stem and Leaf plot, and Scatterplot
- Calculate the marginal, joint, union, and conditional probabilities for a contingency table 
- Calculate exact and cumulative probabilities for Binomial and Poisson distributions
- Calculate cumulative probability for Normal distribution
- Sample size estimation
- Confidence interval and Hypothesis test for
    - one population mean
    - difference between two population means (independent samples)
    - population mean difference of paired populations (dependent samples)
    - one population proportion
    - difference between two population proportions
- One-Way Analysis of Variance (ANOVA)
- Chi-Square test of independence
- Simple Linear Regression and Pearson Correlation Coefficient

## Who is CougarStats for?

- Students enrolled in an introductory statistics class
- Instructors wanting to use an open-source tool in their labs

## Developers
### Current project members
- [Ashok Krishnamurthy Ph.D.](https://github.com/ashokkrish): Project supervisor, template and model designer, copyright holder
- [Bryce Carson](https://github.com/bryce-carson): Senior Developer
- [Samantha Brian](https://github.com/samantha-v-brian): Developer

### Past project members
- [Michael Myer](https://github.com/m-myer): Senior Developer
- [Michael Walsh](https://github.com/mwals360): Developer

## Financial support of the development of CougarStats**
| Grant and/or Funding Agency | Recipient | Date(s) |
|--------|--------|--------|
| FST Mount Royal University Student Research Grant | Michael Myer | June through August 2023 |
| Provost's Teaching-Learning Enhancement Grant (TLEG) | Michael Myer | September 2023 through April 2024 |
| TLEG | Bryce Carson | November 2024 through January 2025 |
| TLEG | Samantha Brian | January 2025 |

## Acknowledgements
CougarStats is partially-derived from the senior project coursework of Crystal Wai, completed in Fall 2022 ([Mount Royal University _COMP 5690: Senior Computer Science Project_](https://catalog.mtroyal.ca/preview_course.php?catoid=26&coid=40800&print)).
