<!---
![CougarStats logo](https://github.com/cwai097/COMP5690/blob/master/www/CougarStats.png)
-->

# _CougarStats_: An R Shiny App for Statistical Data Analysis

## Overview

<img align="right" src="https://github.com/cwai097/COMP5690/blob/master/www/CougarStats.png" alt="rank" width="200" style="margin-top: 20px" /> 

**CougarStats** is an open-source platform-independent browser-based interface for statistical data analysis. 

You can use the app on our website: <https://www.cougarstats.ca/>

or using our backup on Shinyapp.io server by clicking <https://cougarstats.shinyapps.io/CougarStats/>

<br>

Alternatively you can send a pull request to download all the files in this repository and run the app by loading `global.R`, `ui.R`, and `server.R` and clicking `Run App`. Note that the ``CougarStats`` project is not on CRAN, just on github.

<br>

CougarStats has also been Dockerized. You can pull the Docker image <a href="https://hub.docker.com/repository/docker/mmyer/cougarstats/general">here</a> by running:

    docker pull mmyer/cougarstats:latest

then start the container:

    docker run -dp 127.0.0.1:3838:3838 mmyer/cougarstats

and navigate to 127.0.0.1:3838 using a browser of your choice to access a copy of CougarStats running locally on your machine.

More information on Docker can be found <a href="https://www.docker.com/get-started/">here</a>.

<br>

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

<br>

## Who is CougarStats for?

- Students enrolled in an introductory statistics class
- Instructors wanting to use an open-source tool in their labs

<br>

## Shiny app authors

* **<a href="https://github.com/ashokkrish">Ashok Krishnamurthy PhD</a>** | *Template design, model structure and maintainer*
* **<a href="https://github.com/m-myer">Michael Myer</a>** | *Senior Developer*
* **<a href="https://github.com/samantha-v-brian">Samantha Brian</a>** | *Developer*

**Acknowledgement**: In Fall 2022 an earlier version of this interactive R Shiny app was presented as **<a href="https://github.com/cwai097">Crystal Wai</a>**'s COMP 5690: Senior Computer Science Project. From June - August 2023 this project was funded by a student research grant (awarded to Michael Myer) conferred by the Faculty of Science and Technology at Mount Royal University. Starting September 2023 this project is funded by a Provost's Teaching-Learning Enhancement Grant (TLEG). Michael Myer is currently hired as a Project Assistant through this grant.
