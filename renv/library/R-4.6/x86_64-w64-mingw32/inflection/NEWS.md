What’s new in version 1.3.7
---------------------------

1. Fix minor issue of Author in DECRIPTION file

What’s new in version 1.3.6
---------------------------

1. Fix minor issue of item values at help files for OS "-linux-x86_64-debian-clang", "r-devel-linux-x86_64-debian-gcc"


What’s new in version 1.3.5
---------------------------

### New functions added for knee point estimation

-   `check_curve()` checks a curve for its convexity type
-   `uik()` finds the UIK estimation for elbow or knee point of a curve
-   `d2uik()` performs UIK method on the approximation of second order
    derivatives

The Unit Invariant Knee (UIK) estimator is described in article:

-   Demetris T. Christopoulos (2016). Introducing Unit Invariant Knee
    (UIK) As an Objective Choice for Elbow Point in Multivariate Data
    Analysis Techniques. Available at SSRN:
    <a href="http://dx.doi.org/10.2139/ssrn.3043076" class="uri">http://dx.doi.org/10.2139/ssrn.3043076</a>

### Vignettes added for the package

Install the inflection package and then read vignettes:

-   `vignette('inflection', package ='inflection')` an introductory
    vignette
-   `vignette('inflectionDevelopingMethods', package ='inflection')`
    reproduction of results for article:
    -   Demetris T. Christopoulos (2014). Developing methods for
        identifying the inflection point of a convex/concave curve.
        arXiv:1206.5478v2 \[math.NA\].
        <a href="https://arxiv.org/pdf/1206.5478v2" class="uri">https://arxiv.org/pdf/1206.5478v2</a>
-   `vignette('inflectionMissionImpossible', package ='inflection')` a
    vignette for ‘impossible’ missions

#### Minor changes

Remove the option to plot in two separate pdfs ‘ese\_iterations.pdf’,
‘ede\_iterations.pdf’ from `findipiterplot()` and open new plot windows
instead of saving files.

What’s new in version 1.3
-------------------------

### Use of parallel computing under request (doparallel=TRUE) for the functions:

-   `ese()`

-   `bese()`

-   `findiplist()`

-   `findipiterplot()`

### Repair of function findipiterplot()

1.  Fix bugs in calling the function
2.  Remove indirect methods CRESE, CREDE due to their limited
    functionality
3.  Plot in two separate pdfs ‘ese\_iterations.pdf’ and
    ‘ede\_iterations.pdf’
4.  Check for the existence of sufficient number of results before
    creating confidence intervals

What’s new in version 1.2
-------------------------

1.  The function eixf(x, y, f, i) was removed as not essentially
    necessary
2.  New functions with self declaring names were added:

-   `ese(x,y,index)`
-   `bese(x,y,index)`
-   `ede(x,y,index)`
-   `edeci(x,y,index)`
-   `bede(x,y,index)` All functions require `length(x)>=4` in order to
    create numeric output

1.  The function `findipiterplot(x,y,index)` was improved and became
    `findipiterplot(x, y, index, plots = TRUE, crossrun = FALSE, ci = FALSE)`
2.  Changes in NAMESPACE
3.  Reference to new paper:

Demetris T. Christopoulos (2016). On the efficient identification of an
inflection point.International Journal of Mathematics and Scientific
Computing, (ISSN: 2231-5330), vol. 6(1).
<a href="https://demovtu.veltech.edu.in/wp-content/uploads/2016/04/Paper-04-2016.pdf" class="uri">https://demovtu.veltech.edu.in/wp-content/uploads/2016/04/Paper-04-2016.pdf</a>
