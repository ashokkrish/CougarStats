Overview
--------

inflection is a package that finds the inflection point of a planar
curve which is given as a data frame of discrete (xi,yi) points.

Basic functions are:

-   `ese()` Extremum Surface Estimator
-   `bese()` Bisection Extremum Surface Estimator
-   `ede()` Extremum Distance Estimator
-   `bede()` Bisection Extremum Distance Estimator
-   `findiplist()` performs both ESE and EDE
-   `check_curve()` checks a curve for its convexity type
-   `uik()` finds the UIK estimation for elbow or knee point of a curve
-   `d2uik()` performs UIK method on the approximation of second order
    derivatives

Install the inflection package and then read vignettes:

-   `vignette('inflection', package ='inflection')`
-   `vignette('inflectionDevelopingMethods', package ='inflection')`
-   `vignette('inflectionMissionImpossible', package ='inflection')`

Installation
------------

``` r
# Install with dependencies:
install.packages('inflection',dependencies=TRUE)
```

Usage
-----

``` r
library(inflection)

data("table_01")
x=table_01$x
y=table_01$y
plot(x,y,col='blue',pch=19,cex=0.25)
cc=check_curve(x,y)
cc
# $ctype
# [1] "convex_concave"
# 
# $index
# [1] 0
ipese=ese(x,y,cc$index);ipese
#      j1  j2 chi
# ESE 170 332   5
ipbese=bese(x,y,cc$index);ipbese
# $iplast
# [1] 5
# 
# $iters
#     n     a     b ESE
# 1 501 2.000 8.000   5
# 2 163 4.556 5.444   5
# 3  75 4.784 5.216   5
# 4  37 4.892 5.108   5
# 5  19 4.952 5.048   5
# 6   9 4.976 5.024   5
# 7   5 4.988 5.012   5
ipede=ede(x,y,cc$index);ipede
#      j1  j2 chi
# EDE 155 347   5
ipbede=bede(x,y,cc$index);ipbede
# $iplast
# [1] 5
# 
# $iters
#     n     a     b EDE
# 1 501 2.000 8.000   5
# 2 193 4.400 5.600   5
# 3 101 4.664 5.336   5
# 4  57 4.808 5.192   5
# 5  33 4.892 5.108   5
# 6  19 4.940 5.060   5
# 7  11 4.964 5.036   5
# 8   7 4.976 5.024   5
# 9   5 4.988 5.012   5
ipall=findiplist(x,y,cc$index);ipall
#      j1  j2 chi
# ESE 170 332   5
# EDE 155 347   5
abline(v=ipese[,3],lty=2)
abline(v=ipede[,3],lty=3,col='red')
#
```

Why should I use inflection package in R?
-----------------------------------------

-   Because it does not imply any kind of functional hypothesis for the
    data under examination
-   Because it can give you an estimation despite the level of noise
    added to initial data
-   Because it is fast and can use parallel computing if you ask for it
-   Due to its simplicity it can handle data sets with more than a
    million rows in negligible execution time
-   It uses sophisticated iterative methods like bisection of Numerical
    Analysis and locates the inflection point when it is not directly
    visible form the first sight

Do scientists use inflection package in their work?
---------------------------------------------------

Yes, inflection package has approximately 20K installations in RStudio
and next is a sample of works that have used it for computing inflection
points:

    - Ferguson, SH, Yurkowski, DJ, Young, BG, et al.(2019). Do intraspecific life history patterns follow interspecific predictions? A test using latitudinal variation in ringed seals. Popul. Ecol.; 1– 12. https://doi.org/10.1002/1438-390X.12008
    - David F. Midgley , Sunil Venaik, Demetris Christopoulos (2018). Culture as a Configuration of Values: An Archetypal Perspective, in (ed.) Experimental Economics and Culture (Research in Experimental Economics, Volume 20) Emerald Publishing Limited, pp.63 - 88. https://www.emeraldinsight.com/doi/abs/10.1108/S0193-230620180000020004
    - Maxwell, T. M., Silva, L. C. R., & Horwath, W. R. ( 2018). Predictable oxygen isotope exchange between plant lipids and environmental water: Implications for ecosystem water balance reconstruction. Journal of Geophysical Research: Biogeosciences, 123, 2941– 2954. https://doi.org/10.1029/2018JG004553
    - Ortega‐García, S, Guevara, L, Arroyo‐Cabrales, J,et al.(2017). The thermal niche of Neotropical nectar‐feeding bats: Its evolution and application to predict responses to global warming. Ecol Evol. 2017; 7: 6691– 6701. https://doi.org/10.1002/ece3.3171
    - Uematsu, A.; Hata, J.; Komaki, Y.; Seki, F.; Yamada, C.; Okahara, N.; Kurotaki, Y.; Sasaki, E. & Okano, (2017). H.Mapping orbitofrontal-limbic maturation in non-human primates: A longitudinal magnetic resonance imaging study NeuroImage, 163, 55 – 67. https://doi.org/10.1016/j.neuroimage.2017.09.028 
    - Enderle R, Sander F, Metzler B (2017). Temporal development of collar necroses and butt rot in association with ash dieback. iForest 10: 529-536. https://doi.org/10.3832/ifor2407-010
    - Jason Gibbs, Neelendra K. Joshi, Julianna K. Wilson, Nikki L. Rothwell, Karen Powers, Mike Haas, Larry Gut, David J. Biddinger, Rufus Isaacs (2017). Does Passive Sampling Accurately Reflect the Bee (Apoidea: Anthophila) Communities Pollinating Apple and Sour Cherry Orchards?, Environmental Entomology, Volume 46, Issue 3, June 2017, Pages 579–588. https://doi.org/10.1093/ee/nvx069
    - Moghaddam, R. F.; Asghari, V.; Moghaddam, F. F.; Lemieux, Y. & Cheriet, M. (2017). A Monte-Carlo approach to lifespan failure performance analysis of the network fabric in modular data centers Journal of Network and Computer Applications, 87, 131 - 146. https://doi.org/10.1016/j.jnca.2017.03.015
    - Hoxha, J.; Jiang, G. & Weng, C. (2016). Automated learning of domain taxonomies from text using background knowledge Journal of Biomedical Informatics, 63, 295 - 306. https://doi.org/10.1016/j.jbi.2016.09.002
    -  Willis CG, Franzone BF, Xi Z and Davis CC (2014). The establishment of Central American migratory corridors and the biogeographic origins of seasonally dry tropical forests in Mexico. Front. Genet. 5:433. https://doi.org/10.3389/fgene.2014.00433

Contact
-------

Please send comments, suggestions or bug breports to
dchristop$econ.uoa.gr
