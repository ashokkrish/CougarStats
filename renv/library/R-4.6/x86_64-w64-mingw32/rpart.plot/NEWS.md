Changes to rpart.plot
---------------------

## 3.1.4 Dec 30, 2025

  Updated some web addresses in the man pages.

## 3.1.3 July 28, 2025

  Updates for CRAN checks in R version 4.5.1.

## 3.1.2 Feb 16, 2024

  Updates for R version 4.3.2.
  For example, had to change "sort.unique" to "sort_unique".

## 3.1.1  May 20, 2021

  Minor updates for R version 4.2.0.

## 3.1.0  July 23, 2021

  Minor updates for R version 4.1.0.
  These updates quieten some warnings from sprintf when prp's trace flag is set.

## 3.0.9  Sep 16, 2020

  We now support models built with rpart(formula, data=func(data)),
  where the data argument is a function call.

  Removed the very old function rpart.plot.version1.

  Quietened the warning for rpartScore models (treat rpartScore as a special case).

  Minor documentation updates.

  Updated the libraries shared with the earth and plotmo packages.

  Updated the test scripts for R version 4.0.3.

## 3.0.8  Aug 21, 2019

  Updated test scripts for the new random number generator
  that came with R version 3.6.0.

  Fixed an incorrect warning: ncol(fit) 5 != length(ylevels) 6.
  This was issued under certain circumstances for multiclass models
  when a class is never predicted in the training data.

  The legend for multiclass models now shows classes that are
  never predicted in the training data, even when rpart doesn't
  include such classes in yval2.

## 3.0.7  Apr 11, 2019

  Minor documentation updates.
  Minor updates to the libraries shared with earth and plotmo.
  This package now requires R version at least 3.4.0.

## 3.0.6  Nov 23, 2018

  Minor changes to internal function calls to prevent warnings when
  options(warnPartialMatchArgs=TRUE).

  Test script touchups.

  Added "LazyData: yes" to the DESCRIPTION file.

## 3.0.5  Nov 2, 2018

  We now handle splits with very small values better (less than 1e-10).
  Thanks to Petr Lenhard for help on this.

  Added the "under.percent" argument to control spacing before
  the percentage when 100 is used with the "extra" argument.

## 3.0.4  Aug 13, 2018

  Variables with values 0 and 1 only are now treated as logical variables
  if roundint=TRUE i.e. they are displayed as "var is 0" and "var is 1".

  Rpart.rules now explicitly shows if the model is a null model.

  We now work better with caret train models. (Extended handling
  of model$call$data, so rpart.plot no longer issues the warning
  "Bad data field in model call" with such models.)

  Documentation updates.

## 3.0.3  Aug 5, 2018

  Survival responses are now once again handled correctly (broken
  in version 3.0.0).  Thanks to Robert Redd for help on this.

  Added the rpart.predict function for printing the rules for predictions.

  If rpart.rules(nn=TRUE) we now prepend (rather than append) the node numbers.

  Tweaks to make the colors more visible in the legend for multiclass models.

  Documentation updates.

## 3.0.2  July 25, 2018

  Documentation fixes.

## 3.0.1  July 19, 2018

  Touchups to the documentation and code for rules.

## 3.0.0  July 10, 2018

  Added the rpart.rules function to display a set of rules for a tree.

  Numbers in the displayed tree will now be a little different for
  some models, because of the following changes:

    (i) The new roundint argument.  With roundint=TRUE (default), we
    display "nsiblings < 3" instead of "nsiblings < 2.5" if all values
    of "nsiblings" in the training data are integers (where "nsiblings"
    could be any variable in the tree, with R type "numeric" or "integer").
    See Section 4.1 in the package vignette.
    If roundint=TRUE and the data used to build the model is no longer
    available, a warning will be issued.

    (ii) When applying the digits argument in splits, if the last digit
    is 5 we now always round up (rather than rounding to even).
    Thus "x < 12.5" is now rounded to "x < 13" (rather than "x < 12",
    which was misleading). The old behaviour was an artifact of the fact
    that base::format rounds to even when the last digit is a 5.

  Splits on variables of class "logical" are now displayed as
  "survived = 0" and "survived = 1".  In previous versions they
  were displayed as "survived < 0.5" and "survived >= 0.5".

  Added the clip.facs argument (default FALSE) to drop predictor
  names in splits on factors.  For example, with clip.facs=TRUE,
  "pclass = 2nd,3rd" is displayed simply as "2nd,3rd".
  See Section 4.1 in the package vignette.

## 2.2.0    May 19, 2018

  Added new option type=5 (puts the split variable name in the interior nodes).

  Added new options extra=10 and extra=11.

  Improved error messages for illegal values of extra.

  Slightly raised right-hand split labels (type=3 and type=4) because
  they sometimes were too close to the node box beneath them.

  The legend for multi-class models now includes all classes, even those
  classes which are never predicted by the model.  Such classes have an
  "(unused)" suffix in the legend.

  Documentation touch ups.

  The vignettes are now compressed with gs and qpdf as in tools::compactPDF,
  (but that happens outside the standard CRAN build system).  It does mean
  that the tar.gz file for rpart.plot is a little smaller (now 628 kByte).

## 2.1.2    Apr 20, 2017

  Added dependency on R version 3.2.0 (necessary for use of anyNA).

  The digits argument can now be negative, meaning use the standard
  R "format" function (with the absolute value of digits).

## 2.1.1    Feb 17, 2017

  Added the BlGnYl palette.

## 2.1.0    Sep 26, 2016

  Added the pal.thresh and pal.node.fun arguments, and the
  show.prp.palettes function.  Tweaked the colors in the
  palettes (among other issues, grays were too light in
  diverging palettes like RdGy).  Box palettes specifying
  a vector of palettes like c("-Reds", "Blues") are
  now accepted.

  The default for node.fun and split.labs is now NULL (instead
  of internal.node.labs and internal.split.labs). The default
  behaviour hasn't changed.

## 2.0.1    Jun 22, 2016

  Using box.palette=0 now works more consistently.

## 2.0.0    Jun 10, 2016

  The function rpart.plot now has different defaults, so it
  automatically creates a colored plot tailored to the type of model.
  This makes the function non backwards-compatible with earlier versions.
  The old rpart.plot function is still available under the
  name "rpart.plot.version1".
  The defaults for prp haven't changed.

  Added the "auto" option to the "extra" argument.

  Added the box.palette argument, a simpler way of coloring nodes
  than the box.col argument.

  Using yesno=2 will print "yes" and "no" at all nodes (instead of
  just at the top).

  Added the yes.text="yes" and no.text="no" arguments (so other
  languages can be used for the yes/no text.)

  When snipping a tree, status messages are now printed on the screen.

  We now better handle NAs in the fitted values.  Nodes for NA predicted
  values are printed with cross hatching.

  Merged the library source file lib.R with the earth and
  plotmo packages's lib.R.

## 1.5.3    Sep 30, 2015
   We now deal correctly with the situation when the user has
   a variable named "text" in the current environment.

   Minor updates for CRAN checks.

## 1.5.2    Feb 4, 2015
   Added an example to the vignette based on a question on Stack Exchange.
   Unified some small utility funcs like printf with my other packages.

## 1.5.1    Dec 14, 2014
   Removed link in the man pages to obsoleted package mvpart.
   Documentation touchups.

## 1.5.0    Nov 30, 2014
   Updates to the documentation, mostly to satisfy CRAN recommendations.
   The slowtests postscript file is no longer included, to reduce package size.

## 1.4-5    Nov 19, 2014
   Added an example to the vignette for wrapping long split labels over multiple lines.
   Added inst/slowtests/webpage-figs.R so user can regenerate the web page figures.
   Added bibliography to the vignette.
   Minor updates to the vignette and test scripts.

## 1.4-4    Feb 5, 2014
   Removed use of ::: by removing outdated conversion from old-style
   rpart objects.  This change was made mainly to satisfy CRAN checks.

   Moved slow tests from the src/tests to the inst/slowtests directory.

## 1.4-3    Dec 20, 2012
   Loosened sanity check in get.class.labs to allow for numerical error.
   Thanks for Sandrine Lunven for help with this issue.

   Tweaked vignette to refer now to the rpart package vignette (instead
   of the old rpart reports).

## 1.4-2    Dec 15, 2012

    Changed some code that was using "0" as a color.
    Changed some code that was doing rep(NULL, n).
    R 2.15-2 complains about the above, although previous versions
    let them slip by.

## 1.4-1    Oct 24, 2012

    Updates for compatibility with rpart_3.1-55 (R 2.15.2).

    Updates to the vignette (extended section on compatibility
    with plot.rpart,  added section on using split.fun, other
    touchups).

## 1.4-0    Oct 10, 2012

    We now minimally access internal functions in the rpart package
    (which are accessed using "rpart:::") and thus are now less
    dependent on changes to those functions.

    We now try harder to support rpart objects with "user" node lab funcs.

    The vignette now has an appendix on mvpart objects.


## 1.3-0    Aug 15, 2012

    There is now more white space in the split text (achieved by
    adding white space to the default eq, lt, and ge args).
    To revert to the old behaviour, explictly set these args.

    Added the trim.fun argument, invoked after each mouse click
    when interactively trimming the tree.

    Incorporated Gene Cutler's fix to get.class.labs (so we no longer
    get an incorrect error message when case weights are used).

    Added Josh Browning's heat tree example to the vignette.

    Touch-ups for the new version of plotmo (1.3-2).

    Removed unnecessary LICENSE file because rpart is now GSL-3.

## 1.2-2    Mar 31, 2011

    Touch-ups for the new version of plotmo (1.2-6)

## 1.2-1    Mar 25, 2011

    Touch-ups to the vignette.

## 1.2-0    Mar 24, 2011

    Added the clip.left.labs argument.
    Extended the vignette.

## 1.1-1    Mar 9, 2011

    We now call the object's text() function unless the object
    has a standard method ("anova", "class", "poisson" or "exp").
    We now support mvpart::mrt objects

## 1.1-0    Mar 5, 2011

    The prp.pdf file is now smaller, thanks to qpdf.
    Made several miscellaneous touch-ups to docs and code.
    Thanks to Jason Roberts for his feedback.

## 1.0-0    Feb 25, 2011

    Initial release
