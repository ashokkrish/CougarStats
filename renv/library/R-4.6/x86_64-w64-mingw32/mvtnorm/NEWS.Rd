
\name{NEWS}
\title{NEWS file for the \pkg{mvtnorm} package}

\section{Changes in Version 1.4-1 (2026-05-06)}{
  \subsection{Bugfixes}{
    \itemize{
      \item Add missing \code{PROTECT}.
      \item Do not report diffs for condition numbers in 
            vignette.
    }
  }
}

\section{Changes in Version 1.4-0 (2026-05-29)}{
  \subsection{Features}{
    \itemize{
      \item Add \code{invchol.syMatrices}, \code{cov2invchol}, and
            \code{cov2chol}.
      \item Avoid swapping data (row <-> column-major order) in a number of
            places.
      \item \code{Mult} now used \proglang{LAPACK}'s \code{dtpmv}.
      \item Multiplication of lower triangular matrices with quadratic
            matrices implemented in \code{Mult}.
      \item \code{vectrick} is now based on \code{Mult} instead of
            \proglang{LAPACK}'s\code{dtrmm}.
      \item \code{*rossprod} is now based on \code{Mult} instead of
            tailored \proglang{C} code.
      \item \code{standardize} now also works for non-unit triangular
            matrices.
    }
  }
  \subsection{Bugfixes}{
    \itemize{
      \item Cleaned up the class structure and corrected constructors for
            \code{syMatrices}.
    }
  }
  \subsection{Other}{
    \itemize{
      \item After more than 15 years on R-forge, \pkg{mvtnorm} is now
            hosted at codeberg. Thank you, WU Vienna, for the long-term
            support!
    }
  }
}

\section{Changes in Version 1.3-8 (2026-05-05)}{
  \subsection{Features}{
    \itemize{
      \item More careful checks of inputs.
    }
  }
  \subsection{Bugfixes}{
    \itemize{
      \item \code{logLik} wasn't able to handle missing, continuous, and
            interval-censored observations at once.
      \item \code{lpmvnorm} failed when a non-missing \code{invcholmean} wasn't a matrix
            with the same dimensions as \code{lower} and \code{upper}.
    }
  }
}

\section{Changes in Version 1.3-7 (2026-04-14)}{
  \subsection{Features}{
    \itemize{
      \item Minor performance improvements in \code{sldmvnorm}.
      \item Speed-up switching from row- to column major order and vice versa.
      \item Add \code{invcholmean} argument where possible.
      \item Improve documentation regarding marginal and conditional
            distributions.
      \item Ship nuweb source file in \code{inst/nuweb}
    }
  }
  \subsection{Bugfixes}{
    \itemize{
      \item \code{simulate} ignored its \code{seed} argument.
    }
  }
}

\section{Changes in Version 1.3-6 (2026-03-12)}{
  \subsection{Bugfixes}{
    \itemize{
      \item \code{sldmvnorm} returned an incorrect log-likelihood
            in the presence of means.
      \item Observations with very small log-probabilities
            are now treated the same in \code{lpmvnorm} and 
            \code{slpmvnorm}.
      \item This reduced differences observed on machines w/o 
            long doubles considerably; still there were minor
            diffs in the package vignette which we now
            prevent from showing up.
    }
  }
}

\section{Changes in Version 1.3-5 (2026-03-10)}{
  \subsection{Documentation}{
    \itemize{
      \item Use \code{REFERENCES.bib}.
    }
  }
}


\section{Changes in Version 1.3-4 (2025-09-15)}{
  \subsection{Features}{
    \itemize{
      \item \code{mvnorm} gains a \code{invcholmean} argument allowing a
             parameterisation leading to convex optimisation problems.
      \item Added \code{zero.print = "."} argument for printing lower
            triangular matrices.
    }
  }
  \subsection{Bugfixes}{
    \itemize{
      \item \code{condDist()} sometimes failed to return a 
            \code{scale} slot.
    }
  }
  \subsection{Documentation}{
    \itemize{
      \item Explain how to use \code{condDist()} to obtain regression
            coefficients for an example involving means.
    }
  }
}

\section{Changes in Version 1.3-3 (2025-01-09)}{
  \subsection{Bugfixes}{
    \itemize{
      \item \code{Miwa(steps = 4097)} are allowed in R, the
            C code assumed \code{steps <= 4096}, now 
            \code{steps == 4097} is possible. Triggered by
            BDR's gcc-UBSAN check on package 
            OptimalGoldstandardDesign.
    }
  }
}


\section{Changes in Version 1.3-2 (2024-11-04)}{
  \subsection{Features}{
    \itemize{
      \item Explain and illustrate that omitting data dimensions produces marginal
            log-likelihoods in Chapter 7.
      \item Add low-level functionality for reduced rank covariances in new
            Chapter 8.
    }
  }
  \subsection{Bugfixes}{
    \itemize{
      \item \code{invcholD} and \code{Dchol} avoided very small diagonals;
            now they also avoid very large diagonals (such that inverting the result
            doesn't give too small diagonals).
      \item Fix partial argument matches, reported by KH.
    }
  }
}

\section{Changes in Version 1.3-1 (2024-09-02)}{
  \subsection{Bugfixes}{
    \itemize{
      \item Challenging numeric test of score function at ML estimate
            failed on Mac M1.
    }
  }
}


\section{Changes in Version 1.2-6 (2024-08-17)}{
  \subsection{Features}{
    \itemize{
      \item Add \code{logdet()} function for computing log-determinants from
            lower triangular matrices.
      \item Speed up \code{solve(<ltMatrices>, ...)}.
    }
  }
  \subsection{Bugfixes}{
    \itemize{
      \item Avoid unnecessary copying of large data objects.
      \item -1/2 -> -1 for det(C) in lmvnorm_src vignette; spotted by Kurt Hornik
      \item Use \code{M\_PI}.
    }
  }
}

\section{Changes in Version 1.2-5 (2024-05-18)}{
  \subsection{Features}{
    \itemize{
      \item Update the \code{Using mvtnorm} package vignette and references therein.
      \item Speed up \code{Mult(<ltMatrices>, transpose = TRUE)}.
      \item Speed up \code{ldmvnorm()}.
      \item Add constructor \code{syMatrices()} for multiple symmetric
            matrices and \code{as.syMatrices()} for coercion from \code{ltMatrices}.}
  }
  \subsection{Bugfixes}{
    \itemize{
      \item Fix segmentation fault or unnecessary error or warning + approximation for
      \code{algorithm = Miwa()} or \code{TVPACK()}, in case dimension
      reduction to one-dimensional is possible, e.g., for \preformatted{
  pmvnorm(lower = rep(-Inf,3), upper = c(-1, Inf, Inf),
          sigma = diag(3), algorithm = Miwa())
     # or
  pmvnorm(lower = c(-Inf,-Inf),
          upper = c(- 1,  Inf), sigma=diag(2), algorithm = TVPACK())
        }
    }
  }
}


\section{Changes in Version 1.2-4 (2023-11-27)}{
  \subsection{Bugfixes}{
    \itemize{
      \item Remove empty print statement from \code{miwa.c}
    }
  }
}

\section{Changes in Version 1.2-3 (2023-08-17)}{
  \subsection{Features}{
    \itemize{
      \item Allow to change \code{rnorm} in \code{rmvnorm}, feature request
            by Ralf Stubner.
    }
  }
  \subsection{Bugfixes}{
    \itemize{
      \item Fix variable declarations in \code{tvpack.f} as reported by Intel
            compilers icx/ipcx/ifx from oneAPI 2023.2.0 and oneMKL 2023.2.0, thanks to BDR
    }
  }
}

\section{Changes in Version 1.2-2 (2023-06-07)}{
  \subsection{Bugfixes}{
    \itemize{
      \item Fix overflow problem reported by ASAN.
    }
  }
  \subsection{Documentation}{
    \itemize{
      \item Be even more careful inverting / computing Cholesky factors for hessians (M1 and macos-arm64).
    }
  }
}

\section{Changes in Version 1.2-1 (2023-06-02)}{
  \subsection{Bugfixes}{
    \itemize{
      \item Avoid attempts to allocate zero length arrays
            for one-dimensional problems.
    }
  }
  \subsection{Documentation}{
    \itemize{
      \item Catch M1mac problems with inverting hessians.
    }
  }
}

\section{Changes in Version 1.2-0 (2023-06-02)}{
  \itemize{
    \item New \code{lpmvnorm()} function for computing multivariate normal log-likelihoods for interval-censored observations.
    \item New \code{slpmvnorm()} function for computing the corresponding score function for interval-censored observations.
    \item New \code{ldmvnorm()} function for computing multivariate normal log-likelihoods for exact observations.
    \item New \code{sldmvnorm()} function for computing the corresponding score function for exact observations.
    \item New \code{ldpmvnorm()} function for computing multivariate normal log-likelihoods for a mix of exact and interval-censored observations.
    \item New \code{sldpmvnorm()} function for computing the corresponding score function for a mix of exact and interval-censored observations.
    \item New class \code{ltMatrices} representing multiple lower triangular matrices, with some useful methods.
    \item New package vignette \code{lmvnorm_src} describing these new features.
    \item \code{pmvnorm()}, \code{qmvnorm()}, \code{pmvt()}, and
          \code{qmvt()} gain a \code{seed} argument.
    \item Regression tests were improved.
    \item Internal standardization is described better in the docs, suggested by Chris Wymant.
    \item New \code{NEWS} file, the old file containing information up to version 1.1-3 is available as \code{NEWS.old}.
  }
}
