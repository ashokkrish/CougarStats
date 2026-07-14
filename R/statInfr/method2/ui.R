#' @param id The id of the integrating application.
statInfrMethodTwoUI <- function(id) {
  ns <- NS(id)

  submoduleUI <- list(sidebarPanel = NULL, mainPanel = NULL)

  submoduleUI$sidebarPanel <- tagList(
    HTML("<label class='si-label'><b>Parameter of Interest</b></label>"),
    radioButtons(
      inputId = ns("popuParameters"),
      label = NULL,
      choiceValues = list(
        "Independent Population Means",
        "Wilcoxon rank sum test",
        "Dependent Population Means",
        "Wilcoxon Signed Rank Test",
        "Population Proportions",
        "Two Population Variances"
      ),
      choiceNames = list(
        "Two Independent Populations (\\( \\mu_{1} - \\mu_{2} \\))",
        "Wilcoxon Rank Sum Test (or the Mann-Whitney U Test)",
        "Dependent (Paired) Populations (\\( \\mu_{d} \\))",
        "Wilcoxon Signed Rank Test (Paired)",
        "Two Population Proportions (\\( p_{1} - p_{2}\\))",
        "Two Population Variances (\\( \\sigma_{1}^2/\\sigma_{2}^2 \\))"
      ),
      selected = "Independent Population Means", # character(0), #
      inline = FALSE
    ), # ,width = '1000px'),

    ## ---------------- Ind Pop Means ------------------------------------------
    conditionalPanel(
      ns = ns,
      condition = "input.popuParameters == 'Independent Population Means'",
      radioButtons(
        inputId = ns("dataAvailability2"),
        label = strong("Data Availability"),
        choiceValues = list(
          "Summarized Data",
          "Enter Raw Data",
          "Upload Data"
        ),
        choiceNames = list(
          "Summarized Data",
          "Enter Raw Data",
          "Upload Data"
        ),
        selected = "Summarized Data", # character(0), #
        inline = TRUE
      ), # ,width = '1000px'),

      ## -------------------- Summarized Data -----------------------------------
      conditionalPanel(
        ns = ns,
        condition = "input.dataAvailability2 == 'Summarized Data'",
        numericInput(
          inputId = ns("sampleSize1"),
          label   = strong("Sample Size 1 (\\( n_{1}\\))"),
          value   = 21,
          min     = 2,
          step    = 1
        ),
        numericInput(
          inputId = ns("sampleMean1"),
          label   = strong("Sample Mean 1 (\\( \\bar{x}_{1}\\))"),
          value   = 29.6,
          step    = 0.00001
        ),
        numericInput(
          inputId = ns("sampleSize2"),
          label   = strong("Sample Size 2 (\\( n_{2}\\))"),
          value   = 21,
          min     = 2,
          step    = 1
        ),
        numericInput(
          inputId = ns("sampleMean2"),
          label   = strong("Sample Mean 2 (\\( \\bar{x}_{2}\\))"),
          value   = 33.9,
          step    = 0.00001
        ),
        radioButtons(
          inputId = ns("bothsigmaKnown"),
          label = strong("Are Population Standard Deviations (\\( \\sigma_{1}\\) and \\( \\sigma_{2}\\)) known?"),
          choiceValues = list(
            "bothKnown",
            "bothUnknown"
          ),
          choiceNames = list(
            "Both Known",
            "Both Unknown"
          ),
          selected = "bothKnown",
          inline = TRUE
        ),

        ## ------------------------ Sigma Both Known -----------------------------
        conditionalPanel(
          ns = ns,
          condition = "input.bothsigmaKnown == 'bothKnown'",
          numericInput(
            inputId = ns("popuSD1"),
            label = strong("Population Standard Deviation 1 (\\( \\sigma_{1}\\)) Value"),
            value = 5.36,
            min = 0.00001,
            step = 0.00001
          ),
          numericInput(
            inputId = ns("popuSD2"),
            label = strong("Population Standard Deviation 2 (\\( \\sigma_{2}\\)) Value"),
            value = 5.97,
            min = 0.00001,
            step = 0.00001
          )
        ), ## Sigma Both Known

        ## -------------------- Sigma Both Unknown -------------------------------
        conditionalPanel(
          ns = ns,
          condition = "input.bothsigmaKnown == 'bothUnknown'",
          radioButtons(
            inputId = ns("bothsigmaEqual"),
            label = strong("Assume Population Variances are equal (\\( \\sigma_{1}^2\\) = \\( \\sigma_{2}^2\\))?"),
            choiceValues = list(
              "TRUE",
              "FALSE"
            ),
            choiceNames = list(
              "Yes (Pooled)",
              "No (Welch-Satterthwaite df)"
            ),
            selected = "TRUE",
            inline = TRUE
          ),
          numericInput(
            inputId = ns("sampSD1"),
            label   = strong("Sample Standard Deviation 1 (\\( s_{1}\\)) Value"),
            value   = 5.24,
            min     = 0.00001,
            step    = 0.00001
          ),
          numericInput(
            inputId = ns("sampSD2"),
            label   = strong("Sample Standard Deviation 2 (\\( s_{2}\\)) Value"),
            value   = 5.85,
            min     = 0.00001,
            step    = 0.00001
          )
        ) ## Sigma Both Unknown
      ), ## Summarized Data

      ## -------------------- Raw Data ------------------------------------------
      conditionalPanel(
        ns = ns,
        condition = "input.dataAvailability2 == 'Enter Raw Data'",
        textAreaInput(
          inputId     = ns("raw_sample1"),
          label       = strong("Sample 1"),
          value       = "101.1,  111.1,  107.6,  98.1,  99.5,  98.7,  103.3,  108.9,  109.1,  103.3",
          placeholder = "Enter values separated by a comma with decimals as points",
          rows        = 3
        ),
        textAreaInput(
          inputId     = ns("raw_sample2"),
          label       = strong("Sample 2"),
          value       = "107.1,  105.0,  98.0,  97.9,  103.3,  104.6,  100.1,  98.2,  97.9",
          placeholder = "Enter values separated by a comma with decimals as points",
          rows        = 3
        ),
        radioButtons(
          inputId = ns("bothsigmaKnownRaw"),
          label = strong("Are Population Standard Deviations (\\( \\sigma_{1}\\) and \\( \\sigma_{2}\\)) known?"),
          choiceValues = list(
            "bothKnown",
            "bothUnknown"
          ),
          choiceNames = list(
            "Both Known",
            "Both Unknown"
          ),
          selected = "bothUnknown",
          inline = TRUE
        ),

        ## ------------------------ Sigma Both Unknown ---------------------------
        conditionalPanel(
          ns = ns,
          condition = "input.bothsigmaKnownRaw == 'bothUnknown'",
          radioButtons(
            inputId = ns("bothsigmaEqualRaw"),
            label = strong("Assume Population Variances are equal (\\( \\sigma_{1}^2\\) = \\( \\sigma_{2}^2\\))?"),
            choiceValues = list(
              "TRUE",
              "FALSE"
            ),
            choiceNames = list(
              "Yes (Pooled)",
              "No (Welch-Satterthwaite df)"
            ),
            selected = "TRUE",
            inline = TRUE
          )
        ), ## Sigma Both Unknown

        ## ------------------------ Sigma Both Known -----------------------------
        conditionalPanel(
          ns = ns,
          condition = "input.bothsigmaKnownRaw == 'bothKnown'",
          numericInput(
            inputId = ns("popuSDRaw1"),
            label   = strong("Population Standard Deviation 1 (\\( \\sigma_{1}\\)) Value"),
            value   = 4.54,
            min     = 0.00001,
            step    = 0.00001
          ),
          numericInput(
            inputId = ns("popuSDRaw2"),
            label   = strong("Population Standard Deviation 2 (\\( \\sigma_{2}\\)) Value"),
            value   = 3.47,
            min     = 0.00001,
            step    = 0.00001
          )
        ) # Sigma Both Known
      ), ## Raw Data

      ## -------------------- Uploaded Data -------------------------------------
      conditionalPanel(
        ns = ns,
        condition = "input.dataAvailability2 == 'Upload Data'",
        newFileInput("indMeansUserData", id),
        selectizeInput(
          inputId = ns("indMeansUplSample1"),
          label = strong("Column for Sample 1"),
          choices = c(""),
          options = list(
            placeholder = "Select a column",
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        selectizeInput(
          inputId = ns("indMeansUplSample2"),
          label = strong("Column for Sample 2"),
          choices = c(""),
          options = list(
            placeholder = "Select a column",
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        radioButtons(
          inputId = ns("bothsigmaKnownUpload"),
          label = strong("Are Population Standard Deviations (\\( \\sigma_{1}\\) and \\( \\sigma_{2}\\)) known?"),
          choiceValues = list(
            "bothKnown",
            "bothUnknown"
          ),
          choiceNames = list(
            "Both Known",
            "Both Unknown"
          ),
          selected = "bothUnknown",
          inline = TRUE
        ),

        ## ------------------------ Sigma Both Unknown ---------------------------
        conditionalPanel(
          ns = ns,
          condition = "input.bothsigmaKnownUpload == 'bothUnknown'",
          radioButtons(
            inputId = ns("bothsigmaEqualUpload"),
            label = strong("Assume Population Variances are equal (\\( \\sigma_{1}^2\\) = \\( \\sigma_{2}^2\\))?"),
            choiceValues = list(
              "TRUE",
              "FALSE"
            ),
            choiceNames = list(
              "Yes (Pooled)",
              "No (Welch-Satterthwaite df)"
            ),
            selected = "TRUE",
            inline = TRUE
          )
        ), ## Sigma Both Unknown

        ## ------------------------ Sigma Both Known ---------------------------
        conditionalPanel(
          ns = ns,
          condition = "input.bothsigmaKnownUpload == 'bothKnown'",
          numericInput(
            inputId = ns("popuSDUpload1"),
            label   = strong("Population Standard Deviation 1 (\\( \\sigma_{1}\\)) Value"),
            value   = "",
            min     = 0.00001,
            step    = 0.00001
          ),
          numericInput(
            inputId = ns("popuSDUpload2"),
            label   = strong("Population Standard Deviation 2 (\\( \\sigma_{2}\\)) Value"),
            value   = "",
            min     = 0.00001,
            step    = 0.00001
          )
        ) ## Sigma Both Known
      ) ## Upload Data
    ), ## Two Independent Samples

    ## ---------------- Wilcoxon Rank Sum Test ------------------------------------------
    conditionalPanel(
      ns = ns,
      condition = "input.popuParameters == 'Wilcoxon rank sum test'",
      radioButtons(
        inputId = ns("wilcoxonRankSumTestData"),
        label = strong("Data Availability"),
        choiceValues = list(
          "Enter Raw Data",
          "Upload Data"
        ),
        choiceNames = list(
          "Enter Raw Data",
          "Upload Data"
        ),
        selected = "Enter Raw Data", # character(0), #
        inline = TRUE
      ), # ,width = '1000px'),

      ## -------------------- Raw Data ------------------------------------------
      conditionalPanel(
        ns = ns,
        condition = "input.wilcoxonRankSumTestData == 'Enter Raw Data'",
        textAreaInput(
          inputId     = ns("rankSumRaw1"),
          label       = strong("Sample 1"),
          value       = "2,  1.25,  8.5,  1.1,  1.25,  3.75,  5.5",
          placeholder = "Enter values separated by a comma with decimals as points",
          rows        = 3
        ),
        textAreaInput(
          inputId     = ns("rankSumRaw2"),
          label       = strong("Sample 2"),
          value       = "1,  1,  0,  3.25,  1,  0.25",
          placeholder = "Enter values separated by a comma with decimals as points",
          rows        = 3
        )
      ), ## Wilcoxon Raw Data

      ## -------------------- Uploaded Data -------------------------------------
      conditionalPanel(
        ns = ns,
        condition = "input.wilcoxonRankSumTestData == 'Upload Data'",
        newFileInput("wilcoxonUpl", id),
        selectizeInput(
          inputId = ns("wilcoxonUpl1"),
          label = strong("Column for Sample 1"),
          choices = c(""),
          options = list(
            placeholder = "Select a column",
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        selectizeInput(
          inputId = ns("wilcoxonUpl2"),
          label = strong("Column for Sample 2"),
          choices = c(""),
          options = list(
            placeholder = "Select a column",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      ) ## Upload Data
    ), ## Wilcoxon Rank Sum Test


    ## ---------------- Dep Pop Means ------------------------------------------
    conditionalPanel(
      ns = ns,
      condition = "input.popuParameters == 'Dependent Population Means'",
      radioButtons(
        inputId = ns("dataTypeDependent"),
        label = strong("Data Availability"),
        choiceValues = list(
          "Enter Raw Data",
          "Upload Data"
        ),
        choiceNames = list(
          "Enter Raw Data",
          "Upload Data"
        ),
        selected = "Enter Raw Data",
        inline = TRUE
      ),

      ## -------------------- Raw Data ------------------------------------------
      conditionalPanel(
        ns = ns,
        condition = "input.dataTypeDependent == 'Enter Raw Data'",
        textAreaInput(
          inputId     = ns("before"),
          label       = strong("Sample 1 (e.g. Before, Pre-Treatment, Baseline)"),
          value       = "484, 478, 492, 444, 436, 398, 464, 476",
          placeholder = "Enter values separated by a comma with decimals as points",
          rows        = 3
        ),
        textAreaInput(
          inputId     = ns("after"),
          label       = strong("Sample 2 (e.g. After, Post-Treatment, Follow-Up)"),
          value       = "488, 478, 480, 426, 440, 410, 458, 460",
          placeholder = "Enter values separated by a comma with decimals as points",
          rows        = 3
        )
      ), ## Raw Data

      ## -------------------- Uploaded Data -------------------------------------
      conditionalPanel(
        ns = ns,
        condition = "input.dataTypeDependent == 'Upload Data'",
        newFileInput("depMeansUserData", id),
        selectizeInput(
          inputId = ns("depMeansUplSample1"),
          label = strong("Column for Sample 1 (e.g. Before, Pre-Treatment, Baseline)"),
          choices = c(""),
          options = list(
            placeholder = "Select a column",
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        selectizeInput(
          inputId = ns("depMeansUplSample2"),
          label = strong("Column for Sample 2 (e.g. After, Post-Treatment, Follow-Up)"),
          choices = c(""),
          options = list(
            placeholder = "Select a column",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      ) ## Upload Data
    ), ## Two Dependent Samples


    ## ---------------- Wilcoxon Signed Rank Test ------------------------------------------
    conditionalPanel(
      ns = ns,
      condition = "input.popuParameters == 'Wilcoxon Signed Rank Test'",
      radioButtons(
        inputId = ns("signedRankTest"),
        label = strong("Data Availability"),
        choiceValues = list(
          "Enter Raw Data",
          "Upload Data"
        ),
        choiceNames = list(
          "Enter Raw Data",
          "Upload Data"
        ),
        selected = "Enter Raw Data",
        inline = TRUE
      ),

      ## -------------------- Raw Data ------------------------------------------
      conditionalPanel(
        ns = ns,
        condition = "input.signedRankTest == 'Enter Raw Data'",
        textAreaInput(
          inputId     = ns("signedRankRaw1"),
          label       = strong("Sample 1 (e.g. Before, Pre-Treatment, Baseline)"),
          value       = "484, 478, 492, 444, 436, 398, 464, 476",
          placeholder = "Enter values separated by a comma with decimals as points",
          rows        = 3
        ),
        textAreaInput(
          inputId     = ns("signedRankRaw2"),
          label       = strong("Sample 2 (e.g. After, Post-Treatment, Follow-Up)"),
          value       = "488, 478, 480, 426, 440, 410, 458, 460",
          placeholder = "Enter values separated by a comma with decimals as points",
          rows        = 3
        )
      ), ## Signed Rank Raw Data

      ## -------------------- Uploaded Data -------------------------------------
      conditionalPanel(
        ns = ns,
        condition = "input.signedRankTest == 'Upload Data'",
        newFileInput("signedRankUpl", id),
        selectizeInput(
          inputId = ns("signedRankUpl1"),
          label = strong("Column for Sample 1 (e.g. Before, Pre-Treatment, Baseline)"),
          choices = c(""),
          options = list(
            placeholder = "Select a column",
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        selectizeInput(
          inputId = ns("signedRankUpl2"),
          label = strong("Column for Sample 2 (e.g. After, Post-Treatment, Follow-Up)"),
          choices = c(""),
          options = list(
            placeholder = "Select a column",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      ) ## Upload Data
    ), ## Wilcoxon Signed Rank Test

    ## ---------------- 2 Pop Proportions --------------------------------------
    conditionalPanel(
      ns = ns,
      condition = "input.popuParameters == 'Population Proportions'",
      numericInput(
        inputId = ns("numSuccesses1"),
        label   = strong("Number of Successes 1 (\\( x_{1}\\))"),
        value   = 174,
        min     = 0,
        step    = 1
      ),
      numericInput(
        inputId = ns("numTrials1"),
        label   = strong("Number of Trials 1 (\\( n_{1}\\))"),
        value   = 300,
        min     = 1,
        step    = 1
      ),
      numericInput(
        inputId = ns("numSuccesses2"),
        label   = strong("Number of Successes 2 (\\( x_{2}\\))"),
        value   = 111,
        min     = 0,
        step    = 1
      ),
      numericInput(
        inputId = ns("numTrials2"),
        label   = strong("Number of Trials 2 (\\( n_{2}\\))"),
        value   = 300,
        min     = 1,
        step    = 1
      )
    ), ## Two Population Proportions

    ## ------------ 2 Pop Standard Deviations ------------------------------------
    conditionalPanel(
      ns = ns,
      condition = "input.popuParameters == 'Two Population Variances'",
      radioButtons(
        inputId = ns("dataAvailability3"),
        label = strong("Data Availability"),
        choiceValues = list(
          "Summary",
          "Variance",
          "Enter Raw Data"
        ),
        choiceNames = list(
          "\\( n_1,\\ n_2,\\ s_1,\\ s_2 \\)",
          "\\( n_1,\\ n_2,\\ s_1^2,\\ s_2^2 \\)",
          "Enter Raw Data"
        ),
        selected = "Summary",
        inline = TRUE
      ),


      ## ------------ Summary (n1, n2, s1, s2) ------------------------------------
      conditionalPanel(
        ns = ns,
        condition = "input.dataAvailability3 == 'Summary'",
        withMathJax(
          tagList(
            numericInput(
              inputId = ns("SDSampleSize1"),
              label   = HTML("<strong>Sample Size 1</strong> \\( (n_1) \\)"),
              value   = 12,
              min     = 1,
              step    = 1
            ),
            numericInput(
              inputId = ns("stdDev1"),
              label   = HTML("<strong>Sample Standard Deviation 1</strong> \\( (s_1) \\)"),
              value   = 3,
              min     = 1,
              step    = 0.01
            ),
            numericInput(
              inputId = ns("SDSampleSize2"),
              label   = HTML("<strong>Sample Size 2</strong> \\( (n_2) \\)"),
              value   = 18,
              min     = 1,
              step    = 1
            ),
            numericInput(
              inputId = ns("stdDev2"),
              label   = HTML("<strong>Sample Standard Deviation 2</strong> \\( (s_2) \\)"),
              value   = 4.8,
              min     = 1,
              step    = 0.01
            )
          )
        )
      ), # summary,

      ## ------------ Variance (n1, s1^2, n2, s2^2) ------------------------------------

      conditionalPanel(
        ns = ns,
        condition = "input.dataAvailability3 == 'Variance'",
        withMathJax(
          tagList(
            numericInput(
              inputId = ns("n1"),
              label   = HTML("<strong>Sample Size 1</strong> \\( (n_1) \\)"),
              value   = 12,
              min     = 1,
              step    = 1
            ),
            numericInput(
              inputId = ns("s1sq"),
              label   = HTML("<strong>Sample Variance 1 </strong>\\( (s_1^2) \\)"),
              value   = 9,
              min     = 1,
              step    = 0.01
            ),
            numericInput(
              inputId = ns("n2"),
              label   = HTML("<strong>Sample Size 2</strong> \\( (n_2) \\)"),
              value   = 18,
              min     = 1,
              step    = 1
            ),
            numericInput(
              inputId = ns("s2sq"),
              label   = HTML("<strong>Sample Variance 2</strong> \\( (s_2^2) \\)"),
              value   = 23.04,
              min     = 1,
              step    = 0.01
            )
          )
        )
      ), ## variance

      ## ------------- Raw Data ---------------------------------------------------------

      conditionalPanel(
        ns = ns,
        condition = "input.dataAvailability3 == 'Enter Raw Data'",
        textAreaInput(
          inputId     = ns("rawSamp1SD"),
          label       = strong("Sample 1 (e.g Class A test scores)"),
          value       = "80, 54, 97, 76, 66, 87, 83, 91",
          placeholder = "Enter values separated by a comma with decimals as points",
          rows        = 3
        ),
        textAreaInput(
          inputId     = ns("rawSamp2SD"),
          label       = strong("Sample 2 (e.g Class B test scores)"),
          value       = "45, 54, 67, 95, 100, 82, 83, 74",
          placeholder = "Enter values separated by a comma with decimals as points",
          rows        = 3
        )
      ) ## Raw Data
    ), ## Two Pop Var

    ## ------------ Confidence Level, Inference Type ---------------------------------

    conditionalPanel(
      ns = ns,
      condition = "input.popuParameters != 'Wilcoxon rank sum test' && input.popuParameters != 'Wilcoxon Signed Rank Test'",
      radioButtons(
        inputId = ns("inferenceType2"),
        label = strong("Inference Type"),
        choiceValues = list(
          "Confidence Interval",
          "Hypothesis Testing"
        ),
        choiceNames = list(
          "Confidence Interval",
          "Hypothesis Testing"
        ),
        selected = "Confidence Interval",
        inline = TRUE
      )
    ),
    conditionalPanel(
      ns = ns,
      condition = "input.inferenceType2 == 'Confidence Interval' && input.popuParameters != 'Wilcoxon rank sum test' && input.popuParameters != 'Wilcoxon Signed Rank Test'",
      radioButtons(
        inputId = ns("confidenceLevel2"),
        label = strong("Confidence Level (\\( 1- \\alpha\\))"),
        choices = c(
          "90%",
          "95%",
          "99%"
        ),
        selected = c("95%"),
        inline = TRUE
      )
    ), ## Confidence Interval

    conditionalPanel(
      ns = ns,
      condition = "input.inferenceType2 == 'Hypothesis Testing' || input.popuParameters == 'Wilcoxon rank sum test' || input.popuParameters == 'Wilcoxon Signed Rank Test'",
      radioButtons(
        inputId = ns("significanceLevel2"),
        label = strong("Significance Level (\\( \\alpha\\))"),
        choices = c(
          "10%",
          "5%",
          "1%"
        ),
        selected = c("5%"),
        inline = TRUE
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.popuParameters == 'Independent Population Means'",
        numericInput(
          inputId = ns("indMeansMuNaught"),
          label   = strong(HTML("Hypothesized Population Mean Difference \\( (\\mu_{1} - \\mu_{2})_{0} \\) Value")),
          value   = 0,
          step    = 0.00001
        )
      ), ## indMeansMuNaught

      conditionalPanel(
        ns = ns,
        condition = "input.popuParameters == 'Dependent Population Means'",
        numericInput(
          inputId = ns("depMeansMuNaught"),
          label   = strong(HTML("Hypothesized Population Mean Difference \\( (\\mu_{d})_{0} \\) Value")),
          value   = 0,
          step    = 0.00001
        )
      ), ## depMeansMuNaught

      conditionalPanel(
        ns = ns,
        condition = "input.popuParameters == 'Population Proportions'",
        numericInput(
          inputId = ns("propDiffNaught"),
          label   = strong(HTML("Hypothesized Population Proportion Difference \\( (p_{1} - p_{2})_{0} \\) Value")),
          value   = 0,
          step    = 0.00001
        )
      ), ## propDiffNaught

      selectizeInput(
        inputId  = ns("altHypothesis2"),
        label    = strong("Alternate Hypothesis (\\( H_{a}\\))"),
        choices  = lessThanInequalGreaterThanChoices123,
        selected = 2,
        ## NOTE: this uses the global "render" object; see global.R.
        options  = list(render = I(render))
      )
    ), ## Hypothesis Testing
    conditionalPanel(
      ns = ns,
      condition = "input.popuParameters == 'Wilcoxon rank sum test'",
      radioButtons(
        inputId  = ns("normaprowrsRankSum"),
        label    = strong("Method"), ## A more descriptive label
        choices  = c("Exact", "Normal approximation (for large samples)"),
        selected = "Exact",
        inline   = TRUE
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.normaprowrsRankSum == 'Normal approximation (for large samples)'", ## This is the inner condition, checking the checkbox
        radioButtons(
          inputId  = ns("continuityCorrectionOption"),
          label    = strong("Continuity correction"),
          choices  = c("True", "False"),
          selected = "True", ## 'selected' argument takes a single value, not a vector
          inline   = TRUE
        )
      )
    ),
    conditionalPanel(
      ns = ns,
      condition = "(input.popuParameters == 'Independent Population Means' && input.dataAvailability2 != 'Summarized Data')",
      p(strong("Graph Options")),
      checkboxInput(
        inputId = ns("indMeansBoxplot"),
        label   = "Side-by-side Boxplot for Sample Data",
        value   = TRUE
      ),
      checkboxInput(
        inputId = ns("indMeansQQPlot"),
        label   = "Q-Q Plots for Sample 1 and Sample 2",
        value   = TRUE
      )
    ), ## Ind Means !Summarized

    conditionalPanel(
      ns = ns,
      condition = "(input.popuParameters == 'Dependent Population Means')",
      p(strong("Graph Options")),
      checkboxInput(
        inputId = ns("depMeansQQPlot"),
        label   = "Q-Q Plot of the Difference (d)",
        value   = TRUE
      )
    ), ## Dep Means Graphs

    conditionalPanel(
      ns = ns,
      condition = "input.popuParameters == 'Wilcoxon rank sum test'",
      p(strong("Graph Options")),
      checkboxInput(
        inputId = ns("sidebysidewRankSum"),
        label   = "Side-by-side Boxplot",
        value   = TRUE
      ),
      checkboxInput(
        inputId = ns("sidebysidewRankQQ"),
        label   = "Q-Q plots for Sample 1 and Sample 2",
        value   = TRUE
      )
    ), ## Wilcoxon Rank Sum Graphs
    ## Wilcoxon Signed Rank Test
    conditionalPanel(
      ns = ns,
      condition = "input.popuParameters == 'Wilcoxon Signed Rank Test'",
      radioButtons(
        inputId  = ns("normaprowrs"),
        label    = strong("Method"), ## A more descriptive label
        choices  = c("Exact", "Normal approximation (for large samples)"),
        selected = "Exact",
        inline   = TRUE
      )
    ),
    conditionalPanel(
      ns = ns,
      condition = "(input.popuParameters == 'Wilcoxon Signed Rank Test')",
      p(strong("Graph Options")),
      checkboxInput(
        inputId = ns("signedRankQQPlot"),
        label   = "Q-Q Plot of the Difference",
        value   = TRUE
      )
    )
  )

  submoduleUI$mainPanel <- tagList(
    ## ---------------- Independent Pop Means ----------------------------------
    conditionalPanel(
      ns = ns,
      condition = "input.popuParameters == 'Independent Population Means'",
      navbarPage(
        id = ns("indPopMeansTabset"),
        selected = "Analysis",
        title = NULL,
        tabPanel(
          id = ns("indPopMeans"),
          title = "Analysis",
          conditionalPanel(
            ns = ns,
            condition = "input.inferenceType2 == 'Confidence Interval'",
            h2(tags$u("Confidence Interval")),# TODO: former titlePanel; set window title dynamically elsewhere.
            br(),
            uiOutput(ns("indMeansCI")),
            br()
          ), ## Confidence interval

          conditionalPanel(
            ns = ns,
            condition = "input.inferenceType2 == 'Hypothesis Testing'",
            h2(tags$u("Hypothesis Test")),# TODO: former titlePanel; set window title dynamically elsewhere.
            br(),
            uiOutput(ns("indMeansHT")),
            br()
          ) ## Hypothesis Testing
        ), ## indPopMeans Analysis tabPanel

        tabPanel(
          id = ns("indPopMeansGraphs"),
          title = "Graphs",
          conditionalPanel(
            ns = ns,
            condition = "input.dataAvailability2 != 'Summarized Data' && input.indMeansBoxplot == 1",
            br(),
            h2(tags$u("Boxplot")),# TODO: former titlePanel; set window title dynamically elsewhere.
            br(),
            plotOptionsMenuUI(
              id = ns("indMeansBoxplot"),
              plotType = "Boxplot",
              title = "Boxplot"
            ),
            uiOutput(ns("renderIndMeansBoxplot")),
            br(),
            br()
          ),
          conditionalPanel(
            ns = ns,
            condition = "input.dataAvailability2 != 'Summarized Data' && input.indMeansQQPlot == 1",
            br(),
            hr(),
            br(),
            h2(tags$u("Q-Q Plots for Sample 1 and Sample 2")),# TODO: former titlePanel; set window title dynamically elsewhere.
            br(),
            plotOptionsMenuUI(
              id = ns("indMeansQQPlot"),
              plotType = "QQ Plot",
              title = "Q-Q Plots"
            ),
            uiOutput(ns("renderIndMeansQQPlot")),
            br(),
            br()
          )
        ), ## indPopMeans Graphs tabPanel

        tabPanel(
          id = ns("indPopMeansData"),
          title = "Uploaded Data",
          uiOutput(ns("renderIndPopMeansData"))
        ) ## indPopMeansData Uploaded Data tabPanel
      ) ## indPopMeansTabset
    ), ## Two Independent Samples


    ## ---------------- Signed Rank Test --------------------------------------
    conditionalPanel(
      ns = ns,
      condition = "input.popuParameters == 'Wilcoxon Signed Rank Test'",
      navbarPage(
        id = ns("signedRankTabset"),
        selected = "Analysis",
        title = NULL,
        tabPanel(
          id = ns("signedRankTab"),
          title = "Analysis",
          conditionalPanel(
            ns = ns,
            condition = "input.inferenceType2 == 'Hypothesis Testing' || input.inferenceType2 == 'Confidence Interval'",
            h2(tags$u("Hypothesis Test")),# TODO: former titlePanel; set window title dynamically elsewhere.
            br(),
            uiOutput(ns("signedRankHypothesisTest")),
            br()
          ) ## Hypothesis Testing
        ), ## Analysis Tab
        tabPanel(
          id = ns("signedRankDataRanks"),
          title = "Data with Ranks",
          conditionalPanel(
            ns = ns,
            condition = "input.inferenceType2 == 'Hypothesis Testing' || input.inferenceType2 == 'Confidence Interval'",

            ## h2("Results"),# TODO: former titlePanel; set window title dynamically elsewhere.
            br(),
            uiOutput(ns("signedRankDataRanks")),
            br()
          ) ## Ranked Results by Group
        ), ## Tabset
        tabPanel(
          id = ns("signedRankGraphs"),
          title = "Graphs",
          conditionalPanel(
            ns = ns,
            condition = "input.popuParameters == 'Wilcoxon Signed Rank Test' && input.signedRankQQPlot == 1",

            ## Q-Q Plot of the Difference
            conditionalPanel(
              ns = ns,
              condition = "input.popuParameters == 'Wilcoxon Signed Rank Test' && input.signedRankQQPlot == 1",
              h2("Q-Q Plot of the Difference"),# TODO: former titlePanel; set window title dynamically elsewhere.
              br(),
              plotOptionsMenuUI(
                id = ns("signedRankQQ"),
                plotType = "QQ Plot",
                title = "Q-Q Plot of the Difference"
              ),
              plotOutput(ns("signedRankQQ")),
              br(),
              br()
            )
          )
        ),
        tabPanel(
          id    = ns("signedRankUploadData"),
          title = "Uploaded Data",
          uiOutput(ns("renderSignedRankUploadData"))
        )
      ) ## Uploaded Data
    ), ## Signed Rank Tabs Whole

    ## ---------------- Dependent Pop Means ----------------------------------
    conditionalPanel(
      ns = ns,
      condition = "input.popuParameters == 'Dependent Population Means'",
      navbarPage(
        id = ns("depPopMeansTabset"),
        selected = "Analysis",
        title = NULL,
        tabPanel(
          id = ns("depPopMeans"),
          title = "Analysis",
          conditionalPanel(
            ns = ns,
            condition = "input.inferenceType2 == 'Confidence Interval'",
            h2(tags$u("Confidence Interval")),# TODO: former titlePanel; set window title dynamically elsewhere.
            br(),
            uiOutput(ns("depMeansCI")),
            br()
          ), ## CI

          conditionalPanel(
            ns = ns,
            condition = "input.inferenceType2 == 'Hypothesis Testing'",
            h2(tags$u("Hypothesis Test")),# TODO: former titlePanel; set window title dynamically elsewhere.
            br(),
            uiOutput(ns("depMeansHT")),
            br()
          ) ## HT
        ), # depPopMeans Analysis tabPanel

        tabPanel(
          id = ns("depPopMeansData"),
          title = "Uploaded Data",
          uiOutput(ns("renderDepPopMeansData"))
        ), # depPopMeansData Uploaded Data tabPanel

        tabPanel(
          id = ns("depMeansDataCalcs"),
          title = "Data with Calculations",
          br(),
          fluidRow(
            column(
              width = 8,
              uiOutput(ns("depMeansTable"))
            ),
            column(
              width = 4,
              br()
            )
          ),
          br(),
          br()
        ), ## Dep means table with calcs

        tabPanel(
          id = ns("depMeansGraphs"),
          title = "Graphs",
          conditionalPanel(
            ns = ns,
            condition = "input.depMeansQQPlot == 1",
            h2(tags$u("Q-Q Plot of the Difference (d)")),# TODO: former titlePanel; set window title dynamically elsewhere.
            br(),
            plotOptionsMenuUI(
              id = ns("depMeansQQPlot"),
              plotType = "QQ Plot",
              title = "Q-Q Plot of the Difference (d)"
            ),
            uiOutput(ns("renderDepMeansQQPlot")),
            br(),
            br()
          )
        ) ## Dep means graphs tab panel
      ) ## depPopMeansTabset
    ), ## Two Dependent Samples

    ## ---------------- 2 Pop Proportions --------------------------------------
    conditionalPanel(
      ns = ns,
      condition = "input.popuParameters == 'Population Proportions'",
      navbarPage(
        id = ns("twoPropTabset"),
        selected = "Analysis",
        title = NULL,
        tabPanel(
          id = ns("twoProp"),
          title = "Analysis",
          conditionalPanel(
            ns = ns,
            condition = "input.inferenceType2 == 'Confidence Interval'",
            h2(tags$u("Confidence Interval")),# TODO: former titlePanel; set window title dynamically elsewhere.
            br(),
            uiOutput(ns("twoPropCI")),
            br()
          ), ## Confidence Interval

          conditionalPanel(
            ns = ns,
            condition = "input.inferenceType2 == 'Hypothesis Testing'",
            h2(tags$u("Hypothesis Test")),# TODO: former titlePanel; set window title dynamically elsewhere.
            br(),
            uiOutput(ns("twoPropHT")),
            br()
          ) ## Hypothesis Testing
        ), ## Analysis Panel
        tabPanel(
          id = ns("twoPropGraphs"),
          title = "Graphs",
          br(),
          div(
            style = "width: 600px; text-align: left;",
            plotOutput(ns("twoPropBarPlot"), height = "400px")
          ),
          div(
            style = "display: flex; justify-content: flex-start;",
            plotOutput(ns("twoPropPieChart"),
                       width = "600px", height = "500px"
                       )
          )
        ) ## Graph Panel
      )
    ), ## Two Population Proportions

    ## ---------------- Wilcoxon Rank Sum --------------------------------------
    conditionalPanel(
      ns = ns,
      condition = "input.popuParameters == 'Wilcoxon rank sum test'",
      navbarPage(
        id = ns("wilcoxonRankSumTabset"),
        selected = "Analysis",
        title = NULL,
        tabPanel(
          id = ns("wilcoxonRankSumTab"),
          title = "Analysis",
          conditionalPanel(
            ns = ns,
            condition = "input.inferenceType2 == 'Hypothesis Testing' || input.inferenceType2 == 'Confidence Interval'",
            h2(tags$u("Hypothesis Test")),# TODO: former titlePanel; set window title dynamically elsewhere.
            br(),
            uiOutput(ns("wilcoxonRankSum")),
            br()
          ) ## Hypothesis Testing
        ), ## Analysis Tab
        tabPanel(
          id = ns("wilcoxonRankSumDataRanks"),
          title = "Data with Ranks",
          conditionalPanel(
            ns = ns,
            condition = "input.inferenceType2 == 'Hypothesis Testing' || input.inferenceType2 == 'Confidence Interval'",
            h2("Ranked Results by Group"),# TODO: former titlePanel; set window title dynamically elsewhere.
            br(),
            uiOutput(ns("wilcoxonRankSumDataRanks")),
            br()
          ) ## Ranked Results by Group
        ), ## Data with Ranks Tab
        tabPanel(
          id = ns("wilcoxonRankSumGraphs"),
          title = "Graphs",
          conditionalPanel(
            ns = ns,
            condition = "input.popuParameters == 'Wilcoxon rank sum test' && (input.sidebysidewRankSum == 1 || input.sidebysidewRankQQ == 1)",

            ## Side-by-side Boxplot
            conditionalPanel(
              ns = ns,
              condition = "input.popuParameters == 'Wilcoxon rank sum test' && input.sidebysidewRankSum == 1",
              h2("Side-by-side Boxplot"),# TODO: former titlePanel; set window title dynamically elsewhere.
              br(),
              plotOptionsMenuUI(
                id = ns("sidebysidewRankSum"),
                plotType = "Boxplot",
                title = "Boxplot"
              ),
              plotOutput(ns("sidebysidewRankSum")),
              br(),
              br()
            ),

            ## Q-Q Plots
            conditionalPanel(
              ns = ns,
              condition = "input.popuParameters == 'Wilcoxon rank sum test' && input.sidebysidewRankQQ == 1",
              h2("Q-Q Plots for Sample 1 and Sample 2"),# TODO: former titlePanel; set window title dynamically elsewhere.
              br(),
              plotOptionsMenuUI(
                id = ns("sidebysidewRankQQ"),
                plotType = "QQ Plot",
                title = "Q-Q Plots"
              ),
              plotOutput(ns("sidebysidewRankQQ")),
              br(),
              br()
            )
          )
        ),
        tabPanel(
          id    = ns("wRankSumData"),
          title = "Uploaded Data",
          uiOutput(ns("renderWRankSumMeansData"))
        )
      ) ## Uploaded Data
    ), ## Wilcoxon rank sum Tabs whole

    ## ------------ Two Pop Var ------------------------------------------

    conditionalPanel(
      ns = ns,
      condition = "input.popuParameters == 'Two Population Variances'",
      navbarPage(
        id = ns("twoPopVarTabset"),
        selected = "Analysis",
        title = NULL,
        tabPanel(
          id = ns("twoPopVar"),
          title = "Analysis",
          conditionalPanel(
            ns = ns,
            condition = "input.inferenceType2 == 'Confidence Interval'",
            h2(tags$u("Confidence Interval")),# TODO: former titlePanel; set window title dynamically elsewhere.
            br(),
            uiOutput(ns("twoPopVarCI")),
            br()
          ), ## CI

          conditionalPanel(
            ns = ns,
            condition = "input.inferenceType2 == 'Hypothesis Testing'",
            h2(tags$u("Hypothesis Test")),# TODO: former titlePanel; set window title dynamically elsewhere.
            br(),
            uiOutput(ns("twoPopVarHT")),
            br()
          ) ## HT
        )
      )
    ) ## Two Pop Var
  )

  submoduleUI
}
