# For the output in the Data table with Ranks Tab
RankedTableOutput <- function(data) {
  renderUI({
    req(data)
    
    df <- if (is.reactive(data)) data() else data
    
    ranked_data_wide <- df %>%
      dplyr::select(Group, Value, Rank) %>%
      dplyr::arrange(Group, Rank) %>%
      dplyr::group_by(Group) %>%
      dplyr::mutate(ObsID = row_number()) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(
        id_cols = ObsID,
        names_from = Group,
        values_from = c(Value, Rank),
        names_sep = " "  
      ) %>%
      dplyr::select(-ObsID) %>%
      dplyr::select(
        order(
          match(
            gsub("(Value|Rank) (.*)", "\\2", names(.)), 
            unique(gsub("(Value|Rank) (.*)", "\\2", names(.)))
          ),
          match(
            gsub("(Value|Rank) (.*)", "\\1", names(.)), 
            c("Value", "Rank")
          )
        )
      ) %>%
      
      dplyr::rename_with(~gsub("Value (.*)", "\\1 Value", .)) %>%
      dplyr::rename_with(~gsub("Rank (.*)", "\\1 Rank", .))
    
    tagList(
      div(
        DT::datatable(
          ranked_data_wide,
          rownames = FALSE,
          options = list(
            pageLength = 10,
            lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "all")),
            scrollX = TRUE,
            columnDefs = list(
              list(className = 'dt-center', targets = "_all"),
              list(width = '120px', targets = "_all")  
            )
          )
        ), 
        style = "width: 95%"
      ),
      br(),
      br()
    )
  })
}

SignedRankTableOutput <- function(data) {
  renderUI({
    req(data)
    
    df <- if (is.reactive(data)) data() else data

    display_data <- df %>%
      dplyr::select(
        `Sample1 Data` = Sample1,
        `Sample2 Data` = Sample2,
        Difference = Value,  
        Rank = Rank,
        `Signed Rank` = SignedRank
      ) %>%
      dplyr::arrange(Rank)
    
    tagList(
      div(
        DT::datatable(
          display_data,
          rownames = FALSE,
          options = list(
            pageLength = 10,
            lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "all")),
            scrollX = TRUE,
            columnDefs = list(
              list(className = 'dt-center', targets = "_all"),
              list(width = '120px', targets = "_all")
            )
          )
        ), 
        style = "width: 95%"
      ),
      br(),
      br()
    )
  })
}