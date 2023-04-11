        select(formatted_csv, full, Com.1, n) %>%
           reactable(
                     defaultSorted = list(n = "desc"),
                     #defaultPageSize = 6,
                     pagination = FALSE,
                     columns = list(
                                    full = colDef(name = "Committee Name"),
                                    Com.1 = colDef(name = "Acronym", footer = "Total"),
                                    n = colDef(name = "Total Bills Received", footer = sprintf("%d", sum(formatted_csv$n)))
                                    #amount.passed = colDef(name = "Total Bills Passed", footer = sprintf("%d", sum(formatted_csv$amount.passed)), width = 133),
                                    #positive = colDef(name = "Positive", footer = sprintf("%d", sum(formatted_csv$positive))),
                                    #neutral = colDef(name = "Neutral", footer = sprintf("%d", sum(formatted_csv$neutral))),
                                    #negative = colDef(name = "Negative", footer = sprintf("%d", sum(formatted_csv$negative))),
                                    #become.law = colDef(name = "Bills Passed into Law", footer = sprintf("%d", sum(formatted_csv$become.law)), width = 160)
                                    ), bordered = TRUE, striped = TRUE, highlight = TRUE,
                details = function(index)
    paste0("The ", formatted_csv[[index, "full"]], " committee has ", formatted_csv[index, "Name"], " as chair. They are",
    if_else(formatted_csv[index, "SwingChair"] == 1,"", " not"), " a part of a swing district. Out of the entire committe, ",
    if_else(formatted_csv[index, "SwingMembers"] == 0,
                        paste("no one of", formatted_csv[index, "TotalMembers"], "is in a swing district. "),
                        if_else(formatted_csv[index, "SwingMembers"] == 1,
                            paste("one person of", formatted_csv[index, "TotalMembers"], "is in a swing district. "),
                            paste0(formatted_csv[index, "SwingMembers"], " of ", formatted_csv[index, "TotalMembers"], " reside in a swing district. "))),
    "In total, this committee received ", formatted_csv[index, "n"], " bills, of which ", formatted_csv[index, "amount.passed"], " ended up passing that committee and ", formatted_csv[index, "become.law"], " became a law."
                ),
                     defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
           )
