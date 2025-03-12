pacman::p_load(tfrmt, dplyr, gt, tidyr)

data_demog

data_demog %>% 
  distinct(rowlbl1,grp)

tfrmt(
  # specify columns in the data
  group = c(rowlbl1,grp),
  label = rowlbl2,
  column = column, 
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2)) %>% 
  print_to_gt(data_demog) %>% 
  tab_options(
    container.width = 900
  )

tfrmt(
  # specify columns in the data
  group = c("rowlbl1", "grp"),
  label = "rowlbl2",
  column = "column", 
  param = "param",
  value = "value",
  sorting_cols = c("ord1", "ord2")) %>% 
  print_to_gt(data_demog) %>% 
  tab_options(
    container.width = 900
  )


tfrmt(
  # specify columns in the data
  group = c("rowlbl1","grp"),
  label = "rowlbl2",
  column = "column", 
  param = "param",
  value = "value",
  sorting_cols = c("ord1", "ord2"),
  # specify value formatting 
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} ({pct} %)", 
                                                                                n = frmt("xxx"),
                                                                                pct = frmt("xx.x"))),
    frmt_structure(group_val = ".default", label_val = "n", frmt("xxx")),
    frmt_structure(group_val = ".default", label_val = c("Mean", "Median", "Min","Max"), frmt("xxx.x")),
    frmt_structure(group_val = ".default", label_val = "SD", frmt("xxx.xx")),
    frmt_structure(group_val = ".default", label_val = ".default", p = frmt_when(">0.99" ~ ">0.99",
                                                                                 "<0.001" ~ "<0.001",
                                                                                 TRUE ~ frmt("x.xxx", missing = "")))
  )) %>% 
  print_to_gt(data_demog) %>% 
  tab_options(
    container.width = 900
  )


tfrmt(
  # specify columns in the data
  group = c("rowlbl1","grp"),
  label = "rowlbl2",
  column = "column", 
  param = "param",
  value = "value",
  sorting_cols = c("ord1", "ord2"),
  # specify value formatting 
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}", 
                                                                                n = frmt("xxx"),
                                                                                pct = frmt_when("==100" ~ "",
                                                                                                "==0" ~ "",
                                                                                                TRUE ~ frmt("(xx.x %)")))),
    frmt_structure(group_val = ".default", label_val = "n", frmt("xxx")),
    frmt_structure(group_val = ".default", label_val = c("Mean", "Median", "Min","Max"), frmt("xxx.x")),
    frmt_structure(group_val = ".default", label_val = "SD", frmt("xxx.xx")),
    frmt_structure(group_val = ".default", label_val = ".default", p = frmt("")),
    frmt_structure(group_val = ".default", label_val = c("n","<65 yrs","<12 months","<25"), p = frmt_when(">0.99" ~ ">0.99",
                                                                                                          "<0.001" ~ "<0.001",
                                                                                                          TRUE ~ frmt("x.xxx", missing = "")))
  ),
  # remove extra cols
  col_plan = col_plan(-grp, 
                      -starts_with("ord") )) %>% 
  print_to_gt(data_demog) %>% 
  tab_options(
    container.width = 900
  )

demotab <- tfrmt(
  # specify columns in the data
  group = c("rowlbl1","grp"),
  label = "rowlbl2",
  column = "column", 
  param = "param",
  value = "value",
  sorting_cols = c("ord1", "ord2"),
  # specify value formatting 
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}", 
                                                                                n = frmt("xxx"),
                                                                                pct = frmt_when("==100" ~ "",
                                                                                                "==0" ~ "",
                                                                                                TRUE ~ frmt("(xx.x %)")))),
    frmt_structure(group_val = ".default", label_val = "n", frmt("xxx")),
    frmt_structure(group_val = ".default", label_val = c("Mean", "Median", "Min","Max"), frmt("xxx.x")),
    frmt_structure(group_val = ".default", label_val = "SD", frmt("xxx.xx")),
    frmt_structure(group_val = ".default", label_val = ".default", p = frmt("")),
    frmt_structure(group_val = ".default", label_val = c("n","<65 yrs","<12 months","<25"), p = frmt_when(">0.99" ~ ">0.99",
                                                                                                          "<0.001" ~ "<0.001",
                                                                                                          TRUE ~ frmt("x.xxx", missing = "")))
  ),
  # remove extra cols
  col_plan = col_plan(-grp, 
                      -starts_with("ord") ),
  # Specify column styling plan
  col_style_plan = col_style_plan(
    col_style_structure(align = c(".",","," "), col = c("Placebo", "Xanomeline Low Dose",
                                                        "Xanomeline High Dose", "Total", "p-value")),
    col_style_structure(align = "left", col = c("rowlbl1","rowlbl2"))
  ),
  
  # Specify row group plan
  row_grp_plan = row_grp_plan(
    row_grp_structure(group_val = ".default", element_block(post_space = " ")),
    label_loc = element_row_grp_loc(location = "column")
  )
  
) %>% 
  print_to_gt(data_demog) %>% 
  tab_options(
    container.width = 900
  )
demotab |> gtsave("tab_1.docx")


##Adverse Effect ---------------------------------------------------------------
data_ae2 <- data_ae %>% 
  group_by(AEBODSYS, AETERM) %>% 
  mutate(pct_high = value[col2=="Xanomeline High Dose" & param=="pct"]) %>% 
  ungroup %>% 
  filter(pct_high >10) %>% 
  select(-pct_high)

tfrmt(
  # specify columns in the data
  group = "AEBODSYS",
  label = "AETERM",
  column = c("col2", "col1"), 
  param = "param",
  value = "value",
  sorting_cols = c("ord1", "ord2")) %>% 
  print_to_gt(data_ae2) %>% 
  tab_options(
    container.width = 1000
  )

tfrmt(
  # specify columns in the data
  group = "AEBODSYS",
  label = "AETERM",
  column = c("col2", "col1"), 
  param = "param",
  value = "value",
  sorting_cols = c("ord1", "ord2"),
  # specify value formatting 
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine("{n} {pct}",
                                n = frmt("XXX"),
                                pct = frmt_when(
                                  "==100" ~ "",
                                  "==0" ~ "",
                                  TRUE ~ frmt("(xx.x %)")))),
    frmt_structure(group_val = ".default", label_val = ".default", 
                   AEs = frmt("[XXX]")),
    frmt_structure(group_val = ".default", label_val = ".default", 
                   pval = frmt_when(">0.99" ~ ">0.99",
                                    "<0.001" ~ "<0.001",
                                    "<0.05" ~ frmt("x.xxx*"),
                                    TRUE ~ frmt("x.xxx", missing ="--")))
  )) %>% 
  print_to_gt(., data_ae2) %>% 
  tab_options(
    container.width = 1000
  )
