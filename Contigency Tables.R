install.packages("gmodels")
library(gmodels)

angtl4 <- read_csv("D:/OneDrive/ncid/GitHub/angtl4/ANGPLT4-Julius.csv")

CrossTable(angtl4$gender, angtl4$ethnic, angtl4$dhf_1997, angtl4$sd_2009)
CrossTable(angtl4$gender, angtl4$ethnic, angtl4$dhf_1997, angtl4$sd_2009,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, prop.c = FALSE)

install.packages("vcd")
library(vcd)

vcd::mosaic(angtl4$gender, angtl4$ethnic,
           shade = TRUE, legend = TRUE)


library(MASS)
data("Titanic")
vcd::mosaic(Titanic)
vcd::mosaic(~ Sex + Age + Survived, data = Titanic,
       main = "Survival on the Titanic", shade = TRUE, legend = TRUE)


angtl4$gender_ethnic <- interaction(angtl4$gender, angtl4$ethnic, sep = " / ")

table1(~ gender+ ethnic |dhf_1997+sd_2009, data=angtl4)
table1(~ gender_ethnic | dhf_1997 + sd_2009, data = angtl4)


library(gmodels)

with(angtl4, {
  row_fac <- interaction(gender, ethnic, sep = " ▸ ")          # rows: Gender ▸ Ethnic
  col_fac <- interaction(dhf_1997, sd_2009, sep = " + ")       # cols: DHF × SD
  
  CrossTable(
    row_fac, col_fac,
    prop.r = FALSE, prop.c = FALSE, prop.t = TRUE,             # counts + overall %
    chisq = FALSE, fisher = FALSE, mcnemar = FALSE,
    format = "SPSS"                                            # compact output
  )
})


with(angtl4, {
  row_fac <- interaction(gender, ethnic, sep = " ▸ ")     # (Gender > Ethnic)
  col_fac <- interaction(dhf_1997, sd_2009, sep = " + ")  # (DHF + SD)
  
  CrossTable(
    row_fac, col_fac,
    prop.r = FALSE, prop.c = FALSE, prop.t = TRUE,
    chisq = FALSE, fisher = FALSE, mcnemar = FALSE,
    format = "SPSS",
    dnn = c("Gender > Ethnic", "DHF + SD")                # set display names
  )
})





with(angtl4, {
  # Combine 3 variables for rows
  row_fac <- interaction(gender, ethnic, in_out_pt, sep = " ▸ ")
  
  # Combine 3 variables for columns
  col_fac <- interaction(dhf_1997, sd_2009, sep = " + ")
  
  CrossTable(
    row_fac, col_fac,
    prop.r = FALSE, prop.c = FALSE, prop.t = TRUE,
    chisq = FALSE, fisher = FALSE, mcnemar = FALSE,
    format = "SPSS",
    dnn = c("Gender ▸ Ethnic ▸ In/Out Patient", "DHF + SD")
  )
})

tab_text <- capture.output( 
  with(angtl4, {
  # Combine 3 variables for rows
  row_fac <- interaction(gender, ethnic, in_out_pt, pl_leak, sep = " ▸ ")
  
  # Combine 3 variables for columns
  col_fac <- interaction(dhf_1997, sd_2009, sep = " + ")
  
  CrossTable(
    row_fac, col_fac,
    prop.r = FALSE, prop.c = FALSE, prop.t = TRUE,
    chisq = FALSE, fisher = FALSE, mcnemar = FALSE,
    format = "SPSS",
    dnn = c("Gender ▸ Ethnic ▸ In/Out Patient ▸ PL Leak", "DHF + SD")
  )
})
)

# Save the text output to a Excel file

library(writexl)

write_xlsx(data.frame(Output = tab_text), "crosstable_text.xlsx")

# Create a Word document with the contingency table text
library(officer)
# Create Word doc with the text table
read_docx() %>%
  body_add_par(paste(tab_text, collapse = "\n"), style = "Normal") %>%
  print(target = "contingency_table.docx")



############

library(dplyr)
library(flextable)
library(officer)

# Create a data frame version of the table
tab_df <- angtl4 %>%
  count(
    Gender = gender,
    Ethnic = ethnic,
    InOutPt = in_out_pt,
    DHF = dhf_1997,
    SD = sd_2009,
    PL_Leak = pl_leak
  )

# Format with flextable
ft <- flextable(tab_df)

# Export to Word
read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "contingency_table.docx")

################
library(dplyr)
library(flextable)
library(officer)
library(tidyr)
library(openxlsx)

# Create nested contingency table
tab_df <- angtl4 %>%
  count(Gender = gender, Ethnic = ethnic, InOutPt = in_out_pt, PL_Leak = pl_leak, 
        DHF = dhf_1997, SD = sd_2009) %>%
  unite("DHF_SD", DHF, SD, sep = "_") %>%
  pivot_wider(names_from = DHF_SD, values_from = n, values_fill = 0)

# Create flextable with nested row headers
ft <- flextable(tab_df) %>%
  merge_v(j = "Gender") %>%
  merge_v(j = "Ethnic") %>%
  merge_v(j = "InOutPt") %>%
  theme_box() %>%
  autofit()

# Export to Excel
write.xlsx(tab_df, "nested_contingency_table.xlsx", rowNames = FALSE)

# Export flextable to Word
read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "nested_contingency_table.docx")



###############

# --- Packages ---
library(dplyr)
library(tidyr)
library(openxlsx)

# --- 1) Build long counts (4 row vars × 2 col vars) ---
tab_long <- angtl4 %>%
  count(
    Gender   = gender,
    Ethnic   = ethnic,
    InOutPt  = in_out_pt,
    PL_Leak  = pl_leak,
    DHF      = dhf_1997,
    SD       = sd_2009,
    name = "Freq"
  )

# --- 2) Pivot to wide with nested column keys (DHF + SD) ---
tab_wide <- tab_long %>%
  arrange(Gender, Ethnic, InOutPt, PL_Leak, DHF, SD) %>%
  pivot_wider(
    names_from  = c(DHF, SD),              # two-level columns
    values_from = Freq,
    values_fill = 0,
    names_sep   = " + "
  )

# Identify row id block and data block
row_id <- tab_wide %>% select(Gender, Ethnic, InOutPt, PL_Leak)
data_mat <- tab_wide %>% select(-Gender, -Ethnic, -InOutPt, -PL_Leak)

# Parse multi-level column headers (DHF / SD) from names like "Yes + Severe"
col_keys  <- colnames(data_mat)
hdr_split <- do.call(rbind, strsplit(col_keys, " \\+ ", fixed = FALSE))
colnames(hdr_split) <- c("DHF", "SD")

# --- 3) Write to Excel with merged multi-level headers and row groups ---
wb <- createWorkbook()
addWorksheet(wb, "Pivot")

# Header layout:
# Row 1 -> DHF labels across blocks
# Row 2 -> SD labels within DHF
# Data start at Row 3
# Left 4 columns are hierarchical row labels

# 3.1 Write left header titles
writeData(wb, "Pivot", x = t(c("Gender","Ethnic","In/OutPt","PL Leak")),
          startCol = 1, startRow = 1, colNames = FALSE)
# Fill the second header row on the left with blanks
writeData(wb, "Pivot", x = matrix("", nrow = 1, ncol = 4),
          startCol = 1, startRow = 2, colNames = FALSE)

# 3.2 Write top multi-row column headers
start_col <- 5  # first data column
writeData(wb, "Pivot", x = t(hdr_split[, "DHF"]),
          startCol = start_col, startRow = 1, colNames = FALSE)
writeData(wb, "Pivot", x = t(hdr_split[, "SD"]),
          startCol = start_col, startRow = 2, colNames = FALSE)

# 3.3 Write body: row identifiers + data matrix
writeData(wb, "Pivot", x = row_id,  startCol = 1,       startRow = 3, colNames = FALSE)
writeData(wb, "Pivot", x = data_mat, startCol = start_col, startRow = 3, colNames = FALSE)

# --- 4) Merge headers (no overlaps) ---

# Merge DHF blocks in header row 1
dhf_rle    <- rle(hdr_split[, "DHF"])
dhf_ends   <- cumsum(dhf_rle$lengths)
dhf_starts <- c(1, head(dhf_ends, -1) + 1)
for (i in seq_along(dhf_rle$values)) {
  if (dhf_rle$lengths[i] > 1) {
    mergeCells(wb, "Pivot",
               cols = c(start_col + dhf_starts[i] - 1, start_col + dhf_ends[i] - 1),
               rows = 1)
  }
}

# Merge SD blocks within each DHF span in header row 2
for (i in seq_along(dhf_rle$values)) {
  s <- dhf_starts[i]; e <- dhf_ends[i]
  sd_rle    <- rle(hdr_split[s:e, "SD"])
  sd_ends   <- s - 1 + cumsum(sd_rle$lengths)
  sd_starts <- c(s, head(sd_ends, -1) + 1)
  for (j in seq_along(sd_rle$values)) {
    if (sd_rle$lengths[j] > 1) {
      mergeCells(wb, "Pivot",
                 cols = c(start_col + sd_starts[j] - 1, start_col + sd_ends[j] - 1),
                 rows = 2)
    }
  }
}

# --- 5) Merge row groups vertically for visual nesting ---
merge_vertical_runs <- function(vec, col_index, first_data_row) {
  # vec is already in display order; merge consecutive identical values
  r <- rle(as.character(vec))
  ends <- cumsum(r$lengths)
  starts <- c(1, head(ends, -1) + 1)
  for (k in seq_along(r$values)) {
    if (r$lengths[k] > 1) {
      r1 <- first_data_row + starts[k] - 1
      r2 <- first_data_row + ends[k] - 1
      mergeCells(wb, "Pivot",
                 cols = col_index,
                 rows = c(r1, r2))
    }
  }
}
first_data_row <- 3
# Ensure rows are ordered for proper grouping
ord <- order(row_id$Gender, row_id$Ethnic, row_id$InOutPt, row_id$PL_Leak)
row_id    <- row_id[ord, , drop = FALSE]
data_mat  <- data_mat[ord, , drop = FALSE]
# Re-write ordered blocks (overwrite is fine)
writeData(wb, "Pivot", x = row_id,   startCol = 1,       startRow = 3, colNames = FALSE)
writeData(wb, "Pivot", x = data_mat, startCol = start_col, startRow = 3, colNames = FALSE)

# Merge each row-id column (1..4)
merge_vertical_runs(row_id$Gender,   col_index = 1, first_data_row = first_data_row)
merge_vertical_runs(row_id$Ethnic,   col_index = 2, first_data_row = first_data_row)
merge_vertical_runs(row_id$InOutPt,  col_index = 3, first_data_row = first_data_row)
merge_vertical_runs(row_id$PL_Leak,  col_index = 4, first_data_row = first_data_row)

# --- 6) Style, borders, freeze panes ---
header_style <- createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE
)

# Correct border definition
body_border  <- createStyle(
  border = "TopBottomLeftRight",
  borderStyle = "thin"
)

# Bold + center the two header rows
addStyle(wb, "Pivot", style = header_style,
         rows = 1:2, cols = 1:(start_col + ncol(data_mat) - 1),
         gridExpand = TRUE)

# Borders for the data region
n_rows <- nrow(row_id)
addStyle(wb, "Pivot", style = body_border,
         rows = 3:(2 + n_rows),
         cols = 1:(start_col + ncol(data_mat) - 1),
         gridExpand = TRUE)

# Auto widths and freeze
setColWidths(wb, "Pivot", cols = 1:(start_col + ncol(data_mat) - 1), widths = "auto")
freezePane(wb, "Pivot", firstActiveRow = 3, firstActiveCol = start_col)

# --- 7) Save Excel ---
saveWorkbook(wb, "contingency_pivot_4row_2col.xlsx", overwrite = TRUE)








tab <- with(angtl4, table(
  `Gender ▸ Ethnic`       = interaction(gender, ethnic, sep = " ▸ "),
  `DHF1997 × SD2009`      = interaction(dhf_1997, sd_2009, sep = " × ")
))

addmargins(tab)  # adds overall row/column totals
