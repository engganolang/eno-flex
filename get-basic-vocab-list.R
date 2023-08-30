library(pdftools)
library(tidyverse)

flexdb <- read_rds('senses_gloss_lang_df7.rds')
flexdb_eng <- flexdb$english_form
flexdb_eng_rgx <- paste("\\b(", paste(flexdb_eng, collapse = "|"), ")\\b", sep = "") |> 
  str_replace_all("\\.", "\\\\.") |> 
  str_replace_all("\\(", "\\\\(") |> 
  str_replace_all("\\)", "\\\\)")


# 1. General service list ======
first1000 <- pdftools::pdf_text("basic-vocab-list/1-general-service-list-headwords-first-1000.pdf")
first1000_chr <- first1000 |> 
  str_split("(\\\n|\\s{2,})") |> 
  map(~.[nzchar(.)]) |> 
  map(~str_subset(., "(^\\d+$|(first|second) 1000)", negate = TRUE)) |> 
  map(~sort(., decreasing = FALSE)) |> 
  unlist()
first1000_rgx <- paste("\\b(", paste(first1000_chr, collapse = "|"), ")\\b", sep = "")
# check which English gloss is detected in the first 1000 words
flexdb |> filter(str_detect(english_form, first1000_rgx))

# TO DO: get the INDEX of the first1000 words that are detected in the flex db

second1000 <- pdftools::pdf_text("basic-vocab-list/1-general-service-list-headwords-second-1000.pdf")
second1000_chr <- second1000 |> 
  str_split("(\\\n|\\s{2,})") |> 
  map(~.[nzchar(.)]) |> 
  map(~str_subset(., "(^\\d+$|(first|second) 1000)", negate = TRUE)) |> 
  map(~sort(., decreasing = FALSE)) |> 
  unlist()
second1000_rgx <- paste("\\b(", paste(second1000_chr, collapse = "|"), ")\\b", sep = "")
# check which English gloss is detected in the second 1000 words
flexdb |> filter(str_detect(english_form, second1000_rgx))


# 2. Oxford 3000 =====
oxf <- pdftools::pdf_text("basic-vocab-list/2-The_Oxford_3000.pdf")
oxf1 <- oxf |> 
  str_split("(\\\n)") |> 
  map(~str_split(., "\\s{2,}")) |> 
  map(~unlist(.))
pos <- c("indefinite article", "adj\\.", "n\\.", "adv\\.", "prep\\.", "exclam\\.", "pron\\.", "det\\.", "conj\\.", "v\\.", "number", "modal", "auxiliary", "noun\\.", "infinitive marker", "definite article")
pos_rgx <- paste("\\b(", paste(pos, collapse = "|"), ")", sep = "")
pos_rgx
cefr_rgx <- "\\b(A1|B1|A2|B2)\\b"
oxf2 <- oxf1 |> 
  map(~str_replace_all(., pos_rgx, "</pos=\\1>")) |> 
  map(~str_replace_all(., cefr_rgx, "</cefr=\\1>")) |> 
  map(~.[nzchar(.)]) |> 
  map(~str_subset(., "(University Press|The Oxford|\\d+ \\/ 11)", negate = TRUE)) |> 
  unlist() |> 
  str_replace_all("\\.(?=\\>)", "")
oxf3 <- oxf2 %>%
  # double
  replace(. == "double </pos=adj>, </pos=det>, </pos=pron>, </pos=v> </cefr=A2>,", 
          "double </pos=adj>, </pos=det>, </pos=pron>, </pos=v> </cefr=A2>, </pos=adv> </cefr=B1>") %>%
  
  # like
  replace(. == "like (find sb/sth pleasant) </pos=v> </cefr=A1>,",
          "like (find sb/sth pleasant) </pos=v> </cefr=A1>, </pos=n> </cefr=B1>") %>%
  
  # match
  replace(. == "match (contest/correspond) </pos=n>,",
          "match (contest/correspond) </pos=n>, </pos=v> </cefr=A1>") %>%
  
  # outside
  replace(. == "outside </pos=adv> </cefr=A1>, </pos=prep>, </pos=noun>, </pos=adj>",
          "outside </pos=adv> </cefr=A1>, </pos=prep>, </pos=noun>, </pos=adj> </cefr=A2>") %>%
  
  # second1
  replace(. == "second1 (next after the first) </pos=det>/",
          "second1 (next after the first) </pos=det>/</pos=number> </cefr=A1>, </pos=adv> </cefr=A2>") %>%
  
  # mis-tagged the word number for POS
  replace(. == "</pos=number> </pos=n> </cefr=A1>, </pos=v> </cefr=A2>",
          "number </pos=n> </cefr=A1>, </pos=v> </cefr=A2>")
oxf4 <- oxf3 |> 
  str_extract("^[^<]+(?=\\<)") |> 
  str_trim(side = "right") |> 
  sort()

oxf5 <- str_replace_all(oxf4, "\\d$", "")
oxf5_rgx <- paste("\\b(", paste(oxf5, collapse = "|"), ")\\b", sep = "")

flexdb |> 
  filter(str_detect(english_form, oxf5_rgx))

oxf5 |> str_subset(flexdb_eng_rgx, negate = TRUE)
