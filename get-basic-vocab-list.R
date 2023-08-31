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




# > flexdb |> pull(gram_vals) |> unique()
# [1] "Adverb"                   "Verb"                     "Indonesian Noun"          "Indonesian Verb"          "Noun"                    
# [6] "Interjection" (done)            "Numeral"  (done)                "Adjective"                NA (done)                 "Pro-adverb"  (done)            
# [11] "Proper  Noun" (done)            "negation" (done)                "Quantifier" (done)               "Relativizer" (done)             "Pronoun" (done)                 
# [16] "Preposition"  (done)            "Classifier" (done)               "Subordinating connective" (done) "Interrogative pro-form" (done)   "Coordinating connective" (done)
# [21] "Demonstrative" (done)           "Auxiliary"  (done)              "Particle" (done)                 "Pro-form" (done)

# The categories marked 'done' above have been manually checked with the first and second general service list and the Oxford 3000
# The next steps are:
## 1. to filter the remaining categories from flexdb
flextocheck <- flexdb |> 
  filter(gram_vals %in% c('Adverb', 'Verb', 'Indonesian Noun', 'Indonesian Verb', 'Noun', 'Adjective'))
## 2. to count the number of words in the english_form (see if it is a single or multiword definition/gloss) and determine english_form that has capital letter (to see proper name and/or glossing of grammar)
flextocheck <- flextocheck |> 
  mutate(eng_form_capital = if_else(str_detect(english_form, '[A-Z]+'), TRUE, FALSE)) |> 
  mutate(n_eng_form = if_else(eng_form_capital, NA, str_count(english_form, "\\b([a-z]+)\\b")))
## 3. to filter the one-word english_forms and create the regex for those words. 
eng_gloss1 <- flextocheck |> 
  filter(n_eng_form == 1) |> 
  pull(english_form) |> 
  str_trim() |> 
  sort()
## 3.1 Also create the regex of one-word item in the manually checked categories
flexcheckdone <- flexdb |> 
  filter(!gram_vals %in% c('Adverb', 'Verb', 'Indonesian Noun', 'Indonesian Verb', 'Noun', 'Adjective')) |>
  mutate(gram_vals = if_else(is.na(gram_vals), 'NA', gram_vals)) |> 
  filter(gram_vals != 'Proper  Noun') |> 
  mutate(eng_form_capital = if_else(str_detect(english_form, '[A-Z]+'), TRUE, FALSE)) |> 
  mutate(n_eng_form = if_else(eng_form_capital, NA, str_count(english_form, "\\b([a-z]+)\\b")))
eng_gloss2 <- flexcheckdone |> 
  filter(n_eng_form == 1) |> 
  pull(english_form) |> 
  str_trim() |> 
  sort()

## 3.2 Combine the eng_gloss1 and eng_gloss2 as tibble
eng_gloss <- tibble(gloss = unique(eng_gloss1), cats = 'eng_gloss1') |> 
  bind_rows(tibble(gloss = unique(eng_gloss2), cats = 'eng_gloss2'))


## 4. to use the one-word regex to filter from the general list words that are not in the regex
eng_gloss3 <- c(unique(eng_gloss$gloss), 'silent', 'quiet', 'pass', 'climb', 'sir', 'we', 'I', 'their', 'its', 'you', 'she', 'he', 'it', 'they', 'her', 'his', 'the', 'able', 'a', 'at', 'me', 'my', 'mine', 'no', 'god', 'talk', 'bad', 'wicked', 'search', 'cut', 'drown', 'fight', 'boat', 'bowl', 'lid', 'hit', 'girl', 'similar', 'scream', 'shout', 'wake', 'awake', 'wine', 'asleep', 'loose', 'split', 'above', 'top', 'born', 'which')
service_list_1_manual_check <- read_lines('basic-vocab-list/1-general-service-1st-already-in-contemporary-FLEx.txt')
service_list_2_manual_check <- read_lines('basic-vocab-list/1-general-service-2nd-already-in-contemporary-FLEx.txt')
eng_gloss3 <- unique(c(eng_gloss3, service_list_1_manual_check, service_list_2_manual_check))

oneword_gloss_not_in_flex <- setdiff(first1000_chr, eng_gloss3)
oneword_gloss_not_in_flex
length(oneword_gloss_not_in_flex)
# [1] 531

oneword_gloss_not_in_service_list <- setdiff(eng_gloss3, first1000_chr)
oneword_gloss_not_in_service_list
length(oneword_gloss_not_in_service_list) # the first one thousand general list
# [1] 534

oneword_gloss_in_flex <- intersect(first1000_chr, eng_gloss3)
oneword_gloss_in_flex
length(oneword_gloss_in_flex)
# [1] 394

### 4.1 turn the not-in-flex items into regex to be searched again in the flex database as flexible regex rather than exact match
oneword_gloss_not_in_flex_rgx <- paste('\\b(', paste(oneword_gloss_not_in_flex, collapse = '|'), ')\\b', sep = '')
flexdb |> 
  filter(str_detect(english_form, oneword_gloss_not_in_flex_rgx)) |> 
  pull(english_form)


## 5. Check GRAMMATICAL glossing in english_form
flexcheckdone |> 
  filter(str_detect(english_form, '[A-Z]{2,}')) |> 
  select(form, english_form, indonesian_form, gram_vals) |> 
  as.data.frame()
