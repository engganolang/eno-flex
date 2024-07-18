# source("FLEX-contemporary-with-examples-new.R")
# The above code is to process the .lift output from the FLEx LEXICON

# Code to generate the SFM file to be imported into FLEx
# by combining data from: 
## (i) current FLEx Lexicon (processed in "FLEX-contemporary-with-examples-new.R") and 
## (ii) the Analysed Interlinear Text (processed in "contemporary-enggano-interlinear-text/processing-all-contemporary-texts-ELAN-FLEX-flextext-NEW.R")
# Gede Rajeg (University of Oxford & Udayana University; 2023)

library(tidyverse)

# lu_form_df <- read_rds("FLEX-lift-pre-fieldwork.rds")
lu_form_df <- read_rds("FLEX-lift-march-2024.rds")
source("contemporary-enggano-interlinear-text/processing-all-contemporary-texts-ELAN-FLEX-flextext-NEW-splitting-morpheme.R")
# eno_elicitation_texts <- read_rds("contemporary-enggano-interlinear-text/eno_contemp_elicitation_only_as_tibble-new-binded.rds")
eno_elicitation_texts <- read_rds("contemporary-enggano-interlinear-text/eno_contemp_elicitation_only_as_tibble-new-binded-march2024.rds")
# eno_natural_texts <- read_rds("contemporary-enggano-interlinear-text/eno_contemp_text_only_as_tibble-new-binded.rds")
eno_natural_texts <- read_rds("contemporary-enggano-interlinear-text/eno_contemp_text_only_as_tibble-new-binded-march2024.rds")

# list of proper name to exclude from the entries
person_entries <- c("Ga", "Engga", "Gede", "Charlotte", "Bill",
                    "Dendi", "Daniel", "Mary", "Bill", "Aron",
                    "Andi", "Adam", "Ali", "Indonesia", "Inggris",
                    "Jonas", "London", "M Raflizen Kaitor",
                    "Maria", "Viktor", "Milson", "Selus")

# `eno_morph_split1` holds data from the .flextext export of FLEx (Analysed, interlinear text)
eno_morph_split1 <- eno_morph_split1 |> 
  
  ## replacing multiple white spaces
  mutate(across(matches("^eno_phrase"), ~str_replace_all(., "\\s{2,}", " ")),
         
         ## trimming trailing white spaces
         across(where(is.character), ~str_trim(., side = "both"))) |>
  
  ## replace underscore separating text number in title with dot
  mutate(text_title = str_replace_all(text_title, "(?<=^\\d\\d)_", ".")) |> 
  
  ## filtering person names
  filter(!lex_entry %in% person_entries & !lex_entry %in% str_to_upper(letters)) |> 
  filter(!word %in% person_entries & !word %in% str_to_upper(letters)) |> 
  
  ## filtering word with `???` gloss
  filter(eno_word_gloss_id != "???")

# Creating helper functions to check glossing
check_gloss_lexentry <- function(df = NULL, 
                                 is_indonesian = TRUE,
                                 idn_gloss_rgx = NULL,
                                 returned_cols = NULL) {
  if (is_indonesian) {
    
    col_to_match <- "(sense_gloss_idn|morph_gloss_id)"
    df1 <- df |> 
      filter(if_any(matches(col_to_match), ~str_detect(., idn_gloss_rgx)))
    
    if (is.null(returned_cols)) {
      
      return(df1)
      
    } else {
      
      return(select(df1, matches(returned_cols)))
      
    }
    
    
  } else {
    
    col_to_match <- "(sense_gloss_en|morph_gloss_en)"
    df1 <- df |> 
      filter(if_any(matches(col_to_match), ~str_detect(., idn_gloss_rgx)))
    
    
    if (is.null(returned_cols)) {
      
      return(df1)
      
    } else {
      
      return(select(df1, matches(returned_cols)))
      
    }
    
  }
  
}

check_gloss_wordform <- function(df = NULL, 
                                 is_indonesian = TRUE,
                                 idn_gloss_rgx = NULL,
                                 returned_cols = NULL) {
  
  if (is_indonesian) {
    
    col_to_match <- "word_gloss_id"
    df1 <- df |> 
      filter(if_any(matches(col_to_match), ~str_detect(., idn_gloss_rgx)))
    
    if (is.null(returned_cols)) {
      
      return(df1)
      
    } else {
      
      return(select(df1, matches(returned_cols)))
      
    }
    
  } else {
    
    col_to_match <- "word_gloss_en"
    df1 <- df |> 
      filter(if_any(matches(col_to_match), ~str_detect(., idn_gloss_rgx)))
    
    if (is.null(returned_cols)) {
      
      return(df1)
      
    } else {
      
      return(select(df1, matches(returned_cols)))
      
    }
    
  }
  
}

check_gloss_example <- function(df = NULL, 
                                is_indonesian = TRUE,
                                idn_gloss_rgx = NULL,
                                returned_cols = NULL) {
  
  if (is_indonesian) {
    
    col_to_match <- "phrase_gloss_id"
    df1 <- df |> 
      filter(if_any(matches(col_to_match), ~str_detect(., idn_gloss_rgx)))
    
    if (is.null(returned_cols)) {
      
      return(df1)
      
    } else {
      
      return(select(df1, matches(returned_cols)))
      
    }
    
  } else {
    
    col_to_match <- "phrase_gloss_en"
    df1 <- df |> 
      filter(if_any(matches(col_to_match), ~str_detect(., idn_gloss_rgx)))
    
    if (is.null(returned_cols)) {
      
      return(df1)
      
    } else {
      
      return(select(df1, matches(returned_cols)))
      
    }
    
  }
  
}


# Fill out NA etymology when the sense_gram has Indonesian Noun/Verb
lu_form_df <- lu_form_df |> 
  
  mutate(etym = replace(etym,
                        str_detect(sense_gram, "Indonesian (Verb|Noun)") & 
                          is.na(etym),
                        "Indonesian Loan"),
         
         ## fix inconsistent labeling
         etym = replace(etym, etym == "Indonesian loan",
                        "Indonesian Loan"),
         
         ## replace the sense_gram Indonesian Noun/Verb to just Verb or Noun
         # so that it gets recognised in FLEx dictionary view
         # sense_gram = str_replace_all(sense_gram,
         #                              "^Indonesian\\s(?=(Verb|Noun)\\b)",
         #                              ""),
         
         ## trim the redundant white spaces
         sense_gram = str_replace_all(sense_gram,
                                      "\\s{2,}",
                                      " ")
         
         ) |> 
  
  ## filter out person entries
  filter(!form %in% person_entries) |> 
  filter(!form %in% str_to_upper(letters)) |> 
  
  ## filter out the starred entry
  filter(str_detect(form, "^\\*", negate = TRUE))

# Run codes for fixing typos
source("contemporary-enggano-interlinear-text/gloss_fixing.R")

eno_example_references <- eno_elicitation_texts |> 
  select(text_title, phrase_id, phrase_line) |> 
  bind_rows(eno_natural_texts |> 
              select(text_title, phrase_id, phrase_line)) |> 
  mutate(across(where(is.character), ~str_replace_all(., "_+", "."))) |> 
  mutate(across(matches("(^eno_phrase|)"), ~str_replace_all(., "\\s{2,}", " ")), 
         across(where(is.character), ~str_trim(., side = "both"))) |>
  mutate(refs = paste(text_title,
                      # "_",
                      # abbreviate(phrase_id, minlength = 5L),
                      "_line:", phrase_line, sep = "")) |> 
  mutate(is_natural_texts = if_else(str_detect(text_title, "^\\d"), 
                                    TRUE, 
                                    FALSE))

# eno_morph_split1 <- eno_morph_split1 |> 
#   left_join(eno_example_references)

# 0. Initial filtering ===========
## `flex_from_text` holds filtered data of the .flextext export
flex_from_text <- eno_morph_split1 |>
  
  ## filtering all non-NAs lex_entry,
  filter(!is.na(lex_entry)) |> 
  
  ## filtering non-NAs word forms,
  filter(!is.na(word)) |> 
  
  ## and filtering non-xxx word forms
  filter(str_detect(word, "\\b(?i)x{2,}", negate = TRUE)) |> 
  
  ## filtering all questionable sentences (marked by "*" or "#")
  # filter(str_detect(eno_phrase, "^[*#]", negate = TRUE)) |> 
  
  ## filtering sentences containing "xxx"
  # filter(str_detect(eno_phrase, "\\b(?i)x{2,}", negate = TRUE)) |> 
  # filter(str_detect(eno_phrase_gloss_id, "\\b(?i)x{2,}", negate = TRUE)) |>
  # filter(str_detect(eno_phrase_gloss_eng, "\\b(?i)x{2,}", negate = TRUE)) |>
  
  ## replace NAs with empty string
  mutate(across(where(is.character), ~replace(., is.na(.), ""))) |> 
  
  mutate(# create column indicating if the lexicon & morpheme are the same with the word
         word_equal_lexentry = if_else(word == lex_entry, TRUE, FALSE),
         morph_equal_lexentry = if_else(morph == lex_entry, TRUE, FALSE))

# check which lex entry in FLEx Lexicon not from the Text
lu_form_df |> 
  select(!matches("^x_")) |> 
  filter(!form %in% eno_morph_split1$lex_entry) |> 
  distinct()

# check which lex entry in FLEx Lexicon that is also from the Text
lu_form_df |> 
  select(!matches("^x_")) |> 
  filter(form %in% eno_morph_split1$lex_entry) |> 
  distinct()

# check which lex entry from the Text not available in the FLEx Lexicon
eno_morph_split1 |> 
  filter(!is.na(word),
         !lex_entry %in% lu_form_df$form) |> 
  select(word, lex_entry) |> 
  distinct()

# check which record has NA lex_entry, 
# but complete word (and its glosses and examples [glosses])
eno_morph_split1 |> 
  filter(is.na(lex_entry), 
         !is.na(word), 
         !is.na(eno_word_gloss_en), 
         !is.na(eno_word_gloss_id), 
         !is.na(eno_phrase), 
         !is.na(eno_phrase_gloss_id), 
         !is.na(eno_phrase_gloss_eng))


# `flex_from_text_mini` holds data from the .flextext export of Interlinear analysis
# with a selected few of columns
flex_from_text_mini <- flex_from_text |> 
  select(lex_entry, homonym_id, morph_gloss_en, morph_gloss_id) |> 
  distinct()
flex_from_text_mini

# `flex_lexicon` holds data from the .lift export of FLEx (Lexicon)
flex_lexicon <- lu_form_df |> 
  select(entry_id,
         lex_entry = form, 
         homonym_id = order, 
         sense_order, 
         morph_gloss_en = sense_gloss_en, 
         morph_gloss_id = sense_gloss_idn, 
         sense_gram,
         etym,
         variant = variant_form) |> 
  mutate(sense_order = replace(sense_order, is.na(sense_order), "0"),
         ps_key = sense_gram,
         across(where(is.character), ~replace(., is.na(.), ""))) |> 
  distinct()
  # We need to run distinct() above...
  # because there are entries that are multiplied due to the presence of
  # sentence example (which is excluded from `flex_lexicon` because that sentence example will be taken from the interlinear text table (`flex_from_text`).

flex_lexicon
  # After running distinct, 
  # the entries are 1559, (it is 1535 after various filtering)
  # which is the number in the November 2023 FLEx post-fieldwork


# check the unique grouping to determine the number of sense/gloss that
# a given entry has
flex_entry_sense_count <- flex_lexicon |> 
  left_join(flex_from_text_mini) |> 
  mutate(across(where(is.character), 
                ~replace(., is.na(.), ""))) |> 
  group_by(entry_id, lex_entry, homonym_id, ps_key, etym, variant) |> 
  summarise(n_sense = n_distinct(sense_order), .groups = "drop") |> 
  arrange(desc(n_sense))
flex_entry_sense_count

# 1. Processing root/main entry =======
## Note: creating wide table for the Sense ID, Variant, and Glosses of the roots

df_lex_and_text <- flex_lexicon |> 
  left_join(flex_from_text_mini)
# Joining with `by = join_by(lex_entry, homonym_id, morph_gloss_en, morph_gloss_id)`


## create the \sn and \va columns:
### markers of the sense number and variant/allomorphs
sn <- df_lex_and_text |> 
  
  ### replace NAs with empty character
  mutate(across(where(is.character), 
                ~replace(., is.na(.), ""))) |> 
  
  ### grouping to create the order of the sense
  group_by(entry_id, lex_entry, homonym_id, ps_key, etym, variant) |> 
  
  ### create the marker of the sense number
  mutate(sense_id = paste("sn_", sense_order, sep = "")) |> 
  
  ### regrouping of the key groups
  group_by(entry_id, lex_entry, homonym_id, etym) |> 
  select(-morph_gloss_en, -morph_gloss_id, -sense_gram, -ps_key) |> 
  
  ### generate the \sn column
  pivot_wider(names_from = sense_id, 
              values_from = sense_order,
              values_fill = "") |> 
  
  ### split the multiple variants/allomorphs
  mutate(va = str_extract_all(variant, "[^ ;]+")) |> 
  
  ### reorganising the columns
  select(entry_id, lex_entry, homonym_id, etym, va, everything(), -variant) |> 
  
  ### generate the \va column
  unnest_wider(col = va, names_sep = "_")
sn

## create the \ge column (English gloss marker)
ge <- df_lex_and_text |> 
  
  ### replace NAs with empty character
  mutate(across(where(is.character), ~replace(., is.na(.), ""))) |> 
  
  ### grouping to create the order of the English gloss
  ### based on the sense_order
  group_by(entry_id, lex_entry, homonym_id, ps_key, etym) |> 
  
  ### create the marker of the English gloss
  mutate(ge_id = paste("ge_", sense_order, sep = "")) |> 
  
  ### regrouping of the key groups
  group_by(entry_id, lex_entry, homonym_id, etym) |> 
  
  ### exclude the irrelevant columns before pivot_wider()
  select(-morph_gloss_id, -sense_order, -sense_gram, -ps_key, -variant) |> 
  
  ### generate the columns of the English gloss
  pivot_wider(names_from = ge_id, 
              values_from = morph_gloss_en,
              values_fill = "")
ge

## create the \gn column (Indonesian [i.e., national lang.] gloss marker)
gn <- df_lex_and_text |> 
  
  ### replace NAs with empty character
  mutate(across(where(is.character), ~replace(., is.na(.), ""))) |> 
  
  ### grouping to create the order of the Indonesian/national lang. gloss
  group_by(entry_id, lex_entry, homonym_id, ps_key, etym) |> 
  
  ### create the marker of the Indonesian/national lang. gloss
  mutate(gn_id = paste("gn_", sense_order, sep = "")) |>
  
  ### regrouping of the key groups
  group_by(entry_id, lex_entry, homonym_id, etym) |> 
  
  ### exclude the irrelevant columns before pivot_wider()
  select(-morph_gloss_en, -sense_order, -sense_gram, -ps_key, -variant) |> 
  
  ### generate the columns of the Indonesian/national lang. gloss
  pivot_wider(names_from = gn_id, 
              values_from = morph_gloss_id,
              values_fill = "")
gn

## create the \ps column (Part-of-Speech marker)
ps <- df_lex_and_text |> 
  
  ### replace NAs with empty character
  mutate(across(where(is.character), ~replace(., is.na(.), ""))) |> 
  
  ### grouping to create the order of the POS, corresponding to the sense
  group_by(entry_id, lex_entry, homonym_id, ps_key, etym) |> 
  
  ### create the marker for the POS
  mutate(ps_id = paste("ps_", sense_order, sep = "")) |> 
  
  ### regrouping of the key groups
  group_by(entry_id, lex_entry, homonym_id, etym) |> 
  
  ### exclude the irrelevant columns before pivot_wider()
  select(-morph_gloss_en, -sense_order, -morph_gloss_id, -ps_key, -variant) |> 
  
  ### generate the columns of the POS
  pivot_wider(names_from = ps_id, 
              values_from = sense_gram,
              values_fill = "")
ps

sn1 <- sn |> # join the \sn (Sense number) and \va (Allomorph) tibble with ...
  left_join(ge) |> # the \ge (English gloss) tibble, and ...
  left_join(ps) |> # the \ps (Part-of-Speech) tibble, and ...
  left_join(gn) |> # the \gn (National [IDN] gloss) tibble.
  select(entry_id, 
         lx = lex_entry, 
         et = etym,
         hm = homonym_id,
         matches("va_"),
         sn_0,
         ps_0,
         ge_0,
         gn_0,
         sn_1,
         ps_1,
         ge_1,
         gn_1,
         sn_2,
         ps_2,
         ge_2,
         gn_2)

# 2. Processing sub-entries =======
## Note: gathering the forms of the sub-entries
## sub-entries are only words that are NOT the same with the root/lex.entry
## sub-entries line must be unique for the following variables 
## to be used in group_by():
## - lex.entry
## - homonym_id
## - morph_type
## - morph_gloss_en
## - morph_gloss_id
## - sense_gram
## - morph_gram
## - etym of the root, and 
## - variant (before parsed)
# tes_lex <- c("pe", "abė", "bak", "kahinu", "a",
#              "arkih", "anu̇k", "yũ'ũ",
#              "pakõ'õã'")

flex_lexicon2 <- lu_form_df |> 
  select(entry_id,
         lex_entry = form,
         homonym_id = order,
         morph_type = trait,
         sense_gram,
         sense_order,
         morph_gloss_en = sense_gloss_en, 
         morph_gloss_id = sense_gloss_idn,
         etym,
         variant = variant_form) |> 
  mutate(sense_order = replace(sense_order, is.na(sense_order), 0),
         homonym_id = replace(homonym_id, is.na(homonym_id), ""),
         across(where(is.character),
                ~replace(., is.na(.), ""))) |> 
  distinct()
  # We need to run distinct() above...
  # because there are entries that are multiplied due to the presence of
  # sentence example.
flex_lexicon2
  # After running distinct, the entries are 1559, (it is 1540 after removing Person Names entries)
  # which is the number in the November 2023 FLEx post-fieldwork

flex_from_text2 <- flex_from_text |> 
  select(lex_entry, 
         homonym_id,
         morph,
         morph_gloss_en,
         morph_gloss_id,
         morph_gram,
         word,
         # w_pos = eno_word_pos,
         w_gloss_id = eno_word_gloss_id,
         w_gloss_en = eno_word_gloss_en) |> 
  distinct() |> 
  mutate(homonym_id = replace(homonym_id, homonym_id == "", ""),
         across(where(is.character), ~replace(., is.na(.), ""))) |> 
  distinct()
flex_from_text2

se <- flex_lexicon2 |> 
  left_join(flex_from_text2) |> 
  mutate(word = if_else(str_detect(sense_gram, "^Proper\\s+Noun$",
                                   negate = TRUE) &
                          str_detect(word, "^[A-Z]", negate = TRUE) &
                          !word %in% person_entries, 
                        tolower(word),
                        word),
         across(where(is.character), ~replace(., is.na(.), "")),
         lx_equals_w = lex_entry == word,
         lx_equals_morph = lex_entry == morph,
         w_equals_morph = word == morph) |> 
  distinct()
## Joining with `by = join_by(lex_entry, homonym_id, morph_gloss_en, morph_gloss_id)`
  
## for the sub-entries, we want to capture 
## - (i) word form of a lex_entry 
## - (ii) word class of the word form (from the w_pos)
## - (iii) gloss/sense of the word form
## - (iv) example with the word form
## but IF the lex_entry is the same with the word form,
## the example sentence should be under the lexical_entry/main entry.
## IF the lex_entry is not the same with the word,
## the word becomes sub-entries and the example is part of the sub-entries
se1 <- se |> 
  
  # select(-w_gloss_id, -w_gloss_en, -morph) |>
  select(-morph_gloss_en, -morph_gloss_id, -w_gloss_id, 
         -w_gloss_en, -morph_gram, -morph) |> 
  
  # group_by(lex_entry, homonym_id, morph_type, 
  #          sense_gram, morph_gram, morph_gloss_en, 
  #          morph_gloss_id, etym, variant, sense_order) |> 
  
  group_by(entry_id, lex_entry, homonym_id, morph_type) |> 
  # the above is the correct group_by variables because the total unique entry
  # is still 1524, the same as unique entry in the FLEx Lexicon (October 2023)
  
  arrange(entry_id, lex_entry, homonym_id, sense_order, sense_gram, word) |>
  
  # Filter out only observation where lex_entry is not the same as the word
  # because that means the lex_entry is component of a more complex form.
  # IF the lex_entry is equal to the word, then no need to have sub-entry
  # of complex form for the lex_entry.
  filter(!lx_equals_w) |>
  
  # We also filtered out non-empty word column
  filter(word != "") |> 
  
  distinct() |> 
  
  # remove prefix and suffix morpheme to reduce sub-entries
  # and `ho` auxiliary
  filter(!morph_type %in% c("prefix", "suffix") &
           lex_entry != "ho") |> 
  
  select(-lx_equals_w, -lx_equals_morph, -w_equals_morph, -sense_gram) |>
  distinct() |> 
  mutate(se_id = paste("se_", row_number(word)-1, sep = ""),
         se_id = str_replace_all(se_id, "(?<=_)(\\d)$", "0\\1")) |> 
  arrange(entry_id, lex_entry, homonym_id, sense_order, se_id) |> 
  select(-sense_order) |> 
  distinct()

# widen the sub-entry column
se2 <- se1 |> 
  pivot_wider(names_from = se_id, 
              values_from = word, 
              values_fill = "") |> 
  rename(lx = lex_entry,
         hm = homonym_id,
         et = etym)
se2

# join the `sn1` and `se2` table
sn2 <- sn1 |> 
  left_join(se2 |> 
              ungroup() |> 
              select(-variant,
                     -morph_type) |> 
              group_by(entry_id, lx, hm))

# check the number of word form a group has (i.e,
# calculating sub-entry per lex-entry)
se_summary <- se1 |> 
  summarise(n_word = n_distinct(word))
se_summary
se_summary |> 
  # check the entry with over 100 sub-entries
  filter(n_word > 100) |>
  count(morph_type, sort = TRUE)


## turn to long table to be represented as SFM =====
sn3 <- sn2 |> 
  mutate(lx_key = lx) |> 
  pivot_longer(cols = -lx_key, 
               names_to = "marker", 
               values_to = "vals") |> 
  mutate(vals = replace(vals, is.na(vals), ""))
sn3
sn4 <- sn3 |> 
  
  # delete empty marker
  filter(vals != "") |>
  
  # remove entry id
  filter(marker != "entry_id") |> 
  
  # create a new marker without number
  mutate(marker_deleted = str_replace_all(marker, "_\\d+$", "")) |> 
  
  select(lx_key, marker_deleted, vals) |> 
  
  # create the SFM structure
  mutate(sfm = paste('\\', marker_deleted, " ", vals, sep = "")) |> 
  
  # add new line marker before the \lx marker
  mutate(sfm = if_else(marker_deleted == "lx",
                       paste("\n", sfm, sep = ""),
                       sfm))
sn4 |> 
  print(n = 50)

## save into SFM plain text ======
### Note: saving the SFM file of roots and their sub-entries of complex forms
# write_lines(pull(sn4, sfm), "FLEX-lexicon-with-sub-entries.db")


# 3. Finding random sample of sentences for lex.entry/ROOT ===========
# The root is also formally the same with the `word` column

## we want a sentence that matches between:
### - the lex.entry (IF the lex.entry and word are the same)
###   - the lex.entry form, its gloss/sense order, homonym id, sense_gram
### - the lex.entry subset (i.e., complex form) that matches between:
###   - its sense/gloss.order, homonym id, sense_gram, and COMPLEX FORM of the lex.entry

### add \xv (ENO) for the lex.entry (stem) that is the same with the word
### add \xn (IDN) for the lex.entry (stem) that is the same with the word
### add \xe (ENG) for the lex.entry (stem) that is the same with the word

## Then, there has to be a separate data frame for example sentences of the sub-entries,
## because in the SFM, this data will be appended after all lexical entry.
## There has to be a marker of main entry (i.e., lex.entry crossref.) in this
## subset of complex form example sentences.

## Another set of data frame is example sentences of the headword/lex_entry
## this needs to be combined with the `sn2` and `flex_lexicon` data to be made
## into SFM.

## Each example of the root (and its translations) for a given sense should
## appear after the \ge and \gn marker


# Retrieving non-NAs `lex_entry` and `word`
flex_from_text <- eno_morph_split1 |> 
  
  ## exclude NAs lex_entry
  filter(!is.na(lex_entry)) |>
  
  ## exclude NAs word
  filter(!is.na(word)) |> 
  
  ## exclude the word containing xxx (unknown strings)
  filter(str_detect(word, "\\b(?i)x{2,}", negate = TRUE)) |> 
  
  mutate(across(where(is.character), ~ replace(., is.na(.), ""))) |> 
  mutate(word_equal_lexentry = if_else(word == lex_entry, TRUE, FALSE),
         morph_equal_lexentry = if_else(morph == lex_entry, TRUE, FALSE)) |> 
  select(-matches("_n$"))

flex_lexicon2 <- lu_form_df |> 
  select(entry_id,
         sense_id,
         lex_entry = form,
         homonym_id = order,
         morph_type = trait,
         sense_gram,
         sense_order,
         morph_gloss_en = sense_gloss_en, 
         morph_gloss_id = sense_gloss_idn,
         etym,
         variant = variant_form) |> 
  mutate(sense_order = replace(sense_order, is.na(sense_order), 0),
         homonym_id = replace(homonym_id, is.na(homonym_id), ""),
         across(where(is.character), ~replace(., is.na(.), ""))) |> 
  distinct()

sent <- flex_lexicon2 |> 
  left_join(flex_from_text |> 
              # remove phrase id/line to maintain uniqueness of the example text
              # because there can be repetition across distinct phrase id
              select(-phrase_id, -phrase_line)) |>
  mutate(word = if_else(str_detect(sense_gram, "^Proper\\s+Noun$",
                                   negate = TRUE), 
                        tolower(word),
                        word),
         across(where(is.character), ~replace(., is.na(.), "")),
         across(where(is.character), ~str_trim(., side = "both")),
         across(matches("^eno_phrase"), ~str_replace_all(., "\\bselus\\b", "Selus")),
         across(matches("^eno_phrase"), ~str_replace_all(., "\\bmaria\\b", "Maria")),
         across(matches("^eno_phrase"), ~str_replace_all(., "\\bali\\b", "Ali")),
         
         # change the first letter to upper case.
         across(matches("^eno_phrase"), ~gsub("^(.)", "\\U\\1", x = ., perl = TRUE)),
         lx_equals_w = lex_entry == word,
         lx_equals_morph = lex_entry == morph,
         w_equals_morph = word == morph) |> 
  distinct()
## Joining with `by = join_by(lex_entry, homonym_id, morph_type, morph_gloss_en, morph_gloss_id)`

# get example for lex_entry that is the same with the word
sent_root <- sent |> 
  filter(lx_equals_w, 
         # eno_phrase_gloss_id != "",
         # eno_phrase_gloss_eng != "",
         lex_entry != eno_phrase) |> 
  select(-lx_equals_morph, -w_equals_morph) |> 
  group_by(entry_id, lex_entry, homonym_id, 
           sense_order, sense_gram) |> 
  # filter(lex_entry %in% "pakõ'õã'") |>
  # filter(lex_entry %in% tes_lex) |> 
  
  ## filtering all questionable sentences (marked by "*" or "#")
  filter(str_detect(eno_phrase, "^[*#]", negate = TRUE)) |>
  
  ## filtering sentences (and glosses) containing "xxx"
  filter(str_detect(eno_phrase, "\\b(?i)x{2,}", negate = TRUE)) |>
  filter(str_detect(eno_phrase_gloss_id, "\\b(?i)x{2,}", negate = TRUE)) |>
  filter(str_detect(eno_phrase_gloss_eng, "\\b(?i)x{2,}", negate = TRUE)) |> 
  
  distinct()

sent_root_sample <- sent_root |> 
  slice_sample(n = 3)

## double-check that no `...phrase...` column(s) has "xxx" in it.
sent_root_sample |> 
  filter(if_any(matches("eno_phrase"), ~str_detect(., "\\b(?i)x{2,}")))

# retrieve the sentences for each language and then pivot_wider() for each lang.
## Enggano
interim_eno <- sent_root_sample |> 
  # filter(lex_entry %in% tes_lex) |> 
  mutate(sent_eno_num = paste("sn", sense_order, "_xv", 
                              row_number(eno_phrase),
                         sep = ""),
         sent_idn_num = str_replace(sent_eno_num, "xv", "xn"),
         sent_eng_num = str_replace(sent_eno_num, "xv", "xe")) |> 
  select(lex_entry, homonym_id, sense_order, sense_gram, 
         morph_gloss_en, morph_gloss_id, etym, sent_eno_num, eno_phrase) |> 
  arrange(entry_id, lex_entry, homonym_id, sense_order, sent_eno_num) 

interim_eno_wide <- interim_eno |> 
  group_by(entry_id, lex_entry, homonym_id) |> 
  select(-sense_order, -morph_gloss_en, -morph_gloss_id, -sense_gram) |> 
  pivot_wider(names_from = sent_eno_num,
              values_from = eno_phrase,
              values_fill = "") |> 
  distinct()

### retrieve the source example reference
interim_eno_ex_reference <- sent_root_sample |> 
  mutate(sent_eno_num = paste("sn", sense_order, "_xv", 
                              row_number(eno_phrase),
                              sep = ""),
         sent_idn_num = str_replace(sent_eno_num, "xv", "xn"),
         sent_eng_num = str_replace(sent_eno_num, "xv", "xe"),
         ref_eno_num = str_replace(sent_eno_num, "xv", "rf")) |> 
  select(lex_entry, homonym_id, sense_order, sense_gram, 
         morph_gloss_en, morph_gloss_id, etym, ref_eno_num, text_title) |> 
  arrange(entry_id, lex_entry, homonym_id, sense_order, ref_eno_num) 

interim_eno_ex_reference_wide <- interim_eno_ex_reference |> 
  group_by(entry_id, lex_entry, homonym_id) |> 
  select(-sense_order, -morph_gloss_en, -morph_gloss_id, -sense_gram) |> 
  pivot_wider(names_from = ref_eno_num,
              values_from = text_title,
              values_fill = "") |> 
  distinct()

## Indonesian
interim_idn <- sent_root_sample |> 
  # filter(lex_entry %in% tes_lex) |> 
  mutate(sent_eno_num = paste("sn", sense_order, "_xv", 
                              row_number(eno_phrase),
                              sep = ""),
         sent_idn_num = str_replace(sent_eno_num, "xv", "xn"),
         sent_eng_num = str_replace(sent_eno_num, "xv", "xe")) |> 
  select(lex_entry, homonym_id, sense_order, sense_gram, 
         morph_gloss_en, morph_gloss_id, etym, sent_idn_num, eno_phrase_gloss_id) |> 
  arrange(entry_id, lex_entry, homonym_id, sense_order, sent_idn_num) 

interim_idn_wide <- interim_idn |> 
  group_by(entry_id, lex_entry, homonym_id) |> 
  select(-sense_order, -morph_gloss_en, -morph_gloss_id, -sense_gram) |> 
  pivot_wider(names_from = sent_idn_num,
              values_from = eno_phrase_gloss_id,
              values_fill = "") |> 
  distinct()

## English
interim_eng <- sent_root_sample |> 
  # filter(lex_entry %in% tes_lex) |> 
  mutate(sent_eno_num = paste("sn", sense_order, "_xv", 
                              row_number(eno_phrase),
                              sep = ""),
         sent_idn_num = str_replace(sent_eno_num, "xv", "xn"),
         sent_eng_num = str_replace(sent_eno_num, "xv", "xe")) |> 
  select(lex_entry, homonym_id, sense_order, sense_gram, 
         morph_gloss_en, morph_gloss_id, etym, sent_eng_num, eno_phrase_gloss_eng) |> 
  arrange(entry_id, lex_entry, homonym_id, sense_order, sent_eng_num) 

interim_eng_wide <- interim_eng |> 
  group_by(entry_id, lex_entry, homonym_id) |> 
  select(-sense_order, -morph_gloss_en, -morph_gloss_id, -sense_gram) |> 
  pivot_wider(names_from = sent_eng_num,
              values_from = eno_phrase_gloss_eng,
              values_fill = "")

# combine the examples for the roots from the three languages
## sent_root1 contains the root/lex_entry forms with their examples
sent_root1 <- interim_eno_wide |> 
  left_join(interim_idn_wide) |> 
  left_join(interim_eng_wide) |>
  left_join(interim_eno_ex_reference_wide) |> 
  ungroup() |> 
  rename(lx = lex_entry,
         hm = homonym_id,
         et = etym) |> 
  distinct() |> 
  group_by(entry_id, lx, hm, et) |> 
  select(entry_id,
         lx,
         hm,
         et,
         
         # Sense 1
         ## example 1 for sense 1
         matches("^sn0[^0-9]+1$"),
         
         ## example 2 for sense 1
         matches("^sn0[^0-9]+2$"),
         
         ## example 3 for sense 1
         matches("^sn0[^0-9]+3$"),
         
         
         # Sense 2 (if any)
         ## example 1 for sense 2
         matches("^sn1[^0-9]+1$"),
         
         ## example 2 for sense 2
         matches("^sn1[^0-9]+2$"),
         
         ## example 3 for sense 2
         matches("^sn1[^0-9]+3$"),
         
         
         # Sense 3 (if any)
         ## example 1 for sense 3
         matches("^sn2[^0-9]+1$"),
         
         ## example 2 for sense 3
         matches("^sn2[^0-9]+2$"),
         
         ## example 3 for sense 3
         matches("^sn2[^0-9]+3$")) |> 
  
  distinct()

# combine the examples for the roots with `sn2` the three languages
sn2_1 <- sn2 |> 
  left_join(sent_root1)

## re-arrange the column to match SFM specification
sn2_2 <- sn2_1 |> 
  select(matches("va"), 
         matches("(sn|ps|ge|gn)_0"), 
         matches("^sn0.+[123]$"),  
         
         matches("(sn|ps|ge|gn)_1"), 
         matches("^sn1.+[123]$"), 
         
         matches("(sn|ps|ge|gn)_2"), 
         matches("^sn2.+[123]$"), 
         matches("se"))

## turn into a long table for SFM format
sn2_3 <- sn2_2 |> 
  mutate(lx_key = lx) |> 
  pivot_longer(cols = -c(lx_key, entry_id), 
               names_to = "marker", 
               values_to = "vals") |> 
  mutate(vals = replace(vals, is.na(vals), ""))

sn2_3

sn2_4 <- sn2_3 |> 
  
  # delete empty marker
  filter(vals != "") |>
  
  # create a new marker without number
  mutate(marker_deleted = str_replace_all(marker, "_\\d+$", ""),
         marker_deleted = str_replace_all(marker_deleted,
                                          "(^sn\\d+_|\\d+$)",
                                          "")) |> 
  
  ungroup() |> 
  select(lx_key, marker_deleted, vals) |> 
  
  # create the SFM structure
  mutate(sfm = paste('\\', marker_deleted, " ", vals, sep = "")) |> 
  
  # add new line marker before the \lx marker
  mutate(sfm = if_else(marker_deleted == "lx",
                       paste("\n", sfm, sep = ""),
                       sfm))

## save into SFM plain text ======
### Note: saving the SFM file of roots and their examples and sub-entries
# write_lines(pull(sn2_4, sfm), "FLEX-lexicon-with-sub-entries-and-root-examples.db")

# IMPORTANT: Try to incorporate the source of the examples! (DONE!)
## the source pattern is: title_segment/line number.
## line number for each phrase can be created on the fly by
## grouping the interlinear dataframe by text_title then
## use row_number() to create the line number

# 4. Finding random sample of sentences for SUB-ENTRIES =============
# get example for lex_entry that is NOT the same with the word and morph forms
## Now we need to re-create the way to create the sub-entries BUT with the
## following additional marker for the word:
## - the sense number for each word (how many Indonesian glosses a word has?)
## - the POS of each sense for each word
### - the eno_word_gloss_id and eno_word_gloss_en for each word and sense
### - the examples for each sense

## Then, the newly created sub-entries with the senses and examples will be
## embedded into the `sn2_2` where it holds the examples of the root!
## To achieve this, we need to include the entry_id, homonym_id, etym, and lex_entry
## columns to be matched with the wide table `sn2_2`
sent_word <- sent |> 
  filter(# retrieve only entries where the root is NOT THE SAME WITH THE WORD
         !lx_equals_w, 
         
         # retrieve example that has Indonesian translation
         # eno_phrase_gloss_id != "",
         
         # ensure the word/subentry is NOT-EMPTY
         word != "") |> 
  
  # remove prefix and suffix morpheme to reduce sub-entries
  filter(!morph_type %in% c("prefix", "suffix") & lex_entry != "ho")
sent_word

## grouping to retrieve a random sample of three examples per sense and POS
sent_word_example_sample <- sent_word |> 
  select(entry_id, 
         lex_entry, 
         morph_type, 
         homonym_id, 
         etym, 
         word, 
         eno_word_gloss_id,
         eno_word_gloss_en, 
         eno_word_pos, 
         eno_phrase, 
         eno_phrase_gloss_id,
         eno_phrase_gloss_eng, 
         text_title) |> 
  distinct() |> 
  group_by(entry_id, 
           lex_entry, 
           morph_type, 
           homonym_id, 
           etym, 
           word, 
           eno_word_pos,
           eno_word_gloss_id) |> 
  
  ## filtering all questionable sentences (marked by "*" or "#")
  filter(str_detect(eno_phrase, "^[*#]", negate = TRUE)) |>
  
  ## filtering sentences (and glosses) containing "xxx"
  filter(str_detect(eno_phrase, "\\b(?i)x{2,}", negate = TRUE)) |>
  filter(str_detect(eno_phrase_gloss_id, "\\b(?i)x{2,}", negate = TRUE)) |>
  filter(str_detect(eno_phrase_gloss_eng, "\\b(?i)x{2,}", negate = TRUE)) |> 
  
  ## get a random sample of three sentences
  slice_sample(n = 3) |> 
  arrange(entry_id, lex_entry, word)

sent_word_example_sample

## double-check that no `...phrase...` column(s) has "xxx" in it.
sent_word_example_sample |> 
  filter(if_any(matches("eno_phrase"), ~str_detect(., "\\b(?i)x{2,}")))

## create the sense number for each word
sent_word_sense_grouping <- sent_word_example_sample |> 
  group_by(entry_id, 
           lex_entry, 
           morph_type, 
           homonym_id, 
           etym, 
           word) |> 
  select(-matches("phrase|refs$|text_title$")) |> 
  distinct() |> 
  mutate(sn_word = paste("snw_", row_number(eno_word_gloss_id)-1, sep = ""),
         sn_word_id = str_replace_all(sn_word, "snw_", ""),
         ps_word = str_replace_all(sn_word, "^sn", "ps"),
         ps_word_id = str_replace_all(ps_word, "^psw_", "")) |> 
  arrange(lex_entry, sn_word)
sent_word_sense_grouping

### re-join the sample examples data set
sent_word_example_sample_sn <- sent_word_example_sample |> 
  left_join(sent_word_sense_grouping)
sent_word_example_sample_sn

nrow(sent_word_example_sample_sn) == nrow(sent_word_example_sample)

## create the IDs for the examples (their glosses, and references)
sent_word_example_sample_sn_ex <- sent_word_example_sample_sn |> 
  mutate(xv_word = paste("xvw_", row_number(eno_phrase)-1, sep = ""),
         xn_word = str_replace_all(xv_word, "^xv", "xn"),
         xe_word = str_replace_all(xv_word, "^xv", "xe"),
         rf_word = str_replace_all(xv_word, "^xv", "rf")) |> 
  arrange(lex_entry, sn_word, xv_word)
sent_word_example_sample_sn_ex
nrow(sent_word_example_sample_sn_ex)

## create the IDs for the sub-entry forms of a lex_entry
sent_word_example_sample_sn_ex_se <-  sent_word_example_sample_sn_ex |> 
  group_by(entry_id, 
           lex_entry, 
           morph_type, 
           homonym_id, 
           etym) |> 
  select(-matches("phrase|refs|text_title|^rf|ps_|sn_|x._|gloss_(id|en)|word_pos")) |> 
  distinct() |> 
  mutate(se_w = paste("sew_", row_number(word)-1, sep = ""),
         se_w = str_replace_all(se_w, "(?<=_)(\\d)$", "0\\1")) |> 
  arrange(lex_entry, se_w)
sent_word_example_sample_sn_ex_se
nrow(sent_word_example_sample_sn_ex_se)

### re-joining with the previous tibbles
sent_word_example_sample_sn_ex_se1 <- sent_word_example_sample_sn_ex |> 
  left_join(sent_word_example_sample_sn_ex_se)
sent_word_example_sample_sn_ex_se1
nrow(sent_word_example_sample_sn_ex_se1) == nrow(sent_word_example_sample)

# pivot wider for the sub-entry forms
interim_word_subentry <- sent_word_example_sample_sn_ex_se1 |> 
  # mutate(word_key = word) |> 
  # group_by(entry_id, lex_entry, homonym_id, morph_type, etym, word_key) |> 
  group_by(entry_id, lex_entry, homonym_id, morph_type, etym) |> 
  select(word, se_w) |> 
  distinct() |> 
  arrange(entry_id, lex_entry, homonym_id) |> 
  pivot_wider(names_from = se_w, values_from = word, values_fill = "")
dim(interim_word_subentry)

# pivot wider for the POS of the word
interim_word_pos <- sent_word_example_sample_sn_ex_se1 |>
  # mutate(word_key = word) |>
  # group_by(entry_id, lex_entry, homonym_id, morph_type, etym, word_key) |>
  mutate(se_ps_num = paste(se_w, "__", ps_word, sep = "")) |> 
  group_by(entry_id, lex_entry, homonym_id, morph_type, etym) |>
  select(eno_word_pos, se_ps_num) |>
  distinct() |>
  arrange(entry_id, lex_entry, homonym_id) |>
  pivot_wider(names_from = se_ps_num, values_from = eno_word_pos, values_fill = "")
dim(interim_word_pos)

# pivot wider for the SENSE ID of the word
interim_word_sense_id <- sent_word_example_sample_sn_ex_se1 |>
  mutate(se_sn_num = paste(se_w, "__", sn_word, sep = "")) |> 
  group_by(entry_id, lex_entry, homonym_id, morph_type, etym) |>
  select(sn_word_id, se_sn_num) |>
  distinct() |>
  arrange(entry_id, lex_entry, homonym_id) |>
  pivot_wider(names_from = se_sn_num, values_from = sn_word_id, values_fill = "")
dim(interim_word_sense_id)

# pivot wider for the Indonesian gloss of the word
interim_word_idn_gloss <- sent_word_example_sample_sn_ex_se1 |> 
  mutate(se_sn_num = paste(se_w, "__", sn_word, "__gn", sep = "")) |> 
  group_by(entry_id, lex_entry, homonym_id, morph_type, etym) |> 
  select(eno_word_gloss_id, se_sn_num) |> 
  distinct() |> 
  arrange(entry_id, lex_entry, homonym_id) |> 
  pivot_wider(names_from = se_sn_num, 
              values_from = eno_word_gloss_id, 
              values_fill = "")
dim(interim_word_idn_gloss)

# pivot wider for the English gloss of the word
interim_word_eng_gloss <- sent_word_example_sample_sn_ex_se1 |> 
  mutate(se_sn_num = paste(se_w, "__", sn_word, "__ge", sep = "")) |> 
  group_by(entry_id, lex_entry, homonym_id, morph_type, etym) |> 
  select(eno_word_gloss_en, se_sn_num) |> 
  distinct() |> 
  arrange(entry_id, lex_entry, homonym_id) |> 
  pivot_wider(names_from = se_sn_num, 
              values_from = eno_word_gloss_en, 
              values_fill = "")
dim(interim_word_eng_gloss)

# EXAMPLE SENTENCES (THEIR GLOSSES, and REFERENCES)
## pivot wider for the Enggano sentences
interim_word_eno_example <- sent_word_example_sample_sn_ex_se1 |> 
  mutate(sent_eno_num = paste(se_w, "__", sn_word, "__", xv_word, sep = "")) |> 
  group_by(entry_id, lex_entry, homonym_id, morph_type, etym) |> 
  select(sent_eno_num, eno_phrase) |> 
  distinct() |> 
  arrange(entry_id, lex_entry, homonym_id) |> 
  pivot_wider(names_from = sent_eno_num, 
              values_from = eno_phrase, 
              values_fill = "")
dim(interim_word_eno_example)

## pivot wider for the Enggano sentence references
interim_word_eno_ex_sources <- sent_word_example_sample_sn_ex_se1 |> 
  mutate(sent_eno_ref_num = paste(se_w, "__", sn_word, "__", rf_word, sep = "")) |> 
  group_by(entry_id, lex_entry, homonym_id, morph_type, etym) |> 
  select(sent_eno_ref_num, text_title) |> 
  distinct() |> 
  arrange(entry_id, lex_entry, homonym_id) |> 
  pivot_wider(names_from = sent_eno_ref_num,
              values_from = text_title,
              values_fill = "")
dim(interim_word_eno_ex_sources)

## pivot wider for the Indonesian sentences
interim_word_idn_example <- sent_word_example_sample_sn_ex_se1 |> 
  mutate(sent_idn_num = paste(se_w, "__", sn_word, "__", xn_word, sep = "")) |> 
  group_by(entry_id, lex_entry, homonym_id, morph_type, etym) |> 
  select(sent_idn_num, eno_phrase_gloss_id) |> 
  distinct() |> 
  arrange(entry_id, lex_entry, homonym_id) |> 
  pivot_wider(names_from = sent_idn_num, 
              values_from = eno_phrase_gloss_id, 
              values_fill = "")
dim(interim_word_idn_example)

# pivot wider for the English sentences
interim_word_eng_example <- sent_word_example_sample_sn_ex_se1 |> 
  mutate(sent_eng_num = paste(se_w, "__", sn_word, "__", xe_word, sep = "")) |> 
  group_by(entry_id, lex_entry, homonym_id, morph_type, etym) |> 
  select(sent_eng_num, eno_phrase_gloss_eng) |> 
  distinct() |> 
  arrange(entry_id, lex_entry, homonym_id) |> 
  pivot_wider(names_from = sent_eng_num, 
              values_from = eno_phrase_gloss_eng, 
              values_fill = "")
dim(interim_word_eng_example)


word_subentry_examples <- interim_word_subentry |> 
  left_join(interim_word_sense_id) |> 
  left_join(interim_word_pos) |> 
  left_join(interim_word_idn_gloss) |> 
  left_join(interim_word_eng_gloss) |> 
  left_join(interim_word_eno_example) |> 
  left_join(interim_word_idn_example) |> 
  left_join(interim_word_eng_example) |> 
  left_join(interim_word_eno_ex_sources) |> 
  rename(lx = lex_entry,
         hm = homonym_id,
         et = etym) |> 
  select(-morph_type)
# Joining with `by = join_by(entry_id, lex_entry, homonym_id, morph_type, etym)`

dim(word_subentry_examples)

subentries_colnames <- str_subset(colnames(word_subentry_examples),
                                  "^sew_\\d\\d$")
subentries_colnames_length <- length(subentries_colnames)
subentries_colnames_id <- str_extract(subentries_colnames, "(?<=^sew_)\\d+")
subentries_main_df <- select(word_subentry_examples, -matches("^sew_"))

sense_indices <- word_subentry_examples |> 
  ungroup() |> 
  select(matches("snw_\\d+$")) |> 
  colnames() |> 
  str_extract("(?<=snw_)\\d+$") |> 
  unique()

pos_indices <- word_subentry_examples |> 
  ungroup() |> 
  select(matches("psw_\\d+$")) |> 
  colnames() |> 
  str_extract("(?<=psw_)\\d+$") |> 
  unique()

xv_indices <- word_subentry_examples |> 
  ungroup() |> 
  select(matches("xvw_\\d+$")) |> 
  colnames() |> 
  str_extract("(?<=xvw_)\\d+$") |> 
  unique()

xn_indices <- word_subentry_examples |> 
  ungroup() |> 
  select(matches("xnw_\\d+$")) |> 
  colnames() |> 
  str_extract("(?<=xnw_)\\d+$") |> 
  unique()

xe_indices <- word_subentry_examples |> 
  ungroup() |> 
  select(matches("xew_\\d+$")) |> 
  colnames() |> 
  str_extract("(?<=xew_)\\d+$") |> 
  unique()

rf_indices <- word_subentry_examples |> 
  ungroup() |> 
  select(matches("rfw_\\d+$")) |> 
  colnames() |> 
  str_extract("(?<=rfw_)\\d+$") |> 
  unique()



# > word_subentry_examples |> colnames() |> str_subset("^sew") |> str_extract("snw_\\d+") |> unique()
# [1] NA      "snw_0" "snw_1" "snw_2" "snw_3"
# > word_subentry_examples |> colnames() |> str_subset("^sew") |> str_extract("snw_\\d+__gn$") |> unique()
# [1] NA          "snw_0__gn" "snw_1__gn" "snw_2__gn" "snw_3__gn"
# > word_subentry_examples |> colnames() |> str_subset("^sew") |> str_extract("snw_\\d+__ge$") |> unique()
# [1] NA          "snw_0__ge" "snw_1__ge" "snw_2__ge" "snw_3__ge"
# > word_subentry_examples |> colnames() |> str_subset("^sew") |> str_extract("psw_\\d+") |> unique()
# [1] NA      "psw_0" "psw_1" "psw_2" "psw_3"
# > word_subentry_examples |> colnames() |> str_subset("^sew") |> str_extract("xvw_\\d+") |> unique()
# [1] NA      "xvw_0" "xvw_1" "xvw_2"
# > word_subentry_examples |> colnames() |> str_subset("^sew") |> str_extract("xnw_\\d+") |> unique()
# [1] NA      "xnw_0" "xnw_1" "xnw_2"
# > word_subentry_examples |> colnames() |> str_subset("^sew") |> str_extract("xew_\\d+") |> unique()
# [1] NA      "xew_0" "xew_1" "xew_2"
# > word_subentry_examples |> colnames() |> str_subset("^sew") |> str_extract("rfw_\\d+") |> unique()
# [1] NA      "rfw_0" "rfw_1" "rfw_2"

# > word_subentry_examples |> select(matches("^sew")) |> colnames() |> str_replace_all("\\d+", "") |> unique()
# Adding missing grouping variables: `entry_id`, `lx`, `hm`, `morph_type`, `et`
# [1] "entry_id"         "lx"               "hm"               "morph_type"       "et"              
# [6] "sew_"             "sew___snw_"       "sew___psw_"       "sew___snw___gn"   "sew___snw___ge"  
# [11] "sew___snw___xvw_" "sew___snw___xnw_" "sew___snw___xew_" "sew___snw___rfw_"

for (se_id in seq_along(subentries_colnames_id)) {
  
  colmatch_rgx <- paste("^sew_", subentries_colnames_id[se_id], sep = "")
  
  interim_df <- select(word_subentry_examples, matches(colmatch_rgx))
  
  interim_df1 <- interim_df |> 
    select(matches(paste(colmatch_rgx, "$", sep = "")),
           matches("snw_0$"),
           matches("psw_0$"),
           matches("snw_0__gn$"),
           matches("snw_0__ge$"),
           matches("snw_0__xvw_0$"),
           matches("snw_0__xnw_0$"),
           matches("snw_0__xew_0$"),
           matches("snw_0__rfw_0$"),
           matches("snw_0__xvw_1$"),
           matches("snw_0__xnw_1$"),
           matches("snw_0__xew_1$"),
           matches("snw_0__rfw_1$"),
           matches("snw_0__xvw_2$"),
           matches("snw_0__xnw_2$"),
           matches("snw_0__xew_2$"),
           matches("snw_0__rfw_2$"),
           
           matches("snw_1$"),
           matches("psw_1$"),
           matches("snw_1__gn$"),
           matches("snw_1__ge$"),
           matches("snw_1__xvw_0$"),
           matches("snw_1__xnw_0$"),
           matches("snw_1__xew_0$"),
           matches("snw_1__rfw_0$"),
           matches("snw_1__xvw_1$"),
           matches("snw_1__xnw_1$"),
           matches("snw_1__xew_1$"),
           matches("snw_1__rfw_1$"),
           matches("snw_1__xvw_2$"),
           matches("snw_1__xnw_2$"),
           matches("snw_1__xew_2$"),
           matches("snw_1__rfw_2$"),
           
           matches("snw_2$"),
           matches("psw_2$"),
           matches("snw_2__gn$"),
           matches("snw_2__ge$"),
           matches("snw_2__xvw_0$"),
           matches("snw_2__xnw_0$"),
           matches("snw_2__xew_0$"),
           matches("snw_2__rfw_0$"),
           matches("snw_2__xvw_1$"),
           matches("snw_2__xnw_1$"),
           matches("snw_2__xew_1$"),
           matches("snw_2__rfw_1$"),
           matches("snw_2__xvw_2$"),
           matches("snw_2__xnw_2$"),
           matches("snw_2__xew_2$"),
           matches("snw_2__rfw_2$"),
           
           matches("snw_3$"),
           matches("psw_3$"),
           matches("snw_3__gn$"),
           matches("snw_3__ge$"),
           matches("snw_3__xvw_0$"),
           matches("snw_3__xnw_0$"),
           matches("snw_3__xew_0$"),
           matches("snw_3__rfw_0$"),
           matches("snw_3__xvw_1$"),
           matches("snw_3__xnw_1$"),
           matches("snw_3__xew_1$"),
           matches("snw_3__rfw_1$"),
           matches("snw_3__xvw_2$"),
           matches("snw_3__xnw_2$"),
           matches("snw_3__xew_2$"),
           matches("snw_3__rfw_2$")
    )
  
  subentries_main_df <- left_join(subentries_main_df, interim_df1)
  
  cat(se_id, sep = "\n")
  
}

word_subentry_examples2 <- sn2_2 |> 
  select(-matches("^se_")) |> 
  left_join(subentries_main_df)

## turn into a long table for SFM format
word_subentry_examples3 <- word_subentry_examples2 |> 
  
  ## filter name of person
  filter(!lx %in% person_entries) |> 
  
  mutate(lx_key = lx) |> 
  pivot_longer(cols = -c(lx_key, entry_id), 
               names_to = "marker", 
               values_to = "vals") |> 
  mutate(vals = replace(vals, is.na(vals), "")) |> 
  filter(marker != "morph_type")
word_subentry_examples3

word_subentry_examples4 <- word_subentry_examples3 |> 

  # editing and addition
  mutate(vals = replace(vals, 
                        lx_key == "ahao" & marker == "gn_1", 
                        "melarang")) |> 
  
  # delete empty marker
  filter(vals != "") |>
  
  # edit the vals for the part of speech
  # mutate(vals = replace(vals, vals %in% c("adj"), "Adjective"),
  #        vals = replace(vals, vals %in% c("adv"), "Adverb"),
  #        vals = replace(vals, vals %in% c("aux"), "Auxiliary"),
  #        vals = replace(vals, vals %in% c("clf"), "Classifier"),
  #        vals = replace(vals, vals %in% c("coordconn"), "Coordinating connective"),
  #        vals = replace(vals, vals %in% c("dem"), "Demonstrative"),
  #        vals = replace(vals, vals %in% c("interj"), "Interjection"),
  #        vals = replace(vals, vals %in% c("interog", "interrog"), "Interrogative pro-form"),
  #        vals = replace(vals, vals %in% c("n"), "Noun"),
  #        vals = replace(vals, vals %in% c("neg", "negation"), "Negation"),
  #        vals = replace(vals, vals %in% c("nprop"), "Proper Noun"),
  #        vals = replace(vals, vals %in% c("num"), "Numeral"),
  #        vals = replace(vals, vals %in% c("prep"), "Preposition"),
  #        vals = replace(vals, vals %in% c("pro-adv"), "Pro-adverb"),
  #        vals = replace(vals, vals %in% c("pro-form"), "Pro-form"),
  #        vals = replace(vals, vals %in% c("pro"), "Pronoun"),
  #        vals = replace(vals, vals %in% c("prt"), "Particle"),
  #        vals = replace(vals, vals %in% c("quant"), "Quantifier"),
  #        vals = replace(vals, vals %in% c("rel"), "Relativizer"),
  #        vals = replace(vals, vals %in% c("subordconn"), "Subordinating connective"),
  #        vals = replace(vals, vals %in% c("v"), "Verb")) |> 
  
  mutate(vals = replace(vals, vals %in% c("Adjective"), "adj"),
         vals = replace(vals, vals %in% c("Adverb"), "adv"),
         vals = replace(vals, vals %in% c("Auxiliary"), "aux"),
         vals = replace(vals, vals %in% c("Classifier"), "clf"),
         vals = replace(vals, vals %in% c("Coordinating connective"), "coordconn"),
         vals = replace(vals, vals %in% c("Demonstrative"), "dem"),
         vals = replace(vals, vals %in% c("Interjection"), "interj"),
         vals = replace(vals, vals %in% c("Interrogative pro-form"), "interog"),
         vals = replace(vals, vals %in% c("interrog"), "interog"),
         vals = replace(vals, vals %in% c("Noun", "Indonesian Noun"), "n"),
         vals = replace(vals, vals %in% c("negation"), "neg"),
         vals = replace(vals, vals %in% c("Negation"), "neg"),
         vals = replace(vals, vals %in% c("Proper Noun"), "nprop"),
         vals = replace(vals, vals %in% c("Numeral"), "num"),
         vals = replace(vals, vals %in% c("Preposition"), "prep"),
         vals = replace(vals, vals %in% c("Pro-adverb"), "pro-adv"),
         vals = replace(vals, vals %in% c("Pro-form"), "pro-form"),
         vals = replace(vals, vals %in% c("Pronoun"), "pro"),
         vals = replace(vals, vals %in% c("Particle"), "prt"),
         vals = replace(vals, vals %in% c("Quantifier"), "quant"),
         vals = replace(vals, vals %in% c("Relativizer"), "rel"),
         vals = replace(vals, vals %in% c("Subordinating connective"), "subordconn"),
         vals = replace(vals, vals %in% c("Verb", "Indonesian Verb"), "v")) |> 
  
  # edit the marker for the sub-entries
  mutate(marker_deleted = str_replace_all(marker, "^sew_+\\d\\d$", "se"),
         marker_deleted = str_replace_all(marker_deleted, "^sew_+\\d+_+snw_+\\d+$", "sn"),
         marker_deleted = str_replace_all(marker_deleted, "^.+psw_+\\d+$", "ps"),
         marker_deleted = str_replace_all(marker_deleted, "^.+gn$", "gn"),
         marker_deleted = str_replace_all(marker_deleted, "^.+ge$", "ge"),
         marker_deleted = str_replace_all(marker_deleted, "^.+xvw.+", "xv"),
         marker_deleted = str_replace_all(marker_deleted, "^.+xnw.+", "xn"),
         marker_deleted = str_replace_all(marker_deleted, "^.+xew.+", "xe"),
         marker_deleted = str_replace_all(marker_deleted, "^.+rfw.+", "rf")) |> 
  
  # create a new marker without number
  mutate(marker_deleted = str_replace_all(marker_deleted, "_\\d+$", ""),
         marker_deleted = str_replace_all(marker_deleted,
                                          "(^sn\\d+_|\\d+$)",
                                          "")) |> 
  
  ungroup() |> 
  select(lx_key, marker_deleted, vals) |> 
  
  # create the SFM structure
  mutate(sfm = paste('\\', marker_deleted, " ", vals, sep = "")) |> 
  
  # add new line marker before the \lx marker
  mutate(sfm = if_else(marker_deleted == "lx",
                       paste("\n", sfm, sep = ""),
                       sfm))

## save into SFM plain text ======
word_subentry_examples4 |>
  # exclude number entry
  filter(str_detect(lx_key, "^[0-9]+$", TRUE)) # |> 
  # pull(sfm) |> 
  # filter(lx_key %in% tes_lex) |>
  # write_lines("FLEX-lexicon-with-sub-entries-and-root-and-subentries-examples.db")
  # write_lines("FLEX-lexicon-with-sub-entries-and-root-and-subentries-examples-2024-07-17.db")



# write_lines(pull(word_subentry_examples4, sfm), "FLEX-lexicon-with-sub-entries-and-root-and-subentries-examples-with-references.db")