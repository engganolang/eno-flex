library(tidyverse)
library(stringi)

# processing the collapsed morphemes and re-joining them with the words

# Textbook materials ======

## read the saved tibble
eno_file_textbook <- "contemporary-enggano-interlinear-text/textbook_lexicon_as_tibble_nov-2024.rds"
eno_tbook <- read_rds(eno_file_textbook) |> 
  list_rbind()

## IMPORTANT, the combining characters in the .flextext output has been NORMALISED! So we need stri_trans_nfc() to filter letter with diacritics like nasal

## identifying the type of words and morphs in the database, 
## whether they are complex words (so that they can be put under sub-entries as derivative)
## or whether they are variant forms (hence, in terms of equality check, they are different from the lexical entries and stay as variant and not put under sub-entries)
eno_tbook <- eno_tbook |> 
  mutate(imperative = if_else(str_detect(morph_gloss_en, "IMP"), TRUE, FALSE),
         complex_word = if_else(str_detect(morph_gloss_en, "__"), TRUE, FALSE),
         is_variant = if_else(!complex_word & lex_entry != morph, TRUE, FALSE))

## select the phrase ID as key and all morph columns
col_to_match <- "^(morph|homonym|lex_entry)"
# eno_morph <- eno |> 
#   select(phrase_id, matches(col_to_match)) |> 
#   distinct()
# nrow(eno_morph)

## split the collapsing marker (___) of the morph columns
# eno_morph_split <- eno_morph |> 
#   mutate(across(matches(col_to_match), ~str_split(., "_+")))
# nrow(eno_morph_split) == nrow(eno_morph)
eno_morph_split <- eno_tbook |> 
  mutate(across(matches(col_to_match), ~str_split(., "_+"))) |> 
  mutate(across(matches(col_to_match), ~map_int(., length), .names = "{.col}_n"))

eno_morph_split1 <- eno_morph_split |> 
  unnest(c(morph, morph_id, morph_type, morph_gloss_en, morph_gloss_id, morph_gram,
           lex_entry, homonym_id), 
         keep_empty = TRUE) |> 
  mutate(across(all_of(colnames(eno_morph_split)),
                \(x) replace(x, x == "NA", NA)))
# nrow(eno_morph_split) == nrow(eno_morph)


eno_morph_split1 |> 
  group_by(word) |> 
  filter(morph_type=="stem", eno_word_pos == "v", morph_n > 1, eno_word_gloss_en == "go") |> 
  select(word, lex_entry, eno_phrase, eno_phrase_gloss_id)

# checking for a given stem in a verb, how many word forms that stem has
eno_morph_split1 |> 
  group_by(word) |> 
  filter(morph_type=="stem", eno_word_pos == "v", morph_n > 1, morph_gloss_en == "go") |> 
  select(word, lex_entry, eno_phrase, eno_phrase_gloss_id) |> 
  group_by(lex_entry) |> 
  summarise(n_form = n_distinct(word))

# collapse the word form into a column for a given stem
eno_morph_split1 |> 
  group_by(word) |> 
  filter(morph_type=="stem", eno_word_pos == "v", morph_n > 1, morph_gloss_en == "go") |> 
  select(word, lex_entry, eno_phrase, eno_phrase_gloss_id) |> 
  select(lex_entry, word) |> 
  ungroup() |> 
  distinct() |> 
  group_by(lex_entry) |> 
  arrange(word) |> 
  mutate(word_form = paste(word, collapse = "; ")) |> 
  ungroup() |> 
  select(-word) |> 
  distinct()
