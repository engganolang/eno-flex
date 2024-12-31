library(tidyverse)

# processing the collapsed morphemes and re-joining them with the words

## read the saved tibble
# eno_file_text <- "output/contemporary/interlinear/eno_contemp_text_as_tibble-new.rds"
# eno_file_text <- "output/contemporary/interlinear/eno_contemp_text_as_tibble-new-march2024.rds"
# eno_file_text <- "output/contemporary/interlinear/eno_contemp_text_as_tibble-2024-12-28.rds"
eno_file_text <- "output/contemporary/interlinear/eno_contemp_text_as_tibble-2024-12-30.rds"
# eno_file_elicit <- "output/contemporary/interlinear/eno_contemp_elicitation_as_tibble-new.rds"
# eno_file_elicit <- "output/contemporary/interlinear/eno_contemp_elicitation_as_tibble-new-march2024.rds"
# eno_file_elicit <- "output/contemporary/interlinear/eno_contemp_elicitation_as_tibble-2024-12-28.rds"
eno_file_elicit <- "output/contemporary/interlinear/eno_contemp_elicitation_as_tibble-2024-12-30.rds"
# eno_file_textbook <- "contemporary-enggano-interlinear-text/textbook_lexicon_as_tibble_oct-2024.rds"
eno_file_textbook <- "output/textbook/interlinear/textbook_lexicon_as_tibble_20241201.rds"

eno_tbook <- read_rds(eno_file_textbook) |> 
  list_rbind()

eno_texts <- read_rds(eno_file_text) |> 
  list_rbind()

eno_all <- bind_rows(eno_texts, list_rbind(read_rds(eno_file_elicit)), eno_tbook)

## IMPORTANT, the combining characters in the .flextext output has been NORMALISED! So we need stri_trans_nfc() to filter letter with diacritics like nasal

## identifying the type of words and morphs in the database, 
## whether they are complex words (so that they can be put under sub-entries as derivative)
## or whether they are variant forms (hence, in terms of equality check, they are different from the lexical entries and stay as variant and not put under sub-entries)
eno <- eno_all |> 
  mutate(eno_phrase_dummy = str_trim(eno_phrase, side = "both"),
         imperative = if_else(str_detect(morph_gloss_en, "IMP"), TRUE, FALSE),
         word = str_replace(word, "\\ǃ$", "!"),
         lex_entry = str_replace(lex_entry, "\\ǃ", ""),
         word = str_replace_all(word, "\\’$", "'"),
         morph = str_replace_all(morph, "\\’", "'"),
         imperative = if_else(str_detect(word, "\\!$") & stri_trans_nfc(str_to_lower(word)) == stri_trans_nfc(str_to_lower(eno_phrase_dummy)), TRUE, imperative),
         complex_word = if_else(str_detect(morph_gloss_en, "__"), TRUE, FALSE),
         is_variant = if_else(!complex_word & str_to_lower(lex_entry) != str_to_lower(morph), TRUE, FALSE))

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
eno_morph_split <- eno |> 
  mutate(across(matches(col_to_match), ~str_split(., "_+"))) |> 
  mutate(across(matches(col_to_match), ~map_int(., length), .names = "{.col}_n"))

eno_morph_split1 <- eno_morph_split |> 
  unnest(c(morph, morph_id, morph_type, morph_gloss_en, morph_gloss_id, morph_gram,
           lex_entry, homonym_id), 
         keep_empty = TRUE) |> 
  mutate(across(all_of(colnames(eno_morph_split)),
                \(x) replace(x, x == "NA", NA))) |>
  ### handling strange exclamation mark attached to the `word` column ====
  mutate(word = str_replace_all(word, "\\!$", ""),
         across(matches("eno_word_gloss_"), ~str_replace_all(., "\\!$", "")),
         across(matches("^morph_gloss_"), ~str_replace_all(., "(\\!$|\\!$)", "")),
         morph_gloss_en = str_replace(morph_gloss_en, "^Aplicative", "Applicative")) |> 
  ### deleting additional strange equal signs from .flextext export (strange from FLEx!!!!) ====
  mutate(across(matches("morph_gloss"), ~str_replace(., "^\\=", "")),
         morph_gloss_en = replace(morph_gloss_en, morph_gloss_en == "REPEAT=", "REPEAT"))
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

# checking the possessed noun after running the code file ...splitting-morpheme.R
eno |> filter(str_detect(morph_gloss_en, "[1-3]..\\.POSS$")) |> select(word, morph_gloss_en) |> distinct() |> nrow()

eno |> 
  filter(str_detect(morph_gloss_en, "[1-3]..\\.POSS$")) |> 
  select(word, morph_gloss_en) |> 
  distinct() |> 
  slice_sample(n = 20)

