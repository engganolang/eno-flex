library(tidyverse)

# processing the collapsed morphemes and re-joining them with the words

## read the saved tibble
eno_file_text <- "contemporary-enggano-interlinear-text/eno_contemp_text_as_tibble-new.rds"
eno_file_elicit <- "contemporary-enggano-interlinear-text/eno_contemp_elicitation_as_tibble-new.rds"

eno <- read_rds(eno_file_text) |> 
  list_rbind()

eno <- bind_rows(eno, list_rbind(read_rds(eno_file_elicit)))

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

