# source("FLEX-contemporary-with-examples-new.R")

# Code to generate the SFM file to be imported into FLEx
# by combining data from current FLEx Lexicon and Analysed Interlinear Text
# Gede Rajeg (University of Oxford & Udayana University; 2023)

library(tidyverse)
library(xml2)
lu_form_df <- read_rds("FLEX-lift-pre-fieldwork.rds")
source("contemporary-enggano-interlinear-text/processing-all-contemporary-texts-ELAN-FLEX-flextext-NEW-splitting-morpheme.R")
eno_elicitation_texts <- read_rds("contemporary-enggano-interlinear-text/eno_contemp_elicitation_only_as_tibble-new-binded.rds")
eno_natural_texts <- read_rds("contemporary-enggano-interlinear-text/eno_contemp_text_only_as_tibble-new-binded.rds")

eno_morph_split1 <- eno_morph_split1 |> 
  mutate(across(matches("^eno_phrase"), ~str_replace_all(., "\\s{2,}", " ")), 
         across(where(is.character), ~str_trim(., side = "both"))) |>
  mutate(text_title = str_replace_all(text_title, "(?<=^\\d\\d)_", "."))

eno_example_references <- eno_elicitation_texts |> 
  select(text_title, phrase_id, phrase_line) |> 
  bind_rows(eno_natural_texts |> 
              select(text_title, phrase_id, phrase_line)) |> 
  mutate(across(where(is.character), ~str_replace_all(., "_+", "."))) |> 
  mutate(across(matches("^eno_phrase"), ~str_replace_all(., "\\s{2,}", " ")), 
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

# filtering all non-NAs lex_entry
lexs <- eno_morph_split1 |> 
  filter(if_all(matches("^lex_entry"), ~!is.na(.)), !is.na(word)) |> 
  mutate(across(where(is.character), ~ replace(., is.na(.), ""))) |> 
  mutate(word_equal_lexentry = if_else(word == lex_entry, TRUE, FALSE),
         morph_equal_lexentry = if_else(morph == lex_entry, TRUE, FALSE))

# check which lex entry in FLEx Lexicon not from the Text
lu_form_df |> 
  filter(!form %in% eno_morph_split1$lex_entry) |> 
  distinct()

# check which lex entry from the Text not available in the FLEx Lexicon
eno_morph_split1 |> 
  filter(!is.na(word),
         !lex_entry %in% lu_form_df$form) |> 
  select(word, lex_entry) |> 
  distinct()

# check which record has NA lex_entry, but complete word (and its glosses and examples [glosses])
eno_morph_split1 |> 
  filter(is.na(lex_entry), 
         !is.na(word), 
         !is.na(eno_word_gloss_en), 
         !is.na(eno_word_gloss_id), 
         !is.na(eno_phrase), 
         !is.na(eno_phrase_gloss_id), 
         !is.na(eno_phrase_gloss_eng))

# # test-case with 'ueh "sleep; lying down"
# lexs |> 
#   filter(lex_entry == "'ueh") |> 
#   select(morph_gloss_en, morph_gloss_id, lex_entry) |> 
#   distinct()
# 
# # trying to turn into SFM file
# lexs1 <- lexs |> 
#   select(lex_entry, homonym_id, morph_gram, everything()) |> 
#   mutate(across(where(is.integer), as.character)) |> 
#   mutate(lex_entry_dup = lex_entry,
#          hom_id_dup = homonym_id,
#          morph_ps_dup = morph_gram) |> 
#   filter(lex_entry %in% c("-a", "bak", "ka-")) |> 
#   pivot_longer(cols = -c(morph_id, word_equal_lexentry, 
#               lex_entry_dup, hom_id_dup, morph_ps_dup), 
#               names_to = "marker", values_to = "vals")
# marker_included <- c("lex_entry", 
#                      # "variant",
#                      "homonym_id",
#                      # "sense_order",
#                      "morph_gram",
#                      "morph_gloss_en",
#                      "morph_gloss_id",
#                      "eno_phrase",
#                      "eno_phrase_gloss_eng",
#                      "eno_phrase_gloss_id",
#                      "word",
#                      "eno_word_pos",
#                      "eno_word_gloss_en",
#                      "eno_word_gloss_id")
# lexs1 <- lexs1 |> filter(marker %in% marker_included)
# marker_levels <-  c("lex_entry", 
#                     # "variant",
#                     "homonym_id",
#                     # "sense_order",
#                     "morph_gram",
#                     "morph_gloss_en",
#                     "morph_gloss_id",
#                     "eno_phrase",
#                     "eno_phrase_gloss_eng",
#                     "eno_phrase_gloss_id",
#                     "word",
#                     "eno_word_pos",
#                     "eno_word_gloss_en",
#                     "eno_word_gloss_id")
# markers <- data.frame(marker = marker_levels, mrk_id = 1:length(marker_levels))
# lexs2 <- lexs1 |> left_join(markers)
# lexs2 <- lexs1 |>
#   mutate(marker = factor(marker, levels = marker_levels))



# lx <- lexs |> 
#   select(lex_entry, homonym_id, morph_gram, morph_gloss_en) |> 
#   filter(lex_entry %in% c("-a", "bak", "ka-")) |> 
#   distinct() |> 
#   arrange(lex_entry, homonym_id) 

lx <- lexs |> 
  select(lex_entry, homonym_id, morph_gloss_en, morph_gloss_id) |> 
  distinct()
lx

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
  # We need to run distinct()
  # because there are entries that are multiplied due to the presence of
  # sentence example.
  # After running distinct, the entries are 1524,
  # which is the number in the October 2023 FLEx pre-fieldwork

flex_lexicon

# check the unique grouping to determine the number of sense/gloss that
# a given entry has
flex_entry_sense_count <- flex_lexicon |> 
  left_join(lx) |> 
  mutate(across(where(is.character), 
                ~replace(., is.na(.), ""))) |> 
  group_by(entry_id, lex_entry, homonym_id, ps_key, etym, variant) |> 
  summarise(n_sense = n_distinct(sense_order)) |> 
  arrange(desc(n_sense))
flex_entry_sense_count

# create the \sn column
sn <- flex_lexicon |> 
  left_join(lx) |> 
  mutate(across(where(is.character), 
                ~replace(., is.na(.), ""))) |> 
  group_by(entry_id, lex_entry, homonym_id, ps_key, etym, variant) |> 
  mutate(sense_id = paste("sn_", sense_order, sep = "")) |> 
  group_by(entry_id, lex_entry, homonym_id, etym) |> 
  select(-morph_gloss_en, -morph_gloss_id, -sense_gram, -ps_key) |> 
  pivot_wider(names_from = sense_id, values_from = sense_order,
              values_fill = "") |> 
  mutate(va = str_extract_all(variant, "[^ ;]+")) |> 
  select(entry_id, lex_entry, homonym_id, etym, va, everything(), -variant) |> 
  unnest_wider(col = va, 
               names_sep = "_")
sn

# create the \ge column
ge <- flex_lexicon |> 
  left_join(lx) |> 
  mutate(across(where(is.character), ~replace(., is.na(.), ""))) |> 
  group_by(entry_id, lex_entry, homonym_id, ps_key, etym) |> 
  mutate(ge_id = paste("ge_", sense_order, sep = "")) |> 
  group_by(entry_id, lex_entry, homonym_id, etym) |> 
  select(-morph_gloss_id, -sense_order, -sense_gram, -ps_key,
         -variant) |> 
  pivot_wider(names_from = ge_id, values_from = morph_gloss_en,
              values_fill = "")
ge

# create the \gn column
gn <- flex_lexicon |> 
  left_join(lx) |> 
  mutate(across(where(is.character), ~replace(., is.na(.), ""))) |> 
  group_by(entry_id, lex_entry, homonym_id, ps_key, etym) |> 
  mutate(gn_id = paste("gn_", sense_order, sep = "")) |> 
  group_by(entry_id, lex_entry, homonym_id, etym) |> 
  select(-morph_gloss_en, -sense_order, -sense_gram, -ps_key,
         -variant) |> 
  pivot_wider(names_from = gn_id, values_from = morph_gloss_id,
              values_fill = "")
gn

# create the \ps column
ps <- flex_lexicon |> 
  left_join(lx) |> 
  mutate(across(where(is.character), ~replace(., is.na(.), ""))) |> 
  group_by(entry_id, lex_entry, homonym_id, ps_key, etym) |> 
  mutate(ps_id = paste("ps_", sense_order, sep = "")) |> 
  group_by(entry_id, lex_entry, homonym_id, etym) |> 
  select(-morph_gloss_en, -sense_order, -morph_gloss_id, -ps_key,
         -variant) |> 
  pivot_wider(names_from = ps_id, values_from = sense_gram,
              values_fill = "")
ps

# create the variant form
# var <- flex_lexicon |> 
#   group_by(lex_entry, homonym_id, ps_key, etym) |> 
#   # group_by(lex_entry, homonym_id, sense_order, sense_gram, 
#   #          morph_gloss_en, morph_gloss_id) |>
#   mutate(va = str_split(variant, "\\s;\\s")) |> 
#   unnest_longer(va) |> 
#   mutate(va = replace(va, is.na(va), "")) |> 
#   mutate(va_id = paste("va_", row_number(va)-1, sep = "")) |> 
#   ungroup() |> 
#   select(lex_entry, homonym_id, va, va_id) |> 
#   group_by(lex_entry, homonym_id) |> 
#   pivot_wider(names_from = va_id, values_from = va)

sn1 <- sn |> 
  left_join(ge) |> 
  left_join(ps) |> 
  left_join(gn) |> 
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


# Processing sub-entries =======
## subentries only words that are NOT the same with the root/lex.entry
## subentries line must be unique for the following variables 
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
tes_lex <- c("pe", "abė", "bak", "kahinu", "a", 
             "arkih", "anu̇k", "yũ'ũ",
             "pakõ'õã'")

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
  # We need to run distinct()
  # because there are entries that are multiplied due to the presence of
  # sentence example.
  # After running distinct, the total entry is 1524,
  # which is the number in the October 2023 FLEx pre-fieldwork

lexs2 <- lexs |> 
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
         across(where(is.character),
                ~replace(., is.na(.), ""))) |> 
  distinct()

se <- flex_lexicon2 |> 
  left_join(lexs2) |> 
  mutate(word = if_else(str_detect(sense_gram, "^Proper\\s+Noun$",
                                   negate = TRUE), 
                        tolower(word),
                        word),
         across(where(is.character), ~replace(., is.na(.), "")),
         lx_equals_w = lex_entry == word,
         lx_equals_morph = lex_entry == morph,
         w_equals_morph = word == morph) |> 
  distinct()
  
## for the subentries, we want to capture 
## - (i) word form of a lex entry 
## - (ii) word class of the word form (from the w_pos)
## - (iii) gloss/sense of the word form
## - (iv) example with the word form
## but IF the lex.entry is the same with the word form,
## the example sentence should be under the lexical entry.
## IF the lex.entry is not the same with the word,
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
  # We also filtered out non-empty word column
  filter(!lx_equals_w, word != "") |>
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


# turn to long table to be represented as SFM =====
sn3 <- sn2 |> 
  mutate(lx_key = lx) |> 
  pivot_longer(cols = -lx_key, names_to = "marker", values_to = "vals") |> 
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

# save into SFM plain text ======
# sn4 |> 
#   # filter(lx_key %in% tes_lex) |> 
#   pull(sfm) |> 
#   write_lines("FLEX-lexicon-with-sub-entries.db")

# subentry <- se2 |> 
#   group_by(lex_entry, homonym_id, sense_order, ps_key, variant, etym) |> 
#   select(word) |> 
#   mutate(w_id = paste("se_", row_number(word)-1, sep = "")) |> 
#   arrange(lex_entry, homonym_id, sense_order, w_id)


# se_sum <- subentry |> 
#   summarise(n_se = n_distinct(word))
# se_sum

# subentry1 <- subentry |> 
#   pivot_wider(names_from = w_id, values_from = word)
# subentry1


# Finding random sample of sentences for lex.entry/ROOT ===========
# that is also formally the same with the `word` column

## we want a sentence that matches between:
### - the lex.entry (IF the lex.entry and word are the same)
###   - the lex.entry form, its gloss/sense order, homonym id, sense_gram
### - the lex.entry subset (i.e., complex form) that matches between:
###   - its sense/gloss.order, homonym id, sense_gram, and COMPLEX FORM of the lex.entry


### add \xv (ENO) for the lex.entry (stem) that is the same with the word
### add \xn (IDN) for the lex.entry (stem) that is the same with the word
### add \xe (ENG) for the lex.entry (stem) that is the same with the word

## Then, there has to be a separate dataframe for example sentences of the sub-entries,
## because in the SFM, this data will be appended after all lexical entry.
## There has to be a marker of main entry (i.e., lex.entry crossref) in this
## subset of complex form example sentences.

## Another set of dataframe is example sentences of the headword/lex_entry
## this needs to be combined with the `sn2` and `flex_lexicon` data to be made
## into SFM.

## Each example of the root (and its translations) for a given sense should
## appear after the \ge and \gn marker


# Retrieving non-NAs `lex_entry` and `word`
lexs <- eno_morph_split1 |> 
  filter(!is.na(lex_entry) & !is.na(word)) |> 
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
         across(where(is.character),
                ~replace(., is.na(.), ""))) |> 
  distinct()


sent <- flex_lexicon2 |> 
  left_join(lexs |> select(-phrase_id)) |>
  mutate(word = if_else(str_detect(sense_gram, "^Proper\\s+Noun$",
                                   negate = TRUE), 
                        tolower(word),
                        word),
         across(where(is.character), ~replace(., is.na(.), "")),
         across(where(is.character), ~str_trim(., side = "both")),
         lx_equals_w = lex_entry == word,
         lx_equals_morph = lex_entry == morph,
         w_equals_morph = word == morph) |> 
  distinct()


# get example for lex_entry that is the same with the word
sent_root <- sent |> 
  filter(lx_equals_w, eno_phrase_gloss_id != "") |> 
  select(-lx_equals_morph, -w_equals_morph) |> 
  group_by(entry_id, lex_entry, homonym_id, 
           sense_order, sense_gram) |> 
  # filter(lex_entry %in% "pakõ'õã'") |>
  # filter(lex_entry %in% tes_lex) |> 
  distinct() |> 
  slice_sample(n = 3)

# retrieve the sentences for each language and then pivot_wider() for each lang.
## Enggano
interim_eno <- sent_root |> 
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

## Indonesian
interim_idn <- sent_root |> 
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
interim_eng <- sent_root |> 
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
sent_root1 <- interim_eno_wide |> 
  left_join(interim_idn_wide) |> 
  left_join(interim_eng_wide) |> 
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
         sn0_xv1,
         sn0_xn1,
         sn0_xe1,
         sn0_xv2,
         sn0_xn2,
         sn0_xe2,
         sn0_xv3,
         sn0_xn3,
         sn0_xe3,
         
         sn1_xv1,
         sn1_xn1,
         sn1_xe1,
         sn1_xv2,
         sn1_xn2,
         sn1_xe2,
         sn1_xv3,
         sn1_xn3,
         sn1_xe3,
         
         sn2_xv1,
         sn2_xn1,
         sn2_xe1,
         sn2_xv2,
         sn2_xn2,
         sn2_xe2,
         sn2_xv3,
         sn2_xn3,
         sn2_xe3) |> 
  distinct()

# combine the examples for the roots with `sn2` the three languages
sn2_1 <- sn2 |> 
  left_join(sent_root1)
## re-arrange the column to match SFM specification
sn2_2 <- sn2_1 |> 
  select(matches("va"), 
         sn_0, ps_0, ge_0, gn_0, 
         sn0_xv1, sn0_xn1, sn0_xe1, 
         sn0_xv2, sn0_xn2, sn0_xe2, 
         sn0_xv3, sn0_xn3, sn0_xe3,  
         
         sn_1, ps_1, ge_1, gn_1, 
         sn1_xv1, sn1_xn1, sn1_xe1, 
         sn1_xv2, sn1_xn2, sn1_xe2, 
         sn1_xv3, sn1_xn3, sn1_xe3, 
         
         sn_2, ps_2, ge_2, gn_2, 
         sn2_xv1, sn2_xn1, sn2_xe1, 
         sn2_xv2, sn2_xn2, sn2_xe2, 
         sn2_xv3, sn2_xn3, sn2_xe3, 
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

# save into SFM plain text ======
# sn2_4 |>
#   # filter(lx_key %in% tes_lex) |>
#   pull(sfm) |>
#   write_lines("FLEX-lexicon-with-sub-entries-and-root-examples.db")

# IMPORTANT: Try to incorporate the source of the examples!
## the source pattern is: title_segment/line number.
## line number for each phrase can be created on the fly by
## grouping the interlinear dataframe by text_title then
## use row_number() to create the line number


# EXAMPLES FOR SUB-ENTRIES =============
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
         
         # ensure the word/subentry is NOT-EMPTY
         word != "",
         
         # retrieve example that has Indonesian translation
         eno_phrase_gloss_id != "") |> 
  
  # remove prefix and suffix morpheme to reduce sub-entries
  filter(!morph_type %in% c("prefix", "suffix") &
           lex_entry != "ho")
sent_word

## grouping to retrieve a random sample of three examples per sense and POS
sent_word_example_sample <- sent_word |> 
  select(entry_id, lex_entry, morph_type, homonym_id, etym, word, eno_word_gloss_id,
         eno_word_gloss_en, eno_word_pos, eno_phrase, eno_phrase_gloss_id,
         eno_phrase_gloss_eng) |> 
  distinct() |> 
  group_by(entry_id, lex_entry, morph_type, homonym_id, etym, word, eno_word_pos,
           eno_word_gloss_id) |> 
  slice_sample(n = 3) |> 
  arrange(entry_id, lex_entry, word)
sent_word_example_sample

## create the sense number for each word
sent_word_sense_grouping <- sent_word_example_sample |> 
  group_by(entry_id, lex_entry, morph_type, homonym_id, etym, word) |> 
  select(-matches("phrase|refs$")) |> 
  distinct() |> 
  mutate(sn_word = paste("snw_", row_number(eno_word_gloss_id)-1, sep = ""),
         sn_word_id = str_replace_all(sn_word, "snw_", ""),
         ps_word = str_replace_all(sn_word, "^sn", "ps"),
         ps_word_id = str_replace_all(ps_word, "^psw_", "")) |> 
  arrange(lex_entry, sn_word)
sent_word_sense_grouping
### re-join the sample examples dataset
sent_word_example_sample_sn <- sent_word_example_sample |> 
  left_join(sent_word_sense_grouping)
sent_word_example_sample_sn

## create the IDs for the examples (and their glosses)
sent_word_example_sample_sn_ex <- sent_word_example_sample_sn |> 
  mutate(xv_word = paste("xvw_", row_number(eno_phrase)-1, sep = ""),
         xn_word = str_replace_all(xv_word, "^xv", "xn"),
         xe_word = str_replace_all(xv_word, "^xv", "xe")) |> 
  arrange(lex_entry, sn_word, xv_word)
sent_word_example_sample_sn_ex

## create the IDs for the sub-entry forms of a lex_entry
sent_word_example_sample_sn_ex_se <-  sent_word_example_sample_sn_ex |> 
  group_by(entry_id, lex_entry, morph_type, homonym_id, etym) |> 
  select(-matches("phrase|refs|ps_|sn_|x._|gloss_(id|en)|word_pos")) |> 
  distinct() |> 
  mutate(se_w = paste("sew_", row_number(word)-1, sep = ""),
         se_w = str_replace_all(se_w, "(?<=_)(\\d)$", "0\\1")) |> 
  arrange(lex_entry, se_w)
sent_word_example_sample_sn_ex_se
### re-joining with the previous tibble
sent_word_example_sample_sn_ex_se1 <- sent_word_example_sample_sn_ex |> 
  left_join(sent_word_example_sample_sn_ex_se)
sent_word_example_sample_sn_ex_se1




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


# pivot wider for the Enggano sentences
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

# pivot wider for the Indonesian sentences
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
  rename(lx = lex_entry,
         hm = homonym_id,
         et = etym) |> 
  select(-morph_type)

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

# > word_subentry_examples |> select(matches("^sew")) |> colnames() |> str_replace_all("\\d+", "") |> unique()
# Adding missing grouping variables: `entry_id`, `lx`, `hm`, `morph_type`, `et`
# [1] "entry_id"         "lx"               "hm"               "morph_type"       "et"              
# [6] "sew_"             "sew___snw_"       "sew___psw_"       "sew___snw___gn"   "sew___snw___ge"  
# [11] "sew___snw___xvw_" "sew___snw___xnw_" "sew___snw___xew_"

for (se in seq_along(subentries_colnames_id)) {
  
  colmatch_rgx <- paste("^sew_", subentries_colnames_id[se], sep = "")
  
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
           matches("snw_0__xvw_1$"),
           matches("snw_0__xnw_1$"),
           matches("snw_0__xew_1$"),
           matches("snw_0__xvw_2$"),
           matches("snw_0__xnw_2$"),
           matches("snw_0__xew_2$"),
           
           matches("snw_1$"),
           matches("psw_1$"),
           matches("snw_1__gn$"),
           matches("snw_1__ge$"),
           matches("snw_1__xvw_0$"),
           matches("snw_1__xnw_0$"),
           matches("snw_1__xew_0$"),
           matches("snw_1__xvw_1$"),
           matches("snw_1__xnw_1$"),
           matches("snw_1__xew_1$"),
           matches("snw_1__xvw_2$"),
           matches("snw_1__xnw_2$"),
           matches("snw_1__xew_2$"),
           
           matches("snw_2$"),
           matches("psw_2$"),
           matches("snw_2__gn$"),
           matches("snw_2__ge$"),
           matches("snw_2__xvw_0$"),
           matches("snw_2__xnw_0$"),
           matches("snw_2__xew_0$"),
           matches("snw_2__xvw_1$"),
           matches("snw_2__xnw_1$"),
           matches("snw_2__xew_1$"),
           matches("snw_2__xvw_2$"),
           matches("snw_2__xnw_2$"),
           matches("snw_2__xew_2$"),
           
           matches("snw_3$"),
           matches("psw_3$"),
           matches("snw_3__gn$"),
           matches("snw_3__ge$"),
           matches("snw_3__xvw_0$"),
           matches("snw_3__xnw_0$"),
           matches("snw_3__xew_0$"),
           matches("snw_3__xvw_1$"),
           matches("snw_3__xnw_1$"),
           matches("snw_3__xew_1$"),
           matches("snw_3__xvw_2$"),
           matches("snw_3__xnw_2$"),
           matches("snw_3__xew_2$")
    )
  
  subentries_main_df <- left_join(subentries_main_df, interim_df1)
  
  cat(se, sep = "\n")
  
}

# word_subentry_examples1 <- word_subentry_examples |> 
#   select(matches("sew_00"),
#          matches("sew_01"),
#          matches("sew_02"),
#          matches("sew_03"),
#          matches("sew_04"),
#          matches("sew_05"),
#          matches("sew_06"),
#          matches("sew_07"),
#          matches("sew_08"),
#          matches("sew_09"),
#          matches("sew_10"),
#          matches("sew_11"),
#          matches("sew_12"),
#          matches("sew_13"),
#          matches("sew_14"),
#          matches("sew_15"),
#          matches("sew_16"),
#          matches("sew_17"),
#          matches("sew_18"),
#          matches("sew_19"),
#          matches("sew_20"),
#          matches("sew_21"),
#          matches("sew_22"),
#          matches("sew_23"),
#          matches("sew_24"),
#          matches("sew_25"),
#          matches("sew_27"),
#          matches("sew_28"),
#          matches("sew_29"),
#          matches("sew_30"),
#          matches("sew_31"),
#          matches("sew_32"),
#          matches("sew_33"),
#          matches("sew_34"),
#          matches("sew_35"),
#          matches("sew_36"),
#          matches("sew_37"),
#          matches("sew_38"),
#          matches("sew_39"),
#          matches("sew_40"),
#          matches("sew_41"),
#          matches("sew_42"))


# rgx <- "^sew_00"


  # select(matches(rgx)) |> 
  # relocate(matches("snw_0$"), .after = matches(paste(rgx, "$", sep = ""))) |> 
  # relocate(matches("snw_0$"), .before = matches("psw_0$")) |> 
  # relocate(matches("psw_0$"), .before = matches("snw_0__gn$")) |> 
  # relocate(matches("snw_0__gn$"), .before = matches("snw_0__ge$")) |>
  # relocate(matches("snw_0__xvw_0$"), .after = matches("snw_0__ge$")) |> 
  # relocate(matches("snw_0__xnw_0$"), .before = matches("snw_0__xew_0$")) |>
  # 
  # relocate(matches("snw_0__xvw_1$"), .after = matches("snw_0__xew_0$")) |> 
  # relocate(matches("snw_0__xnw_1$"), .before = matches("snw_0__xew_1$")) |>
  # 
  # relocate(matches("snw_0__xvw_2$"), .after = matches("snw_0__xew_1$")) |> 
  # relocate(matches("snw_0__xnw_2$"), .before = matches("snw_0__xew_2$")) |>
  # 
  # relocate(matches("snw_1$"), .after = matches("snw_0__xew_2$")) |>
  # relocate(matches("snw_1$"), .before = matches("psw_1$")) |> 
  # relocate(matches("psw_1$"), .before = matches("snw_1__gn$")) |> 
  # relocate(matches("snw_1__gn$"), .before = matches("snw_1__ge$")) |>
  # relocate(matches("snw_1__xvw_0$"), .after = matches("snw_1__ge$")) |> 
  # relocate(matches("snw_1__xnw_0$"), .before = matches("snw_1__xew_0$")) |>
  # 
  # relocate(matches("snw_1__xvw_1$"), .after = matches("snw_1__xew_0$")) |> 
  # relocate(matches("snw_1__xnw_1$"), .before = matches("snw_1__xew_1$")) |>
  # 
  # relocate(matches("snw_1__xvw_2$"), .after = matches("snw_1__xew_1$")) |> 
  # relocate(matches("snw_1__xnw_2$"), .before = matches("snw_1__xew_2$")) |>
  # 
  # 
  # relocate(matches("snw_2$"), .after = matches("snw_1__xew_2$")) |>
  # relocate(matches("snw_2$"), .before = matches("psw_2$")) |> 
  # relocate(matches("psw_2$"), .before = matches("snw_2__gn$")) |> 
  # relocate(matches("snw_2__gn$"), .before = matches("snw_2__ge$")) |>
  # relocate(matches("snw_2__xvw_0$"), .after = matches("snw_2__ge$")) |> 
  # relocate(matches("snw_2__xnw_0$"), .before = matches("snw_2__xew_0$")) |>
  # 
  # relocate(matches("snw_2__xvw_1$"), .after = matches("snw_2__xew_0$")) |> 
  # relocate(matches("snw_2__xnw_1$"), .before = matches("snw_2__xew_1$")) |>
  # 
  # relocate(matches("snw_2__xvw_2$"), .after = matches("snw_2__xew_1$")) |> 
  # relocate(matches("snw_2__xnw_2$"), .before = matches("snw_2__xew_2$")) |> 
  # 
  # 
  # 
  # relocate(matches("snw_3$"), .after = matches("snw_2__xew_2$")) |>
  # relocate(matches("snw_3$"), .before = matches("psw_3$")) |> 
  # relocate(matches("psw_3$"), .before = matches("snw_3__gn$")) |> 
  # relocate(matches("snw_3__gn$"), .before = matches("snw_3__ge$")) |>
  # relocate(matches("snw_3__xvw_0$"), .after = matches("snw_3__ge$")) |> 
  # relocate(matches("snw_3__xnw_0$"), .before = matches("snw_3__xew_0$")) |>
  #   
  # relocate(matches("snw_3__xvw_1$"), .after = matches("snw_3__xew_0$")) |> 
  # relocate(matches("snw_3__xnw_1$"), .before = matches("snw_3__xew_1$")) |>
  #   
  # relocate(matches("snw_3__xvw_2$"), .after = matches("snw_3__xew_1$")) |> 
  # relocate(matches("snw_3__xnw_2$"), .before = matches("snw_3__xew_2$")) |> 
  # 
  #  
  # colnames()
  

word_subentry_examples2 <- sn2_2 |> 
  select(-matches("^se_")) |> 
  left_join(subentries_main_df)


## turn into a long table for SFM format
word_subentry_examples3 <- word_subentry_examples2 |> 
  mutate(lx_key = lx) |> 
  pivot_longer(cols = -c(lx_key, entry_id), 
               names_to = "marker", 
               values_to = "vals") |> 
  mutate(vals = replace(vals, is.na(vals), "")) |> 
  filter(marker != "morph_type")
word_subentry_examples3

word_subentry_examples4 <- word_subentry_examples3 |> 
  
  # delete empty marker
  filter(vals != "") |>
  
  # edit the marker for the sub-entries
  mutate(marker_deleted = str_replace_all(marker, "^sew_+\\d\\d$", "se"),
         marker_deleted = str_replace_all(marker_deleted, "^sew_+\\d+_+snw_+\\d+$", "sn"),
         marker_deleted = str_replace_all(marker_deleted, "^.+psw_+\\d+$", "ps"),
         marker_deleted = str_replace_all(marker_deleted, "^.+gn$", "gn"),
         marker_deleted = str_replace_all(marker_deleted, "^.+ge$", "ge"),
         marker_deleted = str_replace_all(marker_deleted, "^.+xvw.+", "xv"),
         marker_deleted = str_replace_all(marker_deleted, "^.+xnw.+", "xn"),
         marker_deleted = str_replace_all(marker_deleted, "^.+xew.+", "xe")) |> 
  
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

# save into SFM plain text ======
word_subentry_examples4 |>
  # filter(lx_key %in% tes_lex) |>
  pull(sfm) |>
  write_lines("FLEX-lexicon-with-sub-entries-and-root-and-subentries-examples.db")






# se <- 0:4
# 
# sense <- 0:3
# 
# ps <- 0:3
# 
# xv <- 0:3
# 
# xn <- 0:3
# 
# 
# xe <- 0:3
# 
# 
# 
# for (i in seq_along(se)) {
#   
#   message("I - level: se_ ", se[i], sep = "\n")
#   
#   for (j in seq_along(sense)) {
#     
#     message("J - level", sep = "\n")
#     
#     for (k in seq_along(ps)) {
#       
#       message(paste("sense ", j, "is before POS ", j, sep = ""))
#       message(paste("POS ", j, "is before IDN gloss", j, sep = ""))
#       message(paste("ENG gloss", j, "is after IDN gloss", j, sep = ""))
#       
#       message("K - level: sn_", sense[j], "__pos__ J ", ps[k], sep = "\n\n")
#     }
#     
#   }
#   
#   
# }



# colnames(word_subentry_examples) <- str_replace_all(colnames(word_subentry_examples),
#                                                     "w_", "")
# 
# word_subentry_examples <- word_subentry_examples |> 
#   relocate(se00__sn0, .after = se00) |> 
#   relocate(se00__ps0, .after = se00__sn0) |> 
#   relocate(se00__sn0__gn, .after = se00__ps0) |> 
#   relocate(se00__sn0__ge, .after = se00__sn0__gn) |> 
#   relocate(se00__sn0__xv0, .after = se00__sn0__ge) |> 
#   relocate(se00__sn0__xn0, .after = se00__sn0__xv0) |> 
#   relocate(se00__sn0__xe0, .after = se00__sn0__xn0) |> 
#   relocate(se00__sn0__xv1, .after = se00__sn0__xe0) |> 
#   relocate(se00__sn0__xn1, .after = se00__sn0__xv1) |> 
#   relocate(se00__sn0__xe1, .after = se00__sn0__xn1) |> 
#   relocate(se00__sn0__xv2, .after = se00__sn0__xe1) |> 
#   relocate(se00__sn0__xn2, .after = se00__sn0__xv2) |> 
#   relocate(se00__sn0__xe2, .after = se00__sn0__xn2) |> 
#   relocate(se00__sn1, .after = se00__sn0__xe2) |> 
#   relocate(se00__ps1, .after = se00__sn1) |> 
#   relocate(se00__sn1__gn, .after = se00__ps1) |> 
#   relocate(se00__sn1__ge, .after = se00__sn1__gn) |> 
#   relocate(se00__sn1__xv0, .after = se00__sn1__ge) |> 
#   relocate(se00__sn1__xn0, .after = se00__sn1__xv0) |> 
#   relocate(se00__sn1__xe0, .after = se00__sn1__xn0) |> 
#   relocate(se00__sn1__xv1, .after = se00__sn1__xe0) |> 
#   relocate(se00__sn1__xn1, .after = se00__sn1__xv1) |> 
#   relocate(se00__sn1__xe1, .after = se00__sn1__xn1) |> 
#   relocate(se00__sn1__xv2, .after = se00__sn1__xe1) |> 
#   relocate(se00__sn1__xn2, .after = se00__sn1__xv2) |> 
#   relocate(se00__sn1__xe2, .after = se00__sn1__xn2) |> 
#   relocate(se00__sn2, .after = se00__sn1__xe2) |> 
#   relocate(se00__ps2, .after = se00__sn2) |> 
#   relocate(se00__sn2__gn, .after = se00__ps2) |> 
#   relocate(se00__sn2__ge, .after = se00__sn2__gn) |> 
#   relocate(se00__sn2__xv0, .after = se00__sn2__ge) |> 
#   relocate(se00__sn2__xn0, .after = se00__sn2__xv0) |> 
#   relocate(se00__sn2__xe0, .after = se00__sn2__xn0) |> 
#   relocate(se00__sn2__xv2, .after = se00__sn2__xe0) |> 
#   relocate(se00__sn2__xn2, .after = se00__sn2__xv2) |> 
#   relocate(se00__sn2__xe2, .after = se00__sn2__xn2)
# 
# word_subentry_examples <- word_subentry_examples |> 
#   relocate(se01__sn0, .after = se01) |> 
#   relocate(se01__ps0, .after = se01__sn0) |> 
#   relocate(se01__sn0__gn, .after = se01__ps0) |> 
#   relocate(se01__sn0__ge, .after = se01__sn0__gn) |> 
#   relocate(se01__sn0__xv0, .after = se01__sn0__ge) |> 
#   relocate(se01__sn0__xn0, .after = se01__sn0__xv0) |> 
#   relocate(se01__sn0__xe0, .after = se01__sn0__xn0) |> 
#   relocate(se01__sn0__xv1, .after = se01__sn0__xe0) |> 
#   relocate(se01__sn0__xn1, .after = se01__sn0__xv1) |> 
#   relocate(se01__sn0__xe1, .after = se01__sn0__xn1) |> 
#   relocate(se01__sn0__xv2, .after = se01__sn0__xe1) |> 
#   relocate(se01__sn0__xn2, .after = se01__sn0__xv2) |> 
#   relocate(se01__sn0__xe2, .after = se01__sn0__xn2) |> 
#   relocate(se01__sn1, .after = se01__sn0__xe2) |> 
#   relocate(se01__ps1, .after = se01__sn1) |> 
#   relocate(se01__sn1__gn, .after = se01__ps1) |> 
#   relocate(se01__sn1__ge, .after = se01__sn1__gn) |> 
#   relocate(se01__sn1__xv0, .after = se01__sn1__ge) |> 
#   relocate(se01__sn1__xn0, .after = se01__sn1__xv0) |> 
#   relocate(se01__sn1__xe0, .after = se01__sn1__xn0) |> 
#   relocate(se01__sn1__xv1, .after = se01__sn1__xe0) |> 
#   relocate(se01__sn1__xn1, .after = se01__sn1__xv1) |> 
#   relocate(se01__sn1__xe1, .after = se01__sn1__xn1) |> 
#   relocate(se01__sn1__xv2, .after = se01__sn1__xe1) |> 
#   relocate(se01__sn1__xn2, .after = se01__sn1__xv2) |> 
#   relocate(se01__sn1__xe2, .after = se01__sn1__xn2) |> 
#   relocate(se01__sn2, .after = se01__sn1__xe2) |> 
#   relocate(se01__ps2, .after = se01__sn2) |> 
#   relocate(se01__sn2__gn, .after = se01__ps2) |> 
#   relocate(se01__sn2__ge, .after = se01__sn2__gn) |> 
#   relocate(se01__sn2__xv0, .after = se01__sn2__ge) |> 
#   relocate(se01__sn2__xn0, .after = se01__sn2__xv0) |> 
#   relocate(se01__sn2__xe0, .after = se01__sn2__xn0) |> 
#   relocate(se01__sn2__xv1, .after = se01__sn2__xe0) |> 
#   relocate(se01__sn2__xn1, .after = se01__sn2__xv1) |> 
#   relocate(se01__sn2__xe1, .after = se01__sn2__xn1) |> 
#   relocate(se01__sn2__xv2, .after = se01__sn2__xe1) |> 
#   relocate(se01__sn2__xn2, .after = se01__sn2__xv2) |> 
#   relocate(se01__sn2__xe2, .after = se01__sn2__xn2)
# 
# word_subentry_examples <- word_subentry_examples |> 
#   relocate(se02__sn0, .after = se02) |> 
#   relocate(se02__ps0, .after = se02__sn0) |> 
#   relocate(se02__sn0__gn, .after = se02__ps0) |> 
#   relocate(se02__sn0__ge, .after = se02__sn0__gn) |> 
#   relocate(se02__sn0__xv0, .after = se02__sn0__ge) |> 
#   relocate(se02__sn0__xn0, .after = se02__sn0__xv0) |> 
#   relocate(se02__sn0__xe0, .after = se02__sn0__xn0) |> 
#   relocate(se02__sn0__xv1, .after = se02__sn0__xe0) |> 
#   relocate(se02__sn0__xn1, .after = se02__sn0__xv1) |> 
#   relocate(se02__sn0__xe1, .after = se02__sn0__xn1) |> 
#   relocate(se02__sn0__xv2, .after = se02__sn0__xe1) |> 
#   relocate(se02__sn0__xn2, .after = se02__sn0__xv2) |> 
#   relocate(se02__sn0__xe2, .after = se02__sn0__xn2) |> 
#   relocate(se02__sn1, .after = se02__sn0__xe2) |> 
#   relocate(se02__ps1, .after = se02__sn1) |> 
#   relocate(se02__sn1__gn, .after = se02__ps1) |> 
#   relocate(se02__sn1__ge, .after = se02__sn1__gn) |> 
#   relocate(se02__sn1__xv0, .after = se02__sn1__ge) |> 
#   relocate(se02__sn1__xn0, .after = se02__sn1__xv0) |> 
#   relocate(se02__sn1__xe0, .after = se02__sn1__xn0) |> 
#   relocate(se02__sn1__xv1, .after = se02__sn1__xe0) |> 
#   relocate(se02__sn1__xn1, .after = se02__sn1__xv1) |> 
#   relocate(se02__sn1__xe1, .after = se02__sn1__xn1) |> 
#   relocate(se02__sn1__xv2, .after = se02__sn1__xe1) |> 
#   relocate(se02__sn1__xn2, .after = se02__sn1__xv2) |> 
#   relocate(se02__sn1__xe2, .after = se02__sn1__xn2) |> 
#   relocate(se02__sn2, .after = se02__sn1__xe2) |> 
#   relocate(se02__ps2, .after = se02__sn2) |> 
#   relocate(se02__sn2__gn, .after = se02__ps2) |> 
#   relocate(se02__sn2__ge, .after = se02__sn2__gn) |> 
#   relocate(se02__sn2__xv0, .after = se02__sn2__ge) |> 
#   relocate(se02__sn2__xn0, .after = se02__sn2__xv0) |> 
#   relocate(se02__sn2__xe0, .after = se02__sn2__xn0)



# str_subset(colnames(word_subentry_examples), "se03") |> sort()

# sent_sample <- eno_morph_split1 |> 
#   # filter(word %in% sn1$lx) |> 
#   select(word, lex_entry, eno_phrase, eno_phrase_gloss_id, 
#          eno_phrase_gloss_eng, 
#          homonym_id, morph_gloss_en, morph_gram) |> 
#   filter(!is.na(eno_phrase_gloss_id), !is.na(eno_phrase_gloss_eng)) |> 
#   arrange(lex_entry) |> 
#   group_by(word, lex_entry, homonym_id, morph_gloss_en, morph_gram)
# sent_sample |> slice_sample(n = 3)





# IMPORTANT: in the sub-entry block, the sub-entry \lx or form needs to be
# also marked with the sub-sense/sub-gloss of the lex_entry because
# the word form/sub-entry can have a match with lex_entry form that can not
# have the same/related sense.


# create the \sn column
# lu_form_df |> 
#   filter(form %in% sn1$lx) |> 
#   select(lex_entry = form, homonym_id = order, sense_order, morph_gloss_en = sense_gloss_en, sense_gram) |> 
#   left_join(lx) |> 
#   mutate(sense_order = replace(sense_order, is.na(sense_order), "0")) |> 
#   group_by(lex_entry, homonym_id, sense_gram) |> 
#   mutate(sense_id = paste("sn_", sense_order, sep = "")) |> 
#   select(-morph_gloss_en, -morph_gram) |> 
#   pivot_wider(names_from = sense_id, values_from = sense_order, values_fill = "") |> 
#   ungroup()

# create the \ge column
# lu_form_df |> 
#   filter(form %in% sn1$lx) |> 
#   select(lex_entry = form, homonym_id = order, sense_order, morph_gloss_en = sense_gloss_en, sense_gram) |> 
#   left_join(lx) |> 
#   mutate(sense_order = replace(sense_order, is.na(sense_order), "0")) |> 
#   group_by(lex_entry, homonym_id, sense_gram) |> 
#   mutate(ge_id = paste("ge_", sense_order, sep = "")) |> 
#   select(-morph_gram, -sense_order) |> 
#   pivot_wider(names_from = ge_id, values_from = morph_gloss_en, values_fill = "") |> 
#   ungroup()

# create the \gn column
# lu_form_df |> 
#   filter(form %in% sn1$lx) |> 
#   select(lex_entry = form, homonym_id = order, sense_order, morph_gloss_id = sense_gloss_idn, sense_gram) |> 
#   left_join(lexs |> 
#               select(lex_entry, homonym_id, morph_gram, morph_gloss_id) |> 
#               filter(lex_entry %in% c("-a", "bak", "ka-")) |> 
#               distinct() |> 
#               mutate(morph_gloss_id = replace(morph_gloss_id,
#                                               morph_gloss_id == "NA",
#                                               "")) |> 
#               arrange(lex_entry, homonym_id) |> 
#               group_by(lex_entry, homonym_id, morph_gram)) |> 
#   mutate(sense_order = replace(sense_order, is.na(sense_order), "0")) |> 
#   mutate(across(where(is.character), ~ replace(., is.na(.), ""))) |> 
#   group_by(lex_entry, homonym_id, sense_gram) |> 
#   mutate(gn_id = paste("gn_", sense_order, sep = "")) |> 
#   select(-morph_gram, -sense_order) |> 
#   pivot_wider(names_from = gn_id, values_from = morph_gloss_id, values_fill = "") |> 
#   ungroup()


# get the number of sense per group of lex, hom, and lex.pos
# sn <- lx |> 
#   group_by(lex_entry, homonym_id, morph_gram) |> 
#   mutate(rn = as.character(row_number(lex_entry)-1)) |>
#   mutate(sense_id = paste("sn_", rn, sep = "")) |> 
#   select(-morph_gloss_en) |> 
#   #ungroup() |> 
#   #mutate(ps_id = paste("ps_", rn, sep = "")) |> 
#   #group_by(lex_entry, homonym_id, morph_gram, ps_id) |> 
#   pivot_wider(names_from = sense_id, 
#               values_from = rn,
#               values_fill = "") |> 
#   ungroup()
# 
# ge <- lx |> 
#   group_by(lex_entry, homonym_id, morph_gram) |> 
#   mutate(rn = as.character(row_number(lex_entry)-1)) |>
#   mutate(gloss_id = paste("ge_", rn, sep = "")) |> 
#   select(-rn) |> 
#   #ungroup() |> 
#   #mutate(ps_id = paste("ps_", rn, sep = "")) |> 
#   #group_by(lex_entry, homonym_id, morph_gram, ps_id) |> 
#   pivot_wider(names_from = gloss_id, 
#               values_from = morph_gloss_en,
#               values_fill = "") |> 
#   ungroup()
# 
# gn <- lexs |> 
#   select(lex_entry, homonym_id, morph_gram, morph_gloss_id) |> 
#   filter(lex_entry %in% c("-a", "bak", "ka-")) |> 
#   distinct() |> 
#   mutate(morph_gloss_id = replace(morph_gloss_id,
#                                   morph_gloss_id == "NA",
#                                   "")) |> 
#   arrange(lex_entry, homonym_id) |> 
#   group_by(lex_entry, homonym_id, morph_gram) |> 
#   mutate(rn = as.character(row_number(lex_entry)-1)) |>
#   mutate(gloss_id = paste("gn_", rn, sep = "")) |> 
#   select(-rn) |> 
#   #ungroup() |> 
#   #mutate(ps_id = paste("ps_", rn, sep = "")) |> 
#   #group_by(lex_entry, homonym_id, morph_gram, ps_id) |> 
#   pivot_wider(names_from = gloss_id, 
#               values_from = morph_gloss_id,
#               values_fill = "") |> 
#   ungroup()
# 
# ps <- lx |> 
#   group_by(lex_entry, homonym_id, morph_gram) |> 
#   mutate(rn = as.character(row_number(lex_entry)-1)) |>
#   mutate(ps_id = paste("ps_", rn, sep = "")) |> 
#   select(-rn, -morph_gloss_en) |> 
#   #ungroup() |> 
#   #mutate(ps_id = paste("ps_", rn, sep = "")) |> 
#   #group_by(lex_entry, homonym_id, morph_gram, ps_id) |> 
#   pivot_wider(names_from = ps_id, 
#               values_from = morph_gram,
#               values_fill = "") |> 
#   ungroup()
# 
# sn1 <- sn |> 
#   select(-morph_gram) |> 
#   left_join(ge) |> 
#   left_join(ps) |> 
#   left_join(gn) |> 
#   select(lx = lex_entry,
#          hm = homonym_id,
#          sn_0,
#          ps_0,
#          ge_0,
#          gn_0,
#          sn_1,
#          ps_1,
#          ge_1,
#          gn_1,
#          sn_2,
#          ps_2,
#          ge_2,
#          gn_2)
# sn1

# lexs3 <- lexs |> 
#   select(eno_word_id, lex_entry, homonym_id, 
#          morph_gram, morph_type, word, eno_word_gloss_id,
#          eno_word_gloss_en, eno_word_pos,
#          eno_phrase, eno_phrase_gloss_id, eno_phrase_gloss_eng) |> 
#   mutate(homonym_id = replace(homonym_id, 
#                               homonym_id == "NA",
#                               "")) |> 
#   arrange(eno_word_id, lex_entry, homonym_id) |> 
#   distinct()
# lexs3
# 
# 
# lexs_mini <- lexs3 |> 
#   filter(morph_type == "stem") |> 
#   select(-eno_word_id) |> 
#   distinct() |> 
#   group_by(lex_entry, homonym_id, morph_gram, word, 
#            eno_word_gloss_id, eno_word_gloss_en, eno_word_pos) |> 
#   slice_sample(n = 1) |> 
#   filter(lex_entry %in% c("-a", "bak", "ka-")) |> 
#   ungroup()
# 
# se <- lexs_mini |> 
#   group_by(lex_entry, homonym_id, morph_gram) |> 
#   mutate(rn = as.character(row_number(lex_entry)),
#          se = paste("se_", rn, sep = ""))
# se |> 
#   select(word, se) |> 
#   pivot_wider(names_from = se, values_from = word, values_fill = "")




