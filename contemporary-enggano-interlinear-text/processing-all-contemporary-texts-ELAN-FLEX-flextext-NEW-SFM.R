# source("FLEX-contemporary-with-examples-new.R")

# Code to generate the SFM file to be imported into FLEx
# by combining data from current FLEx Lexicon and Analysed Interlinear Text
# Gede Rajeg (University of Oxford & Udayana University; 2023)

library(tidyverse)
library(xml2)
lu_form_df <- readr::read_rds("FLEX-lift-pre-fieldwork.rds")
source("contemporary-enggano-interlinear-text/processing-all-contemporary-texts-ELAN-FLEX-flextext-NEW-splitting-morpheme.R")

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
  select(lex_entry = form, 
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
  group_by(lex_entry, homonym_id, ps_key, etym, variant) |> 
  summarise(n_sense = n_distinct(sense_order)) |> 
  arrange(desc(n_sense))
flex_entry_sense_count

# create the \sn column
sn <- flex_lexicon |> 
  left_join(lx) |> 
  mutate(across(where(is.character), 
                ~replace(., is.na(.), ""))) |> 
  group_by(lex_entry, homonym_id, ps_key, etym, variant) |> 
  mutate(sense_id = paste("sn_", sense_order, sep = "")) |> 
  group_by(lex_entry, homonym_id, etym) |> 
  select(-morph_gloss_en, -morph_gloss_id, -sense_gram, -ps_key) |> 
  pivot_wider(names_from = sense_id, values_from = sense_order,
              values_fill = "") |> 
  mutate(va = str_extract_all(variant, "[^ ;]+")) |> 
  select(lex_entry, homonym_id, etym, va, everything(), -variant) |> 
  unnest_wider(col = va, 
               names_sep = "_")
sn

# create the \ge column
ge <- flex_lexicon |> 
  left_join(lx) |> 
  mutate(across(where(is.character), ~replace(., is.na(.), ""))) |> 
  group_by(lex_entry, homonym_id, ps_key, etym) |> 
  mutate(ge_id = paste("ge_", sense_order, sep = "")) |> 
  group_by(lex_entry, homonym_id, etym) |> 
  select(-morph_gloss_id, -sense_order, -sense_gram, -ps_key,
         -variant) |> 
  pivot_wider(names_from = ge_id, values_from = morph_gloss_en,
              values_fill = "")
ge

# create the \gn column
gn <- flex_lexicon |> 
  left_join(lx) |> 
  mutate(across(where(is.character), ~replace(., is.na(.), ""))) |> 
  group_by(lex_entry, homonym_id, ps_key, etym) |> 
  mutate(gn_id = paste("gn_", sense_order, sep = "")) |> 
  group_by(lex_entry, homonym_id, etym) |> 
  select(-morph_gloss_en, -sense_order, -sense_gram, -ps_key,
         -variant) |> 
  pivot_wider(names_from = gn_id, values_from = morph_gloss_id,
              values_fill = "")
gn

# create the \ps column
ps <- flex_lexicon |> 
  left_join(lx) |> 
  mutate(across(where(is.character), ~replace(., is.na(.), ""))) |> 
  group_by(lex_entry, homonym_id, ps_key, etym) |> 
  mutate(ps_id = paste("ps_", sense_order, sep = "")) |> 
  group_by(lex_entry, homonym_id, etym) |> 
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
  select(lx = lex_entry, 
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
  select(lex_entry = form,
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
  group_by(lex_entry, homonym_id, morph_type, sense_gram) |> 
  # the above is the correct group_by variables because the total unique entry
  # is still 1524, the same as unique entry in the FLEx Lexicon (October 2023)
  
  arrange(lex_entry, homonym_id, sense_gram, sense_order, word) |>
  
  # Filter out only observation where lex_entry is not the same as the word
  # because that means the lex_entry is component of a more complex form.
  # IF the lex_entry is equal to the word, then no need to have sub-entry
  # of complex form for the lex_entry.
  filter(!lx_equals_w, word != "") |>
  distinct() |> 
  
  # remove prefix and suffix morpheme to reduce sub-entries
  # and `ho` auxiliary
  filter(!morph_type %in% c("prefix", "suffix") &
           lex_entry != "ho") |> 
  
  select(-lx_equals_w, -lx_equals_morph, -w_equals_morph) |>
  mutate(se_id = paste("se_", row_number(word)-1, sep = ""),
         se_id = str_replace_all(se_id, "(?<=_)(\\d)$", "0\\1")) |> 
  arrange(lex_entry, homonym_id, sense_order, se_id) |> 
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
              select(-sense_gram,
                     -variant,
                     -morph_type) |> 
              group_by(lx, hm))

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
sn4 |> 
  # filter(lx_key %in% tes_lex) |> 
  pull(sfm) # |> 
  # write_lines("FLEX-lexicon-with-sub-entries.db")

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


# Finding random sample of sentences for lex.entry, ===========
# that is also the value of `word` column

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


sent_sample <- eno_morph_split1 |> 
  # filter(word %in% sn1$lx) |> 
  select(word, lex_entry, eno_phrase, eno_phrase_gloss_id, 
         eno_phrase_gloss_eng, 
         homonym_id, morph_gloss_en, morph_gram) |> 
  filter(!is.na(eno_phrase_gloss_id), !is.na(eno_phrase_gloss_eng)) |> 
  arrange(lex_entry) |> 
  group_by(word, lex_entry, homonym_id, morph_gloss_en, morph_gram)
sent_sample |> slice_sample(n = 3)





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




