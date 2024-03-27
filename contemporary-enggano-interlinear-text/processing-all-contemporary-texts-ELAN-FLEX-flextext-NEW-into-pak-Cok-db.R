# to generate data for Pak Cok's system, run the codes in "...flextext-NEW-SFM.R" from line 10 until line 325

flex_from_text1 <- flex_from_text |> 
  mutate(homonym_id = replace(homonym_id, homonym_id == "", "0"))
flex_lexicon1 <- flex_lexicon |> 
  mutate(homonym_id = replace(homonym_id, homonym_id == "", "0"))

# 1. Finding words equal with lexical entry vs. those that are not ====
## We need to identify which word form (in `word` column of `flex_from_text`) is the same with the `lex_entry` form; in the `flex_from_text` data frame, identify this by filtering via `word_equal_lexentry` column
word_equal_lexentry <- flex_from_text1 |> 
  filter(word_equal_lexentry)
word_different_from_lexentry <- flex_from_text1 |> 
  filter(!word_equal_lexentry)

# 2. Pre-caution/checking ====
## pre-caution for convergence between `flex_lexicon` and `flex_from_text` for the `lex_entry` numbers
### identify the number of entries/rows for `lex_entry` in `flex_lexicon` that ARE also present in `flex_from_text` (esp. entries where the `word` is equal to `lex_entry`)
flex_lexicon1 |> 
  filter(stringi::stri_trans_nfc(lex_entry) %in% 
           stringi::stri_trans_nfc(word_equal_lexentry$lex_entry))
### A tibble: 900 × 10

### identify the number of entries/rows for `lex_entry` in `flex_lexicon` that are NOT present in `flex_from_text` (esp. entries where the `word` is equal to `lex_entry`)
entry_added_directly_to_the_lexicon <- flex_lexicon1 |> 
  filter(!stringi::stri_trans_nfc(lex_entry) %in% 
           stringi::stri_trans_nfc(word_equal_lexentry$lex_entry))
entry_added_directly_to_the_lexicon
### A tibble: 662 × 10

### so the total of 662 + 900 is 1,562 (this is the total rows in the `flex_lexicon`, which is a match)
nrow(flex_lexicon1)

### identify the number of unique `lex_entry` (in `flex_from_text`) that is also in the `flex_lexicon`
word_equal_lexentry |> 
  filter(lex_entry %in% flex_lexicon1$lex_entry) |> 
  select(lex_entry) |> 
  distinct()
### A tibble: 797 × 1 (there are 797 unique lexical entries in `flex_from_text` also present in the lexical entries in `flex_lexicon`)

### identify the number of unique `lex_entry` (in `flex_lexicon`) that is also in the `flex_from_text`
flex_lexicon1 |> 
  filter(stringi::stri_trans_nfc(lex_entry) %in% 
           stringi::stri_trans_nfc(word_equal_lexentry$lex_entry)) |> 
  select(lex_entry) |> 
  distinct()
### A tibble: 797 × 1 
### (there are 797 unique lexical entries in `flex_lexicon` that matches with lexical entries in `flex_from_text`)
### this result shows convergence between the number and types of lexical entries in `flex_lexicon` and `flex_from_text`

#### there are NO UNMATCHED lexical entry between `flex_from_text` and `flex_lexicon` as shown below
word_equal_lexentry |> 
  filter(!lex_entry %in% flex_lexicon1$lex_entry)
#### A tibble: 0 × 29

### identify the number of unique `lex_entry` (in `flex_from_text`) that is also in the `flex_lexicon` (for `word_different_from_lexentry`)
word_different_from_lexentry |> 
  filter(lex_entry %in% flex_lexicon1$lex_entry) |> 
  select(lex_entry) |> 
  distinct()
### A tibble: 700 × 1 (there are 700 unique lexical entries in `flex_from_text` [for `word_different_from_lexentry`] also present in the lexical entries in `flex_lexicon`)

flex_lexicon1 |> 
  filter(stringi::stri_trans_nfc(lex_entry) %in% 
           stringi::stri_trans_nfc(word_different_from_lexentry$lex_entry)) |> 
  select(lex_entry) |> 
  distinct()
### A tibble: 700 × 1
### (there are 700 unique lexical entries in `flex_lexicon` that matches with lexical entries in `flex_from_text` [for `word_different_from_lexentry`])
### this result shows convergence between the number and types of lexical entries in `flex_lexicon` and `flex_from_text` [for `word_different_from_lexentry`]

#### there are NO UNMATCHED lexical entry between `flex_from_text` [for `word_different_from_lexentry`] and `flex_lexicon` as shown below
word_different_from_lexentry |> 
  filter(!lex_entry %in% flex_lexicon1$lex_entry)
#### A tibble: 0 × 29

tes_lex <- c("pe", "abė", "bak", "kahinu", "a",
             "arkih", "anu̇k", "yũ'ũ",
             "pakõ'õã'")

bu_verb <- flex_from_text1 |> 
  filter(lex_entry == "bu-")
ki_verb <- flex_from_text1 |> 
  filter(lex_entry == "ki-")
pa_verb <- flex_from_text1 |> 
  filter(lex_entry == "pa-")

# 3. Linking ID for root and complex form ====

lex <- flex_from_text1 |>
  # exclude prefix and suffix
  # filter(!morph_type %in% c("prefix", "suffix")) |>
  select(word, eno_word_id, eno_word_gloss_id, eno_word_gloss_en, eno_word_pos, morph_type,
         homonym_id, lex_entry, morph, morph_gloss_en, morph_gloss_id, morph_gram,
         word_equal_lexentry, phrase_id, 
         # phrase_line, 
         eno_phrase, eno_phrase_gloss_id, eno_phrase_gloss_eng, text_title) |> 
  distinct()

lex1 <- flex_lexicon1 |> 
  select(-ps_key) |> 
  mutate(lexicon_id = entry_id) |> 
  left_join(lex) |> 
  group_by(lex_entry, homonym_id) |> 
  mutate(n_word_per_lex_entry = n_distinct(word)) |> 
  ungroup() |> 
  mutate(root_word_id = "") |> 
  select(root_word_id, entry_id, lexicon_id, everything()) # |> 
  # mutate(entry_id = replace(entry_id, !is.na(lexicon_id) & is.na(entry_id), lexicon_id))

lex2 <- lex1 |> 
  
  # add the root_word_id for complex word (that is not equal with the lex_entry)
  mutate(root_word_id = if_else(!word_equal_lexentry & !morph_type %in% c("root", "suffix", "prefix"),  ## the morph type == "root" is only for "ho="
                                entry_id,
                                root_word_id)) |> 
  select(root_word_id, everything())

lex3 <- lex2 |> 
  mutate(entry_id = if_else(root_word_id != "" & !word_equal_lexentry & !morph_type %in% c("root", "suffix", "prefix"),
                            eno_word_id,
                            entry_id)) |> 
  mutate(entry_id = if_else(!is.na(lexicon_id) & is.na(entry_id),
                            lexicon_id,
                            entry_id)) |> 
  distinct() |> 
  select(entry_id, root_word_id, everything())


lex4 <- lex3 |> 
  bind_rows(flex_lexicon |> 
              mutate(sources = "lexicon") |> 
              rename(word = lex_entry,
                     eno_word_gloss_en = morph_gloss_en,
                     eno_word_gloss_id = morph_gloss_id,
                     eno_word_pos = sense_gram,
                     homonym_id_lexicon = homonym_id) |> 
              select(-ps_key))

lex5 <- lex4 |> 
  filter(!is.na(word)) |> 
  mutate(homonym_id = if_else(is.na(homonym_id) & !is.na(homonym_id_lexicon),
                              homonym_id_lexicon,
                              homonym_id),
         homonym_id_lexicon = if_else(is.na(homonym_id_lexicon) & !is.na(homonym_id),
                                      homonym_id,
                                      homonym_id_lexicon))

lex6 <- lex5 |> 
  select(id = entry_id,
         root_word_id,
         word,
         morph,
         lex_entry,
         variant_for_root = variant,
         etymology_for_root = etym,
         indonesian = eno_word_gloss_id,
         english = eno_word_gloss_en,
         homonym_id,
         sense_order,
         word_class = eno_word_pos,
         word_type = morph_type,
         word_equal_lexentry,
         example_id = phrase_id,
         example_enggano = eno_phrase,
         example_indonesian = eno_phrase_gloss_id,
         example_english = eno_phrase_gloss_eng,
         text_title,
         n_word_per_lex_entry,
         sources) |> 
  distinct() |> 
  # mutate(homonym_id = if_else(is.na(homonym_id), "", homonym_id)) |> 
  mutate(across(where(is.character), ~replace_na(., "")))

lex7 <- lex6 |> 
  filter(!word_type %in% c("prefix", "suffix", "root"))

lex8 <- lex7 |> 
  mutate(word_type = if_else(sources == "lexicon" & str_detect(word, "^\\-"), 
                             "suffix", word_type)) |> 
  mutate(word_type = if_else(sources == "lexicon" & str_detect(word, "\\-$"), 
                             "prefix", word_type)) |> 
  mutate(word_type = if_else(sources == "lexicon" & !word_type %in% c("prefix", "suffix") & str_detect(word, "\\-", negate = TRUE), 
                             "root", word_type)) |> 
  mutate(word_type = if_else(sources == "lexicon" & !word_type %in% c("prefix", "suffix", "root") & str_detect(word, "\\-", negate = FALSE), 
                             "complex form", word_type)) |> 
  mutate(word_type = if_else(str_detect(word, "\\="), 
                             "clitic", word_type)) |> 
  mutate(word_type = if_else(sources != "lexicon" & !word_equal_lexentry & word_type == "stem",
                             "complex form", word_type)) |> 
  mutate(word_type = if_else(sources != "lexicon" & word_equal_lexentry & word_type == "stem",
                             "root", word_type))

lex9 <- lex8 |> 
  filter(!id %in% entry_added_directly_to_the_lexicon$entry_id, 
         !is.na(word_equal_lexentry)) |> 
  bind_rows(lex8 |> 
              filter(id %in% entry_added_directly_to_the_lexicon$entry_id) |> 
              mutate(word_equal_lexentry = if_else(is.na(word_equal_lexentry), 
                                                   FALSE, 
                                                   word_equal_lexentry)))

lex10 <- lex9 |> 
  mutate(word_class = if_else(word_class == "subordconn",
                              "Subordinating connective",
                              word_class),
         word_class = if_else(word_class == "coordconn",
                              "Coordinating connective",
                              word_class),
         word_class = if_else(word_class == "adj",
                              "Adjective",
                              word_class),
         word_class = if_else(word_class == "adv",
                              "Adverb",
                              word_class),
         word_class = if_else(word_class == "aux",
                              "Auxiliary",
                              word_class),
         word_class = if_else(word_class == "clf",
                              "Classifier",
                              word_class),
         word_class = if_else(word_class == "dem",
                              "Demonstrative",
                              word_class),
         word_class = if_else(word_class == "interj",
                              "Interjection",
                              word_class),
         word_class = if_else(word_class == "interrog",
                              "Interrogative pro-form",
                              word_class),
         word_class = if_else(word_class == "n",
                              "Noun",
                              word_class),
         word_class = if_else(word_class %in%  c("neg", "negation"),
                              "Negation",
                              word_class),
         word_class = if_else(word_class %in%  c("nprop"),
                              "Proper Noun",
                              word_class),
         word_class = if_else(word_class %in%  c("num"),
                              "Numeral",
                              word_class),
         word_class = if_else(word_class %in%  c("prep"),
                              "Preposition",
                              word_class),
         word_class = if_else(word_class %in%  c("pro"),
                              "Pronoun",
                              word_class),
         word_class = if_else(word_class %in%  c("pro-adv"),
                              "Pro-adverb",
                              word_class),
         word_class = if_else(word_class %in%  c("pro-form"),
                              "Pro-form",
                              word_class),
         word_class = if_else(word_class %in%  c("prt"),
                              "Particle",
                              word_class),
         word_class = if_else(word_class %in%  c("quant"),
                              "Quantifier",
                              word_class),
         word_class = if_else(word_class %in%  c("rel"),
                              "Relativizer",
                              word_class),
         word_class = if_else(word_class %in%  c("v"),
                              "Verb",
                              word_class),
         word_class = if_else(sources == "lexicon" & word_class == "" & word  %in% c("mata", stringi::stri_trans_nfc("ẽp")),
                              "Noun",
                              word_class),
         word_class = if_else(sources == "lexicon" & str_detect(word, "(\\-$|^\\-)") & str_detect(english, "(\\bPOSS\\b|\\bGEN\\b)"),
                              "Noun",
                              word_class),
         word_class = if_else(sources == "lexicon" & str_detect(word, "(\\-$|^\\-)") & str_detect(english, "(\\bPOSS\\b|\\bGEN\\b)"),
                              str_c(word_class, " possessive/genitive ", word_type, sep = ""),
                              word_class),
         word_class = if_else(sources == "lexicon" & english == "CLF" & word_class == "",
                              "Classifier",
                              word_class),
         word_class = if_else(sources == "lexicon" & str_detect(word, "(\\-$|^\\-)") & word_class == "Classifier",
                              str_c(word_class, word_type, sep = " "),
                              word_class),
         word_class = if_else(sources == "lexicon" & str_detect(word, "reduplication\\-$") & word_class == "Verb",
                              str_c("Reduplication verbal ", word_type, sep = ""),
                              word_class),
         english = if_else(sources == "lexicon" & word == "reduplication-" & english == "REDUP",
                           "Prefix as reduplication modifying the Tense, Aspect, and Mood (TAM) of the verb",
                           english),
         word_class = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & english == "NM",
                              str_c("Nominalising ", word_type, sep = ""),
                              word_class),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & word == "e-" & english == "NM",
                           "Nominalisation for action",
                           english),
         word_class = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & english == "DEF",
                              str_c("Definite", word_type, sep = " "),
                              word_class),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & english == "DEF",
                           str_c(str_to_sentence(word_type), " for indicating definite noun", sep = ""),
                           english),
         word_class = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & str_detect(english, "NMLZ"),
                              str_c("Noun ", word_type, sep = ""),
                              word_class),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & english == "NMLZ",
                           str_c(str_to_sentence(word_type), " for forming noun", sep = ""),
                           english),
         word_class = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & english == "REDUP" & word_class == "Noun",
                              "Noun reduplication",
                              word_class),
         english = if_else(sources == "lexicon" & word == "-full reduplication" & english == "REDUP",
                           "Full reduplication on noun modifying the number of the noun",
                           english),
         word_class = if_else(sources == "lexicon" & str_detect(word, "(^\\-|\\-$)") & english == "VBLZ",
                              str_c(word_class, " ", word_type, sep = ""),
                              word_class),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & english == "VBLZ",
                           str_c("Verbalising", word_type, sep = " "),
                           english),
         english = if_else(sources == "lexicon" & word_type == "suffix" & english == "LOC.NMLZ", 
                           str_c("Suffix forming Locative noun (", english, ")", sep = ""),
                           english),
         english = if_else(sources == "lexicon" & word_type == "suffix" & english == "PAT.NMLZ", 
                           str_c("Suffix forming Patient noun (", english, ")", sep = ""),
                           english),
         word_class = if_else(sources == "lexicon" & str_detect(word, "(^\\-|\\-$)") & word %in% c("aba-", "bu-", "-∅"),
                              str_c("Verb ", word_type, sep = ""),
                              word_class),
         word_class = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & str_detect(english, "[0-9]") & word_class == "Verb",
                              str_c(word_class, " agreement ", word_type, sep = ""),
                              word_class),
         word_class = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & english == "HUM",
                              str_c(word_class, word_type, sep = " "),
                              word_class),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & english == "HUM",
                           str_c(word_class, " for human noun", sep = ""),
                           english)) |> 
  mutate(english = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & str_detect(english, "\\bPOSS\\b") & str_detect(english, "SG\\b"),
                           str_replace(english, "SG\\b\\.", " singular "),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & str_detect(english, "\\bPOSS\\b") & str_detect(english, "PL\\b"),
                           str_replace(english, "PL\\b\\.", " plural "),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & str_detect(english, "\\bPOSS\\b"),
                           str_replace(english, "\\bPOSS\\b", "possessive"),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & str_detect(english, "[0-9]PL\\b"),
                           str_replace(english, "([0-9])PL\\b", "\\1 plural"),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & str_detect(english, "\\.?EXCL\\.?"),
                           str_replace(english, "\\.?EXCL\\.?", " exclusive "),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & str_detect(english, "^[1]"),
                           str_replace(english, "^[1]", "First (1st) person "),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & str_detect(english, "^[2]"),
                           str_replace(english, "^[2]", "Second (2nd) person "),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & str_detect(english, "^[3]"),
                           str_replace(english, "^[3]", "Third (3rd) person "),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & str_detect(english, "^IMP\\.2"),
                           str_replace(english, "^IMP\\.2", "Imperative form for the second (2nd) person "),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & str_detect(english, "SG$"),
                           str_replace(english, "SG$", "singular"),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & str_detect(english, "^PL$"),
                           str_replace(english, "^PL$", "Plural"),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & str_detect(english, "^FUT$"),
                           str_replace(english, "^FUT$", "Future tense marker"),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & str_detect(english, "^APPL$"),
                           str_replace(english, "^APPL$", "Applicative marker"),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & str_detect(english, "^PASS$"),
                           str_replace(english, "^PASS$", "Passive marker"),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & sources == "lexicon" & str_detect(english, "^CLF$"),
                           str_replace(english, "^CLF$", "Classifier"),
                           english))




# lex4 <- lex3 |> 
#   select(word, 
#          root_word_id, 
#          english = eno_word_gloss_en, 
#          indonesian = eno_word_gloss_id, 
#          word_class = eno_word_pos, 
#          phrase_id, 
#          phrase_line, 
#          text_title,
#          word_equal_lexentry,
#          n_word_per_lex_entry,
#          word_type = morph_type,
#          lex_entry) |> 
#   mutate(sources = "texts") |> 
#   bind_rows(flex_lexicon |> 
#               rename(id = entry_id,
#                      word = lex_entry,
#                      english = morph_gloss_en,
#                      indonesian = morph_gloss_id,
#                      word_class = sense_gram) |> 
#               select(-ps_key) |> 
#               mutate(sources = "lexicon")) |> 
#   filter(!is.na(word))
# 
# 
# #|> 
#   # change the root_word_id into id
#   mutate(id = "") |> 
#   select(id, root_word_id, everything()) |> 
#   # add the same root_word_id for lex_entry that has more than one word forms but the lex_entry also appears in the word col. (hence, word_equal_lexentry == TRUE)
#   mutate(id = if_else(word_equal_lexentry & 
#                         id == "" &
#                         n_word_per_lex_entry > 1,
#                       str_c(lex_entry, "_", homonym_id, sep = ""),
#                       id)) |> 
#   
#   select(id, root_word_id, everything()) |>
#   
#   # then for the id that is empty, word equal with lex_entry, root_word_id is empty, and n word per lexical entry is 1, create the id from the lex_entry and homonym_id
#   mutate(id = if_else(word_equal_lexentry &
#                         id == "" &
#                         root_word_id == "" &
#                         n_word_per_lex_entry == 1,
#                       str_c(lex_entry, "_", homonym_id, sep = ""),
#                       id)) |> 
#   # for lexical entry that has no ID (meaning that this lexical entry has no independent appearance as word in the text we need to create an independent ID for this root that later becomes its ID when extracted)
#   mutate(lex_entry_id = if_else(id == "" & !word_equal_lexentry & root_word_id != "", 
#                                 root_word_id, 
#                                 id)) # |> 
#   # now the next step is to create the id for the empty id
#   mutate(id = if_else(id == "",
#                       str_c("lx_0", row_number(), "_", word, sep = ""),
#                       id))



## root_word_id == "" should be the same coming from lex_entry and hom. ID

## to integrate prefix, remove the word column


# ### get lexical entry from `word_equal_lexentry` that is present in `flex_lexicon`
# flex_lexicon_lex_entry_equal_word <- flex_lexicon |> 
#   filter(stringi::stri_trans_nfc(lex_entry) %in% 
#            stringi::stri_trans_nfc(word_equal_lexentry$lex_entry)) |> 
#   select(-ps_key)
# 
# 
# 
# 
# flex_from_text_and_lexicon <- flex_lexicon |> 
#   mutate(data_source = "lexicon") |> 
#   rename(word = lex_entry) |> 
#   bind_rows(flex_from_text |> 
#               mutate(data_source = "texts") |> 
#               left_join(flex_lexicon |> 
#                           rename(entry_id_2 = entry_id)))


# flex_from_text_and_lexicon <- flex_from_text |> 
#   left_join(flex_lexicon) |> 
#   mutate(root_word_id = paste(lex_entry, "_", homonym_id, sep = "")) |> 
#   select(root_word_id, everything())
# 
# words_tb <- flex_from_text_and_lexicon |> 
#   select(root_word_id, word, eno_word_pos, morph_type, morph, 
#          homonym_id, lex_entry, morph_gram, etym, word_equal_lexentry, variant) |> 
#   distinct()
# words_tb_non_affix <- words_tb |>
#   filter(!morph_type %in% c("prefix", "suffix"))
# 
# words_tb1 <- words_tb_non_affix |> 
#   filter(!word_equal_lexentry) |> 
#   select(root_word_id, word, eno_word_pos, morph_type, word_equal_lexentry) |> 
#   bind_rows(words_tb_non_affix |> 
#               select(root_word_id, 
#                      word = lex_entry, 
#                      morph_type, 
#                      homonym_id, 
#                      eno_word_pos = morph_gram, 
#                      etym, 
#                      variant, 
#                      word_equal_lexentry) |> 
#               filter(!word_equal_lexentry))

# root <- flex_from_text |> 
#   filter(morph_type %in% c("stem", "root", "phrase")) |> 
#   select(word, eno_word_gloss_id, eno_word_gloss_en, eno_word_pos, 
#          morph, morph_type, homonym_id, lex_entry, morph_gloss_en, 
#          morph_gloss_id, morph_gram, word_equal_lexentry) |> 
#   distinct() |> 
#   filter(word_equal_lexentry) |> 
#   mutate(data_set = "word_equal_lexentry")
# non_root <- flex_from_text |> 
#   filter(morph_type %in% c("stem", "root", "phrase")) |> 
#   select(word, eno_word_gloss_id, eno_word_gloss_en, eno_word_pos, 
#          morph, morph_type, homonym_id, lex_entry, morph_gloss_en, 
#          morph_gloss_id, morph_gram, word_equal_lexentry) |> 
#   distinct() |> 
#   filter(!word_equal_lexentry) |> 
#   mutate(data_set = "word_different_from_lexentry")

