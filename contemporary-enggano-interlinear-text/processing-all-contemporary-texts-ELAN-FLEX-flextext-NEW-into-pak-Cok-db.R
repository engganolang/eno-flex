# To generate data for Pak Cok's system, run the codes in "...flextext-NEW-SFM.R" from line 10 until line 325. Then run the following codes from line 2 to 133. Then skip over the codes inside "OLD CODES" and go to section 4. debuging
library(stringi)
flex_from_text1 <- flex_from_text |> 
  mutate(homonym_id = replace(homonym_id, homonym_id == "", "0")) |> 
  # fix the "eit" glossing
  mutate(homonym_id = replace(homonym_id, word == "eit" & homonym_id == "2", "1"),
         morph_gram = replace(morph_gram, word == "eit" & morph_gloss_en == "drink", "n"),
         morph_gloss_en = replace(morph_gloss_en, word == "eit" & morph_gloss_en == "drink", "banana"),
         morph_gloss_id = replace(morph_gloss_id, word == "eit" & morph_gloss_id == "minum", "pisang")) |> 
  # fix the "dak", "kidė", "ma'", "enan", and "keab" glossing
  mutate(morph_gloss_en = replace(morph_gloss_en, lex_entry == "dak" & homonym_id == "1" & morph_gloss_en == "NEG", "no; not"),
         morph_gloss_id = replace(morph_gloss_id, lex_entry == "dak" & homonym_id == "1" & morph_gloss_id == "NEG", "tidak"),
         morph_gloss_en = replace(morph_gloss_en, lex_entry == "keab" & homonym_id == "0" & morph_gloss_en == "NEG", "not exist"),
         morph_gloss_id = replace(morph_gloss_id, lex_entry == "enan" & morph_gloss_id == "mengali", "menggali"),
         
         lex_entry = replace(lex_entry, morph == "-menan" & morph_gloss_en == "dig" & lex_entry == "-full reduplication", "enan"),
         morph_type = replace(morph_type, morph == "-menan" & morph_gloss_en == "dig", "stem"),
         morph_id = replace(morph_id, morph == "-menan" & morph_gloss_en == "dig", "d7f713e8-e8cf-11d3-9764-00c04f186933"),
         morph = replace(morph, morph == "-menan" & morph_gloss_en == "dig", "enan"),
         
         morph_gloss_en = replace(morph_gloss_en, lex_entry == stri_trans_nfc("kidė") & morph_gloss_en == "like" & homonym_id == "0", "be.like"),
         morph_gloss_id = replace(morph_gloss_id, lex_entry == "ma'" & morph_gloss_id == "mak", "ibu")) |> 
  mutate(across(where(is.character), ~str_trim(., side = "both"))) |> 
  mutate(# create column indicating if the lexicon & morpheme are the same with the word
    word_equal_lexentry = if_else(word == lex_entry, TRUE, FALSE),
    morph_equal_lexentry = if_else(morph == lex_entry, TRUE, FALSE))
flex_lexicon1 <- flex_lexicon |> 
  mutate(homonym_id = replace(homonym_id, homonym_id == "", "0")) |> 
  mutate(across(where(is.character), ~str_trim(., side = "both")))

# 1. Finding words equal with lexical entry vs. those that are not ====
## We need to identify which word form (in `word` column of `flex_from_text`) is the same with the `lex_entry` form; in the `flex_from_text` data frame, identify this by filtering via `word_equal_lexentry` column
word_equal_lexentry <- flex_from_text1 |> 
  filter(word_equal_lexentry)
word_different_from_lexentry <- flex_from_text1 |> 
  filter(!word_equal_lexentry)

# 2. Pre-caution/checking ====
## pre-cautionary check for convergence between `flex_lexicon` and `flex_from_text` for the `lex_entry` numbers
### identify the number of entries/rows for `lex_entry` in `flex_lexicon` that ARE also present in `flex_from_text`
flex_lexicon1 |> 
  filter(stringi::stri_trans_nfc(lex_entry) %in% 
           stringi::stri_trans_nfc(flex_from_text1$lex_entry))
### A tibble: 1,252 × 10

### identify the number of entries/rows for `lex_entry` in `flex_lexicon` that are NOT present in `flex_from_text`
entry_added_directly_to_the_lexicon <- flex_lexicon1 |> 
  filter(!stringi::stri_trans_nfc(lex_entry) %in% 
           stringi::stri_trans_nfc(flex_from_text1$lex_entry))
entry_added_directly_to_the_lexicon
### A tibble: 310 × 10

### so the total of 310 + 1,252 is 1,562 (this is the total rows in the `flex_lexicon`, which is a match)
nrow(flex_lexicon1)

### identify the number of unique `lex_entry` (in `flex_from_text`) that is also in the `flex_lexicon`
flex_from_text1 |> 
  filter(lex_entry %in% flex_lexicon1$lex_entry) |> 
  select(lex_entry, homonym_id) |> 
  distinct()
### A tibble: 1,176 × 2 (there are 1,176 unique combination of lexical entries and homonym ID in `flex_from_text`; the lexical entries types/forms are also present in `flex_lexicon`)

### identify the number of unique `lex_entry` (in `flex_lexicon`) that is also in the `flex_from_text`
flex_lexicon1 |> 
  filter(stringi::stri_trans_nfc(lex_entry) %in% 
           stringi::stri_trans_nfc(flex_from_text1$lex_entry)) |> 
  select(lex_entry, homonym_id) |> 
  distinct()
### A tibble: 1,195 × 2
### (there are 1,195 unique combination of lexical entries and homonym ID in `flex_lexicon` that matches with lexical entries forms in `flex_from_text`)

#### there are NO UNMATCHED lexical entry between `flex_from_text` and `flex_lexicon` as shown below
flex_from_text1 |> 
  filter(!lex_entry %in% flex_lexicon1$lex_entry)
#### A tibble: 0 × 29

### identify the number of unique `lex_entry` (in `flex_from_text`) that is also in the `flex_lexicon` (for `word_different_from_lexentry`)
word_different_from_lexentry |> 
  filter(lex_entry %in% flex_lexicon1$lex_entry) |> 
  select(lex_entry) |> 
  distinct()
### A tibble: 700 × 1 (there are 700 unique lexical entries in `flex_from_text` [for `word_different_from_lexentry`] also present in the lexical entries in `flex_lexicon`)
### identify the number of unique `lex_entry` (in `flex_from_text`) that is also in the `flex_lexicon` (for `word_different_from_lexentry`)
#### NOW INCLUDING THE HOMONYM ID
word_different_from_lexentry |> 
  filter(lex_entry %in% flex_lexicon1$lex_entry) |> 
  select(lex_entry, homonym_id) |> 
  distinct()
### A tibble: 745 × 2

flex_lexicon1 |> 
  filter(stringi::stri_trans_nfc(lex_entry) %in% 
           stringi::stri_trans_nfc(word_different_from_lexentry$lex_entry)) |> 
  select(lex_entry) |> 
  distinct()
### A tibble: 700 × 1
### (there are 700 unique lexical entries in `flex_lexicon` that matches with lexical entries in `flex_from_text` [for `word_different_from_lexentry`])
### NOW WITH HOMONYM ID
flex_lexicon1 |> 
  filter(stringi::stri_trans_nfc(lex_entry) %in% 
           stringi::stri_trans_nfc(word_different_from_lexentry$lex_entry)) |> 
  select(lex_entry, homonym_id) |> 
  distinct()


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
         phrase_line,
         eno_phrase, eno_phrase_gloss_id, eno_phrase_gloss_eng, text_title) |> 
  distinct()
nrow(lex)

## OLD CODES below - begin ====

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
  # the next line is to turn the word id into the general entry id, especially when the entry of the word has root id (hence complex word and not equal with the lexical entry from the Lexicon)
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
         lex_entry_en = morph_gloss_en,
         lex_entry_id = morph_gloss_id,
         lex_entry_sense_gram = sense_gram,
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

word_table <- lex10 |> 
  select(id, 
         root_word_id, 
         word, 
         root_form = lex_entry, 
         variant_for_root, 
         etymology_for_root, 
         homonym_id, 
         word_class, 
         word_type) |> 
  distinct()

meaning_table <- lex10 |> 
  select(word_id = id, 
         word, 
         word_meaning_idn = indonesian,
         word_meaning_eng = english,
         word_class_of_word = word_class,
         root_word_id,
         root_form = lex_entry,
         homonym_id) |> 
  distinct()

# to_show_to_pak_cok_team1 <- lex10 |>
#   select(word_id = id,
#          word,
#          word_meaning_idn = indonesian,
#          word_meaning_eng = english,
#          word_class_for_word = word_class,
#          word_type,
#          root_word_id,
#          root = lex_entry,
#          etymology = etymology_for_root,
#          root_meaning_order = sense_order,
#          homonym_id_for_root = homonym_id,
#          example_id,
#          example_enggano,
#          example_indonesian,
#          example_english,
#          text_title) |>
#   distinct() |>
#   select(-example_id) |>  # exclude example ID to retrieve distinct example sentences
#   distinct()

to_show_to_pak_cok_team <- lex10 |> 
  rename(word_id = id, 
         word_meaning_idn = indonesian, 
         word_meaning_eng = english, 
         word_class_for_word = word_class, 
         root = lex_entry, 
         etymology = etymology_for_root,
         root_meaning_order = sense_order, 
         homonym_id_for_root = homonym_id) |> 
  distinct() |> 
  select(-example_id) |>  # exclude example ID to retrieve distinct example sentences
  distinct()


# to_show_to_pak_cok_team |> 
  # writexl::write_xlsx(path = "to_show_to_pak_cok_team.xlsx", format_headers = F)



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

## OLD CODES above - end ====

# 4. debugging for the no-root-word-id ==========
no_root_word_id <- readxl::read_xlsx("no_root_word_id.xlsx")
no_root_word_id_rgx <- paste("(", paste(unique(no_root_word_id$root_word_id), collapse = "|"), ")", sep = "")


## TRIAL CODE FOR DEBUGGING =====
#### run the code above from line 2 until line 133

#### Work out later how to capture lexical entry "buh" as prefix and as stem because the prefix and stem morphemes of "buh" are put under the same lexical entry "buh".


lex1 <- lex |> 
  left_join(flex_lexicon1 |> 
              mutate(lexicon_id = entry_id)) |> 
  mutate(root_word_id = "") |> 
  select(root_word_id, 
         entry_id, 
         lexicon_id, 
         eno_word_id, 
         word, eno_word_pos, eno_word_gloss_id, eno_word_gloss_en, 
         lex_entry, homonym_id, sense_order, sense_gram, etym, variant, 
         morph_gloss_en, morph_gloss_id, everything()) |> 
  group_by(lex_entry, homonym_id, sense_order) |> 
  arrange(lex_entry, homonym_id, sense_order) |> 
  ungroup() |> 
  # mutate(entry_id_new = str_c(lex_entry, "_", homonym_id, "_", entry_id, sep = "")) |>
  mutate(entry_id_new = str_c(lex_entry, "_", homonym_id, "_", entry_id, "_sn", sense_order, sep = "")) |>
  select(root_word_id, entry_id_new, entry_id, lexicon_id, eno_word_id, everything())

lex1 |> filter(is.na(entry_id_new))

lex1 <- lex1 |> filter(!is.na(entry_id_new))

lex1 |> filter(is.na(entry_id_new))

lex1 |> filter(is.na(root_word_id))

# lex1 |> filter(lex_entry == "bak") -> baksample

lex2 <- lex1 |> # baksample |> 
  
  # add the root_word_id for complex word (that is not equal with the lex_entry)
  mutate(root_word_id = if_else(!word_equal_lexentry & !morph_type %in% c("root", "suffix", "prefix"),  ## the morph type == "root" is only for "ho="
                                entry_id_new,
                                root_word_id)) |> 
  select(root_word_id, everything())

flex_lexicon2 <- flex_lexicon1 |> 
  # mutate(entry_id_new = str_c(lex_entry, "_", homonym_id, "_", entry_id, sep = "")) |>
  mutate(entry_id_new = str_c(lex_entry, "_", homonym_id, "_", entry_id, "_sn", sense_order, sep = "")) |>
  select(-entry_id) |> 
  rename(id = entry_id_new) |> 
  select(id, everything())


lex3 <- lex2 |> 
  mutate(id = "") |> 
  # the next line is to turn the word id into the general entry id, especially when the entry of the word has root id (hence complex word and not equal with the lexical entry from the Lexicon)
  mutate(id = if_else(root_word_id != "" & !word_equal_lexentry & !morph_type %in% c("root", "suffix", "prefix"),
                      eno_word_id,
                      id)) |> 
  distinct() |> 
  select(id, root_word_id, everything()) |> 
  mutate(id = if_else(root_word_id == "" & word_equal_lexentry & morph_type %in% c("phrase", "stem"),
                      entry_id_new,
                      id)) |> 
  select(-entry_id, -lexicon_id) |> 
  mutate(sources = "texts")


#### processes to get entry whose roots has no independent occurrence
lex3 |> filter(!word_equal_lexentry) -> x

y <- flex_lexicon2 |> 
  filter(id %in% filter(x, !root_word_id %in% lex3$id)$root_word_id) |> 
  mutate(word = lex_entry,
         eno_word_pos = sense_gram,
         eno_word_gloss_en = morph_gloss_en,
         eno_word_gloss_id = morph_gloss_id, 
         sources = "lexicon")

##### the output of the above code can be added to the main database as independent `word` entry.
###### for instance, the root "'a'" 'show' does not appear in its own but only in complex forms.
###### meanwhile the root "'ueh" 'sleep ; lie' can appear independently (e.g., only "'ueh" 'sleep' is attested independently, but not "'ueh" 'lie').

##### checking
lex3 |> bind_rows(y) |> filter(etym == "ahae") |> filter(is.na(root_word_id))
lex3 |> bind_rows(y) |> filter(etym == "pu'u") |> as.data.frame()
lex3 |> bind_rows(y) |> filter(word == "'ueh") |> print(n=Inf)
lex3 |> bind_rows(y) |> filter(lex_entry == "'ueh")

lex4 <- lex3 |> 
  bind_rows(y) |> 
  mutate(word_type = "")

### identify `word` that acts as the root form ====
lex5 <- lex4 |> 
  mutate(word_type = replace(word_type,
                             is.na(root_word_id) & sources == "lexicon",
                             "root")) |> 
  mutate(word_type = replace(word_type,
                             root_word_id == "" &
                               !morph_type %in% c("suffix", "root", "prefix", "phrase") &
                               word_equal_lexentry,
                             "root")) |> 
  mutate(word_type = replace(word_type,
                             root_word_id == "" &
                               morph_type == "phrase" &
                               word_equal_lexentry,
                             "phrase")) |> 
  mutate(word_type = replace(word_type, 
                             morph_type == "phrase" &
                               morph_gloss_id %in% c("berkentut", "arus air"),
                             "root")) |> 
  mutate(word_type = replace(word_type,
                             root_word_id != "" &
                               !morph_type %in% c("prefix", "root", "suffix") &
                               !word_equal_lexentry,
                             "complex word")) |> 
  mutate(word_type = replace(word_type,
                             morph_type == "root",
                             "clitic")) |> 
  mutate(morph_type = replace(morph_type,
                              lex_entry == "-∅" &
                                morph_type == "prefix",
                              "suffix"))

prefix_and_suffix_dbase <- lex5 |> 
  filter(morph_type %in% c("prefix", "suffix"))

wordsdb <- lex5 |> 
  mutate(word_equal_lexentry = replace(word_equal_lexentry,
                                       is.na(root_word_id) &
                                         word == lex_entry,
                                       TRUE)) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  filter(!morph_type %in% c("prefix", "suffix", "root")) |> 
  mutate(ex_eno_removed = if_else(str_detect(eno_phrase, "(^[*#?]|^[*]\\/[#]|\\b(?i)x{2,})", negate = FALSE),
                                  TRUE,
                                  FALSE),
         ex_idn_removed = if_else(str_detect(eno_phrase_gloss_id, "\\b(?i)x{2,}", negate = FALSE),
                                  TRUE,
                                  FALSE),
         ex_eng_removed = if_else(str_detect(eno_phrase_gloss_eng, "\\b(?i)x{2,}", negate = FALSE),
                                  TRUE,
                                  FALSE),
         ex_idn_removed = if_else(ex_eno_removed, TRUE, ex_idn_removed),
         ex_eng_removed = if_else(ex_eno_removed, TRUE, ex_eng_removed))
  # 
  # ### exclude example sentences that contain "*", "?", and/or "#"
  # filter(str_detect(eno_phrase, "(^[*#?]|^[*]\\/[#])", negate = TRUE)) |> 
  # ### exclude example sentences that contain "xxx"
  # filter(str_detect(eno_phrase, "\\b(?i)x{2,}", negate = TRUE)) |> 
  # filter(str_detect(eno_phrase_gloss_eng, "\\b(?i)x{2,}", negate = TRUE)) |>
  # filter(str_detect(eno_phrase_gloss_id, "\\b(?i)x{2,}", negate = TRUE))

##### combine the directly added lexicon with those from the texts

additional_entry <- filter(filter(flex_lexicon2, !lex_entry %in% wordsdb$lex_entry), 
                           lex_entry %in% entry_added_directly_to_the_lexicon$lex_entry)
prefixes <- flex_lexicon2 |> 
  filter(lex_entry %in% prefix_and_suffix_dbase$lex_entry[prefix_and_suffix_dbase$morph_type == "prefix"]) |> 
  filter(lex_entry != "buh") |> 
  mutate(word = lex_entry,
         eno_word_pos = sense_gram,
         eno_word_gloss_en = morph_gloss_en,
         eno_word_gloss_id = morph_gloss_id,
         sources = "lexicon3",
         word_type = if_else(lex_entry == "ho=", "clitic", "prefix"))
suffixes <- flex_lexicon2 |> 
  filter(lex_entry %in% prefix_and_suffix_dbase$lex_entry[prefix_and_suffix_dbase$morph_type == "suffix"]) |> 
  filter(lex_entry != "ha") |> 
  mutate(word = lex_entry,
         eno_word_pos = sense_gram,
         eno_word_gloss_en = morph_gloss_en,
         eno_word_gloss_id = morph_gloss_id,
         sources = "lexicon3",
         word_type = "suffix")

wordsdb1 <- wordsdb |> 
  bind_rows(additional_entry |> 
              mutate(word = lex_entry,
                     eno_word_pos = sense_gram,
                     eno_word_gloss_en = morph_gloss_en,
                     eno_word_gloss_id = morph_gloss_id, 
                     sources = "lexicon2",
                     word_type = "root")) |> 
  bind_rows(prefixes, suffixes)

wordsdb2 <- wordsdb1 |> 
  select(id, 
         root_word_id, 
         word, 
         word_type, 
         word_class = eno_word_pos, 
         lex_entry, 
         variant,
         etym,
         indonesian = eno_word_gloss_id, 
         english = eno_word_gloss_en, 
         homonym_id, 
         sense_order, 
         ex_ID = phrase_id, 
         ex_eno = eno_phrase, 
         ex_idn = eno_phrase_gloss_id, 
         ex_eng = eno_phrase_gloss_eng, 
         text_title, 
         sources,
         ex_eno_removed,
         ex_idn_removed,
         ex_eng_removed) |> 
  distinct()
##### keep only homonym ID for the word that is 'root'
wordsdb3 <- wordsdb2 |> 
  mutate(homonym_id = replace(homonym_id,
                              !word_type  %in%  c("root", "prefix", "suffix"),
                              ""),
         homonym_id = replace(homonym_id,
                              word_type  %in% c("root", "prefix", "suffix") &
                                homonym_id == "0",
                              ""),
         etym = replace(etym,
                        word_type == "complex word",
                        ""))

##### fix the label for word class
wordsdb4 <- wordsdb3 |> 
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
         word_class = if_else(str_detect(sources, "^lexicon") & word_class == "" & word  %in% c(stringi::stri_trans_nfc("ẽp")),
                              "Noun",
                              word_class),
         word_class = if_else(str_detect(sources, "^lexicon") & str_detect(word, "(\\-$|^\\-)") & str_detect(english, "(\\bPOSS\\b|\\bGEN\\b)"),
                              "Noun",
                              word_class),
         word_class = if_else(str_detect(sources, "^lexicon") & str_detect(word, "(\\-$|^\\-)") & str_detect(english, "(\\bPOSS\\b|\\bGEN\\b)"),
                              str_c(word_class, " possessive/genitive ", word_type, sep = ""),
                              word_class),
         word_class = if_else(str_detect(sources, "^lexicon") & english == "CLF" & word_class == "",
                              "Classifier",
                              word_class),
         word_class = if_else(str_detect(sources, "^lexicon") & str_detect(word, "(\\-$|^\\-)") & word_class == "Classifier",
                              str_c(word_class, word_type, sep = " "),
                              word_class),
         
         word_class = if_else(str_detect(sources, "^lexicon") & str_detect(word, "reduplication\\-$") & word_class == "Verb",
                              str_c("Reduplication verbal ", word_type, sep = ""),
                              word_class),
         english = if_else(str_detect(sources, "^lexicon") & word == "reduplication-" & english == "REDUP",
                           "Prefix as reduplication modifying the Tense, Aspect, and Mood (TAM) of the verb",
                           english),
         
         word_class = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & english == "NM",
                              str_c("Nominalising ", word_type, sep = ""),
                              word_class),
         english = if_else(str_detect(word, "(^\\-|\\-$)") &  str_detect(sources, "^lexicon") & word == "e-" & english == "NM",
                           "Nominalisation for action",
                           english),
         
         word_class = if_else(str_detect(word, "(^\\-|\\-$)") &  str_detect(sources, "^lexicon") & english == "DEF",
                              str_c("Definiteness", word_type, sep = " "),
                              word_class),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & english == "DEF",
                           str_c(str_to_sentence(word_type), " for indicating definite noun", sep = ""),
                           english),
         
         word_class = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & str_detect(english, "NMLZ"),
                              str_c("Noun ", word_type, sep = ""),
                              word_class),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & english == "NMLZ",
                           str_c(str_to_sentence(word_type), " for forming noun", sep = ""),
                           english),
         
         word_class = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & english == "REDUP" & word_class == "Noun",
                              "Noun reduplication",
                              word_class),
         english = if_else(str_detect(sources, "^lexicon") & word == "-full reduplication" & english == "REDUP",
                           "Full reduplication on noun modifying the number of the noun",
                           english),
         
         word_class = if_else(str_detect(sources, "^lexicon") & str_detect(word, "(^\\-|\\-$)") & english == "VBLZ",
                              str_c(word_class, " ", word_type, sep = ""),
                              word_class),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & english == "VBLZ",
                           str_c("Verbalising", word_type, sep = " "),
                           english),
         
         english = if_else(str_detect(sources, "^lexicon") & word_type == "suffix" & english == "LOC.NMLZ", 
                           str_c("Suffix forming Locative noun (", english, ")", sep = ""),
                           english),
         english = if_else(str_detect(sources, "^lexicon") & word_type == "suffix" & english == "PAT.NMLZ", 
                           str_c("Suffix forming Patient noun (", english, ")", sep = ""),
                           english),
         
         word_class = if_else(str_detect(sources, "^lexicon") & str_detect(word, "(^\\-|\\-$)") & word %in% c("aba-", "bu-", "-∅"),
                              str_c("Verb ", word_type, sep = ""),
                              word_class),
         
         word_class = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & str_detect(english, "[0-9]") & word_class == "Verb",
                              str_c(word_class, " subject-agreement ", word_type, sep = ""),
                              word_class),
         
         word_class = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & english == "HUM",
                              str_c(word_class, word_type, sep = " "),
                              word_class),
         
         english = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & english == "HUM",
                           str_c(str_to_sentence(word_type), " for human noun", sep = ""),
                           english)) |> 
  
  mutate(english = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & str_detect(english, "\\bPOSS\\b") & str_detect(english, "SG\\b"),
                           str_replace(english, "SG\\b\\.", " singular "),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & str_detect(english, "\\bPOSS\\b") & str_detect(english, "PL\\b"),
                           str_replace(english, "PL\\b\\.", " plural "),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & str_detect(english, "\\bPOSS\\b"),
                           str_replace(english, "\\bPOSS\\b", "possessive"),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & str_detect(english, "[0-9]PL\\b"),
                           str_replace(english, "([0-9])PL\\b", "\\1 plural"),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & str_detect(english, "\\.?EXCL\\.?"),
                           str_replace(english, "\\.?EXCL\\.?", " exclusive "),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & str_detect(english, "^[1]"),
                           str_replace(english, "^[1]", "First (1st) person "),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & str_detect(english, "^[2]"),
                           str_replace(english, "^[2]", "Second (2nd) person "),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & str_detect(english, "^[3]"),
                           str_replace(english, "^[3]", "Third (3rd) person "),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & str_detect(english, "^IMP\\.2"),
                           str_replace(english, "^IMP\\.2", "Imperative form for the second (2nd) person "),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & str_detect(english, "SG$"),
                           str_replace(english, "SG$", "singular"),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & str_detect(english, "^PL$"),
                           str_replace(english, "^PL$", "Plural"),
                           english),
         
         word_class = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & str_detect(english, "^FUT$"),
                              str_c("Verbal suffix for future tense"),
                              word_class),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & str_detect(english, "^FUT$"),
                           str_replace(english, "^FUT$", "Future tense/volitional marker"),
                           english),
         
         
         english = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & str_detect(english, "^APPL$"),
                           str_replace(english, "^APPL$", "Applicative marker"),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & str_detect(english, "^PASS$"),
                           str_replace(english, "^PASS$", "Passive marker"),
                           english),
         english = if_else(str_detect(word, "(^\\-|\\-$)") & str_detect(sources, "^lexicon") & str_detect(english, "^CLF$"),
                           str_replace(english, "^CLF$", "Classifier"),
                           english)) |> 
  
  mutate(word_class = replace(word_class, english == "ANTIP", "Verb prefix"),
         english = replace(english, english == "ANTIP", "Antipassive marker"),
         
         word_class = replace(word_class, english == "BA", "Verb prefix"),
         
         english = replace(english, english == "PERF" & word == "ho=",
                           "Perfective marker"),
         
         word_class = replace(word_class, word == "hi-" & english == "REPEAT", 
                              "Verb prefix"),
         english = replace(english, word == "hi-" & english == "REPEAT",
                           "Prefix marking repeated action"),
         
         word_class = replace(word_class, english %in% c("KI", "RECIP", "CAUS", "PA"),
                              "Verb prefix"),
         word_class = replace(word_class, english == "Passive marker" & word_type == "prefix" & word == "di-", "Verb prefix"),
         english = replace(english, english == "ABA" & word_type == "prefix",
                           "The *aba*- prefix marks consecutive clauses in chained events, expressing the meaning of 'go and do something'. Thus, *aba*- marked verbs often occur as the second predicate in multi-verb constructions or clause chaining."),
         english = replace(english, english == "BA" & word_type == "prefix",
                           "The *ba*- prefix is related to *aba*- in form and function but, unlike *aba*-, *ba*- verbs do not take subject agreement markers."),
         english = replace(english, english == "CAUS" & word == "pa-",
                           "Causative prefix when attached to intransitive verb stems"),
         english = replace(english, english == "RECIP" & word == "pa-",
                           "Reciprocal and related middle functions"),
         english = replace(english, word_type == "prefix" & english == "GEN",
                           "Genitive or Possessive prefix"),
         english = if_else(str_detect(english, "\\.INCL") & word_type == "prefix",
                           str_replace(english, "\\.INCL", " inclusive"),
                           english),
         english = if_else(str_detect(english, "INCL\\.") & word_type == "suffix",
                           str_replace(english, "INCL\\.", "inclusive "),
                           english),
         word_class = replace(word_class, english %in% c("Applicative marker", "Plural") & word_type == "suffix",
                              "Verb suffix"),
         english = str_replace_all(english, "\\s+", " "),
         english = str_trim(english, side = "both")
         )

##### keep sense order for the ROOT for now (based on sense order in LEXICON)
wordsdb5 <- wordsdb4 |> 
  mutate(sense_order = replace(sense_order,
                               !word_type %in% c("root", "prefix", "suffix"),
                               ""),
         sense_order = replace(sense_order,
                               sense_order == "2",
                               "3"),
         sense_order = replace(sense_order,
                               sense_order == "1",
                               "2"),
         sense_order = replace(sense_order,
                               sense_order == "0",
                               "1")) |> 
  mutate(across(where(is.character), ~str_trim(., side = "both"))) |> 
  
  #### replace the first lower case letter into upper case in the example sentences
  
  mutate(ex_idn = if_else(ex_idn != "" & str_detect(ex_idn, "^[[:lower:]]"),
                          gsub("^([[:lower:]])", "\\U\\1", ex_idn, perl = TRUE),
                          ex_idn),
         ex_eng = if_else(ex_eng != "" & str_detect(ex_eng, "^[[:lower:]]"),
                          gsub("^([[:lower:]])", "\\U\\1", ex_eng, perl = TRUE),
                          ex_eng))

example_to_translate_into_english <- wordsdb5 |> 
  filter(ex_idn != "", ex_eng == "") |> 
  select(ex_ID, ex_eno, ex_idn, ex_eng) |> 
  distinct()
example_to_translate_into_indonesian <- wordsdb5 |> 
  filter(ex_idn == "", ex_eng != "") |> 
  select(ex_ID, ex_eno, ex_idn, ex_eng) |> 
  distinct()
ex_idn_to_translate <- example_to_translate_into_english |> 
  select(ex_idn) |> 
  distinct()
ex_eng_to_translate <- example_to_translate_into_indonesian |> 
  select(ex_eng) |> 
  distinct()
# get the DeepL API key from Kahler repo code file 0-source...
## now translate the Indonesian translation of the Example into English
# ex_idn_to_translate <- ex_idn_to_translate |> 
#   mutate(ex_eng_deepl = deeplr::translate2(text = ex_idn,
#                                            target_lang = "EN",
#                                            source_lang = "ID",
#                                            split_sentences = FALSE,
#                                            preserve_formatting = TRUE,
#                                            auth_key = deeplkey))
# ex_idn_to_translate |> writexl::write_xlsx("ex_idn_to_translate.xlsx")

## now translate the English translation of the Example into Indonesian
# ex_eng_to_translate <- ex_eng_to_translate |>
#   mutate(ex_idn_deepl = deeplr::translate2(text = ex_eng,
#                                            target_lang = "ID",
#                                            source_lang = "EN",
#                                            split_sentences = FALSE,
#                                            preserve_formatting = TRUE,
#                                            auth_key = deeplkey))
# ex_eng_to_translate |> writexl::write_xlsx("ex_eng_to_translate.xlsx")

ex_idn_to_translate <- readxl::read_xlsx("ex_idn_to_translate.xlsx")
ex_eng_to_translate <- readxl::read_xlsx("ex_eng_to_translate.xlsx")

example_to_translate_into_english <- example_to_translate_into_english |> 
  left_join(ex_idn_to_translate)
example_to_translate_into_indonesian <- example_to_translate_into_indonesian |> 
  left_join(ex_eng_to_translate)

wordsdb6 <- wordsdb5 |> 
  left_join(example_to_translate_into_english) |> 
  left_join(example_to_translate_into_indonesian) |> 
  mutate(ex_eng = if_else(ex_idn != "" & ex_eng == "",
                          ex_eng_deepl,
                          ex_eng)) |> 
  mutate(ex_idn = if_else(ex_idn == "" & ex_eng != "",
                          ex_idn_deepl,
                          ex_idn)) |> 
  select(-ex_eng_deepl, -ex_idn_deepl) |> 
  ### exclude example sentences containing the "FOR" in the English translation
  filter(str_detect(ex_eng, "^FOR", negate = TRUE))

to_show_pak_cok_team <- wordsdb6 |> 
  rename(word_id = id, 
         root = lex_entry, 
         root_variant_form = variant, 
         root_etymology = etym, 
         root_meaning_order = sense_order, 
         root_homonym_id = homonym_id) |> 
  mutate(ex_eno_removed = if_else(ex_idn == "" & ex_eng == "",
                                  TRUE,
                                  ex_eno_removed),
         ex_idn_removed = if_else(ex_eno_removed,
                                  TRUE,
                                  ex_idn_removed),
         ex_eng_removed = if_else(ex_eno_removed,
                                  TRUE,
                                  ex_eng_removed)) |> 
  # remove number entries
  filter(str_detect(word, "^[0-9]+$", negate = TRUE))


## save entries marked with Indonesian loan to be manually coded which entry of Indonesian loan to include
# to_show_pak_cok_team |> 
#   count(word, english, indonesian, root_etymology, sort = TRUE) |> 
#   filter(str_detect(root_etymology, "Indones", FALSE)) |> 
#   writexl::write_xlsx("indonesian_loan.xlsx")

## load back the entries with Indonesian loan to be used to filtered out the relevant Indonesian loan
idn_loan_excl <- readxl::read_xlsx("indonesian_loan.xlsx") |> 
  filter(excluded == "y")


# Fix the Indonesian loan into just loanwords, but first, exclude the excluded loanwords
to_show_pak_cok_team <- to_show_pak_cok_team |> 
  
  # exclude loan word
  filter(!word %in% idn_loan_excl$word) |> 
  
  mutate(root_etymology = replace(root_etymology,
                                  str_detect(root_etymology, "Indonesian"),
                                  "loanwords"))

# get the total entries for the FLEx database for EnoLEX proceeding to be put in Table 1.
# the total entries here are unique combination of the word column (which captures root form and complex form), root column, and homonym_id
to_show_pak_cok_team |> 
  select(word, root, root_homonym_id) |> 
  distinct() |> 
  nrow()

# BELOW IS THE CODE TO SAVE THE DATABASE TO GOOGLE SHEET TO SHARE TO PAK COK's TEAM
# googlesheets4::write_sheet(data = to_show_pak_cok_team, ss = "1QHUeq-a1Nn_knlmT1J8cTneVoDExmV7wngqoAl6feVE", sheet = "to_show_pak_cok_team_new")


root_id <- unique(to_show_pak_cok_team$word_id)
length(str_subset(root_id, no_root_word_id_rgx)) == length(unique(no_root_word_id$root_word_id))


















# googledrive::drive_ls(googledrive::as_id("1JIuY52VL6SsQdFkoC7VVgLo_JratQ0x1"))
# googledrive::drive_create("flex-output-for-pak-cok-team", path = googledrive::as_id("1JIuY52VL6SsQdFkoC7VVgLo_JratQ0x1"), type = "spreadsheet")

# Created Drive file:
#   • flex-output-for-pak-cok-team <id: 1QHUeq-a1Nn_knlmT1J8cTneVoDExmV7wngqoAl6feVE>
#   With MIME type:
#   • application/vnd.google-apps.spreadsheet