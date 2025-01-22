### A new code file for processing into web- and mobile-based dictionary
### December 31st, 2024

lex1 <- flex_lexicon1 |> 
  mutate(lexicon_id = entry_id,
         lex_entry = stri_trans_nfc(lex_entry)) |> 
  left_join(lex |> mutate(lex_entry = stri_trans_nfc(lex_entry))) |> 
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

lex1 |> filter(is.na(morph_gloss_id))
lex1 |> filter(is.na(morph_gloss_en))

lex_entry_in_lexicon_without_words_in_texts <- filter(lex1, is.na(word))

lex1 <- lex1 |> filter(!is.na(entry_id_new))

lex2 <- lex1 |> # baksample |> 
  
  # add morph type for NAs morph type after left join where the .lift content is on the left (x) while the .flextext is on the right (y)
  mutate(morph_type = replace(morph_type,
                              is.na(morph_type) & is.na(word) &
                                str_detect(lex_entry, "^\\="),
                              "enclitic"),
         morph_type = replace(morph_type,
                              is.na(morph_type) & is.na(word) &
                                str_detect(lex_entry, "\\=$"),
                              "proclitic"),
         morph_type = replace(morph_type,
                              is.na(morph_type) & is.na(word) &
                                str_detect(lex_entry, "^\\-"),
                              "suffix"),
         morph_type = replace(morph_type,
                              is.na(morph_type) & is.na(word) &
                                str_detect(lex_entry, "\\-$"),
                              "prefix"),
         morph_type = replace(morph_type,
                              is.na(morph_type) & is.na(word) &
                                str_detect(lex_entry, " "),
                              "phrase"),
         morph_type = replace(morph_type,
                              is.na(morph_type) & is.na(word) &
                                str_detect(lex_entry, " ", negate = TRUE),
                              "stem")) |> 
  # add complex_word info for NAs complex_word after left join where the .lift content is on the left (x) while the .flextext is on the right (y)
  mutate(complex_word = replace(complex_word,
                                is.na(complex_word) & is.na(word) &
                                  str_detect(lex_entry, " "),
                                TRUE),
         complex_word = replace(complex_word,
                                is.na(complex_word) & is.na(word) &
                                  str_detect(lex_entry, " ", negate = TRUE),
                                FALSE),
         is_variant = replace(is_variant,
                              is.na(is_variant)  & is.na(word) ,
                              FALSE),
         word_equal_lexentry = replace(word_equal_lexentry,
                                       is.na(word_equal_lexentry) & is.na(word),
                                       TRUE),
         morph_equal_lexentry = replace(morph_equal_lexentry,
                                        is.na(morph_equal_lexentry) & is.na(word),
                                        TRUE),
         lexentry_equal_phrase = replace(lexentry_equal_phrase,
                                         is.na(lexentry_equal_phrase) & is.na(word),
                                         FALSE)) |> 
  
  # add value for the root_word_id for complex word (that is word not equal with the lex_entry) and for morpheme that is only stem and phrase (i.e., not root, suffix, prefix)
  mutate(root_word_id = if_else(!word_equal_lexentry & complex_word & !morph_type %in% c("root", "suffix", "prefix"),  ## the morph type == "root" is only for "ho="
                                entry_id_new,
                                root_word_id)) |> 
  
  select(root_word_id, everything()) |> 
  
  # give the NA eno_word_id the same id as the entry_id_new
  mutate(eno_word_id = if_else(is.na(eno_word_id),
                               entry_id_new,
                               eno_word_id)) |> 
  # give the NA word the same content from the lex_entry (this is the lex_entry directly added to the LEXICON but not appearing in TEXTS)
  mutate(word = if_else(is.na(word) & !is.na(lex_entry),
                        lex_entry,
                        word),
         eno_word_pos = if_else(is.na(eno_word_pos) & !is.na(sense_gram),
                        sense_gram,
                        eno_word_pos),
         eno_word_gloss_id = if_else(is.na(eno_word_gloss_id) & !is.na(morph_gloss_id),
                                morph_gloss_id,
                                eno_word_gloss_id),
         eno_word_gloss_en = if_else(is.na(eno_word_gloss_en) & !is.na(morph_gloss_en),
                                     morph_gloss_en,
                                     eno_word_gloss_en))

flex_lexicon2 <- flex_lexicon1 |> 
  # mutate(entry_id_new = str_c(lex_entry, "_", homonym_id, "_", entry_id, sep = "")) |>
  mutate(entry_id_new = str_c(lex_entry, "_", homonym_id, "_", entry_id, "_sn", sense_order, sep = "")) |>
  select(-entry_id) |> 
  rename(id = entry_id_new) |> 
  select(id, everything())


lex3 <- lex2 |> 
  mutate(id = "") |> 
  # the next line is to turn the word id (i.e., eno_word_id) into the general entry id, especially when the entry of the word has root id (hence complex word and not equal with the lexical entry from the Lexicon)
  mutate(id = if_else(root_word_id != "" & !word_equal_lexentry & complex_word & !morph_type %in% c("root", "suffix", "prefix"),
                      eno_word_id,
                      id)) |> 
  distinct() |> 
  select(id, root_word_id, everything()) |> 
  mutate(id = if_else(root_word_id == "" & word_equal_lexentry & morph_type %in% c("phrase", "stem"),
                      entry_id_new,
                      id)) |> 
  select(-entry_id, -lexicon_id) |> 
  mutate(sources = if_else(is.na(text_title),
                           "lexicon",
                           "texts"))

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
  mutate(word_type = "") |> 
  # add morph type for NAs morph type after left join where the .lift content is on the left (x) while the .flextext is on the right (y)
  mutate(morph_type = replace(morph_type,
                              is.na(morph_type) &
                                str_detect(lex_entry, "^\\="),
                              "enclitic"),
         morph_type = replace(morph_type,
                              is.na(morph_type) &
                                str_detect(lex_entry, "\\=$"),
                              "proclitic"),
         morph_type = replace(morph_type,
                              is.na(morph_type) &
                                str_detect(lex_entry, "^\\-"),
                              "suffix"),
         morph_type = replace(morph_type,
                              is.na(morph_type) &
                                str_detect(lex_entry, "\\-$"),
                              "prefix"),
         morph_type = replace(morph_type,
                              is.na(morph_type) &
                                str_detect(lex_entry, " "),
                              "phrase"),
         morph_type = replace(morph_type,
                              is.na(morph_type) &
                                str_detect(lex_entry, " ", negate = TRUE),
                              "stem")) |> 
  # add complex_word info for NAs complex_word after left join where the .lift content is on the left (x) while the .flextext is on the right (y)
  mutate(complex_word = replace(complex_word,
                                is.na(complex_word) &
                                  str_detect(lex_entry, " "),
                                TRUE),
         complex_word = replace(complex_word,
                                is.na(complex_word) &
                                  str_detect(lex_entry, " ", negate = TRUE),
                                FALSE),
         word_equal_lexentry = replace(word_equal_lexentry,
                                       is.na(word_equal_lexentry) & stri_trans_nfc(word) == stri_trans_nfc(lex_entry),
                                       TRUE),
         morph_equal_lexentry = replace(morph_equal_lexentry,
                                        is.na(morph_equal_lexentry),
                                        TRUE),
         lexentry_equal_phrase = replace(lexentry_equal_phrase,
                                         is.na(lexentry_equal_phrase),
                                         FALSE),
         is_variant = replace(is_variant,
                              is.na(is_variant),
                              FALSE))

### identify `word` that acts as the root form ====
lex5 <- lex4 |> 
  mutate(word_type = replace(word_type,
                             is.na(root_word_id) & sources == "lexicon",
                             "root")) |>
  mutate(word_type = replace(word_type,
                             str_detect(morph_type, "clitic"),
                             "clitic")) |> 
  mutate(word_type = replace(word_type,
                             morph_type == "prefix",
                             "prefix")) |> 
  mutate(word_type = replace(word_type,
                             morph_type == "suffix",
                             "suffix")) |> 
  mutate(morph_type = replace(morph_type,
                              lex_entry == "-∅" &
                                morph_type == "prefix",
                              "suffix")) |> 
  mutate(word_type = replace(word_type,
                             root_word_id == "" &
                               !morph_type %in% c("suffix", "root", "prefix", "phrase", "enclitic", "proclitic") &
                               word_equal_lexentry,
                             "root")) |> 
  mutate(word_type = replace(word_type,
                             root_word_id == "" &
                               morph_type == "phrase" &
                               word_equal_lexentry,
                             "phrase")) |> 
  mutate(word_type = replace(word_type,
                             root_word_id == "" &
                               morph_type == "phrase" &
                               !word_equal_lexentry,
                             "root")) |> 
  # mutate(word_type = replace(word_type, 
  #                            morph_type == "phrase" &
  #                              morph_gloss_id %in% c("berkentut", "arus air"),
  #                            "root")) |> 
  mutate(word_type = replace(word_type,
                             root_word_id != "" &
                               !morph_type %in% c("prefix", "root", "suffix", "enclitic", "proclitic") &
                               !word_equal_lexentry &
                               complex_word,
                             "complex word")) |>
  mutate(word_type = replace(word_type,
                             is_variant & morph_type == "stem",
                             "root"),
         word_type = replace(word_type,
                             word_type == "" & morph_type == "stem" & !complex_word & !is_variant,
                             "root"))

prefix_and_suffix_dbase <- lex5 |> 
  filter(morph_type %in% c("prefix", "suffix"))

wordsdb <- lex5 |> 
  mutate(word_equal_lexentry = replace(word_equal_lexentry,
                                       is.na(root_word_id) &
                                         stri_trans_nfc(word) == stri_trans_nfc(lex_entry),
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
         word_type = if_else(str_detect(lex_entry, "\\=$"), "clitic", "prefix"))
suffixes <- flex_lexicon2 |> 
  filter(lex_entry %in% prefix_and_suffix_dbase$lex_entry[prefix_and_suffix_dbase$morph_type == "suffix"]) |> 
  filter(lex_entry != "ha") |> 
  mutate(word = lex_entry,
         eno_word_pos = sense_gram,
         eno_word_gloss_en = morph_gloss_en,
         eno_word_gloss_id = morph_gloss_id,
         sources = "lexicon3",
         word_type = if_else(str_detect(lex_entry, "^\\="), "clitic", "suffix"))

wordsdb1 <- wordsdb |> 
  # bind_rows(additional_entry |> 
  #             mutate(word = lex_entry,
  #                    eno_word_pos = sense_gram,
  #                    eno_word_gloss_en = morph_gloss_en,
  #                    eno_word_gloss_id = morph_gloss_id, 
  #                    sources = "lexicon2",
  #                    word_type = "root")) |> 
  mutate(exclude = if_else(str_detect(lex_entry, "\\=") & complex_word, TRUE, FALSE)) |> # exclude lex entry where it is clitic and the word is complex word
  filter(!exclude) |> 
  bind_rows(prefixes, suffixes) |> 
  mutate(eno_word_pos = replace(eno_word_pos, eno_word_pos == "" & word == "yaham" & text_title == "Verbal Morphology 04", "n"),
         eno_word_pos = replace(eno_word_pos, eno_word_pos == "" & word == "ubuh" & text_title == "30.Kelapa Kopra", "v"),
         eno_word_pos = replace(eno_word_pos, eno_word_pos == "" & word == stri_trans_nfc("ẽp"), "Noun")) |> 
  mutate(imperative = replace(imperative, is.na(imperative) & sources %in% c("lexicon", "lexicon3"), FALSE)) |> 
  mutate(complex_word = replace(complex_word, is.na(complex_word) & sources %in% c("lexicon", "lexicon3"), FALSE),
         is_variant = replace(is_variant, is.na(is_variant) & sources %in% c("lexicon", "lexicon3"), FALSE),
         is_variant = replace(is_variant, is_variant & word_equal_lexentry, FALSE),
         is_variant = replace(is_variant, word_type == "root" & !word_equal_lexentry & !is_variant, TRUE),
         word_equal_lexentry = if_else(is.na(word_equal_lexentry) & word == lex_entry, TRUE, word_equal_lexentry)) |> 
  
  # change imperative structure into lower case word
  mutate(word = if_else(imperative, str_to_lower(word), word)) |> 
  
  # handle the variant
  mutate(id = if_else(is_variant & !word_equal_lexentry & root_word_id == "", eno_word_id, id),
         root_word_id = if_else(is_variant & !word_equal_lexentry & root_word_id == "", entry_id_new, root_word_id)) |> 
  
  # handle the glossing of word that is equal with lex_entry but has different `eno_word_gloss_...` and `morph_gloss_...`
  mutate(eno_word_gloss_id = if_else(word_equal_lexentry & eno_word_gloss_id != morph_gloss_id, morph_gloss_id, eno_word_gloss_id),
         eno_word_gloss_en = if_else(word_equal_lexentry & eno_word_gloss_en != morph_gloss_en, morph_gloss_en, eno_word_gloss_en)) |> 
  
  mutate(across(matches("root_word_id|entry_id_new|eno_word_id"), ~replace_na(., ""))) |> 
  
  # handling the empty IDs for =a, =de, and a=
  mutate(id = if_else(id == "" & word == "a=" & lex_entry == "a=" & morph_gloss_en %in% c("if", "when"), eno_word_id, id),
         id = if_else(id == "" & word == "de" & lex_entry  %in% c("=de") & eno_word_gloss_id == "(milik)nya", eno_word_id, id),
         id = if_else(id == "" & word == "a" & lex_entry  %in% c("=a"), eno_word_id, id),
         root_word_id = if_else(root_word_id == "" & word == "a" & lex_entry  %in% c("=a", "=de"), entry_id_new, root_word_id)) |> 
  
  # handling Sentence case word
  mutate(word = if_else(is_variant & eno_word_pos != "nprop",
                        str_to_lower(word),
                        word)) |> 
  
  # handling the variant relation
  mutate(variant_relation = "",
         variant_relation = if_else(is_variant,
                                    str_c('"', word, '" adalah varian dari "', lex_entry, '"', sep = ""),
                                    variant_relation))
  
  
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
         ex_eng_removed,
         imperative,
         complex_word,
         is_variant,
         variant_relation,
         word_equal_lexentry,
         morph_equal_lexentry,
         lexentry_equal_phrase) |> 
  distinct()
##### keep only homonym ID for the word that is 'root'
wordsdb3 <- wordsdb2 |> 
  mutate(homonym_id = replace(homonym_id,
                              !word_type  %in%  c("root", "prefix", "suffix", "clitic"),
                              ""),
         homonym_id = replace(homonym_id,
                              word_type  %in% c("root", "prefix", "suffix", "clitic") &
                                homonym_id == "0",
                              ""),
         etym = replace(etym,
                        complex_word,
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
                               !word_type %in% c("root", "prefix", "suffix", "clitic"),
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
# ex_idn_to_translate |> writexl::write_xlsx("input/contemporary/web-and-app/ex_idn_to_translate.xlsx")
# ex_idn_to_translate |> writexl::write_xlsx("input/contemporary/web-and-app/ex_idn_to_translate-2025.xlsx")

## now translate the English translation of the Example into Indonesian
# ex_eng_to_translate <- ex_eng_to_translate |>
#   mutate(ex_idn_deepl = deeplr::translate2(text = ex_eng,
#                                            target_lang = "ID",
#                                            source_lang = "EN",
#                                            split_sentences = FALSE,
#                                            preserve_formatting = TRUE,
#                                            auth_key = deeplkey))
# ex_eng_to_translate |> writexl::write_xlsx("input/contemporary/web-and-app/ex_eng_to_translate.xlsx")
# ex_eng_to_translate |> writexl::write_xlsx("input/contemporary/web-and-app/ex_eng_to_translate-2025.xlsx")

# ex_idn_to_translate <- readxl::read_xlsx("input/contemporary/web-and-app/ex_idn_to_translate.xlsx")
ex_idn_to_translate <- readxl::read_xlsx("input/contemporary/web-and-app/ex_idn_to_translate-2025.xlsx")
# ex_eng_to_translate <- readxl::read_xlsx("input/contemporary/web-and-app/ex_eng_to_translate.xlsx")
ex_eng_to_translate <- readxl::read_xlsx("input/contemporary/web-and-app/ex_eng_to_translate-2025.xlsx")

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
#   writexl::write_xlsx("input/contemporary/web-and-app/indonesian_loan.xlsx")

## load back the entries with Indonesian loan to be used to filtered out the relevant Indonesian loan
idn_loan_excl <- readxl::read_xlsx("input/contemporary/web-and-app/indonesian_loan.xlsx") |> 
  filter(excluded == "y")


cullex_ge1_complex1 <- cullex_ge1_complex |> 
  filter(ID != 156)
cullex_ge1_root
cullex_ge0_root1 <- cullex_ge0_root |> 
  mutate(across(matches("(word|lx)$"), ~str_replace(., "u̇'i", stri_trans_nfc ("ė'ėi"))))

# Fix the Indonesian loan into just loanwords, but first, exclude the excluded loanwords
to_show_pak_cok_team <- to_show_pak_cok_team |> 
  
  # join cultural items images
  left_join(cullex_ge1_complex1 |> select(word, file_basename, pc)) |> 
  
  # exclude loan word
  filter(!word %in% idn_loan_excl$word) # |> 

# mutate(root_etymology = replace(root_etymology,
#                                 str_detect(root_etymology, "Indonesian"),
#                                 "loanwords"))
myfilters <- to_show_pak_cok_team$word == cullex_ge1_root$word[1] & to_show_pak_cok_team$indonesian == cullex_ge1_root$gn_1[1]
to_show_pak_cok_team$pc[myfilters] <- cullex_ge1_root$pc[1]
to_show_pak_cok_team$file_basename[myfilters] <- cullex_ge1_root$file_basename[1]

myfilters <- to_show_pak_cok_team$word == cullex_ge1_root$word[2] & to_show_pak_cok_team$indonesian == cullex_ge1_root$gn_1[2]
to_show_pak_cok_team$pc[myfilters] <- cullex_ge1_root$pc[2]
to_show_pak_cok_team$file_basename[myfilters] <- cullex_ge1_root$file_basename[2]

to_show_pak_cok_team <- to_show_pak_cok_team |> 
  left_join(cullex_ge0_complex |> select(word, file_basename1 = file_basename, pc1 = pc)) |> 
  mutate(pc = if_else(!is.na(pc1), pc1, pc),
         file_basename = if_else(!is.na(file_basename1), file_basename1, file_basename)) |> 
  select(-file_basename1, -pc1) |> 
  left_join(cullex_ge0_root1 |> select(word, english = ge_0, fb1 = file_basename, pc1 = pc)) |> 
  mutate(pc = if_else(!is.na(pc1), pc1, pc),
         file_basename = if_else(!is.na(fb1), fb1, file_basename)) |> 
  select(-fb1, -pc1) |> 
  mutate(file_basename = replace_na(file_basename, ""),
         pc = replace_na(pc, ""))
# get the total entries for the FLEx database for EnoLEX proceeding to be put in Table 1.
# the total entries here are unique combination of the word column (which captures root form and complex form), root column, and homonym_id
to_show_pak_cok_team |> 
  select(word, root, root_homonym_id) |> 
  distinct() |> 
  nrow()

# BELOW IS THE CODE TO SAVE THE DATABASE TO GOOGLE SHEET TO SHARE TO PAK COK's TEAM
# googlesheets4::write_sheet(data = to_show_pak_cok_team, ss = "1QHUeq-a1Nn_knlmT1J8cTneVoDExmV7wngqoAl6feVE", sheet = "to_show_pak_cok_team_new")
# googlesheets4::write_sheet(data = to_show_pak_cok_team, ss = "1QHUeq-a1Nn_knlmT1J8cTneVoDExmV7wngqoAl6feVE", sheet = "to_show_pak_cok_team_20241230")
# googlesheets4::write_sheet(data = to_show_pak_cok_team, ss = "1QHUeq-a1Nn_knlmT1J8cTneVoDExmV7wngqoAl6feVE", sheet = "to_show_pak_cok_team_20250105")
googlesheets4::write_sheet(data = to_show_pak_cok_team, ss = "1QHUeq-a1Nn_knlmT1J8cTneVoDExmV7wngqoAl6feVE", sheet = "to_show_pak_cok_team_20250122")


root_id <- unique(to_show_pak_cok_team$word_id)
length(str_subset(root_id, no_root_word_id_rgx)) == length(unique(no_root_word_id$root_word_id))

















## The code bits to create the Google Spreadsheet file for the output for Pak Cok's team ===========
# googledrive::drive_ls(googledrive::as_id("1JIuY52VL6SsQdFkoC7VVgLo_JratQ0x1"))
# googledrive::drive_create("flex-output-for-pak-cok-team", path = googledrive::as_id("1JIuY52VL6SsQdFkoC7VVgLo_JratQ0x1"), type = "spreadsheet")

# Created Drive file:
#   • flex-output-for-pak-cok-team <id: 1QHUeq-a1Nn_knlmT1J8cTneVoDExmV7wngqoAl6feVE>
#   With MIME type:
#   • application/vnd.google-apps.spreadsheet
