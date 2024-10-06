# Code to process the .flextext export of the analyzed interlinear text
# Gede Rajeg (University of Oxford & Universitas Udayana; 2023)

library(tidyverse)
library(xml2)

# A. Natural texts ==============

# xml_file <- dir("contemporary-enggano-interlinear-text/", 
#                 pattern = "all-contemp-texts", 
#                 full.names = TRUE)

## read-in the .flextext export of the analyzed interlinear texts =====
xml_file <- dir("contemporary-enggano-interlinear-text/", 
                pattern = "all-contemp-texts-with-idn-gloss", 
                full.names = TRUE) |> 
  # get the March Meeting Version 2024
  str_subset("march2024")


## retrieve the interlinear-text node =====
interlinear_text <- xml_file |> 
  read_xml() |> 
  xml_find_all("interlinear-text")

## retrieve the title of each text from the title attribute =====
title <- interlinear_text |> 
  xml_find_all("item[@type=\"title\" and @lang=\"eno\"]") |> 
  xml_text() |> 
  str_replace_all("^(\\d+)[^A-Za-z]+", "\\1_")

## retrieve the paragraph node =====
paragraphs <- xml_find_all(interlinear_text, "paragraphs")

## retrieve the phrases node =====
## the phrases node contains the baseline original text of the target lang.
phrases <- paragraphs |> 
  map(xml_find_all, ".//phrases")

# phrases <- phrases[i]

## 1. Gathering the original texts (under the <phrases> node) ======

### get the Enggano text segment/node
eno_text <- phrases |> 
  map(xml_find_all, "phrase/item[@type=\"txt\" and @lang=\"eno\"]") |> 
  map(xml_text)

## get the ID of the Enggano text segment/node
eno_text_id <- phrases |> 
  map(xml_find_all, "phrase") |> 
  map(xml_attr, "guid")

## get the segment/text number (i.e., line number in the baseline text)
eno_segnum <- phrases |> 
  map(xml_find_all, "phrase/item[@type=\"segnum\" and @lang=\"en\"]") |> 
  map(xml_text)

## get the English free translation of the segment/text
eno_text_gloss_eng <- phrases |> 
  map(~map(., ~xml_find_all(., "phrase/item[@type=\"gls\" and @lang=\"en\"]"))) |> 
  map(~map(., ~xml_text(.))) |> 
  # map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
  # map(~map(., \(x) if(!is.na(x) & nchar(x) == 0) NA else x)) |> 
  map(unlist) |> 
  map(\(x) replace(x, nchar(x) == 0, NA))

## get the Indonesian free translation of the segment/text
eno_text_gloss_idn <- phrases |> 
  map(~map(., ~xml_find_all(., "phrase/item[@type=\"gls\" and @lang=\"id\"]"))) |> 
  map(~map(., ~xml_text(.))) |> 
  # map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
  # map(~map(., \(x) if(!is.na(x) & nchar(x) == 0) NA else x)) |> 
  map(unlist) |> 
  map(\(x) replace(x, nchar(x) == 0, NA))

## turn the above data into a tibble for each text title
eno_text_gloss_df <- pmap(list(a = eno_text_id, b = eno_segnum,
                               c = eno_text,
                               d = eno_text_gloss_idn,
                               e = eno_text_gloss_eng),
                          \(a, b, c, d, e) 
                          tibble(phrase_id = a, 
                                 phrase_line = b,
                                 eno_phrase = c, 
                                 eno_phrase_gloss_id = d, 
                                 eno_phrase_gloss_eng = e)) |> 
  map2(.y = title, ~mutate(., text_title = .y))

## combine the tibbles list from all texts into one tibble
eno_text_gloss_df_all <- list_rbind(eno_text_gloss_df)
eno_text_gloss_df |> 
  # write_rds("contemporary-enggano-interlinear-text/eno_contemp_text_only_as_tibble-new.rds")
  write_rds("contemporary-enggano-interlinear-text/eno_contemp_text_only_as_tibble-new-march2024.rds")
eno_text_gloss_df_all |> 
  # write_rds("contemporary-enggano-interlinear-text/eno_contemp_text_only_as_tibble-new-binded.rds")
  write_rds("contemporary-enggano-interlinear-text/eno_contemp_text_only_as_tibble-new-binded-march2024.rds")

## 2. Gathering the WORD and MORPH (and their glosses, POS, etc.) ======

### create an empty list of the same length as the number of the natural texts 
df_all <- vector(mode = "list", length = length(title))

### ensure that the length of the list elements equals the length of the title
length(df_all) == length(title)

### for lop to process each text
for (i in seq_along(title)) {
  
  # i <- 1
  
  phrase_node <- phrases[[i]] |> 
    xml_find_all("phrase")
  
  eno_text_id_subset <- eno_text_id[[i]]
  
  ### print some progress message
  message(paste("processing \"", title[i], "\"\n", sep = ""))
  
  ### eno word form =====
  word_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |> 
    map(~map(., ~xml_find_first(., "item[@type=\"txt\" and @lang=\"eno\"]"))) |> 
    map(~map(., ~xml_text(.))) 
  
  df_eno_word <- word_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
    map(~map(., \(x) if(!nzchar(x)) NA else x)) |> 
    map(unlist) |> 
    map(~tibble(word = .)) |> 
    map2(.y = eno_text_id_subset, ~mutate(.x, phrase_id =.y))
  
  ### eno English gloss =====
  word_gloss_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |> 
    map(~map(., ~xml_find_first(., "item[@type=\"gls\" and @lang=\"en\"]"))) |> 
    map(~map(., ~xml_text(.)))
  
  df_eno_word_gloss_eng <- word_gloss_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
    map(~map(., \(x) if(!nzchar(x)) NA else x)) |> 
    map(unlist) |>
    map(~tibble(eno_word_gloss_en = .))
  
  all(map_int(word_interim, length) == map_int(word_gloss_interim, length))
  
  ### eno Indonesian gloss =====
  word_gloss_id_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |> 
    map(~map(., ~xml_find_first(., "item[@type=\"gls\" and @lang=\"id\"]"))) |> 
    map(~map(., ~xml_text(.)))
  
  df_eno_word_gloss_id <- word_gloss_id_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
    map(~map(., \(x) if(!nzchar(x)) NA else x)) |> 
    map(unlist) |> 
    map(~tibble(eno_word_gloss_id = .))
  
  all(map_int(word_interim, length) == map_int(word_gloss_id_interim, length))
  all(map_int(word_gloss_interim, length) == map_int(word_gloss_id_interim, length))
  
  ### eno English part-of-speech =====
  word_pos_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |> 
    map(~map(., ~xml_find_first(., "item[@type=\"pos\" and @lang=\"en\"]"))) |> 
    map(~map(., ~xml_text(.)))
  df_eno_word_pos <- word_pos_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
    map(~map(., \(x) if(!nzchar(x)) NA else x)) |> 
    map(unlist) |>
    map(~tibble(eno_word_pos = .))
  
  all(map_int(word_interim, length) == map_int(word_pos_interim, length))
  all(map_int(word_gloss_interim, length) == map_int(word_pos_interim, length))
  all(map_int(word_gloss_id_interim, length) == map_int(word_pos_interim, length))
  
  ### eno Word ID =====
  word_id_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |> 
    map(~map(., ~xml_attr(., "guid")))
  df_eno_word_id <- word_id_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., \(x) if(!nzchar(x)) NA else x)) |>
    map(unlist) |> 
    map(~tibble(eno_word_id = .))
  
  all(map_int(word_interim, length) == map_int(word_id_interim, length))
  
  ### morpheme FORM =====
  morph_form_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_find_first(., "item[@type=\"txt\" and @lang=\"eno\"]"))) |> 
    map(~map(., ~xml_text(.))) 
  df_eno_morph <- morph_form_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., ~paste(., collapse= "___"))) |> 
    map(unlist) |> 
    map(~tibble(morph = .)) |> 
    map(~mutate(., morph = replace(morph, morph == "NA", NA)))
  
  ### morpheme ID =====
  morph_id_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_attr(., "guid")))
  df_eno_morph_id <- morph_id_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
    map(~map(., ~paste(., collapse= "___"))) |> 
    map(unlist) |> 
    map(~tibble(morph_id = .)) |> 
    map(~mutate(., morph_id = replace(morph_id, morph_id == "NA", NA)))
  
  all(map_int(morph_form_interim, length) == map_int(morph_id_interim, length))
  
  ### morpheme TYPE ====
  morph_type_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_attr(., "type")))
  df_eno_morph_type <- morph_type_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., ~paste(., collapse = "___"))) |> 
    map(unlist) |> 
    map(~tibble(morph_type = .)) |> 
    map(~mutate(., morph_type = replace(morph_type, morph_type == "NA", NA)))
  
  all(map_int(morph_form_interim, length) == map_int(morph_type_interim, length))
  
  ### morpheme LEXICAL ENTRIES =====
  morph_lex_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_find_first(., "item[@type=\"cf\" and @lang=\"eno\"]"))) |> 
    map(~map(., ~xml_text(.)))
  df_eno_morph_lex_entry <- morph_lex_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., \(x) replace(x, nchar(x)==0, NA))) |>
    map(~map(., ~paste(., collapse= "___"))) |> 
    map(unlist) |> 
    map(~tibble(lex_entry = .)) |> 
    map(~mutate(., lex_entry = replace(lex_entry, lex_entry == "NA", NA)))
  
  all(map_int(morph_form_interim, length) == map_int(morph_lex_interim, length))
  
  ### morpheme HOMONYM ID ======
  morph_hom_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_find_first(., "item[@type=\"hn\" and @lang=\"eno\"]"))) |> 
    map(~map(., ~xml_text(.)))
  df_eno_morph_homonym_id <- morph_hom_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., ~paste(., collapse= "___"))) |> 
    map(unlist) |> 
    map(~tibble(homonym_id = .)) |> 
    map(~mutate(., homonym_id = replace(homonym_id, homonym_id == "NA", NA)))
  
  all(map_int(morph_form_interim, length) == map_int(morph_hom_interim, length))
  
  ### morpheme GLOSS ======
  #### English ====
  morph_gloss_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_find_first(., "item[@type=\"gls\" and @lang=\"en\"]"))) |> 
    map(~map(., ~xml_text(.)))
  df_eno_morph_gloss_eng <- morph_gloss_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., \(x) replace(x, nchar(x)==0, NA))) |>
    map(~map(., ~paste(., collapse= "___"))) |> 
    map(unlist) |> 
    map(~tibble(morph_gloss_en = .)) |> 
    map(~mutate(., morph_gloss_en = replace(morph_gloss_en, 
                                            morph_gloss_en == "NA", NA))) 
  
  #### Indonesian =====
  morph_gloss_id_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_find_first(., "item[@type=\"gls\" and @lang=\"id\"]"))) |> 
    map(~map(., ~xml_text(.)))
  df_eno_morph_gloss_idn <- morph_gloss_id_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., \(x) replace(x, nchar(x)==0, NA))) |>
    map(~map(., ~paste(., collapse= "___"))) |> 
    map(unlist) |> 
    map(~tibble(morph_gloss_id = .)) |> 
    map(~mutate(., morph_gloss_id = replace(morph_gloss_id, 
                                            morph_gloss_id == "NA", NA))) 
  
  all(map_int(morph_form_interim, length) == map_int(morph_gloss_interim, length))
  all(map_int(morph_form_interim, length) == map_int(morph_gloss_id_interim, length))
  
  ### morpheme GRAMMATICAL CATEGORY ======
  morph_gram_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_find_first(., "item[@type=\"msa\" and @lang=\"en\"]"))) |> 
    map(~map(., ~xml_text(.)))
  df_eno_morph_gram <- morph_gram_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., \(x) replace(x, nchar(x)==0, NA))) |>
    map(~map(., ~paste(., collapse= "___"))) |> 
    map(unlist) |> 
    map(~tibble(morph_gram = .)) |> 
    map(~mutate(., morph_gram = replace(morph_gram, morph_gram == "NA", NA)))
  
  all(map_int(morph_form_interim, length) == map_int(morph_gram_interim, length))
  
  lexinput <- list(a = df_eno_word,
                   b = df_eno_word_id,
                   c = df_eno_word_gloss_id,
                   d = df_eno_word_gloss_eng,
                   e = df_eno_word_pos,
                   f = df_eno_morph,
                   g = df_eno_morph_id,
                   h = df_eno_morph_type,
                   i = df_eno_morph_homonym_id,
                   j = df_eno_morph_lex_entry,
                   k = df_eno_morph_gloss_eng,
                   l = df_eno_morph_gloss_idn,
                   m = df_eno_morph_gram)
  
  df <- pmap(lexinput, function(a, b, c, d, e, f, g, 
                                h, i, j, k, l, m) 
    bind_cols(a, b, c, d, e, f, g, h, i, j, k, l, m)) |> 
    list_rbind() |> 
    mutate(text_title = title[i]) |> 
    left_join(eno_text_gloss_df[[i]])
  
  df_all[[i]] <- df
  
  # i <- i + 1
  
} # end of if

### save the tibbles for each text ======
df_all |> 
  # write_rds("contemporary-enggano-interlinear-text/eno_contemp_text_as_tibble-new.rds")
  write_rds("contemporary-enggano-interlinear-text/eno_contemp_text_as_tibble-new-march2024.rds")




# B. Elicitation ===============

## Processing the elicitation data (non-natural texts)

library(tidyverse)
library(xml2)

xml_file <- dir("contemporary-enggano-interlinear-text/", 
                pattern = "all-contemp-elicitation", 
                full.names = TRUE) |> 
  # get the March Meeting Version 2024
  str_subset("march2024")

interlinear_text <- xml_file |> 
  read_xml() |> 
  xml_find_all("interlinear-text")

title <- interlinear_text |> 
  xml_find_all("item[@type=\"title\" and @lang=\"eno\"]") |> 
  xml_text() |> 
  str_replace_all("^(\\d+)[^A-Za-z]+", "\\1_")

paragraphs <- xml_find_all(interlinear_text, "paragraphs")

phrases <- paragraphs |> 
  map(xml_find_all, ".//phrases")

# phrases <- phrases[i]

## 1. Gathering the original texts (under the <phrases> node) ======


### Enggano texts/elicited materials
eno_text <- phrases |> 
  map(xml_find_all, "phrase/item[@type=\"txt\" and @lang=\"eno\"]") |> 
  map(xml_text)

### phrase ID
eno_text_id <- phrases |> 
  map(xml_find_all, "phrase") |> 
  map(xml_attr, "guid")

### segment number (i.e., the line number of the Enggano texts)
eno_segnum <- phrases |> 
  map(xml_find_all, "phrase/item[@type=\"segnum\" and @lang=\"en\"]") |> 
  map(xml_text)

### English free translation of the texts/elicited materials
eno_text_gloss_eng <- phrases |> 
  map(~map(., ~xml_find_all(., "phrase/item[@type=\"gls\" and @lang=\"en\"]"))) |> 
  map(~map(., ~xml_text(.))) |> 
  # map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
  # map(~map(., \(x) if(!is.na(x) & nchar(x) == 0) NA else x)) |> 
  map(unlist) |> 
  map(\(x) replace(x, nchar(x) == 0, NA))

### Indonesian free translation of the texts/elicited materials
eno_text_gloss_idn <- phrases |> 
  map(~map(., ~xml_find_all(., "phrase/item[@type=\"gls\" and @lang=\"id\"]"))) |> 
  map(~map(., ~xml_text(.))) |> 
  # map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
  # map(~map(., \(x) if(!is.na(x) & nchar(x) == 0) NA else x)) |> 
  map(unlist) |> 
  map(\(x) replace(x, nchar(x) == 0, NA))

### combine the text data from different elicited files into a list of tibbles
eno_text_gloss_df <- pmap(list(a = eno_text_id, b = eno_text,
                               c = eno_text_gloss_idn,
                               d = eno_text_gloss_eng,
                               e = eno_segnum),
                          \(a, b, c, d, e) 
                          tibble(phrase_id = a, 
                                 phrase_line = e,
                                 eno_phrase = b, 
                                 eno_phrase_gloss_id = c, 
                                 eno_phrase_gloss_eng = d)) |> 
  map2(.y = title, ~mutate(., text_title = .y))
eno_text_gloss_df_all <- list_rbind(eno_text_gloss_df)

### save the list tibbles
eno_text_gloss_df |> 
  # write_rds("contemporary-enggano-interlinear-text/eno_contemp_elicitation_only_as_tibble-new.rds")
  write_rds("contemporary-enggano-interlinear-text/eno_contemp_elicitation_only_as_tibble-new-march2024.rds")


### save the binded tibbles (non-list format)
eno_text_gloss_df_all |> 
  # write_rds("contemporary-enggano-interlinear-text/eno_contemp_elicitation_only_as_tibble-new-binded.rds")
  write_rds("contemporary-enggano-interlinear-text/eno_contemp_elicitation_only_as_tibble-new-binded-march2024.rds")


## 2. Gathering the WORD and MORPH (and their glosses, POS, etc.) ======

df_all <- vector(mode = "list", length = length(title))
length(df_all) == length(title)

for (i in seq_along(title)) {
  
  
  phrase_node <- phrases[[i]] |> 
    xml_find_all("phrase")
  
  eno_text_id_subset <- eno_text_id[[i]]
  
  message(paste("processing \"", title[i], "\"\n", sep = ""))
  
  ### eno word form =====
  phrase_node |> 
    map(xml_find_all, "words/word") |> 
    map(~map(., ~xml_find_first(., "item[@type=\"txt\" and @lang=\"eno\"]"))) |> 
    map(~map(., ~xml_text(.))) -> word_interim 
  word_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
    map(~map(., \(x) if(!nzchar(x)) NA else x)) |> 
    map(unlist) |> 
    map(~tibble(word = .)) |> 
    map2(.y = eno_text_id_subset, ~mutate(.x, phrase_id =.y)) -> df_eno_word
  
  ### eno English gloss =====
  phrase_node |> 
    map(xml_find_all, "words/word") |> 
    map(~map(., ~xml_find_first(., "item[@type=\"gls\" and @lang=\"en\"]"))) |> 
    map(~map(., ~xml_text(.))) -> word_gloss_interim
  word_gloss_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
    map(~map(., \(x) if(!nzchar(x)) NA else x)) |> 
    map(unlist) |>
    map(~tibble(eno_word_gloss_en = .)) -> df_eno_word_gloss_eng
  
  all(map_int(word_interim, length) == map_int(word_gloss_interim, length))
  
  ### eno Indonesian gloss =====
  phrase_node |> 
    map(xml_find_all, "words/word") |> 
    map(~map(., ~xml_find_first(., "item[@type=\"gls\" and @lang=\"id\"]"))) |> 
    map(~map(., ~xml_text(.))) -> word_gloss_id_interim
  word_gloss_id_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
    map(~map(., \(x) if(!nzchar(x)) NA else x)) |> 
    map(unlist) |> 
    map(~tibble(eno_word_gloss_id = .)) -> df_eno_word_gloss_id
  
  all(map_int(word_interim, length) == map_int(word_gloss_id_interim, length))
  all(map_int(word_gloss_interim, length) == map_int(word_gloss_id_interim, length))
  
  ### eno English part-of-speech =====
  phrase_node |> 
    map(xml_find_all, "words/word") |> 
    map(~map(., ~xml_find_first(., "item[@type=\"pos\" and @lang=\"en\"]"))) |> 
    map(~map(., ~xml_text(.))) -> word_pos_interim
  word_pos_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
    map(~map(., \(x) if(!nzchar(x)) NA else x)) |> 
    map(unlist) |>
    map(~tibble(eno_word_pos = .)) -> df_eno_word_pos
  
  all(map_int(word_interim, length) == map_int(word_pos_interim, length))
  all(map_int(word_gloss_interim, length) == map_int(word_pos_interim, length))
  all(map_int(word_gloss_id_interim, length) == map_int(word_pos_interim, length))
  
  ### eno Word ID =====
  phrase_node |> 
    map(xml_find_all, "words/word") |> 
    map(~map(., ~xml_attr(., "guid"))) -> word_id_interim
  word_id_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., \(x) if(!nzchar(x)) NA else x)) |>
    map(unlist) |> 
    map(~tibble(eno_word_id = .)) -> df_eno_word_id
  
  all(map_int(word_interim, length) == map_int(word_id_interim, length))
  
  ### morpheme FORM =====
  phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_find_first(., "item[@type=\"txt\" and @lang=\"eno\"]"))) |> 
    map(~map(., ~xml_text(.))) -> morph_form_interim 
  morph_form_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., \(x) replace(x, nchar(x)==0, NA))) |>
    map(~map(., ~paste(., collapse= "___"))) |> 
    map(unlist) |> 
    map(~tibble(morph = .)) |> 
    map(~mutate(., morph = replace(morph, morph == "NA", NA))) -> df_eno_morph
  
  ### morpheme ID =====
  phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_attr(., "guid"))) -> morph_id_interim
  morph_id_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
    map(~map(., ~paste(., collapse= "___"))) |> 
    map(unlist) |> 
    map(~tibble(morph_id = .)) |> 
    map(~mutate(., morph_id = replace(morph_id, morph_id == "NA", NA))) -> df_eno_morph_id
  
  all(map_int(morph_form_interim, length) == map_int(morph_id_interim, length))
  
  ### morpheme TYPE ====
  phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_attr(., "type"))) -> morph_type_interim
  morph_type_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., ~paste(., collapse = "___"))) |> 
    map(unlist) |> 
    map(~tibble(morph_type = .)) |> 
    map(~mutate(., morph_type = replace(morph_type, morph_type == "NA", NA))) -> df_eno_morph_type
  
  all(map_int(morph_form_interim, length) == map_int(morph_type_interim, length))
  
  ### morpheme LEXICAL ENTRIES =====
  phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_find_first(., "item[@type=\"cf\" and @lang=\"eno\"]"))) |> 
    map(~map(., ~xml_text(.))) -> morph_lex_interim
  morph_lex_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., \(x) replace(x, nchar(x)==0, NA))) |>
    map(~map(., ~paste(., collapse= "___"))) |> 
    map(unlist) |> 
    map(~tibble(lex_entry = .)) |> 
    map(~mutate(., lex_entry = replace(lex_entry, lex_entry == "NA", NA))) -> df_eno_morph_lex_entry
  
  all(map_int(morph_form_interim, length) == map_int(morph_lex_interim, length))
  
  ### morpheme HOMONYM ID ======
  phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_find_first(., "item[@type=\"hn\" and @lang=\"eno\"]"))) |> 
    map(~map(., ~xml_text(.))) -> morph_hom_interim
  morph_hom_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., ~paste(., collapse= "___"))) |> 
    map(unlist) |> 
    map(~tibble(homonym_id = .)) |> 
    map(~mutate(., homonym_id = replace(homonym_id, homonym_id == "NA", NA))) -> df_eno_morph_homonym_id
  
  all(map_int(morph_form_interim, length) == map_int(morph_hom_interim, length))
  
  ### morpheme GLOSS ======
  #### English ====
  phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_find_first(., "item[@type=\"gls\" and @lang=\"en\"]"))) |> 
    map(~map(., ~xml_text(.))) -> morph_gloss_interim
  morph_gloss_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., \(x) replace(x, nchar(x)==0, NA))) |>
    map(~map(., ~paste(., collapse= "___"))) |> 
    map(unlist) |> 
    map(~tibble(morph_gloss_en = .)) |> 
    map(~mutate(., morph_gloss_en = replace(morph_gloss_en, morph_gloss_en == "NA", NA))) -> df_eno_morph_gloss_eng
  
  #### Indonesian ====
  phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_find_first(., "item[@type=\"gls\" and @lang=\"id\"]"))) |> 
    map(~map(., ~xml_text(.))) -> morph_gloss_id_interim
  morph_gloss_id_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., \(x) replace(x, nchar(x)==0, NA))) |>
    map(~map(., ~paste(., collapse= "___"))) |> 
    map(unlist) |> 
    map(~tibble(morph_gloss_id = .)) |> 
    map(~mutate(., morph_gloss_id = replace(morph_gloss_id, morph_gloss_id == "NA", NA))) -> df_eno_morph_gloss_idn
  
  all(map_int(morph_form_interim, length) == map_int(morph_gloss_interim, length))
  all(map_int(morph_form_interim, length) == map_int(morph_gloss_id_interim, length))
  
  ### morpheme GRAMMATICAL CATEGORY ======
  phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_find_first(., "item[@type=\"msa\" and @lang=\"en\"]"))) |> 
    map(~map(., ~xml_text(.))) -> morph_gram_interim
  morph_gram_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., \(x) replace(x, nchar(x)==0, NA))) |>
    map(~map(., ~paste(., collapse= "___"))) |> 
    map(unlist) |> 
    map(~tibble(morph_gram = .)) |> 
    map(~mutate(., morph_gram = replace(morph_gram, morph_gram == "NA", NA))) -> df_eno_morph_gram
  
  all(map_int(morph_form_interim, length) == map_int(morph_gram_interim, length))
  
  lexinput <- list(a = df_eno_word,
                   b = df_eno_word_id,
                   c = df_eno_word_gloss_id,
                   d = df_eno_word_gloss_eng,
                   e = df_eno_word_pos,
                   f = df_eno_morph,
                   g = df_eno_morph_id,
                   h = df_eno_morph_type,
                   i = df_eno_morph_homonym_id,
                   j = df_eno_morph_lex_entry,
                   k = df_eno_morph_gloss_eng,
                   l = df_eno_morph_gloss_idn,
                   m = df_eno_morph_gram)
  
  df <- pmap(lexinput, function(a, b, c, d, e, f,
                                g, 
                                h, i, j, k, l, m) bind_cols(a, b, c, d, e, f,
                                                         g, 
                                                         h, i, j, k, l, m)) |> 
    list_rbind() |> 
    mutate(text_title = title[i]) |> 
    left_join(eno_text_gloss_df[[i]])
  
  df_all[[i]] <- df
  
}


df_all |> 
  # write_rds("contemporary-enggano-interlinear-text/eno_contemp_elicitation_as_tibble-new.rds")
  write_rds("contemporary-enggano-interlinear-text/eno_contemp_elicitation_as_tibble-new-march2024.rds")


# C. Textbooks ===============

## Processing the textbook data (teaching materials)

library(tidyverse)
library(xml2)

## read-in the .flextext export of the analyzed interlinear texts =====
xml_file <- dir("contemporary-enggano-interlinear-text/", 
                pattern = "textbook\\.flextext", 
                full.names = TRUE)

## retrieve the interlinear-text node =====
interlinear_text <- xml_file |> 
  read_xml() |> 
  xml_find_all("interlinear-text")

## retrieve the title of each text from the title attribute =====
title <- interlinear_text |> 
  xml_find_all("item[@type=\"title\" and @lang=\"eno\"]") |> 
  xml_text() |> 
  str_replace_all("^(\\d+)[^A-Za-z]+", "\\1_")

## retrieve the paragraph node =====
paragraphs <- xml_find_all(interlinear_text, "paragraphs")

## retrieve the phrases node =====
## the phrases node contains the baseline original text of the target lang.
phrases <- paragraphs |> 
  map(xml_find_all, ".//phrases")

# phrases <- phrases[i]

## 1. Gathering the original texts (under the <phrases> node) ======

### get the Enggano text segment/node
eno_text <- phrases |> 
  map(xml_find_all, "phrase/item[@type=\"txt\" and @lang=\"eno\"]") |> 
  map(xml_text)

## get the ID of the Enggano text segment/node
eno_text_id <- phrases |> 
  map(xml_find_all, "phrase") |> 
  map(xml_attr, "guid")

## get the segment/text number (i.e., line number in the baseline text)
eno_segnum <- phrases |> 
  map(xml_find_all, "phrase/item[@type=\"segnum\" and @lang=\"en\"]") |> 
  map(xml_text)

## get the English free translation of the segment/text
eno_text_gloss_eng <- phrases |> 
  map(~map(., ~xml_find_all(., "phrase/item[@type=\"gls\" and @lang=\"en\"]"))) |> 
  map(~map(., ~xml_text(.))) |> 
  # map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
  # map(~map(., \(x) if(!is.na(x) & nchar(x) == 0) NA else x)) |> 
  map(unlist) |> 
  map(\(x) replace(x, nchar(x) == 0, NA))

## get the Indonesian free translation of the segment/text
eno_text_gloss_idn <- phrases |> 
  map(~map(., ~xml_find_all(., "phrase/item[@type=\"gls\" and @lang=\"id\"]"))) |> 
  map(~map(., ~xml_text(.))) |> 
  # map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
  # map(~map(., \(x) if(!is.na(x) & nchar(x) == 0) NA else x)) |> 
  map(unlist) |> 
  map(\(x) replace(x, nchar(x) == 0, NA))

## turn the above data into a tibble for each text title
eno_text_gloss_df <- pmap(list(a = eno_text_id, b = eno_segnum,
                               c = eno_text,
                               d = eno_text_gloss_idn,
                               e = eno_text_gloss_eng),
                          \(a, b, c, d, e) 
                          tibble(phrase_id = a, 
                                 phrase_line = b,
                                 eno_phrase = c, 
                                 eno_phrase_gloss_id = d, 
                                 eno_phrase_gloss_eng = e)) |> 
  map2(.y = title, ~mutate(., text_title = .y)) |> 
  
  # remove section symbol
  map(~mutate(., eno_phrase = str_replace_all(eno_phrase, "§\\s$", "")))

## combine the tibbles list from all texts into one tibble
eno_text_gloss_df_all <- list_rbind(eno_text_gloss_df) |> 
  
  # remove section symbol
  mutate(eno_phrase = str_replace_all(eno_phrase, "§\\s$", ""))
  
eno_text_gloss_df |> 
  
  # remove section symbol
  map(~mutate(., eno_phrase = str_replace_all(eno_phrase, "§\\s$", ""))) |> 

  # write_rds("contemporary-enggano-interlinear-text/eno_contemp_text_only_as_tibble-new.rds")
  write_rds("contemporary-enggano-interlinear-text/textbook_as_tibble_oct-2024.rds")
eno_text_gloss_df_all |> 
  
  # remove section symbol
  mutate(eno_phrase = str_replace_all(eno_phrase, "§\\s$", "")) |> 
  
  # write_rds("contemporary-enggano-interlinear-text/eno_contemp_text_only_as_tibble-new-binded.rds")
  write_rds("contemporary-enggano-interlinear-text/textbook_as_tibble_oct-2024-binded.rds")

## 2. Gathering the WORD and MORPH (and their glosses, POS, etc.) ======

### create an empty list of the same length as the number of the natural texts 
df_all <- vector(mode = "list", length = length(title))

### ensure that the length of the list elements equals the length of the title
length(df_all) == length(title)

### for lop to process each text
for (i in seq_along(title)) {
  
  # i <- 1
  
  phrase_node <- phrases[[i]] |> 
    xml_find_all("phrase")
  
  eno_text_id_subset <- eno_text_id[[i]]
  
  ### print some progress message
  message(paste("processing \"", title[i], "\"\n", sep = ""))
  
  ### eno word form =====
  word_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |> 
    map(~map(., ~xml_find_first(., "item[@type=\"txt\" and @lang=\"eno\"]"))) |> 
    map(~map(., ~xml_text(.))) 
  
  df_eno_word <- word_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
    map(~map(., \(x) if(!nzchar(x)) NA else x)) |> 
    map(unlist) |> 
    map(~tibble(word = .)) |> 
    map2(.y = eno_text_id_subset, ~mutate(.x, phrase_id =.y))
  
  ### eno English gloss =====
  word_gloss_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |> 
    map(~map(., ~xml_find_first(., "item[@type=\"gls\" and @lang=\"en\"]"))) |> 
    map(~map(., ~xml_text(.)))
  
  df_eno_word_gloss_eng <- word_gloss_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
    map(~map(., \(x) if(!nzchar(x)) NA else x)) |> 
    map(unlist) |>
    map(~tibble(eno_word_gloss_en = .))
  
  all(map_int(word_interim, length) == map_int(word_gloss_interim, length))
  
  ### eno Indonesian gloss =====
  word_gloss_id_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |> 
    map(~map(., ~xml_find_first(., "item[@type=\"gls\" and @lang=\"id\"]"))) |> 
    map(~map(., ~xml_text(.)))
  
  df_eno_word_gloss_id <- word_gloss_id_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
    map(~map(., \(x) if(!nzchar(x)) NA else x)) |> 
    map(unlist) |> 
    map(~tibble(eno_word_gloss_id = .))
  
  all(map_int(word_interim, length) == map_int(word_gloss_id_interim, length))
  all(map_int(word_gloss_interim, length) == map_int(word_gloss_id_interim, length))
  
  ### eno English part-of-speech =====
  word_pos_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |> 
    map(~map(., ~xml_find_first(., "item[@type=\"pos\" and @lang=\"en\"]"))) |> 
    map(~map(., ~xml_text(.)))
  df_eno_word_pos <- word_pos_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
    map(~map(., \(x) if(!nzchar(x)) NA else x)) |> 
    map(unlist) |>
    map(~tibble(eno_word_pos = .))
  
  all(map_int(word_interim, length) == map_int(word_pos_interim, length))
  all(map_int(word_gloss_interim, length) == map_int(word_pos_interim, length))
  all(map_int(word_gloss_id_interim, length) == map_int(word_pos_interim, length))
  
  ### eno Word ID =====
  word_id_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |> 
    map(~map(., ~xml_attr(., "guid")))
  df_eno_word_id <- word_id_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., \(x) if(!nzchar(x)) NA else x)) |>
    map(unlist) |> 
    map(~tibble(eno_word_id = .))
  
  all(map_int(word_interim, length) == map_int(word_id_interim, length))
  
  ### morpheme FORM =====
  morph_form_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_find_first(., "item[@type=\"txt\" and @lang=\"eno\"]"))) |> 
    map(~map(., ~xml_text(.))) 
  df_eno_morph <- morph_form_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., ~paste(., collapse= "___"))) |> 
    map(unlist) |> 
    map(~tibble(morph = .)) |> 
    map(~mutate(., morph = replace(morph, morph == "NA", NA)))
  
  ### morpheme ID =====
  morph_id_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_attr(., "guid")))
  df_eno_morph_id <- morph_id_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
    map(~map(., ~paste(., collapse= "___"))) |> 
    map(unlist) |> 
    map(~tibble(morph_id = .)) |> 
    map(~mutate(., morph_id = replace(morph_id, morph_id == "NA", NA)))
  
  all(map_int(morph_form_interim, length) == map_int(morph_id_interim, length))
  
  ### morpheme TYPE ====
  morph_type_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_attr(., "type")))
  df_eno_morph_type <- morph_type_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., ~paste(., collapse = "___"))) |> 
    map(unlist) |> 
    map(~tibble(morph_type = .)) |> 
    map(~mutate(., morph_type = replace(morph_type, morph_type == "NA", NA)))
  
  all(map_int(morph_form_interim, length) == map_int(morph_type_interim, length))
  
  ### morpheme LEXICAL ENTRIES =====
  morph_lex_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_find_first(., "item[@type=\"cf\" and @lang=\"eno\"]"))) |> 
    map(~map(., ~xml_text(.)))
  df_eno_morph_lex_entry <- morph_lex_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., \(x) replace(x, nchar(x)==0, NA))) |>
    map(~map(., ~paste(., collapse= "___"))) |> 
    map(unlist) |> 
    map(~tibble(lex_entry = .)) |> 
    map(~mutate(., lex_entry = replace(lex_entry, lex_entry == "NA", NA)))
  
  all(map_int(morph_form_interim, length) == map_int(morph_lex_interim, length))
  
  ### morpheme HOMONYM ID ======
  morph_hom_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_find_first(., "item[@type=\"hn\" and @lang=\"eno\"]"))) |> 
    map(~map(., ~xml_text(.)))
  df_eno_morph_homonym_id <- morph_hom_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., ~paste(., collapse= "___"))) |> 
    map(unlist) |> 
    map(~tibble(homonym_id = .)) |> 
    map(~mutate(., homonym_id = replace(homonym_id, homonym_id == "NA", NA)))
  
  all(map_int(morph_form_interim, length) == map_int(morph_hom_interim, length))
  
  ### morpheme GLOSS ======
  #### English ====
  morph_gloss_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_find_first(., "item[@type=\"gls\" and @lang=\"en\"]"))) |> 
    map(~map(., ~xml_text(.)))
  df_eno_morph_gloss_eng <- morph_gloss_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., \(x) replace(x, nchar(x)==0, NA))) |>
    map(~map(., ~paste(., collapse= "___"))) |> 
    map(unlist) |> 
    map(~tibble(morph_gloss_en = .)) |> 
    map(~mutate(., morph_gloss_en = replace(morph_gloss_en, 
                                            morph_gloss_en == "NA", NA))) 
  
  #### Indonesian =====
  morph_gloss_id_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_find_first(., "item[@type=\"gls\" and @lang=\"id\"]"))) |> 
    map(~map(., ~xml_text(.)))
  df_eno_morph_gloss_idn <- morph_gloss_id_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., \(x) replace(x, nchar(x)==0, NA))) |>
    map(~map(., ~paste(., collapse= "___"))) |> 
    map(unlist) |> 
    map(~tibble(morph_gloss_id = .)) |> 
    map(~mutate(., morph_gloss_id = replace(morph_gloss_id, 
                                            morph_gloss_id == "NA", NA))) 
  
  all(map_int(morph_form_interim, length) == map_int(morph_gloss_interim, length))
  all(map_int(morph_form_interim, length) == map_int(morph_gloss_id_interim, length))
  
  ### morpheme GRAMMATICAL CATEGORY ======
  morph_gram_interim <- phrase_node |> 
    map(xml_find_all, "words/word") |>
    map(~map(., ~xml_find_all(., "morphemes/morph"))) |>
    map(~map(., ~xml_find_first(., "item[@type=\"msa\" and @lang=\"en\"]"))) |> 
    map(~map(., ~xml_text(.)))
  df_eno_morph_gram <- morph_gram_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    map(~map(., \(x) replace(x, nchar(x)==0, NA))) |>
    map(~map(., ~paste(., collapse= "___"))) |> 
    map(unlist) |> 
    map(~tibble(morph_gram = .)) |> 
    map(~mutate(., morph_gram = replace(morph_gram, morph_gram == "NA", NA)))
  
  all(map_int(morph_form_interim, length) == map_int(morph_gram_interim, length))
  
  lexinput <- list(a = df_eno_word,
                   b = df_eno_word_id,
                   c = df_eno_word_gloss_id,
                   d = df_eno_word_gloss_eng,
                   e = df_eno_word_pos,
                   f = df_eno_morph,
                   g = df_eno_morph_id,
                   h = df_eno_morph_type,
                   i = df_eno_morph_homonym_id,
                   j = df_eno_morph_lex_entry,
                   k = df_eno_morph_gloss_eng,
                   l = df_eno_morph_gloss_idn,
                   m = df_eno_morph_gram)
  
  df <- pmap(lexinput, function(a, b, c, d, e, f, g, 
                                h, i, j, k, l, m) 
    bind_cols(a, b, c, d, e, f, g, h, i, j, k, l, m)) |> 
    list_rbind() |> 
    mutate(text_title = title[i]) |> 
    left_join(eno_text_gloss_df[[i]])
  
  df_all[[i]] <- df
  
  # i <- i + 1
  
} # end of if

### save the tibbles for each text ======
df_all |> 
  # filter out the NAs in the word
  map(~filter(., !is.na(word))) |> 
  # write_rds("contemporary-enggano-interlinear-text/eno_contemp_text_as_tibble-new.rds")
  write_rds("contemporary-enggano-interlinear-text/textbook_lexicon_as_tibble_oct-2024.rds")
