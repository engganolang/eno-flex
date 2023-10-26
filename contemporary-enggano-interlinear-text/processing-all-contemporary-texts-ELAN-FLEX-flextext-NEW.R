library(tidyverse)
library(xml2)

xml_file <- "C:\\Users\\GRajeg\\Downloads\\all-contemp-texts.flextext"

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

# 1. Gathering the original texts (under the <phrases> node) ======

eno_text <- phrases |> 
  map(xml_find_all, "phrase/item[@type=\"txt\" and @lang=\"eno\"]") |> 
  map(xml_text)
eno_text_id <- phrases |> 
  map(xml_find_all, "phrase") |> 
  map(xml_attr, "guid")
## remove the strange structure of phrase id in Malakoni text
# eno_text_id[[16]] <- eno_text_id[[16]][eno_text_id[[16]]!="fc192939-df22-4ac5-ac04-b70643253f56"]
eno_segnum <- phrases |> 
  map(xml_find_all, "phrase/item[@type=\"segnum\" and @lang=\"en\"]") |> 
  map(xml_text)
eno_text_gloss_eng <- phrases |> 
  map(~map(., ~xml_find_all(., "phrase/item[@type=\"gls\" and @lang=\"en\"]"))) |> 
  map(~map(., ~xml_text(.))) |> 
  # map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
  # map(~map(., \(x) if(!is.na(x) & nchar(x) == 0) NA else x)) |> 
  map(unlist) |> 
  map(\(x) replace(x, nchar(x) == 0, NA))
eno_text_gloss_idn <- phrases |> 
  map(~map(., ~xml_find_all(., "phrase/item[@type=\"gls\" and @lang=\"id\"]"))) |> 
  map(~map(., ~xml_text(.))) |> 
  # map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
  # map(~map(., \(x) if(!is.na(x) & nchar(x) == 0) NA else x)) |> 
  map(unlist) |> 
  map(\(x) replace(x, nchar(x) == 0, NA))

eno_text_gloss_df <- pmap(list(a = eno_text_id, b = eno_text,
                               c = eno_text_gloss_idn,
                               d = eno_text_gloss_eng),
                          \(a, b, c, d) 
                          tibble(phrase_id = a, 
                                 eno_phrase = b, 
                                 eno_phrase_gloss_id = c, 
                                 eno_phrase_gloss_eng = d)) |> 
  map2(.y = title, ~mutate(., text_title = .y))
eno_text_gloss_df_all <- list_rbind(eno_text_gloss_df)


# 2. Gathering the WORD and MORPH (and their glosses, POS, etc.) ======

df_all <- vector(mode = "list", length = length(title))
length(df_all) == length(title)

for (i in seq_along(title)) {
  
  
  phrase_node <- phrases[[i]] |> 
    xml_find_all("phrase")
  
  eno_text_id_subset <- eno_text_id[[i]]
  
  message(paste("processing \"", title[i], "\"\n", sep = ""))
  
  ## eno word form =====
  phrase_node |> 
    map(xml_find_all, "words/word") |> 
    map(xml_find_first, "item[@type=\"txt\" and @lang=\"eno\"]") |> 
    map(xml_text) -> word_interim 
  word_interim |> 
    map(\(x) if(identical(x, character(0))) NA else x) |> 
    map(~tibble(word = .)) |> 
    map2(.y = eno_text_id_subset, ~mutate(.x, phrase_id =.y)) -> df_eno_word
  
  ## eno English gloss =====
  phrase_node |> 
    map(xml_find_all, "words/word") |> 
    map(xml_find_first, "item[@type=\"gls\" and @lang=\"en\"]") |> 
    map(xml_text) -> word_gloss_interim
  word_gloss_interim |> 
    map(\(x) if(identical(x, character(0))) NA else x) |> 
    map(~tibble(eno_word_gloss_en = .)) -> df_eno_word_gloss_eng
  
  all(map_int(word_interim, length) == map_int(word_gloss_interim, length))
  
  ## eno Indonesian gloss =====
  phrase_node |> 
    map(xml_find_all, "words/word") |> 
    map(xml_find_first, "item[@type=\"gls\" and @lang=\"id\"]") |> 
    map(xml_text) -> word_gloss_id_interim
  word_gloss_id_interim |> 
    map(\(x) if(identical(x, character(0))) NA else x) |> 
    map(~tibble(eno_word_gloss_id = .)) -> df_eno_word_gloss_id
  
  all(map_int(word_interim, length) == map_int(word_gloss_id_interim, length))
  all(map_int(word_gloss_interim, length) == map_int(word_gloss_id_interim, length))
  
  ## eno English part-of-speech =====
  phrase_node |> 
    map(xml_find_all, "words/word") |> 
    map(xml_find_first, "item[@type=\"pos\" and @lang=\"en\"]") |> 
    map(xml_text) -> word_pos_interim
  word_pos_interim |> 
    map(\(x) if(identical(x, character(0))) NA else x) |> 
    map(~tibble(eno_word_pos = .)) -> df_eno_word_pos
  
  all(map_int(word_interim, length) == map_int(word_pos_interim, length))
  all(map_int(word_gloss_interim, length) == map_int(word_pos_interim, length))
  all(map_int(word_gloss_id_interim, length) == map_int(word_pos_interim, length))
  
  ## eno Word ID =====
  phrase_node |> 
    map(xml_find_all, "words/word") |> 
    map(xml_attr, "guid") -> word_id_interim
  word_id_interim |> 
    map(\(x) if(identical(x, character(0))) NA else x) |> 
    map(~tibble(eno_word_id = .)) -> df_eno_word_id
  
  all(map_int(word_interim, length) == map_int(word_id_interim, length))
  
  ## morpheme FORM =====
  phrase_node |> 
    map(xml_find_all, "words/word/morphemes/morph") |> 
    map(xml_find_first, "item[@type=\"txt\" and @lang=\"eno\"]") |> 
    map(xml_text) -> morph_form_interim 
  morph_form_interim |> 
    map(\(x) if(identical(x, character(0))) NA else x) |> 
    map(paste, collapse = "___") |> 
    map(~tibble(morph = .)) |> 
    map(~mutate(., morph = replace(morph, morph == "NA", NA))) -> df_eno_morph
  
  ## morpheme ID =====
  phrase_node |> 
    map(xml_find_all, "words/word/morphemes/morph") |> 
    map(xml_attr, "guid") -> morph_id_interim
  morph_id_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
    map(paste, collapse = "___") |> 
    map(~tibble(morph_id = .)) |> 
    map(~mutate(., morph_id = replace(morph_id, morph_id == "NA", NA))) -> df_eno_morph_id
  
  all(map_int(morph_form_interim, length) == map_int(morph_id_interim, length))
  
  ## morpheme TYPE ====
  phrase_node |> 
    map(xml_find_all, "words/word/morphemes/morph") |> 
    map(xml_attr, "type") -> morph_type_interim
  morph_type_interim |> 
    map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
    map(paste, collapse = "___") |> 
    map(~tibble(morph_type = .)) |> 
    map(~mutate(., morph_type = replace(morph_type, morph_type == "NA", NA))) -> df_eno_morph_type
  
  all(map_int(morph_form_interim, length) == map_int(morph_type_interim, length))
  
  ## morpheme LEXICAL ENTRIES =====
  phrase_node |> 
    map(xml_find_all, "words/word/morphemes/morph") |> 
    map(xml_find_first, "item[@type=\"cf\" and @lang=\"eno\"]") |> 
    map(xml_text) -> morph_lex_interim
  morph_lex_interim |> 
    map(\(x) if(identical(x, character(0))) NA else x) |> 
    map(paste, collapse = "___") |> 
    map(~tibble(lex_entry = .)) |> 
    map(~mutate(., lex_entry = replace(lex_entry, lex_entry == "NA", NA))) -> df_eno_morph_lex_entry
  
  all(map_int(morph_form_interim, length) == map_int(morph_lex_interim, length))
  
  ## morpheme HOMONYM ID ======
  phrase_node |> 
    map(xml_find_all, "words/word/morphemes/morph") |> 
    map(xml_find_first, "item[@type=\"hn\" and @lang=\"eno\"]") |> 
    map(xml_text) -> morph_hom_interim
  morph_hom_interim |> 
    map(\(x) if(identical(x, character(0))) NA else x) |> 
    map(paste, collapse = "___") |> 
    map(~tibble(homonym_id = .)) |> 
    map(~mutate(., homonym_id = replace(homonym_id, homonym_id == "NA", NA))) -> df_eno_morph_homonym_id
  
  all(map_int(morph_form_interim, length) == map_int(morph_hom_interim, length))
  
  ## morpheme GLOSS ======
  phrase_node |> 
    map(xml_find_all, "words/word/morphemes/morph") |> 
    map(xml_find_first, "item[@type=\"gls\" and @lang=\"en\"]") |> 
    map(xml_text) -> morph_gloss_interim
  morph_gloss_interim |> 
    map(\(x) if(identical(x, character(0))) NA else x) |> 
    map(paste, collapse = "___") |> 
    map(~tibble(morph_gloss_en = .)) |> 
    map(~mutate(., morph_gloss_en = replace(morph_gloss_en, morph_gloss_en == "NA", NA))) -> df_eno_morph_gloss_eng
  
  all(map_int(morph_form_interim, length) == map_int(morph_gloss_interim, length))
  
  ## morpheme GRAMMATICAL CATEGORY ======
  phrase_node |> 
    map(xml_find_all, "words/word/morphemes/morph") |> 
    map(xml_find_first, "item[@type=\"msa\" and @lang=\"en\"]") |> 
    map(xml_text) -> morph_gram_interim
  morph_gram_interim |> 
    map(\(x) if(identical(x, character(0))) NA else x) |> 
    map(paste, collapse = "___") |> 
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
                   l = df_eno_morph_gram)
  
  df <- pmap(lexinput, function(a, b, c, d, e, f,
                                g, 
                                h, i, j, k, l) bind_cols(a, b, c, d, e, f,
                                                         g, 
                                                         h, i, j, k, l)) |> 
    list_rbind() |> 
    mutate(text_title = title[i]) |> 
    left_join(eno_text_gloss_df[[i]])
  
  df_all[[i]] <- df
  
}


# df_all1 <- read_rds("enggano_contemp_text_as_table.rds")
df_all1 |> filter(str_detect(text_title, "^02"))
