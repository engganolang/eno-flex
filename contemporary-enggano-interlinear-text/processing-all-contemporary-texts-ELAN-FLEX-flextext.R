library(tidyverse)
library(xml2)

xml_file <- "C:\\Users\\GRajeg\\Downloads\\all-contemp-texts.flextext"

interlinear_text <- xml_file |> 
  read_xml() |> 
  xml_find_all("interlinear-text")

i <- 1

title <- interlinear_text |> 
  xml_find_all("item[@type=\"title\" and @lang=\"eno\"]") |> 
  xml_text() |> 
  str_replace_all("^(\\d+)[^A-Za-z]+", "\\1_")

paragraphs <- xml_find_all(interlinear_text, "paragraphs")

phrases <- paragraphs |> 
  map(xml_find_all, ".//phrases")


# 1

phrases <- phrases[-16]

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
                          \(a, b, c, d) tibble(phrase_id = a, eno_phrase = b, eno_phrase_gloss_id = c, eno_phrase_gloss_eng = d))
eno_text_gloss_df_all <- list_rbind(eno_text_gloss_df)
eno_text_gloss_df_all |> write_rds("eno_text_gloss_df_all.rds")

# 2
eno_words <- phrases |>
  map(~map(., ~xml_find_all(., "phrase/words/word/item[@type=\"txt\" and @lang=\"eno\"]"))) |>
  map(~map(., ~xml_text(.)))
eno_words_id <- phrases |>
  map(~map(., ~xml_find_all(., "phrase/words/word"))) |>
  map(~map(., ~xml_attr(., "guid"))) |>
  map(~map(., \(x) x[!is.na(x)]))
## combine and link eno words and eno words id
eno_words_and_id <- map2(eno_words, eno_words_id, ~tibble(words = .x,
                                                          words_id = .y))
## combine and link eno words and eno words id with the phrase id
eno_words_and_id <- eno_words_and_id |>
  map2(.y = eno_text_id, ~mutate(., phrase_id = .y))

# 3

df_all <- vector(mode = "list", length = length(title))

for (i in seq_along(title)) {
  x <- phrases[[i]] |> map(xml_find_all, ".//words/word")
  eno_text_id_subset <- eno_text_id[[i]]
  
  message(paste("processing \"", title[i], "\"\n", sep = ""))
  
  if (i == 16) {
    
    cat("skipping", i, "\n")
    
    next
    
  } else {
    
    
    # x <- phrases[c(16, 29, 31)] |> map(xml_find_all, ".//words/word")
    # x <- phrases[[29]] |> map(xml_find_all, ".//words/word")
    # z <- x[1] |> map(~map(., ~xml_find_all(., ".//morphemes"))) %>% .[[1]] %>% .[34:35]
    # z <- x[2] |> map(~map(., ~xml_find_all(., ".//morphemes"))) %>% .[[1]] %>% .[c(80, 86)]
    
    # accessing the Word line in the FLEx interlinear
    # df_eno_word <- x[c(34, 80, 86, 91)] |> 
    #   map(~map(., ~xml_find_first(., "item[@type=\"txt\" and @lang=\"eno\"]"))) |> 
    #   map(~map(., ~xml_text(.))) |> 
    #   map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
    #   map(unlist) |> 
    #   map(~tibble(eno_word = .))
    
    
    
    df_eno_word <- x |> 
      map(~map(., ~xml_find_first(., "item[@type=\"txt\" and @lang=\"eno\"]"))) |> 
      map(~map(., ~xml_text(.))) |> 
      map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
      map(unlist) |> 
      map2(.y = eno_text_id_subset, ~tibble(eno_word = .x, phrase_id = .y))
    
    # accessing the Word line in the FLEx interlinear: English gloss of the word
    # df_eno_word_gloss_eng <- x[c(34, 80, 86, 91)] |> 
    #   map(~map(., ~xml_find_first(., "item[@type=\"gls\" and @lang=\"en\"]"))) |> 
    #   map(~map(., ~xml_text(.))) |> 
    #   map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
    #   map(unlist) |> 
    #   map(~tibble(eno_word_gloss_en = .))
    df_eno_word_gloss_eng <- x |> 
      map(~map(., ~xml_find_first(., "item[@type=\"gls\" and @lang=\"en\"]"))) |> 
      map(~map(., ~xml_text(.))) |> 
      map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
      map(unlist) |> 
      map(~tibble(eno_word_gloss_en = .))
    
    # accessing the Word line in the FLEx interlinear: Indonesian gloss of the word
    # df_eno_word_gloss_id <- x[c(34, 80, 86, 91)] |> 
    #   map(~map(., ~xml_find_first(., "item[@type=\"gls\" and @lang=\"id\"]"))) |> 
    #   map(~map(., ~xml_text(.))) |> 
    #   map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
    #   map(unlist) |> 
    #   map(~tibble(eno_word_gloss_id = .))
    df_eno_word_gloss_id <- x |> 
      map(~map(., ~xml_find_first(., "item[@type=\"gls\" and @lang=\"id\"]"))) |> 
      map(~map(., ~xml_text(.))) |> 
      map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
      map(unlist) |> 
      map(~tibble(eno_word_gloss_id = .))
    
    # accessing the Word line in the FLEx interlinear: POS of the word
    # df_eno_word_pos <- x[c(34, 80, 86, 91)] |> 
    #   map(~map(., ~xml_find_first(., "item[@type=\"pos\" and @lang=\"en\"]"))) |> 
    #   map(~map(., ~xml_text(.))) |> 
    #   map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
    #   map(unlist) |> 
    #   map(~tibble(eno_word_pos = .))
    df_eno_word_pos <- x |> 
      map(~map(., ~xml_find_first(., "item[@type=\"pos\" and @lang=\"en\"]"))) |> 
      map(~map(., ~xml_text(.))) |> 
      map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
      map(unlist) |> 
      map(~tibble(eno_word_pos = .))
    
    # accessing the word ID in the FLEx interlinear
    # df_eno_word_id <- x[c(34, 80, 86, 91)] |> 
    #   map(~map(., ~xml_attr(., "guid"))) |> 
    #   map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    #   map(unlist) |> 
    #   map(~tibble(eno_word_id = .))
    df_eno_word_id <- x |> 
      map(~map(., ~xml_attr(., "guid"))) |> 
      map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
      map(unlist) |> 
      map(~tibble(eno_word_id = .))
    
    # accessing the Morpheme tier: morpheme form
    # df_eno_morph <- x[c(34, 80, 86, 91)] |> 
    #   map(~map(., ~xml_find_all(., ".//morphemes/morph/item[@type=\"txt\" and @lang=\"eno\"]"))) |> 
    #   map(~map(., ~xml_text(.))) |> 
    #   map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    #   map(~map(., ~paste(., collapse= "___"))) |> 
    #   map(unlist) |> 
    #   # map(function(x) x[nzchar(x)]) |>
    #   map(~tibble(morph = .)) |> 
    #   map(~mutate(., morph = replace(morph, morph == "NA", NA)))
    df_eno_morph <- x |> 
      map(~map(., ~xml_find_all(., ".//morphemes/morph/item[@type=\"txt\" and @lang=\"eno\"]"))) |> 
      map(~map(., ~xml_text(.))) |> 
      map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
      map(~map(., ~paste(., collapse= "___"))) |> 
      map(unlist) |> 
      # map(function(x) x[nzchar(x)]) |>
      map(~tibble(morph = .)) |> 
      map(~mutate(., morph = replace(morph, morph == "NA", NA)))
    
    # accessing the Morpheme tier: morpheme form ID
    # df_eno_morph_id <- x[c(34, 80, 86, 91)] |> 
    #   map(~map(., ~xml_find_all(., ".//morphemes/morph"))) |> 
    #   map(~map(., ~xml_attr(., "guid"))) |> 
    #   map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    #   map(~map(., ~paste(., collapse= "___"))) |> 
    #   map(unlist) |> 
    #   # map(function(x) x[nzchar(x)]) |>
    #   map(~tibble(morph_id = .)) |> 
    #   map(~mutate(., morph_id = replace(morph_id, morph_id == "NA", NA)))
    df_eno_morph_id <- x |> 
      map(~map(., ~xml_find_all(., ".//morphemes/morph"))) |> 
      map(~map(., ~xml_attr(., "guid"))) |> 
      map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
      map(~map(., ~paste(., collapse= "___"))) |> 
      map(unlist) |> 
      # map(function(x) x[nzchar(x)]) |>
      map(~tibble(morph_id = .)) |> 
      map(~mutate(., morph_id = replace(morph_id, morph_id == "NA", NA)))
    
    # accessing the Morpheme tier: morpheme type attribute
    # df_eno_morph_type <- x[c(34, 80, 86, 91)] |> 
    #   map(~map(., ~xml_find_all(., ".//morphemes/morph"))) |> 
    #   map(~map(., ~xml_attr(., "type"))) |> 
    #   map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    #   map(~map(., ~paste(., collapse = "___"))) |> 
    #   map(unlist) |> 
    #   # map(function(x) x[nzchar(x)]) |>
    #   map(~tibble(morph_type = .)) |> 
    #   map(~mutate(., morph_type = replace(morph_type, 
    #                                       morph_type == "NA", 
    #                                       NA)))
    df_eno_morph_type <- x |> 
      map(~map(., ~xml_find_all(., ".//morphemes/morph"))) |> 
      map(~map(., ~xml_attr(., "type"))) |> 
      map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
      map(~map(., ~paste(., collapse = "___"))) |> 
      map(unlist) |> 
      # map(function(x) x[nzchar(x)]) |>
      map(~tibble(morph_type = .)) |> 
      map(~mutate(., morph_type = replace(morph_type, 
                                          morph_type == "NA", 
                                          NA)))
    
    # accessing the Morpheme tier: lex. Entries
    # df_eno_morph_lex_entry <- x[c(34, 80, 86, 91)] |> 
    #   map(~map(., ~xml_find_all(., ".//morphemes/morph/item[@type=\"cf\" and @lang=\"eno\"]"))) |> 
    #   map(~map(., ~xml_text(.))) |> 
    #   map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    #   map(~map(., ~paste(., collapse= "___"))) |> 
    #   map(unlist) |> 
    #   # map(function(x) x[nzchar(x)]) |>
    #   map(~tibble(lex_entry = .)) |> 
    #   map(~mutate(., lex_entry = replace(lex_entry, lex_entry == "NA", NA)))
    df_eno_morph_lex_entry <- x |> 
      map(~map(., ~xml_find_all(., ".//morphemes/morph/item[@type=\"cf\" and @lang=\"eno\"]"))) |> 
      map(~map(., ~xml_text(.))) |> 
      map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
      map(~map(., ~paste(., collapse= "___"))) |> 
      map(unlist) |> 
      # map(function(x) x[nzchar(x)]) |>
      map(~tibble(lex_entry = .)) |> 
      map(~mutate(., lex_entry = replace(lex_entry, lex_entry == "NA", NA)))
    
    # accessing the Morpheme tier: homonym ID
    # df_eno_morph_homonym_id <- x[c(34, 80, 86, 91)] |> 
    #   map(~map(., ~xml_find_all(., ".//morphemes/morph/item[@type=\"hn\" and @lang=\"eno\"]"))) |> 
    #   map(~map(., ~xml_text(.))) |> 
    #   map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    #   map(~map(., ~paste(., collapse= "___"))) |> 
    #   map(unlist) |> 
    #   # map(function(x) x[nzchar(x)]) |>
    #   map(~tibble(homonym_id = .)) |> 
    #   map(~mutate(., homonym_id = replace(homonym_id, homonym_id == "NA", NA)))
    df_eno_morph_homonym_id <- x |> 
      map(~map(., ~xml_find_all(., ".//morphemes/morph/item[@type=\"hn\" and @lang=\"eno\"]"))) |> 
      map(~map(., ~xml_text(.))) |> 
      map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
      map(~map(., ~paste(., collapse= "___"))) |> 
      map(unlist) |> 
      # map(function(x) x[nzchar(x)]) |>
      map(~tibble(homonym_id = .)) |> 
      map(~mutate(., homonym_id = replace(homonym_id, homonym_id == "NA", NA)))
    
    # accessing the Morpheme tier: morpheme gloss
    # df_eno_morph_gloss_eng <- x[c(34, 80, 86, 91)] |> 
    #   map(~map(., ~xml_find_all(., ".//morphemes/morph/item[@type=\"gls\" and @lang=\"en\"]"))) |> 
    #   map(~map(., ~xml_text(.))) |> 
    #   map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    #   map(~map(., ~paste(., collapse= "___"))) |> 
    #   map(unlist) |> 
    #   # map(function(x) x[nzchar(x)]) |>
    #   map(~tibble(morph_gloss_en = .)) |> 
    #   map(~mutate(., morph_gloss_en = replace(morph_gloss_en, morph_gloss_en == "NA", NA)))
    df_eno_morph_gloss_eng <- x |> 
      map(~map(., ~xml_find_all(., ".//morphemes/morph/item[@type=\"gls\" and @lang=\"en\"]"))) |> 
      map(~map(., ~xml_text(.))) |> 
      map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
      map(~map(., ~paste(., collapse= "___"))) |> 
      map(unlist) |> 
      # map(function(x) x[nzchar(x)]) |>
      map(~tibble(morph_gloss_en = .)) |> 
      map(~mutate(., morph_gloss_en = replace(morph_gloss_en, morph_gloss_en == "NA", NA)))
    
    # accessing the Morpheme tier: morpheme grammatical category
    # df_eno_morph_gram <- x[c(34, 80, 86, 91)] |> 
    #   map(~map(., ~xml_find_all(., ".//morphemes/morph/item[@type=\"msa\" and @lang=\"en\"]"))) |> 
    #   map(~map(., ~xml_text(.))) |> 
    #   map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
    #   map(~map(., ~paste(., collapse= "___"))) |> 
    #   map(unlist) |> 
    #   # map(function(x) x[nzchar(x)]) |>
    #   map(~tibble(morph_gram = .)) |> 
    #   map(~mutate(., morph_gram = replace(morph_gram, morph_gram == "NA", NA)))
    df_eno_morph_gram <- x |> 
      map(~map(., ~xml_find_all(., ".//morphemes/morph/item[@type=\"msa\" and @lang=\"en\"]"))) |> 
      map(~map(., ~xml_text(.))) |> 
      map(~map(., \(x) if(identical(x, character(0))) NA else x)) |>
      map(~map(., ~paste(., collapse= "___"))) |> 
      map(unlist) |> 
      # map(function(x) x[nzchar(x)]) |>
      map(~tibble(morph_gram = .)) |> 
      map(~mutate(., morph_gram = replace(morph_gram, morph_gram == "NA", NA)))
    
    
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
      mutate(text_title = title[i])
    
    df_all[[i]] <- df
    
  }

  
}

# IMPORTANT, still needs to work on 16 Malakoni
df_all |> list_rbind() |> write_rds("enggano_contemp_text_as_table.rds")

df_all1 <- read_rds("enggano_contemp_text_as_table.rds")
eno_text_gloss_df_all <- read_rds("eno_text_gloss_df_all.rds")

df_all2 <- df_all1 |> 
  left_join(eno_text_gloss_df_all) 

df_all2 |> 
  filter(str_detect(morph_gloss_en, "_?exist_?")) |> 
  select(eno_word, eno_word_gloss_en, morph, morph_gloss_en, eno_phrase, eno_phrase_gloss_eng, text_title)


