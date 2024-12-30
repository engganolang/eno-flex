# Code to process the .lift export of FLEx "Lexicon" into tibble
# Gede Rajeg (University of Oxford & Universitas Udayana; 2023-2024)

library(tidyverse)
library(xml2)
library(stringi)

# flexdb <- read_xml("input/textbook/lift/textbook-LIFT.lift") # Textbook processing
# flexdb <- read_xml("input/textbook/lift/textbook-LIFT-20241110.lift") # Textbook processing
# flexdb <- read_xml("input/textbook/lift/textbook-LIFT-20241129.lift") # Textbook processing
flexdb <- read_xml("input/textbook/lift/textbook-LIFT-20241201.lift") # Textbook processing


# MAIN ENTRY ======

# extract only the <entry>
myentry <- flexdb %>% 
  xml_find_all("entry")
length(myentry) # this number should be the same with the number of the (FILTERED) entries in FLEx LEXICON feature.

# determine the length of children for each <entry>
myentry_length <- myentry %>% 
  xml_length()
myentry_length

# get the id for each <entry> node
myentry_id <- myentry %>% 
  xml_attr("id")
myentry_id_num <- myentry_id %>% str_extract("(?<=_).+$")
myentry_id_form <- myentry_id %>% str_extract("^.+(?=_)") # if the entry has citation form in its FLEx entry, then this line of code will extract that citation form from the entry ID

## get the CHILDREN elements of the main entry ======
## get the unique elements name of the <entry> node
myentry_children_name <- myentry %>% 
  map(~xml_children(.)) %>% 
  map(~xml_name(.)) %>% 
  unlist() %>% 
  unique()
myentry_children_name
# [1] "lexical-unit" (done)  "trait" (done)       "etymology" (done)     "relation"      "sense" (done)        
# [6] "note" (done)          "variant" (done)      "pronunciation" (NA) "citation" (done)


# LEXICAL-UNIT: extract the <lexical-unit> using purrr::map() for each <entry> =====

## gather the lexical unit =======
lu_form <- myentry %>% 
  # .[test_range] %>% 
  map(~xml_find_all(., "lexical-unit/form[@lang=\"eno\"]/text")) %>% 
  map(~xml_text(.))

### check the length of the form/text elements
lu_form_length <- lu_form |> map_int(length)
length(lu_form_length)

### duplicate the entry id as many as the number of elements of form/text
lu_form_entry_id <- rep(myentry_id_num, lu_form_length)
length(lu_form_entry_id)

### create the tibble for the lu form
lu_form_df <- lu_form |> 
  map(~tibble(form = .)) |> 
  list_rbind()

### create the tibble for the lu form
lu_form_df <- lu_form_df |> 
  mutate(entry_id = lu_form_entry_id)

## gather the language attribute of the form ======
lu_text_source <- myentry %>% 
  map(~xml_find_all(., "lexical-unit/form[@lang=\"eno\"]")) %>% 
  map(~xml_attr(., "lang"))
lu_text_source_length <- lu_text_source |> map_int(length)
lu_text_source_entry_id <- rep(myentry_id_num, lu_form_length)
lu_text_source_vect <- unlist(lu_text_source)
lu_text_source_df <- tibble(lang = lu_text_source_vect,
                            entry_id = lu_text_source_entry_id)

## gather the entry order =====
entry_attr <- myentry |> xml_attrs()
entry_attr_df <- entry_attr |> 
  map(~tibble(attr = ., types = names(.))) |> 
  map(~pivot_wider(., names_from = types, values_from = attr)) |> 
  list_rbind() |> 
  rename(entry_id = guid)

## gather the lexical unit TRAIT =====
lu_trait <- myentry %>% 
  map(~xml_find_first(., "trait[@name='morph-type']")) %>% 
  map(~xml_attr(., "value"))
lu_trait_vect <- rep(unlist(lu_trait), lu_form_length)

## gather the LU form, id, language source, lu_order, and trait
lu_form_df <- tibble(entry_id = lu_text_source_entry_id,
                     form = unlist(lu_form),
                     lang = unlist(lu_text_source),
                     trait = lu_trait_vect) |> 
  left_join(select(entry_attr_df, entry_id, order),
            by = join_by("entry_id"))

# SENSE: extract children elements within <sense>, including sense id attributes =====

# s <- senses[c(424, 1248, 248, 271, 1365, 1299)]
## find all <sense> nodes =====
senses <- myentry %>% 
  map(~xml_find_all(., "sense"))
names(senses) <- myentry_id_num

s <- senses

s |> 
  map(~map(., ~xml_attr(., "id"))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA))) -> s_id_interim
s_id_interim |> 
  map(unlist) |> 
  map(~tibble(sense_id = .)) -> s_id

s |> 
  map(~map(., ~xml_attr(., "order"))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA))) |> 
  map(unlist) |> 
  map(~tibble(sense_order = .)) -> s_order

s |> 
  map(~map(., ~xml_find_all(., "grammatical-info"))) |> 
  map(~map(., ~xml_attr(., "value"))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA))) |> 
  map(unlist) |> 
  map(~tibble(sense_gram = .)) -> s_gram

s |> 
  map(~map(., ~xml_find_all(., "gloss[@lang=\"en\"]/text"))) |> 
  map(~map(., ~xml_text(.))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA))) |> 
  map(unlist) |> 
  map(~tibble(sense_gloss_en = .)) -> s_ge

s |> 
  map(~map(., ~xml_find_all(., "gloss[@lang=\"id\"]/text"))) |> 
  map(~map(., ~xml_text(.))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA))) |> 
  map(unlist) |> 
  map(~tibble(sense_gloss_idn = .)) -> s_gn

s |> 
  map(~map(., ~xml_find_all(., "definition/form[@lang=\"en\"]/text"))) |> 
  map(~map(., ~xml_text(.))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA))) |> 
  map(unlist) |> 
  map(~tibble(sense_definition_en = .)) -> s_de

s |> 
  map(~map(., ~xml_find_all(., "definition/form[@lang=\"id\"]/text"))) |> 
  map(~map(., ~xml_text(.))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA))) |> 
  map(unlist) |> 
  map(~tibble(sense_definition_idn = .)) -> s_dn

s_df <- pmap(list(a = s_id, b = s_order, c = s_gram, d = s_ge, e = s_gn, 
                  f = s_de, g = s_dn),
             \(a, b, c, d, e, f, g) bind_cols(a, b, c, d, e, f, g))
s_df <- map2(.x = s_df, .y = names(s_df), ~mutate(.x, entry_id = .y)) |> 
  list_rbind()

lu_form_df <- lu_form_df |> 
  left_join(s_df)
# The code below checks if after combining different rows of senses of an entry, the entry's total is the same with FLEx lexicon
lu_form_df |> select(entry_id, form) |> distinct()

## EXAMPLE ======
# s_id_indices <- c(1448, 1454, 1462, 1257, 1263)
# s_id_mini <- s_id_interim[s_id_indices]
# s_ex <- s[s_id_indices] |> 
s_ex_eno <- s |> 
  map(~map(., ~xml_find_all(., "example/form[@lang=\"eno\"]"))) |> 
  map(~map(., ~xml_text(.))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA)))

# s_ex_eng <- s[s_id_indices] |> 
s_ex_eno_source <- s |> 
  map(~map(., ~xml_find_all(., "example[@source]"))) |> 
  map(~map(., ~xml_attr(., "source"))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA)))

s_ex_eng <- s |>
  map(~map(., ~xml_find_all(., "example/translation[@type=\"Free translation\"]"))) |> 
  map(~map(., ~xml_find_first(., "form[@lang=\"en\"]"))) |> 
  map(~map(., ~xml_text(.))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA)))

s_ex_idn <- s |>
  map(~map(., ~xml_find_all(., "example/translation[@type=\"Free translation\"]"))) |> 
  map(~map(., ~xml_find_first(., "form[@lang=\"id\"]"))) |> 
  map(~map(., ~xml_text(.))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA)))

s_ex_eno_df <- map2(s_ex_eno, s_id_interim, 
                    ~tibble(x_eno = .x, sense_id = .y)) |> 
  map(unnest_longer, c(x_eno, sense_id))
s_ex_eno_df <- map2(s_ex_eno_df, names(s_ex_eno_df), 
                    ~mutate(.x, entry_id = .y))

# s_ex_eno_source_df <- map2(s_ex_eno_source, s_id_interim, 
#                            ~tibble(x_eno_source = .x, sense_id = .y)) |>
#   map(unnest_longer, c(x_eno_source, sense_id))
# s_ex_eno_source_df <- map2(s_ex_eno_source_df, names(s_ex_eno_source_df), 
#                            ~mutate(.x, entry_id = .y))
s_ex_eno_source_df <- map(s_ex_eno_source, ~tibble(x_eno_source = .x)) |> 
  map(unnest_longer, x_eno_source)

# s_ex_eng_df <- map2(s_ex_eng, s_id_interim, 
#                     ~tibble(x_eng = .x, sense_id = .y)) |> 
#   map(unnest_longer, c(x_eng, sense_id))
# s_ex_eng_df <- map2(s_ex_eng_df, names(s_ex_eng_df), 
#                     ~mutate(.x, entry_id = .y))
s_ex_eng_df <- map(s_ex_eng, ~tibble(x_eng = .x)) |> 
  map(unnest_longer, x_eng)

# s_ex_idn_df <- map2(s_ex_idn, s_id_interim, 
#                     ~tibble(x_idn = .x, sense_id = .y)) |> 
#   map(unnest_longer, c(x_idn, sense_id))
# s_ex_idn_df <- map2(s_ex_idn_df, names(s_ex_idn_df), 
#                     ~mutate(.x, entry_id = .y))
s_ex_idn_df <- map(s_ex_idn, ~tibble(x_idn = .x)) |> 
  map(unnest_longer, x_idn)

# map2(s_ex_idn_df[c(899, 1298)], names(s_ex_idn_df)[c(899, 1298)], ~mutate(.x, entry_id = .y))
# map2(map2(s_ex_idn_df[c(899, 1298)], names(s_ex_idn_df)[c(899, 1298)], ~mutate(.x, entry_id = .y)), map2(s_ex_eno_df[c(899, 1298)], names(s_ex_eno_df)[c(899, 1298)], ~mutate(.x, entry_id = .y)), ~left_join(.x, .y, by = join_by("entry_id")))

ex_l <- list(a = s_ex_eno_source_df,
             b = s_ex_eno_df,
             c = s_ex_idn_df,
             d = s_ex_eng_df)

s_ex_all_df <- pmap(ex_l, \(a, b, c, d) bind_cols(a, b, c, d))

s_ex_all_df <- s_ex_all_df |> 
  map(~select(., entry_id, sense_id, everything(.)))

s_ex_all_df <- s_ex_all_df |> 
  map(~mutate(., across(where(is.logical), as.character))) |> 
  list_rbind()

# ETYMOLOGY ====
etym <- myentry |> 
  map(~xml_find_all(., "etymology"))
names(etym) <- myentry_id_num

etym_df <- etym |> 
  map(~xml_find_all(., "gloss[@lang=\"en\"]")) |> 
  map(~xml_text(.)) |> 
  map(\(x) if(identical(x, character(0))) NA else x) |> 
  map(\(x) replace(x, nchar(x) == 0, NA)) |> 
  map(\(x) tibble(etym = x))
etym_df <- etym_df |> 
  map2(.y = names(etym_df), ~mutate(.x, entry_id = .y)) |> 
  list_rbind()

etym_df1 <- etym |> 
  map(~xml_find_all(., "gloss[@lang=\"id\"]")) |> 
  map(~xml_text(.)) |> 
  map(\(x) if(identical(x, character(0))) NA else x) |> 
  map(\(x) replace(x, nchar(x) == 0, NA)) |> 
  map(\(x) tibble(etym = x))
etym_df1 <- etym_df1 |> 
  map2(.y = names(etym_df1), ~mutate(.x, entry_id = .y)) |> 
  list_rbind()

etym_type <- etym |> 
  map(~xml_attr(., "type")) |> 
  map(\(x) if(identical(x, character(0))) NA else x) |> 
  map(\(x) replace(x, nchar(x) == 0, NA)) |> 
  map(\(x) tibble(etym_type = x))
etym_type <- etym_type |> 
  map2(.y = names(etym_type), ~mutate(.x, entry_id = .y)) |> 
  list_rbind()

lu_form_df <- lu_form_df |> 
  left_join(etym_df) |> 
  left_join(etym_df1) |> 
  left_join(etym_type)

# VARIANT: extract <variant> using purrr::map() for each <entry> ======
var_form <- myentry %>% 
  map(~xml_find_all(., "variant/form[@lang=\"eno\"]/text")) %>% 
  map(~xml_text(.)) |> 
  map(\(x) replace(x, nchar(x) == 0, NA)) |>
  map(\(x) if(identical(x, character(0))) NA else x) |> 
  map(~paste(., collapse = " ; ")) |> 
  unlist()

var_traits <- myentry %>% 
  map(~xml_find_all(., "variant/trait[@name=\"morph-type\"]")) %>% 
  map(~xml_attr(., "value")) |> 
  map(\(x) replace(x, nchar(x) == 0, NA)) |>
  map(\(x) if(identical(x, character(0))) NA else x) |> 
  map(~paste(., collapse = " ; ")) |> 
  unlist()

variants_df <- tibble(variant_form = var_form, 
                      variant_trait = var_traits, 
                      entry_id = myentry_id_num) |> 
  mutate(across(matches("var"), ~replace(., . == "NA", NA)))

lu_form_df <- lu_form_df |> 
  left_join(variants_df) |> 
  arrange(form, order)

# FLORA AND FAUNA =====
## FLORA ====
flora_df1 <- read_rds("G:/.shortcut-targets-by-id/1MO3Q9KZIODxlfPRyjLTtDUZuVkecFBp6/Enggano/enggano-dictionary/flora-fauna/flora_df1.rds")
flora_root_in_flex_with_image <- flora_df1 |> 
  filter(is.na(MAIN_ENTRY)) |> 
  filter(IN_FLEX == "y") |> 
  mutate(POS = replace(POS, POS == "n", "Noun")) |>
  mutate(POS = replace(POS, POS == "v", "Verb")) |> 
  mutate(ENGGANO = stringi::stri_trans_nfc(ENGGANO)) |> 
  rename(form = ENGGANO,
         sense_gloss_en = ENGLISH,
         sense_gloss_idn = INDONESIAN) |>  # rename the relevant column for left join
  select(form, sense_gloss_en, sense_gloss_idn, sense_gram = POS, CROSSREF, pc, VARIANT)

## FAUNA ====
fauna_df1 <- read_rds("G:/.shortcut-targets-by-id/1MO3Q9KZIODxlfPRyjLTtDUZuVkecFBp6/Enggano/enggano-dictionary/flora-fauna/fauna_df1.rds")
fauna_root_in_flex_with_image <- fauna_df1 |> 
  filter(is.na(MAIN_ENTRY)) |> # get Enggano form that becomes the root/lex entry,
  filter(IN_FLEX == "y" # in Charlotte's FLEx
         ) |>  # and that has image
  mutate(POS = replace(POS, POS == "n", "Noun"),
         POS = replace(POS, POS == "v", "Verb")) |> 
  mutate(ENGGANO = stringi::stri_trans_nfc(ENGGANO)) |> 
  rename(form = ENGGANO,
         sense_gloss_en = ENGLISH,
         sense_gloss_idn = INDONESIAN) |>  # rename the relevant column for left join
  select(form, sense_gloss_en, sense_gloss_idn, sense_gram = POS, CROSSREF, pc, VARIANT)

# Don't include the example sentences from the commented code below
# lu_form_df <- lu_form_df |> 
#   left_join(s_ex_all_df)


# NOTE: extract the <note> for the lexeme entry ======
## re-run later
# notes <- myentry |> 
#   map(~xml_find_all(., 'note'))
# names(notes) <- myentry_id_num
# notes_children <- notes |> 
#   map(~xml_children(.)) |> 
#   map(~xml_name(.)) |> 
#   unlist() |> 
#   unique()
# notes_children
# # [1] "form"
# notes_grandchilren <- notes |> 
#   map(~xml_find_all(., 'form')) |> 
#   map(~xml_children(.)) |> 
#   map(~xml_name(.)) |> 
#   unlist() |> 
#   unique()
# notes_grandchilren
# # [1] "text"
# notes_text_lang <- notes |> 
#   map(~xml_find_all(., 'form')) |> 
#   map(~xml_attr(., 'lang'))
# notes_text_lang |> unlist() |> unique()
# # [1] "en" --- SO I DO NOT NEED TO MAKE A COLUMN FOR LANGUAGE OF THE NOTE
# notes_text <- notes |> 
#   map(~xml_find_all(., 'form/text')) |> 
#   map(~xml_text(.))
# notes_text_df <- map2(.x = notes_text,
#                       .y = names(notes_text),
#                       ~tibble(entry_id = .y,
#                               notes_lexeme = .x)) |> 
#   list_rbind() |> 
#   mutate(notes_lexeme = if_else(str_detect(notes_lexeme, "^\\("),
#                                 str_replace_all(notes_lexeme, "(^\\(|\\)$)", ""),
#                                 notes_lexeme))
# notes_text_df


# write_rds(lu_form_df, "output/textbook/lift/textbook-LIFT.rds")
# write_rds(lu_form_df, "output/textbook/lift/textbook-LIFT-20241110.rds")
# write_rds(lu_form_df, "output/textbook/lift/textbook-LIFT-20241129.rds")
write_rds(lu_form_df, "output/textbook/lift/textbook-LIFT-20241201.rds")
