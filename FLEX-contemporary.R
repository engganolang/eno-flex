library(tidyverse)
library(xml2)
library(googledrive)
library(googlesheets4)


flexdb <- read_xml("FLEX-contemporary.xml")


# MAIN ENTRY ======

# extract only the <entry>
myentry <- flexdb %>% 
  xml_find_all("entry")

# determine the length of children for each <entry>
myentry_length <- myentry %>% 
  xml_length()
myentry_length

# get the id for each <entry> node
myentry_id <- myentry %>% 
  xml_attr("id")
myentry_id_num <- myentry_id %>% str_extract("(?<=_).+$")
myentry_id_form <- myentry_id %>% str_extract("^.+(?=_)")

## get the CHILDREN elements of the main entry ======
## get the unique elements name of the <entry> node
myentry_children_name <- myentry %>% 
  map(~xml_children(.)) %>% 
  map(~xml_name(.)) %>% 
  unlist() %>% 
  unique()
myentry_children_name
# [1] "lexical-unit"  "trait"         "etymology"     "relation"      "sense"        
# [6] "note"          "variant"       "pronunciation" "citation"  


# LEXICAL-UNIT: extract the <lexical-unit> using purrr::map() for each <entry> =====

## gather the lexical unit =======
lu_form <- myentry %>% 
  # .[test_range] %>% 
  map(~xml_find_all(., "lexical-unit/form/text")) %>% 
  map(~xml_text(.))
### check the length of the form/text elements
lu_form_length <- lu_form |> map_int(length)
### duplicate the entry id as many as the number of elements of form/text
lu_form_entry_id <- rep(myentry_id_num, lu_form_length)
### create the tibble for the lu form
lu_form_df <- lu_form |> 
  map(~tibble(form = .)) |> 
  list_rbind()
### create the tibble for the lu form
lu_form_df <- lu_form_df |> 
  mutate(entry_id = lu_form_entry_id)

## gather the language attribute of the form ======
lu_text_source <- myentry %>% 
  map(~xml_find_all(., "lexical-unit/form")) %>% 
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

## find all <sense> nodes =====
senses <- myentry %>% 
  map(~xml_find_all(., "sense"))

## check the children of the "sense" element ====
senses |> map(~xml_children(.)) |> map(~xml_name(.)) |> unlist() |> unique()
### [1] "grammatical-info"            "gloss"            "definition"
senses |> map(~xml_find_all(.,"grammatical-info")) |> map(~xml_children(.)) |> map(~xml_name(.)) |> unlist() |> unique()
### [1] "trait"
senses |> map(~xml_find_all(.,"gloss")) |> map(~xml_children(.)) |> map(~xml_name(.)) |> unlist() |> unique()
### [1] "text"
senses |> map(~xml_find_all(.,"definition")) |> map(~xml_children(.)) |> map(~xml_name(.)) |> unlist() |> unique()
### [1] "form"

senses_attr <- senses |> 
  map(xml_attrs) |> 
  map(unlist)
names(senses_attr) <- myentry_id

## check the name of the attribute of sense element
senses_attr |> unlist() |> names() |> unique()
### [1] "id"    "order"

## check how many senses a given LU has
senses_attr_length <- senses_attr |> 
  map_int(length)
unique(senses_attr_length)
# [1] 1 4 6

## gather the id and sense order into a tibble depending on the number of senses an entry has.
senses_attr[which(senses_attr_length == 1)] <- senses_attr[which(senses_attr_length == 1)] |> 
  map(~tibble(sense_id = unname(.))) |> 
  map(~mutate(., order = NA))
senses_attr[which(senses_attr_length == 4)] <- senses_attr[which(senses_attr_length == 4)] |> 
  map(~str_c(.[c(1, 3)], "__", .[c(2, 4)], sep = "")) |> 
  map(~tibble(sense_id = .)) |> 
  map(~separate(., "sense_id", into = c("sense_id", "order"), sep = "__"))
senses_attr[which(senses_attr_length == 6)] <- senses_attr[which(senses_attr_length == 6)] |> 
  map(~str_c(.[c(1, 3, 5)], "__", .[c(2, 4, 6)], sep = "")) |> 
  map(~tibble(sense_id = .)) |> 
  map(~separate(., "sense_id", into = c("sense_id", "order"), sep = "__"))
senses_attr <- map2(.x = senses_attr, .y = names(senses_attr), ~mutate(.x, entry_id = .y))
senses_attr_df <- senses_attr |> 
  list_rbind() |> 
  select(entry_id, sense_id, order)



# names(senses) <- paste(myentry_id, "__", sep = "")
# senses_id <- senses |> 
#   map(~xml_attr(., "id")) |> 
#   unlist()
# senses_id <- tibble(entry_id = names(senses_id),
#                     sense_id = unname(senses_id))
# senses_id
# senses_order <- senses |> 
#   map(~xml_attr(., "order")) |> 
#   unlist()
# senses_order <- tibble(entry_id = names(senses_order),
#                        sense_order = unname(senses_order))
# senses_order

names(senses) <- myentry_id_num

senses_gloss <- senses |> 
  map(~xml_find_all(., "gloss"))
length(senses_gloss)
# [1] 1425
sum(map_int(senses_gloss, length))
# [1] 2900

senses_gloss_lang <- senses_gloss |> 
  map(~xml_attr(., "lang"))
length(senses_gloss_lang)
# [1] 1425
sum(map_int(senses_gloss_lang, length))
# [1] 2900

senses_gloss_form <- senses_gloss |> 
  map(~xml_find_all(., "text")) |> 
  map(xml_text)
length(senses_gloss_form)
# [1] 1425
sum(map_int(senses_gloss_form, length))
# [1] 2900

senses_gloss_id <- senses |> 
  map(~xml_attr(., "id"))
senses_gloss_order <- senses |> 
  map(~xml_attr(., "order"))
sense_gloss_id_order <- map2(.x = senses_gloss_id, 
                             .y = senses_gloss_order, 
                             ~tibble(sense_id = .x, 
                                     sense_order = .y)) |> 
  list_rbind()

senses_gloss_lang_df <- map2(.x = senses_gloss_lang, 
                             .y = senses_gloss_form, 
                             ~tibble(sense_lang = .x, 
                                     sense_form = .y))
senses_gloss_lang_df <- map2(.x = senses_gloss_lang_df,
                             .y = names(senses_gloss_lang_df),
                             ~mutate(.x, entry_id = .y))
senses_gloss_lang_df <- list_rbind(senses_gloss_lang_df) |> 
  group_by(entry_id) |> 
  mutate(sense_form_n = length(sense_form)) |> 
  ungroup()
nrow(senses_gloss_lang_df)
# [1] 2900
sort(unique(senses_gloss_lang_df$sense_form_n))
# [1] 1 2 3 4 5 6





senses_gloss_lang_df1 <- senses_gloss_lang_df |> 
  group_by(entry_id) |> 
  mutate(sense_langs = if_else(sense_form_n == 2,
                               paste(sense_lang, collapse = "___"),
                               NA),
         sense_forms = if_else(sense_form_n == 2,
                               paste(sense_form, collapse = "___"),
                               NA),
         sense_langs = if_else(sense_form_n == 3,
                               paste(sense_lang, collapse = "___"),
                               sense_langs),
         sense_forms = if_else(sense_form_n == 3,
                               paste(sense_form, collapse = "___"),
                               sense_forms),
         sense_langs = if_else(sense_form_n == 4,
                               paste(sense_lang, collapse = "___"),
                               sense_langs),
         sense_forms = if_else(sense_form_n == 4,
                               paste(sense_form, collapse = "___"),
                               sense_forms),
         sense_langs = if_else(sense_form_n == 5,
                               paste(sense_lang, collapse = "___"),
                               sense_langs),
         sense_forms = if_else(sense_form_n == 5,
                               paste(sense_form, collapse = "___"),
                               sense_forms),
         sense_langs = if_else(sense_form_n == 6,
                               paste(sense_lang, collapse = "___"),
                               sense_langs),
         sense_forms = if_else(sense_form_n == 6,
                               paste(sense_form, collapse = "___"),
                               sense_forms))
senses_gloss_lang_df2 <- senses_gloss_lang_df1 |> 
  mutate(sense_langs = if_else(sense_form_n == 6,
                              str_replace_all(sense_langs,
                                              "^(..___..)___(..___..)___(..___..)$",
                                              "\\1@\\2@\\3"),
                              sense_langs),
         sense_forms = if_else(sense_form_n == 6,
                               str_replace_all(sense_forms,
                                               "^([^_]+?___[^_]+?)___([^_]+?___[^_]+?)___([^_]+?___[^_]+?)$",
                                               "\\1@\\2@\\3"),
                               sense_forms),
         sense_langs = if_else(sense_form_n == 4,
                               str_replace_all(sense_langs,
                                               "^(..___..)___(..___..)$",
                                               "\\1@\\2"),
                               sense_langs),
         sense_forms = if_else(sense_form_n == 4,
                               str_replace_all(sense_forms,
                                               "^([^_]+?___[^_]+?)___([^_]+?___[^_]+?)$",
                                               "\\1@\\2"),
                               sense_forms),
         sense_langs = if_else(sense_form_n == 3,
                               str_replace_all(sense_langs,
                                               "^(..___..)___(..)$",
                                               "\\1@\\2"),
                               sense_langs),
         sense_forms = if_else(sense_form_n == 3,
                               str_replace_all(sense_forms,
                                               "^([^_]+?___[^_]+?)___([^_]+?)$",
                                               "\\1@\\2"),
                               sense_forms),
         sense_langs = if_else(sense_form_n == 5,
                               str_replace_all(sense_langs,
                                               "^(..___..)___(..___..)___(..)$",
                                               "\\1@\\2@\\3"),
                               sense_langs),
         sense_forms = if_else(sense_form_n == 5,
                               str_replace_all(sense_forms,
                                               "^([^_]+?___[^_]+?)___([^_]+?___[^_]+?)___([^_]+?)$",
                                               "\\1@\\2@\\3"),
                               sense_forms),
         sense_langs = if_else(sense_form_n == 1,
                               sense_lang,
                               sense_langs),
         sense_forms = if_else(sense_form_n == 1,
                               sense_form,
                               sense_forms)) |> 
  select(entry_id, sense_form_n, sense_langs, sense_forms) |> 
  distinct()

### IMPORTANT TO CHECK WHICH ENTRY HAS A SINGLE-LANGUAGE GLOSS (i.e., not pairing between en and id, such as en, and en)
### this en and en indicates that each of these English glosses has its own sense id and needs special treatment
senses_gloss_lang_df2 |> 
  filter(str_detect(sense_langs, "\\ben_+en\\b")) # checking -- POSITIVE results
senses_gloss_lang_df2 |> 
  filter(str_detect(sense_langs, "\\bid_+id\\b")) # checking -- NULL results
senses_gloss_lang_df2 |> 
  filter(str_detect(sense_langs, "@id$")) # checking -- NULL results
senses_gloss_lang_df2 |> 
  filter(str_detect(sense_langs, "\\bid_+en\\b")) # checking -- NULL results
#### the code below is customised for the output of the above # checking code
senses_gloss_lang_df2 <- senses_gloss_lang_df2 |> 
  mutate(sense_langs = if_else(entry_id == "3b3467d3-d573-43f6-b697-2bcc6871d5c2",
                               str_replace_all(sense_langs, 
                                               "\\b(en)___(en)\\b",
                                               "\\1@\\2"),
                               sense_langs),
         sense_forms = if_else(entry_id == "3b3467d3-d573-43f6-b697-2bcc6871d5c2",
                               str_replace_all(sense_forms,
                                               "___",
                                               "@"),
                               sense_forms),
         sense_langs = if_else(entry_id == "d3cb53e0-5aef-4123-9135-4e9a95c78e70",
                               str_replace_all(sense_langs,
                                               "\\b(en)___(en)\\b",
                                               "\\1@\\2"),
                               sense_langs),
         sense_forms = if_else(entry_id == "d3cb53e0-5aef-4123-9135-4e9a95c78e70",
                               str_replace_all(sense_forms,
                                               "(\\@[^_]+)___([^_@]+$)",
                                               "\\1@\\2"),
                               sense_forms))


senses_gloss_lang_df3 <- senses_gloss_lang_df2 |> 
  mutate(sense_langs = str_split(sense_langs, "\\@"),
         sense_forms = str_split(sense_forms, "\\@")) |> 
  unnest(c(sense_langs, sense_forms)) |> 
  separate(col = sense_langs, 
           into = c("english", "indonesian"),
           sep = "___",
           fill = 'right') |> 
  separate(col = sense_forms, 
           into = c("english_form", "indonesian_form"),
           sep = "___",
           fill = 'right')
senses_gloss_lang_df3

## add the sense_id and sense_order, and the lu form =====
senses_gloss_lang_df4 <- senses_gloss_lang_df3 |> 
  ungroup() |> 
  mutate(sense_id = senses_attr_df$sense_id, 
         sense_order = senses_attr_df$order) |> 
  left_join(lu_form_df, relationship = "many-to-many", by = join_by("entry_id")) |> 
  select(entry_id, form, lang, trait, order, sense_id, sense_order, english, english_form, indonesian, indonesian_form)





# SENSES: GRAMMATICAL INFO =====

# senses_grm <- senses |> 
#   map(~xml_find_all(., "grammatical-info"))
# senses_grm_n <- senses_grm |> 
#   map(length) |> 
#   map(~tibble(grm_n = .))
# senses_grm_n <- map2(.x = senses_grm_n,
#                      .y = names(senses_grm),
#                      ~mutate(.x, entry_id = .y)) |> 
#   list_rbind()
# 
# senses_grm_vals <- senses_grm |> 
#   map(~xml_attr(., "value"))
# senses_grm_vals[which(senses_grm_n$grm_n<1)] <- NA
# senses_grm_vals_df <- senses_grm_vals |> 
#   map(~tibble(vals = .))
# senses_grm_vals_df <- map2(.x = senses_grm_vals_df,
#                            .y = names(senses_grm_vals),
#                            ~mutate(.x, entry_id = .y)) |> 
#   list_rbind()
# 
# senses_grm_txt <- senses_grm |> 
#   map(xml_text)

senses_grm <- senses |> 
  map(as.character) |> 
  unlist()
senses_grm_vals <- senses_grm |> 
  str_extract("(?<=grammatical\\-info)[^<]+<\\/grammatical.info\\>") |> 
  str_extract("(?<=value\\=\")[^\"]+(?=\")")
senses_grm_id <- senses_grm |> 
  str_extract("(?<=sense id\\=\")[^\"]+(?=\"\\>)")
senses_grm_order <- senses_grm |> 
  str_extract("(?<=order\\=\")\\d(?=\"\\>)")
senses_grm_entry_id <- names(senses_grm)
senses_grm_df <- tibble(entry_id = senses_grm_entry_id,
                        sense_id = senses_grm_id,
                        # sense_order = senses_grm_order,
                        gram_vals = senses_grm_vals)
## join the senses_gloss_lang_df4 with senses_grm_df
senses_gloss_lang_df5 <- senses_gloss_lang_df4 |> 
  left_join(senses_grm_df, by = join_by(entry_id, sense_id))
senses_gloss_lang_df5










# ETYMOLOGY ========
## get the etymology element =====
etym <- myentry |> 
  map(~xml_find_all(., "etymology"))

### check the attributes and their names of the etymology element ====
etym |> 
  map(~xml_attrs(.)) |> 
  map(unlist) |> 
  unlist() |> 
  unique()
# [1] "borrowed" "" 
etym |> 
  map(~xml_attrs(.)) |> 
  map(unlist) |> 
  unlist() |> 
  names() |> 
  unique()
# [1] "type"   "source"

### check the children of the etymology =====
etym |> 
  map(~xml_children(.)) |> 
  map(~xml_name(.)) |> 
  unlist() |> 
  unique()
### [1] "gloss" "field"
#### check the grand children of </etymology> ======
etym |> 
  map(~xml_find_all(., "gloss")) |> 
  map(~xml_children(.)) |> 
  map(~xml_name(.)) |> 
  unlist() |> 
  unique()
# [1] "text"

##### check the children of </text> =====
etym |> 
  map(~xml_find_all(., "gloss/text")) |> 
  map(~xml_children(.)) |> 
  map(~xml_name(.)) |> 
  unlist() |> 
  unique()
# [1] "span"

## get the text element under gloss =====
etym_text <- etym |> 
  map(~xml_find_all(., "gloss/text")) |> 
  map(~xml_text(.))
### element 187 has two elements
names(etym_text) <- myentry_id_num

### create a tibble for the text element under gloss =====
etym_df <- map2(.x = names(etym_text), 
                .y = etym_text, 
                ~tibble(entry_id = .x, etyms = .y)) |> 
  list_rbind()


### join the senses, lu, and etym into one tibble =====
senses_gloss_lang_df6 <- senses_gloss_lang_df5 |> 
  left_join(etym_df, 
            by = join_by(entry_id), 
            relationship = "many-to-many")












# VARIANT: extract <variant> using purrr::map() for each <entry> ======


variant <- myentry |> 
  map(~xml_find_all(., "variant"))

variant |> 
  map(~xml_attrs(.)) |> 
  map(unlist) |> 
  unlist() |> 
  unique()
# character(0)

### check the children of the VARIANT =====
variant |> 
  map(~xml_children(.)) |> 
  map(~xml_name(.)) |> 
  unlist() |> 
  unique()
# [1] "form"  "trait"

## gather the <variant> forms ====
variant_form <- myentry %>% 
  # .[test_range] %>% 
  map(~xml_find_all(., "variant/form/text")) %>% 
  map(~xml_text(.)) # %>% 
variant_form[lengths(variant_form) == 0] <- NA
# map(~tibble(variant_form = .)) %>% 
# map2_df(.x = ., .y = myentry_id, ~mutate(.x, id = .y)) %>%  # add the "id" attributes
# select(id, everything())
# map2(.x = ., .y = myentry_id_form, ~mutate(.x, lu = .y)) %>% 
# map2_df(.x = ., .y = myentry_id_num, ~mutate(.x, id = .y)) # add the numeric id
names(variant_form) <- myentry_id_num
variant_form_df <- map2(.x = names(variant_form), 
                        .y = variant_form,
                        ~tibble(entry_id = .x,
                                variants = .y)) |> 
  list_rbind()
variant_form_df1 <- variant_form_df |> 
  filter(!is.na(variants)) |> 
  group_by(entry_id) |> 
  mutate(variants1 = paste(variants, collapse = " ; ")) |> 
  ungroup() |> 
  select(entry_id, variants = variants1) |> 
  distinct()

## gather the <variant> <trait> values ====
variant_traits <- myentry %>% 
  # .[test_range] %>%
  map(~xml_find_all(., "variant")) %>% 
  map(~xml_find_all(., "trait[@name='morph-type']")) %>% 
  map(~xml_attr(., "value"))
variant_traits[lengths(variant_traits) == 0] <- NA
names(variant_traits) <- myentry_id_num
variant_traits_df <- map2(.x = names(variant_traits),
                          .y = variant_traits,
                          ~tibble(entry_id = .x,
                                  variant_form_traits = .y)) |> 
  list_rbind()

variant_traits_df1 <- variant_traits_df |> 
  filter(!is.na(variant_form_traits)) |> 
  group_by(entry_id) |> 
  mutate(variant_form_traits = paste(variant_form_traits, collapse = " ; ")) |> 
  ungroup() |> 
  select(entry_id, variant_form_traits) |> 
  distinct()

## Join the variant FORMS and TRAITS ========
variants_df <- variant_form_df1 |> left_join(variant_traits_df1, by = join_by("entry_id"))

## create a tibble of <variant> forms and <trait> ====== #
# variant_form1 <- unlist(variant_form)
# variant_form1_names <- names(variant_form1)
# variant_form1_names <- str_replace_all(variant_form1_names, "__.*$", "")
# variant_form1_df <- tibble(variant_form = unname(variant_form1), id = variant_form1_names)
# 
# variant_traits1 <- unlist(variant_traits)
# variant_traits1_names <- names(variant_traits1)
# variant_traits1_names <- str_replace_all(variant_traits1_names, "__.*$", "")
# variant_traits1_df <- tibble(variant_morph_type = unname(variant_traits1), id = variant_traits1_names)
# 
# variant_form_and_trait_df <- variant_form1_df |> 
#   filter(!is.na(variant_form)) |> 
#   left_join(variant_traits1_df |> 
#               filter(!is.na(variant_morph_type)), 
#             by = join_by("id"), 
#             relationship = "many-to-many") |> 
#   distinct()



### JOIN the variant with the LU and senses =======

senses_gloss_lang_df7 <- senses_gloss_lang_df6 |> 
  left_join(variants_df, by = join_by("entry_id"))


### save to google spreadsheets TO BE CHECKED WITH THE BASIC VOCAB LIST ======
#### create an empty google spreadsheet ======
drive_create(name = "FLEx_contemporary_lexicon",
             path = "https://drive.google.com/drive/folders/1YalnbNCFVC01oPGZAwtmudlmyUfUyNdI",
             type = "spreadsheet")
# Created Drive file:
#   • FLEx_contemporary_lexicon <id: 1hY1KtfpxOr6v3-NoahryQVHTvMc7T369zyFrXbGvSzo>
#   With MIME type:
#   • application/vnd.google-apps.spreadsheet

#### now save the FLEX lexicon to the google spreadsheet ======
##### full entries
sheet_write(senses_gloss_lang_df7,
            ss = "1hY1KtfpxOr6v3-NoahryQVHTvMc7T369zyFrXbGvSzo",
            sheet = "Sheet1")
##### selected ones for checking with the basic vocab list
senses_gloss_lang_df7 |> 
  select(entry_id, trait, form, sense_id, english_form, indonesian_form, gram_vals) |> 
  sheet_write(ss = "1hY1KtfpxOr6v3-NoahryQVHTvMc7T369zyFrXbGvSzo",
              sheet = "To-check-with-basic-vocab")
senses_gloss_lang_df7 |> 
  write_rds("senses_gloss_lang_df7.rds")



## combine lexical unit, variant_form and variant_trait ======= #
# lu_form_df1 <- lu_form_df |> 
#   left_join(variant_form_and_trait_df, 
#             by = join_by("id"), 
#             relationship = "many-to-many")

# ================================ #


# IGNORE THE FOLLOWING CODES (OLD ONES)

# LEXICAL-UNIT (with SENSE data): extract the <lexical-unit> using purrr::map() for each <entry> =====
## gather the lexical unit, text source, and lexical unit trait =======
lu_form <- myentry %>% 
  .[sort(c(test_range, test_range_sense_one))] %>% 
  map(~xml_find_all(., "lexical-unit/form/text")) %>% 
  map(~xml_text(.))
lu_order <- myentry %>% 
  .[sort(c(test_range, test_range_sense_one))] %>% 
  xml_attr("order")
lu_text_source <- myentry %>% 
  .[sort(c(test_range, test_range_sense_one))] %>% 
  map(~xml_find_all(., "lexical-unit/form")) %>% 
  map(~xml_attr(., "lang"))
lu_trait <- myentry %>% 
  .[sort(c(test_range, test_range_sense_one))] %>% 
  map(~xml_find_all(., "trait[@name='morph-type']")) %>% 
  map(~xml_attr(., "value"))

## create the tibble =====
lu_form_df <- pmap_df(list(a = lu_form, b = lu_text_source, c = lu_trait, d = myentry_id, e = lu_order),
                      function(d, a, e, c, b) tibble(id = d,
                                                     lu = a,
                                                     lu_order = e,
                                                     lu_morph_type = c,
                                                     text_source = b))
