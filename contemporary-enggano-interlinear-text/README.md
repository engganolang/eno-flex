My personal notes on processing the .lift and .flextext exported data
================
[Gede Primahadi Wijaya
Rajeg](https://www.ling-phil.ox.ac.uk/people/gede-rajeg)
<a itemprop="sameAs" content="https://orcid.org/0000-0002-2047-8621" href="https://orcid.org/0000-0002-2047-8621" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a><br>University
of Oxford, UK & Universitas Udayana, Indonesia

- [Code file for processing the exported .lift data from FLEx
  `Lexicon`](#code-file-for-processing-the-exported-lift-data-from-flex-lexicon)
- [Code files for processing the exported .flextext data from FLEx
  interlinear text in
  `Texts and Words`](#code-files-for-processing-the-exported-flextext-data-from-flex-interlinear-text-in-texts-and-words)
- [The exported .lift data from FLEx
  `Lexicon`](#the-exported-lift-data-from-flex-lexicon)
- [On the “eno_word_id” value](#on-the-eno_word_id-value)
- [Textbook materials](#textbook-materials)
  - [Processing steps](#processing-steps)
    - [Processing the exported .flextext data for the
      Textbook](#processing-the-exported-flextext-data-for-the-textbook)
    - [Processing the exported .lift
      data](#processing-the-exported-lift-data)
    - [Processing into Standard Format Marker (SFM)
      file](#processing-into-standard-format-marker-sfm-file)
  - [On importing to FLEx](#on-importing-to-flex)
    - [Entries to merge](#entries-to-merge)
    - [Change sub-entries from derivative to
      variant](#change-sub-entries-from-derivative-to-variant)
- [On sub-entries](#on-sub-entries)
  - [Sub-entries type](#sub-entries-type)

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
[<img src="../file-oxweb-logo.gif" width="84"
alt="The University of Oxford" />](https://www.ox.ac.uk/)
[<img src="../file-lingphil.png" width="83"
alt="Faculty of Linguistics, Philology and Phonetics, the University of Oxford" />](https://www.ling-phil.ox.ac.uk/)
[<img src="../file-ahrc.png" width="325"
alt="Arts and Humanities Research Council (AHRC)" />](https://www.ukri.org/councils/ahrc/)
</br>*This work is part of the [AHRC-funded project
(AH/W007290/1)](https://app.dimensions.ai/details/grant/grant.12915105)
on the lexical resources for Enggano, led by the Faculty of Linguistics,
Philology and Phonetics at the University of Oxford, UK. Visit the
[central webpage of the Enggano
project](https://enggano.ling-phil.ox.ac.uk/)*.
<p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/">
<a property="dct:title" rel="cc:attributionURL" href="https://github.com/engganolang/eno-flex">R
codes and dataset for processing the Enggano FLEx database into the
digital and printed Contemporary Enggano dictionary</a> by
<a rel="cc:attributionURL dct:creator" property="cc:attributionName" href="https://orcid.org/0000-0002-2047-8621">Gede
Primahadi Wijaya Rajeg</a> is licensed under
<a href="https://creativecommons.org/licenses/by-nc-sa/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">Creative
Commons Attribution-NonCommercial-ShareAlike 4.0
International<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/nc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/sa.svg?ref=chooser-v1" alt=""></a>
</p>
<!-- badges: end -->

# Code file for processing the exported .lift data from FLEx `Lexicon`

The code file to process the .lift export from FLEx `Lexicon` is
[FLEX-contemporary-with-examples-new.R](https://github.com/engganolang/eno-flex/blob/main/FLEX-contemporary-with-examples-new.R).
This code file is the basis for the following output data, namely:

1.  “`FLEX-lift-pre-fieldwork.rds`” (as per 2023)

2.  “`FLEX-lift-march-2024.rds`” (as per March 2024 project meeting at
    Udayana University)

# Code files for processing the exported .flextext data from FLEx interlinear text in `Texts and Words`

The codes to process the .flextext export data need to be run in the
order given below. However, before these codes are run, I need to fix
anomaly in the .flextext output based on the notes given in
[all-contemp-interlinear-texts-NOTES.txt](https://github.com/engganolang/eno-flex/blob/main/contemporary-enggano-interlinear-text/all-contemp-interlinear-texts-NOTES.txt).

1)  [processing-all-contemporary-texts-ELAN-FLEX-flextext-NEW.R](https://github.com/engganolang/eno-flex/blob/main/contemporary-enggano-interlinear-text/processing-all-contemporary-texts-ELAN-FLEX-flextext-NEW.R)

**Description** : This code file in
[(1)](https://github.com/engganolang/eno-flex/blob/main/contemporary-enggano-interlinear-text/processing-all-contemporary-texts-ELAN-FLEX-flextext-NEW.R)
processes the .flextext data for the natural language corpus (see the
`A. Natural texts` section in the code) and elicitation materials (see
the `B. Elicitation` section in the code).

**Input data**:

- The input file for the natural corpus is
  “`contemporary-enggano-interlinear-text/all-contemp-texts-with-idn-gloss-march2024.flextext`”
  (for the March Project Meeting 2024 version) and
  “`contemporary-enggano-interlinear-text/all-contemp-texts-with-idn-gloss.flextext`”
  (for the older, post first fieldwork version in late 2023).

- The input file for the elicitation data is
  “`contemporary-enggano-interlinear-text/all-contemp-elicitation-march2024.flextext`”
  (for the March Project Meeting 2024 version) and
  “`contemporary-enggano-interlinear-text/all-contemp-elicitation.flextext`”
  (for the older, post first fieldwork version in late 2023).

**Output data**: The codes in
[(1)](https://github.com/engganolang/eno-flex/blob/main/contemporary-enggano-interlinear-text/processing-all-contemporary-texts-ELAN-FLEX-flextext-NEW.R)
generates the output data in .rds.

- The first is .rds file for the original texts and their English and
  Indonesian translation in tibble format (the combined/binded one from
  the list version appears in
  “`contemporary-enggano-interlinear-text/eno_contemp_text_only_as_tibble-new-binded-march2024.rds`”
  for the natural corpus and in
  “`contemporary-enggano-interlinear-text/eno_contemp_elicitation_only_as_tibble-new-binded-march2024.rds`”
  for the elicitation).

- The second .rds data generated by
  [(1)](https://github.com/engganolang/eno-flex/blob/main/contemporary-enggano-interlinear-text/processing-all-contemporary-texts-ELAN-FLEX-flextext-NEW.R)
  is for the words/lexicon (generated via the for-loop in the code
  file); this data appears as tibbles in
  “`contemporary-enggano-interlinear-text/eno_contemp_text_as_tibble-new-march2024.rds`”
  for the natural corpus and in
  “`contemporary-enggano-interlinear-text/eno_contemp_elicitation_as_tibble-new-march2024.rds`”
  for the elicitation.

------------------------------------------------------------------------

2)  [processing-all-contemporary-texts-ELAN-FLEX-flextext-NEW-splitting-morpheme.R](https://github.com/engganolang/eno-flex/blob/main/contemporary-enggano-interlinear-text/processing-all-contemporary-texts-ELAN-FLEX-flextext-NEW-splitting-morpheme.R)
    (NB. this code is called from within the code to generate the SFM
    file; cf. code file in (3) below)

------------------------------------------------------------------------

3)  [processing-all-contemporary-texts-ELAN-FLEX-flextext-NEW-SFM.R](https://github.com/engganolang/eno-flex/blob/main/contemporary-enggano-interlinear-text/processing-all-contemporary-texts-ELAN-FLEX-flextext-NEW-SFM.R)
    (NB. from within this code, we call the following two codes:
    [processing-all-contemporary-texts-ELAN-FLEX-flextext-NEW-splitting-morpheme.R](https://github.com/engganolang/eno-flex/blob/main/contemporary-enggano-interlinear-text/processing-all-contemporary-texts-ELAN-FLEX-flextext-NEW-splitting-morpheme.R)
    (code file 2) and
    [gloss_fixing.R](https://github.com/engganolang/eno-flex/blob/main/contemporary-enggano-interlinear-text/gloss_fixing.R))

# The exported .lift data from FLEx `Lexicon`

The latest, exported `Lexicon` data based on the code file
[FLEX-contemporary-with-examples-new.R](https://github.com/engganolang/eno-flex/blob/main/FLEX-contemporary-with-examples-new.R)
(pre- and post-October-2023 fieldwork called
`FLEX-lift-pre-fieldwork.rds`, or the March 2024 version called
`FLEX-lift-march-2024.rds`) contains some example sentences for certain
lexical entries. This leads to the presence of duplicated observation in
`FLEX-lift-pre-fieldwork.rds`/`FLEX-lift-march-2024.rds` (to be read
into R as `lu_form_df` tibble). The example sentences are contained in
the `"x_eno"`, `"x_idn"`, and `"x_eng"` columns of the `lu_form_df`.

Even though the `x_...` columns for example sentences have been excluded
and then the `distinct()` function has been run, there are still
duplicates due to the presence of multiple senses of a given lexical
entry. These multiple senses (shown in the `"sense_gloss_en"` and/or
`"sense_gloss_idn"` column of the `lu_form_df` tibble) are represented
into different rows. The example is *ueh* that is glossed as ‘sleep’ and
‘lie’. Hence, *ueh* has two entries/rows in `lu_form_df`.

The following code shows how to get the entries that are duplicated due
to having more than one sense/gloss.

``` r
duplicated_entries <- lu_form_df |> 
  
  # exclude the example sentences column
  select(!matches("^x_")) |> 
  
  # get distinct observation
  distinct() |> 
  
  # count the number of entry_id, showing there are duplicated entries, still.
  count(entry_id, sort = TRUE)

duplicated_entries
#> # A tibble: 1,498 × 2
#>    entry_id                                 n
#>    <chr>                                <int>
#>  1 08783f5e-4959-4e0d-9116-66fe4521a642     3
#>  2 1f424931-2ea2-451a-a1c2-4df563d5c02f     3
#>  3 4770df89-2cbf-461b-85e4-622c5672c9a4     3
#>  4 559176ad-5709-4954-a2fe-43bae2266860     3
#>  5 bc8d7273-8c4e-4594-9f4c-61e12c5baa14     3
#>  6 d3cb53e0-5aef-4123-9135-4e9a95c78e70     3
#>  7 d95e6c32-37a6-4fb8-831d-ab766d92ebcf     3
#>  8 028ed846-636e-44c0-ac94-bbc114b0db90     2
#>  9 05560db9-b94c-46d4-827b-063875aaab42     2
#> 10 0f85ae05-e368-4f71-b4e4-cd6d206d9f22     2
#> # ℹ 1,488 more rows
```

The following code shows preview of entries with multiple senses.

``` r
lu_form_df |> 
  
  # From the `duplicated_entries` tibble above,
  # filter only `"entry_id"` column whose occurrences are more than once,
  # suggesting that such entry has more than one sense/gloss.
  filter(entry_id %in% pull(filter(duplicated_entries, n > 1),
                            entry_id)) |> 
  
  # Print the relevant columns
  select(entry_id, form, sense_order, sense_gloss_en, sense_gloss_idn)
#> # A tibble: 121 × 5
#>    entry_id                     form  sense_order sense_gloss_en sense_gloss_idn
#>    <chr>                        <chr> <chr>       <chr>          <chr>          
#>  1 8cf2917d-78b3-449b-a220-d35… 'ueh  0           "sleep "       tidur          
#>  2 8cf2917d-78b3-449b-a220-d35… 'ueh  1           "lie"          berbaring      
#>  3 3b3467d3-d573-43f6-b697-2bc… -de   0           "3SG.POSS"     <NA>           
#>  4 3b3467d3-d573-43f6-b697-2bc… -de   1           "DEF"          <NA>           
#>  5 8678edc2-cb75-4601-b038-c22… a-    0           "if"           kalau          
#>  6 8678edc2-cb75-4601-b038-c22… a-    1           "when"         kalau          
#>  7 37a9f7dd-1759-4db6-aef8-f2b… abu̇h  0           "already"      sudah          
#>  8 37a9f7dd-1759-4db6-aef8-f2b… abu̇h  1           "done"         selesai        
#>  9 3729b50e-750d-4c31-9b19-67e… ahao  0           "custom"       adat           
#> 10 3729b50e-750d-4c31-9b19-67e… ahao  1           "forbid"       <NA>           
#> # ℹ 111 more rows
```

# On the “eno_word_id” value

- IMPORTANT: The same word form may have different `eno_word_id` because
  this same word form comes from different texts!

- For example, the word “kapa’ueh” ‘put; lay down’ has three unique IDs
  because these three IDs are coming from three different texts for this
  one word.

# Textbook materials

This section contains my notes to process the textbook-based print
dictionary from the .flextext and .lift outputs.

The R packages used are tidyverse, xml2, and stringi

- IMPORTANT: Ensure in FLEx gloss tab that the writing system keyboard
  in each free translation field is a match!

  - If this is not the case, the use of English keyboard in Indonesian
    field will render the language element of the .flextext output as
    “en” for this Indonesian field.

## Processing steps

The very first steps among others are exporting (i) the .flextext file
from FLEx Analyze tab in TEXTS AND WORDS and (ii) the .lift file from
FLEx LEXICON feature.

The latest exported files from these two components of FLEx that are
used for the print dictionary are:

- `textbook/textbook-interlinear-20241201.flextext`

- `textbook/textbook-LIFT-20241201.lift`

\[TO BE UPDATED: how to handle FLORA and FAUNA, and CULTURAL DATA for
photos\]

### Processing the exported .flextext data for the Textbook

#### Input data

The input data file is
`textbook/textbook-interlinear-20241201.flextext`.

#### Code file/bits

The R codes used are under the [`C. Textbooks`
section](https://github.com/engganolang/eno-flex/blob/ffd88cebe419b1f9546700a0fa563113a16d985b/contemporary-enggano-interlinear-text/processing-all-contemporary-texts-ELAN-FLEX-flextext-NEW.R#L672)
in the code file
[`contemporary-enggano-interlinear-text/processing-all-contemporary-texts-ELAN-FLEX-flextext-NEW.R`](https://github.com/engganolang/eno-flex/blob/main/contemporary-enggano-interlinear-text/processing-all-contemporary-texts-ELAN-FLEX-flextext-NEW.R)\`.

#### Output/generated file/data

There are several outputs of data stored under the `textbook`
sub-directory (included in the filename of the generated data below for
expository purpose):

- **textbook/textbook_as_tibble_20241201.rds**. It contains the Enggano
  texts and free translations in English and Indonesian, and the
  source/reference information. This is an R list of data frames/tibbles
  per text title. (the latest data to use before importing into FLEx
  whereby I performed further manual on-the-spot post-processing in the
  newly imported FLEx project)

- **textbook/textbook_as_tibble_20241201-binded.rds**. It is the binded
  combined version of **textbook/textbook_as_tibble_20241201.rds** into
  a tibble data. (the latest data to use before importing into FLEx
  whereby I performed further manual on-the-spot post-processing in the
  newly imported FLEx project)

- ~~**contemporary-enggano-interlinear-text/textbook_lexicon_as_tibble_oct-2024.rds**~~
  **textbook/textbook_lexicon_as_tibble_20241201.rds**. This is a tibble
  of interlinearised morpheme-by-morpheme analyses (the latest data to
  use before importing into FLEx whereby I performed further manual
  on-the-spot post-processing in the newly imported FLEx project)

### Processing the exported .lift data

#### Input data

The input data file is `textbook/textbook-LIFT-20241201.lift`.

#### Code file/bits

The R code used is
[`contemporary-enggano-interlinear-text/textbook-LIFT-processing-code.R`](https://github.com/engganolang/eno-flex/blob/main/contemporary-enggano-interlinear-text/textbook-LIFT-processing-code.R).

#### Output/generated file/data

The main output file from processing the .lift file is called
**textbook/textbook-LIFT-20241201.rds**.

### Processing into Standard Format Marker (SFM) file

#### Input data

The input data uses the generated data from the previous two steps (plus
data from FLORA and FAUNA \[Details to be updated\]).

#### Code file/bits

There are two main codes:

1.  [contemporary-enggano-interlinear-text/textbook-splitting-code.R](https://github.com/engganolang/eno-flex/blob/main/contemporary-enggano-interlinear-text/textbook-splitting-code.R).
    This code file split the combined morphemes into their own entry/row
    for tidy data format. The input data for this code is
    `textbook/textbook_lexicon_as_tibble_20241201.rds`. This first code
    file is internally called from the second code below.

2.  [contemporary-enggano-interlinear-text/textbook-SFM.R](https://github.com/engganolang/eno-flex/blob/main/contemporary-enggano-interlinear-text/textbook-SFM.R).
    This code file calls the
    [contemporary-enggano-interlinear-text/textbook-splitting-code.R](https://github.com/engganolang/eno-flex/blob/main/contemporary-enggano-interlinear-text/textbook-splitting-code.R).
    The input data for this second code are as follows:

    - `textbook/textbook-LIFT-20241201.rds` (the output of .lift
      processing)

    - `textbook/textbook_as_tibble_20241201-binded.rds` (the Enggano
      phrase/text and its free English and Indonesian translations)

    - Flora and Fauna data \[To be updated!!!\]

#### Output/generated file/data

The main output is an SFM data in .db extension, namely
**textbook/textbook-dictionary-20241213-no-example-for-same-phrase.db**.

This SFM data is imported into a new FLEx project for the printed
dictionary. There are several manual post-editing of the entries
directly in FLEx. There are several notes in the NOTEBOOK feature of
this FLEx project recording such processes. Also, I manually added
several cultural items and tools for the subentries into FLEx directly,
while some other pictures (esp. FLORA and FAUNA, and the pictures of
root forms of the Cultural Lexicon data) are directly linked through
coding in the
[contemporary-enggano-interlinear-text/textbook-SFM.R](https://github.com/engganolang/eno-flex/blob/main/contemporary-enggano-interlinear-text/textbook-SFM.R).

## On importing to FLEx

This note records entry to merge between FLORA & FAUNA data, Cultural
Items data, and Textbook materials. The SFM of FLORA & FAUNA is
separately processed from Cultural Items + Textbook materials. **UPDATE:
17 Dec 2024**; this manual merging is no longer needed as the FLORA and
FAUNA data are processed together with the textbook materials.

### Entries to merge

1.  puru ‘leaf’ (delete the third sense duplicate of *puru*)

2.  it ‘banana’

    - merge sense 2 banana (that has image) with sense 1 also banana
      (coming from textbook materials)

3.  abeh ‘bambu’ (-merge sense as well)

4.  akė ‘patah’ (-merge sense as well)

5.  amai ‘tebu’

6.  ame ‘kima’

7.  anima

8.  anu’un

9.  apu ‘jenis kayu’

10. apu’ ‘snake’

11. are’iar ‘belalang’ (check the NAs!)

12. arkih ‘beras’

13. bak kaha’ ‘sun’ and ‘clock’

14. bakayė ‘sirsak’

15. be ‘anjing’

16. bih ‘lebah’

17. dadė ‘cabe’

18. dih ‘daun muda; pucuk’; CHECK the glossing for “dih nan”

19. dudiad ‘durian’

20. ea ‘tulang’

21. eai ‘ikan’

22. CHECK hapu̇-hapu̇ dop

23. hia

24. hĩu ‘buah’

25. hĩũk ‘kutu’

26. ho’ku ‘hutan’

27. CROSSREF. between “hu̇ė” ‘pick up’ and “hė” ‘pick up individually’
    and “roro” ‘pick up collectively’

28. iu’ ‘owl’

29. iuk ‘skin’

30. jekor ‘jengkol’

31. CROSSREF. between “ka ke’ep” ‘chalk’ which could be derived from
    ‘bird feces’

32. CROSSREF. between “ka udauh” ‘awan’ which could be derived from the
    ‘feces of the thunder’

33. SUBENTRIES FOR “kahai’ datuh” and “kahai’ dibu” and “kahai’ kak”
    ‘twenty’ and “kahi kak” ‘twenty’

34. SUBENTRIES FOR “kak hoba’a” ‘orang yang sudah meninggal’ “kak
    kiparuruha heo iub” ‘pembantu rumah tangga’ ‘orang membantu dalam
    rumah’

35. kãkõ ‘kangkung’

36. kamiu needs picture

37. CROSSREF between kãp and kanap ‘kepala suku’

38. kapaer ‘pepaya’

39. kapi ‘sapi’

40. kapareak

41. ka’pi iė̃p ‘membersihkan rumput’ link to ‘clean’ and ‘rumput’

42. karbo

43. jamur

44. kė ‘menanam’

45. ke’ep ‘bird’

46. kiadeb ‘ayam’

47. kih ‘semut’

48. kia ‘nyamuk’

49. belalang

50. kikoh

51. ikan hiu

52. lipan

53. kite ‘kayu merbau’

54. ko’ia ‘melinjo’

55. ko’ka biji

56. kopen ‘caterpillar’

57. korea ‘cekakak’

58. ku ‘kayu/wood’

59. kuahai

60. kud ‘kuda’

61. ku̇h ‘penyu’

62. CROSSREF for Indonesian ‘tunggit’

63. kunyit

64. mana’ai ‘ular’

65. mea ‘kucing’

66. mikmik

67. minata

68. mum

69. mũo ‘bunga’

70. na’inė

71. pakihieb

72. paik ‘udang’

73. katak/kodok

74. seludung kelapa/pelepah

75. pĩ‘ĩ ’keong hutan’

76. po ‘kelapa’

77. porpi’

78. puk ‘pusar’

79. puru/pururu

80. taripa

81. teke/cengkeh

82. tiri ‘sirih’

83. tumi-tumi

84. ubi’

85. udep ‘talas/taro’

86. uki ‘manggo’

87. yahaid

88. yãkãh ‘gagang’/‘stem’

89. jagung

90. CROSSREF ‘kuburan’

91. yẽãp

92. yėp ’rumput

93. yi’ pantat

94. SUBENTRIES: yub yahmi’i ‘prison’ (mi’ hukum)

95. yum ‘lalat’

### Change sub-entries from derivative to variant

1.  teb/tem

2.  anah ‘thus’

3.  Apoho

4.  arib he aru ‘seven’ (check the orig. FLEx file)

5.  bahem

6.  Bakaor

NOTE : for this section, it has been fixed in the R code (esp. in
`textbook-splitting-code.R`) where I identified a variable for grouping
words as complex forms when the morph_gloss_en contains `__`, which I
use as a separator for morpheme-by-morpheme glossing of the complex
forms. So, if words have these morpheme structures, they are treated as
subentries of the root (to distinguish them with variant)

# On sub-entries

## Sub-entries type

1.  The SFM can include custom marker indicating the specific type of
    the sub-entries.

2.  In the new FLEx project, set up the corresponding specific sub-entry
    types in the List for “Complex Form Types”.

    1.  This specific sub-entry type is given in the Name and
        Abbreviation fields.

    2.  The Reverse Name and Reverse Abbr. of the specific sub-entry
        type is also prepared in the FLEx List.

3.  Finally, in the mapping during import, I will only specify the
    specific sub-entry types with the custom marker, without the need to
    create marker for the Reverse Name and Reverse Abbr.

4.  **TO DO (DONE)**: prepare the list of the relevant complex form
    types for matching between SFM Marker and FLEx List. (DONE)
