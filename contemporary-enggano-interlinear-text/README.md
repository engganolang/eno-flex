[Gede Primahadi Wijaya
Rajeg](https://www.ling-phil.ox.ac.uk/people/gede-rajeg)
<a itemprop="sameAs" content="https://orcid.org/0000-0002-2047-8621" href="https://orcid.org/0000-0002-2047-8621" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a><br>University
of Oxford, UK & Universitas Udayana, Indonesia

<!-- README.md is generated from README.Rmd. Please edit that file -->

# My personal notes on processing the .lift and .flextext exported data

<!-- badges: start -->

[<img src="../file-oxweb-logo.gif" width="84"
alt="The University of Oxford" />](https://www.ox.ac.uk/)
[<img src="../file-lingphil.png" width="83"
alt="Faculty of Linguistics, Philology and Phonetics, the University of Oxford" />](https://www.ling-phil.ox.ac.uk/)
[<img src="../file-ahrc.png" width="325"
alt="Arts and Humanities Research Council (AHRC)" />](https://www.ukri.org/councils/ahrc/)
</br>*This work is part of the [AHRC-funded
project](https://app.dimensions.ai/details/grant/grant.12915105) on the
lexical resources for Enggano, led by the Faculty of Linguistics,
Philology and Phonetics at the University of Oxford, UK. Visit the
[central webpage of the Enggano
project](https://enggano.ling-phil.ox.ac.uk/)*. <!-- badges: end -->

## The exported .lift data from FLEx `Lexicon`

The latest, exported `Lexicon` data (pre- and post-October-2023
fieldwork) contains some example sentences for certain lexical entries.
This leads to the presence of duplicated observation in
`FLEX-lift-pre-fieldwork.rds` (read into R as `lu_form_df` tibble). The
examples are contained in the `"x_eno"`, `"x_idn"`, and `"x_eng"`
columns of `lu_form_df`.

Even though the `x_...` columns have been excluded and then the
`distinct()` function has been run, there are still duplicates due to
the presence of multiple senses of a given lexical entry. These multiple
senses (shown in the `"sense_gloss_en"` and/or `"sense_gloss_idn"`
column of the `lu_form_df` tibble) are represented into different rows.
The example is *ueh* that is glossed as ‘sleep’ and ‘lie’. Hence, *ueh*
has two entries/rows in `lu_form_df`.

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

## On sub-entries

### Sub-entries type

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

4.  **TO DO**: prepare the list of the relevant complex form types for
    matching between SFM Marker and FLEx List.
