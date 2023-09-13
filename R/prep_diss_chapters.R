#' Convert Google Doc to Rmd
#'
#' Takes a Google Doc with embedded elements (chunks) and converts it into an Rmd that can be used with different LaTeX templates. This was designed specifically for aggiedown, and converting Google Docs that have Zotero citations embedded via the plug-in, formatted to their BibTex format.
#'
#' @param GDOC_URL the URL to the Google Doc
#' @param GDOC_PATH the path to a directory where the conversions will be stored
#' @param intro a Boolean argument to indicate whether this document is the Introduction section (e.g. has no chunks or headers embedded). Default intro = F.
#'
#' @return Four files: 01_drive_manuscript.txt, 01_drive_manuscript.docx, 02_manuscript.Rmd, 04_refs.bib
#'
#' @import googledrive
#' @import rmarkdown
#' @import stringr
#' @import readtext
#' @import bib2df
#'
#' @export

prep_diss_chapters <- function(GDOC_URL, GDOC_PATH, intro = F){
  # download chapter from drive
  googledrive::drive_download(GDOC_URL,
                              path = paste0(GDOC_PATH, '01_drive_manuscript.docx'),
                              overwrite = T)

  # convert the word to text (could do markdown but it does weird things with the backticks)
  rmarkdown::pandoc_convert(paste0(GDOC_PATH, '01_drive_manuscript.docx'),
                            output =  paste0(GDOC_PATH, '01_drive_manuscript.txt'))

  # Here I can manipulate the document
  txt <- readtext::readtext(paste0(GDOC_PATH, '01_drive_manuscript.txt'))[,2]
  # Start at header
  if(intro == T){
    txt <- txt
  } else {
    start <- str_locate(txt, '#\\s\\w')
    txt <- stringr::str_sub(txt, start[1], nchar(txt))
  }

  # Take references -- put into anystyle, but don't remove footnotes (after refs)
  ref_start <- str_locate(txt, '#\\sReferences')
  ref_end <- str_locate_all(txt, '\\[\\^1\\]')
  if(lengths(ref_end) == 0){
    ref_end = nchar(txt)
  } else {
    ref_end = ref_end[[1]][lengths(ref_end)/2]
  }
  refs <- str_sub(txt, ref_start[1], ref_end[1]-1)
  # Remove the reference label
  refs <- str_remove(refs, '# References\\\n\\\n')
  # Remove the zotero embeddings, but keep these as an index for later
  refs <- str_remove_all(refs, '\\(https\\://www\\.zotero.*(?=\\\n\\\n)')
  refs <- str_remove_all(refs, '\\[|\\]|\\*')
  # Remove linebreaks so help the parser
  refs <- str_replace_all(refs, '\\\n(?!\\\n)', ' ')

  # If bibtext formatting setting in Zotero
  if(str_detect(refs,'^\\\\@')){
    # If bibtext, then remove \\ before @
    refs <- str_remove_all(refs, '\\\\(?=@)')
    # if bibtex, save as .bib
    write.table(refs, paste0(GDOC_PATH, '04_refs.bib'),
                row.name = F, col.names = F, quote = F)

    # For bibtex Zotero
    bt <- bib2df::bib2df(paste0(GDOC_PATH, '04_refs.bib'))

    # Fill in spaces in ref text
    spaced_bibkeys <- str_which(bt$BIBTEXKEY, '\\\n|\\s')
    if(length(spaced_bibkeys) > 0){
      for(i in spaced_bibkeys){
        refs <- str_replace_all(refs, bt$BIBTEXKEY[i],
                                str_replace_all(bt$BIBTEXKEY[i], '\\\n|\\s', '_'))
      }
    }

    # Correct et al
    ## I don;t know that I need to do what I did above, right?
    refs <- str_replace_all(refs, 'et(_?\\s?)al\\.\\\\', 'et_al\\.')

    # Re-writing refs with correction
    write.table(refs, paste0(GDOC_PATH, '04_refs.bib'),
                row.name = F, col.names = F, quote = F)

    # Fill in spaces in bib file
    bt$BIBTEXKEY <- str_replace_all(bt$BIBTEXKEY, '\\\n|\\s', '_')
    # Correct et al
    bt$BIBTEXKEY <- str_replace_all(bt$BIBTEXKEY, 'et(_|\\s)al\\.\\\\', 'et_al\\.')

    # Remove the references from the text but keep the footnotes
    txt <- paste(str_remove(str_sub(txt, 1, ref_start[1]), "#?#$"),
                 str_sub(txt, ref_end[1], nchar(txt)))
    # If there is duplicate thing, like (NOSB 2004, 2006)
    ## This has been too hard so I am just abandoning that
    #dupl <- '\\[(\\w|\\d|_|\\s|\\n)+_\\d{4},(\\s|\\n)\\d{4}\\]'
    #dupl_loc <- data.frame(str_locate_all(txt, dupl))
    #str_sub(txt, dupl_loc[1,1], dupl_loc[1,2])

    #  if(nrow(dupl_loc) > 0){
    #    for(i in 1:nrow(dupl_loc)){
    #      dupl_loc <- data.frame(str_locate_all(txt, dupl))
    #      sample <- str_sub(txt, dupl_loc[i,1]+1, dupl_loc[i,2]-1)
    #      years <- str_extract_all(sample, '\\d{4}')
    #      years <- years[[1]]
    #      author <- str_extract(sample, '(\\w|\\d|_|\\s|\\n)+(?=_\\d{4},)')
    #      str_sub(txt, dupl_loc[i,1], dupl_loc[i,2]) <- paste(author,years[1],
    #                                                          author, years[2],
    #                                                          sep = '_')
    #    }
    #  }

    # Correct where linebreaks were in text
    inlinecit <- '\\[(\\w|\\d|_|\\s|\\n|\\-)+(al\\.\\\\{0,2})?_\\d{4}\\]'
    cit_loc <- data.frame(str_locate_all(txt, inlinecit))
    if(nrow(cit_loc) > 0){
      for(i in 1:nrow(cit_loc)){
        str_sub(txt, cit_loc[i,1], cit_loc[i,2]) <- str_replace_all(str_sub(txt, cit_loc[i,1], cit_loc[i,2]), '\\n|\\s', '_')
      }
    }
    # Check work
    if(nrow(cit_loc) > 0){
      for(i in 1:nrow(cit_loc)){
        results <- str_detect(str_sub(txt, cit_loc[i,1], cit_loc[i,2]), '\\n|\\s')
      }
    }

    # Correct where et al were in text
    etal_loc <- data.frame(str_locate_all(txt, 'et(_|\\s)al\\.\\\\'))
    str_sub(txt, etal_loc[1,1]-5, etal_loc[1,2]+5)
    ## I don;t know that I need to do what I did above, right?
    txt <- str_replace_all(txt, 'et(_?\\s?)al\\.\\\\', 'et_al\\.')


    # Then remove where the zotero link i
    links <- str_extract_all(txt, '\\(https\\://www\\.zotero\\.org/google\\-docs/\\?(.{6}|broken\\=laytXQ)\\)')
    txt <- str_remove_all(txt, '\\(https\\://www\\.zotero\\.org/google\\-docs/\\?(.{6}|broken\\=laytXQ)\\)')

    # identify inline based on key (everything should be matched) and assign @
    # if i arrange longest to shortest char, will that overcome my challenges of some getting misassigned (e.g. Hieberg_Binz_Truffer_2020 getting Binz_Truffer_2020)
    bt$nchar <- nchar(bt$BIBTEXKEY)
    bt <- bt %>% dplyr::select(BIBTEXKEY, nchar) %>% dplyr::arrange(desc(nchar))
    # Still bugs. So what about finding the absolute locations of each key
    for(i in 1:length(bt$BIBTEXKEY)){
      str_locate_all(txt, bt$BIBTEXKEY[i])
      df_locs <- data.frame(str_locate_all(txt, paste0('(?<=\\d_)',
                                                       bt$BIBTEXKEY[i],
                                                       '|(?<=\\[)', bt$BIBTEXKEY[i])))
      if(nrow(df_locs) == 0){
        next
      } else {
        df_locs <- df_locs[,1:2]
        if(nrow(df_locs) == 1){
          str_sub(txt, df_locs$start, df_locs$end) <- paste0('@',str_sub(txt, df_locs$start, df_locs$end))
          # it does a weird repeating thing because the strong counts get messed up and it doesn't seem to vectorize well, so if longer then just one instances i re-run it every time
        } else {
          for(j in 1:nrow(df_locs)){
            one_loc <- data.frame(str_locate_all(txt, paste0('(?<=\\d_@?)',
                                                             bt$BIBTEXKEY[i],
                                                             '|(?<=\\[@?)',
                                                             bt$BIBTEXKEY[i])))
            one_loc <- one_loc[j,1:2]
            str_sub(txt, one_loc$start, one_loc$end) <- paste0('@',str_sub(txt, one_loc$start, one_loc$end))
          }
        }
      }
    }

    # if multiple references then replace _ with ;
    txt <- str_replace_all(txt, '_@', '; @')

    inlinecit <- '\\[@(\\w|\\d|_|@)+\\d{4}\\]'
    cit_loc <- data.frame(str_locate_all(txt, inlinecit))

  } else {
    # If not bibtext, Save as text to be read by the parser
    write.table(refs, paste0(GDOC_PATH, '04_refs.txt'),
                row.name = F, col.names = F, quote = F)

    # parse takes a biblio -- this runs in terminal but not here
    # but running in terminal yields a bad extraction, but when I paste
    # the test into anystyle.io it is great. I don't know why.
    #system(paste('anystyle --overwrite -f bib parse',
    #             paste0(GDOC_PATH, '04_refs.txt'),
    #             paste0(GDOC_PATH, 'bib')))

    # Replace inline refs with bibtext
    #refs <- str_split(refs, '\\\n')
    #refs <- unlist(refs)
    #refs <- refs[refs != '']
    #
    #name <- tolower(str_remove(word(refs), ','))
    #year <- str_extract(refs, '\\d{4}')
    #inline <- paste0(name,year)

    # Remove the references from the text but keep the footnotes
    txt <- paste(str_remove(str_sub(txt, 1, ref_start[1]), "#?#$"),
                 str_sub(txt, ref_end[1], nchar(txt)))

    # Then remove where the zotero link is
    txt <- str_remove_all(txt, '\\(https\\://www\\.zotero\\.org/google\\-docs/\\?.{6}\\)')
    txt <- str_remove_all(txt, '\\[(?=\\()|(?<=\\))\\]')
  }


  # Keep backticks for code chunks
  txt <- stringr::str_replace_all(txt, '\\\\`\\\\`\\\\`', '```')
  # Allow chaptermark
  cm <- str_locate(txt, 'chaptermark')
  if(!is.na(cm[1,1])){
    str_sub(txt, cm[1]-2, cm[2]) <- '\\chaptermark'
  }

  # And then make sure code within chunks doesn't have markup
  chunks <- data.frame(str_locate_all(txt, '```'))
  if(nrow(chunks) != 0){
    for(i in seq(1,nrow(chunks), 2)){
      if(i == 1){
        rpl <- str_remove_all(str_sub(txt, chunks$end[i], chunks$start[i+1]), '\\\\')
        rpl <- str_replace_all(rpl, '(?<!\\})\\n(?!`)', ' ')
        str_sub(txt, chunks$end[i], chunks$start[i+1]) <- rpl
      } else {
        newchunks <- data.frame(str_locate_all(txt, '```'))
        rpl <- str_remove_all(str_sub(txt, newchunks$end[i], newchunks$start[i+1]), '\\\\')
        rpl <- str_replace_all(rpl, '(?<!\\})\\n(?!`)', ' ')
        str_sub(txt, newchunks$end[i], newchunks$start[i+1]) <- rpl
      }
    }
  }

  # Remove {.underline}
  txt <- stringr::str_replace_all(txt, '\\{\\.underline\\}', '')
  # Get rid of backslash before double quotes
  str_locate(txt, '\\\\"')
  str_sub(txt, 1390, 1395)
  txt <- str_replace_all(txt, '\\\\"', '"')
  str_sub(txt, 1390, 1395)
  # Remove media files
  media <- stringr::str_locate(txt, '\\!\\[\\]\\(media\\/image\\d\\.(png|jpg)\\)')
  stringr::str_sub(txt, media[1]-50, media[2])
  txt <- stringr::str_replace_all(txt, '\\!\\[\\]\\(media\\/image\\d\\.(png|jpg)\\)', '')
  # Add backslash to brackets
  txt <- str_replace_all(txt, '\\\\\\[', '\\[')
  txt <- str_replace_all(txt, '\\\\\\]', '\\]')
  # Allow there to be commending out
  txt <- stringr::str_replace_all(txt, '\\\\\\<\\!\\\\-\\\\--', '<!---')
  txt <- stringr::str_replace_all(txt, '\\\\-\\\\--\\\\\\>', '--->')
  write.table(txt, paste0(GDOC_PATH, '02_manuscript.Rmd'),
              col.names = F, row.names = F, quote = F)


}
