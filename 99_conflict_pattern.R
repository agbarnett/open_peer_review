# 99_conflict_pattern.R
# text used to declare no conflict of interest; lots of options needed for varying styles and even spelling errors
# November 2025

conflict_pattern = c('^(no . )?(the|all) (authors? have|author has|authors?) (report|indicated|declared? )?(that )?(they have |there (are|is) )?no( known)? (compl?ete?ing|conflicts? of)( financial)? interests?',
                     '^(i|we) declare that there (are|is) no conflicts? of interest',
                     '^no authors (have|declare) competing interests?',
                     '^authors? do not have (any )?competing interests?',
                     '^the authors? reports? no  (competing|conflicts? of) interests?',
                     '^there (is|are) no (competing|conflicts? of) interests?',
                     '^(all the )?authors (declare|state) that (they have |there are )? no (competing|conflicts? of) interests?',
                     '^no( potential)? (competing|conflicts? of) interests?',
                     '^(i|we) (declare|have) no competing interests?',
                     'the(se)? authors? have no support or funding to report',
                     '^no.?$',
                     '^none.?$')
conflict_pattern = paste(conflict_pattern, collapse = '|') # OR

## the following patterns are for funders:

## pattern to end of sentence
funding_start = '^th(e|is|ese) (stud(y|ies)|trials?|experiments?|research|works?|surveys?|reviews?|meta.analys(i|e)s) (are|is|was|were) (part(ly)? )?(funded|financed|supported) by( the)?'
to_end = ".+?(?=\\. |\\.$)"
start_pattern = paste('(?<=', funding_start, ')', to_end, sep='')

## remove everything between brackets
remove_brackets = '\\([^()]*\\)'

## remove things that can get confused with full-stops
dot_pattern = ('u\\.s\\.(a\\.)?|\\bno\\.|u\\.k\\.')
