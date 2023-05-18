# Split Sentences

Processes a book with sentences to put each separate sentence on a separate line.
It should also break too long sentences.

## Prepare

Books are usually in PDF. Using Acrobat Reader or Okular, there is the option to export them
to plain text. Do that for the book(s) you want to split.

## Split
The executable is quite simple, it takes an input file and an output file.
The results are reasonable, but might not be perfect. Especially within names (Prof. Barabas)
it is hard to distinguish between a name or a sentence separation.

## Postprocess

The first and last lines (of the PDF / exported file) are usually not really part of the book.
You can delete them manually. It is useful to open the PDF to see what should stay, and what
should be deleted.