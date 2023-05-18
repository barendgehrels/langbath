# Import Tatoeba

Imports CSV files from Tatoeba (a web site with sentences and translations in various languages,
together with spoken versions) into an SQLite database.

# Prepare

## Download CSV's

Go to https://tatoeba.org/en/downloads

Download three CSV files:
* sentences.tar.bz2
* links.tar.bz2
* sentences_with_audio.tar.bz2

## Unzip
Unzip them in a local folder (for example: `~/tmp/tatoeba`), using:
```
tar -xf ~/Downloads/sentences.tar.bz2
```
The other CSV files similarly

## Create language id mapping file
First time only: create, manually, a file `lang_mapping.csv`, with the contents (for example):
```
rus<tab>16
cat<tab>44
eng<tab>1
por<tab>13
```

The first column contains the language as known in Tatoeba.
The second column contains the numeric language ID as known in our source (`lb_const.pas`).
These numerical ID's are stored into our langbath sqlite database

# Import

## Change the code (!)
At this moment, the languages to import the sentences from are hard coded.
Also the level (indirect translations) to stop is hard coded.
It can be changed in the file `lb_import_tatoeba_sentences` (procedure `ImportTatoebaSentences`).
Rebuild.

## Call import
```
bin/import_tatoeba -f ~/tmp/tatoeba -d ~/tmp/tatoeba/tatoeba_imported.db
```
It can take a few minutes, depending on the options.

## Vacuum
AFTER IMPORT: vacuum the sqlite database, because the raw tatoeba tables are deleted
after conversion to the langbath-datamodel, but their occupied space in the file is still there.

## Copy
For the Kotlin app (currently in a private repo), move or copy the database:
```
cp ~/tmp/tatoeba/tatoeba_imported.db ../babel-magic/babel-chant-app/src/main/assets/tatoeba.db
```
