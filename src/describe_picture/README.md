# Describe Picture

Experimental program to get automatic corrections on own texts

## Use case

* The user retrieves a random picture (possibly with a topic)
* The user describes the picture in the language he is learning (the TARGET language)
* He/she presses "languagetool.org" and get corrections and hints
* He/she presses "deepl" and the text is translated into another language, and then
  translated back. That way the user sometimes gets corrections, but sometimes
  alternatives (and sometimes nonsense).
  * For example: RUSSIAN -> POLISH -> RUSSIAN (because polish is related)
  * or SPANISH -> (PORTUGUESE,CATALAN) -> SPANISH and the most matching one is selected
  * (matching is done with Levenshtein distance)
* Also, the user can get a translation in his own language (to verify the meaning)

## Inifile

The program needs an `ini-file` with URL's for unsplash, languagetool.org and deepl.

This file needs to be in the configuration folder:
* On Linux: `~/.config/langbath/langbath.ini`
* On Windows: `\users\MYNAME\AppData\Local\langbath\langbath.ini`

The contents is, for example:

```
[describe_picture]
unsplash_api_url=https://api.unsplash.com/photos/random
unsplash_api_key=your key here
deepl_api_url=https://api-free.deepl.com/v2/translate
deepl_api_key=your key here
target_language=RU
via_languages=PL,CS
check_language=NL
```

Language codes can be found here:
https://www.deepl.com/nl/docs-api/translating-text/request/

