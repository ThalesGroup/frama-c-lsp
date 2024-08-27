Bugs : 
- corrupted json sent by server for did save when a diagnostic with a unicode character is sent, makes the extension crash
- syntax coloration not perfect for ghost code (function names are not colored as they should be), include c coloration in ghost code
- find definition and find declaration only search by matching the symbol, do not support macros and multiple declarations
- doesn't support filenames with spaces
- tested only on unix system


Go To Declaration : 
- if a variable is declared twice (one globally and one inside a function), no declaration is returned
