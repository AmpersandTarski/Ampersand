This library enables importing from within an Ampersand application of database contents as specified by the contents of an Excel spreadsheet

This stuff seems to work but has not been thoroughly tested.

WHAT HAVE WE HERE
=================
- "class.importexcel.php" contains PHP code for parsing the Excel file contents and filling the database
- "excel_import.php" outputs HTML for creating a form within which you can specify the file to upload
- "excel_parse.php" receives the file and calls the class.importexcel code to fill the database
- the 'excel\' directory (and subdirs) contain all the library code that is needed.


IF YOU ENCOUNTER PROBLEMS
=========================
[1] If you have problems running the classes, please see the 'install.txt' in the Excel directory (that contains the 'Classes' directory)

[2] PHP may impose limimtations on the use of memory, or on the size of the file to be uploaded. This may lead to errors such as the inability to upload (large) files. You can handle issues related to this by editing the 'PHP.ini' file of the webserver, and making sure that it contains lines such as the following, with (possibly adjusted) appropriate values:

; Maximum amount of memory a script may consume (128MB)
; http://php.net/memory-limit
memory_limit = 128M

; Maximum size of POST data that PHP will accept.
; http://php.net/post-max-size
post_max_size = 128M

Rieks