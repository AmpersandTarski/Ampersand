Ampersand prototypes are capable of executing 'plugins', i.e. (small) pieces of code that are called whenever a specific VIOLATION is signalled. Examples are:
- populating or depopulating a pair of atoms in a specified relation
- computing the transitive closure of a given relation
- computing comparison relations of time(stamp)-atoms
- sending an Email or SMS 

When the code for generic plugins is small, they are located in the directory '<OUrepository>\trunk\static\php\plugins\'. Within this directory, there is a subdirectory 'lib' that is intended for 'library-files' that are used by the plugin code. All of these files, and hence the plugins whose code they contain, will be available whenever a prototype is generated.

When the code for generic plugins is large, or a plugin is nog sufficiently generic, we may still want to put this in the repository. An example of this is the plugin that can be used to import populations from an Excel file. Such code will be stored in the directory '<OUrepository>\trunk\large_php_plugins\'. For each large or non-generic plugin, this directory contains a subdirectory (e.g. ImportExcelFiles) that contains the code. The contents of such a plugin should be copied to the directory '<htdocs>\<prototypedir>\php\plugins' directory in order to be used.