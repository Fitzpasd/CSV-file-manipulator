-- Shane Fitzpatrick 09487581
-- Module that describes all the different help strings that may be output
module CmdHelp where

-- Determines the appropriate string to print to the user
parseHelp :: String -> String
parseHelp [] = "\n***The following commands are available:***\n- load\n- save\n- report\n- count\n- list\n- distinct\n- nooutput\n- date-fix\n- grid-fix\n- reformat\n- sort\n- select\n- show\n- update\n- delete\n- insert\n- help\n- quit\n\n***For more information on each command, type \"help cmd\" - where cmd is the desired command***\n"
parseHelp "load" = "\n- Syntax: \"load x\" where x = a filename.\n- Function: The contents of the loaded file will become the current record set.  First line of file is assumed to be the column names.\n- Example: > load \"myFile.csv\"\n"  
parseHelp "save" = "\n- Syntax: \"save x\" where x = a filename.\n- Function: The contents of the file will be replaced with the current record set.\n- Example: > save \"myFile.csv\"\n"
parseHelp "report" = "\n- Syntax: \"report x\" where x = \"registrations\" or \"completions\".\n- Function: \n\tregistrations - reports the number of maps each club has registerd.\n\tcompletions - reports the number of maps which should be complete at the current date.\n-Example: > report registrations\n" 
parseHelp "count" = "\n- Syntax: \"count \" followed by a list of conditions.\n- Function: Dispalys the number of records satisfying the conditions.\n- Example: > count $3=\"*Cork*\" $1=\"*B*\" \n"
parseHelp "list" = "\n- Syntax: \"list \" followed by a list of conditions.\n- Function: Displays the records that satisfy the conditions.\n- Example: > list $3=\"*Cork*\" $1=\"*B*\"\n"
parseHelp "distinct" = "\n- Syntax: \"distinct x\" where x = a single column. \n- Function: Displays the number of unique values values contained in that column.\n-Example: > distinct $3 \n"
parseHelp "date-fix" = "\n- Syntax: \"date-fix x y\" where x = a column and y = a date format.\n- Function: Reformats the date to the desired format for all output. Error to apply this to a non-date field.\n-Formats: DD/MM/YYYY, MM/DD/YYYY, MM/YYYY, YYYY\n- Example: > date-fix $7 \"YYYY-MM-DD\"\n"
parseHelp "grid-fix" = "\n- Syntax: \"grid-fix x y\" where x = a column and y = a grid format (4 or 6). \n- Function: Reformats the grid to the desired format for all ouput. Error to apply this to a non-grid field.\n- Example: > grid-fix $7 4  \n"
parseHelp "reformat" = "\n- Syntax: \"reformat x y\" where x = a column and y = \"uppercase\" or \"capitalize\" or \"lowercase\". \n- Function:\n\tuppercase - convert text to uppercase.\n\tcapitalize - capitalize the first letter of each word, convert others to lowercase.\n\tlowercase - convert text to lowercase.\n\ttrim - remove excess whitespace from the column.\n- Example: > reformat $1 uppercase \n"
parseHelp "sort" = "\n- Syntax: \"sort \" followed by a list of columns and \"ascending\" or \"descending\". \n- Function: Sets the sort order for the field names for all output.\n- Example: > sort $2 descending $5 ascending \n"
parseHelp "select" = "\n- Syntax: \"select \" followed by a list of conditions or \"all\". \n- Function: Selects the appropriate records.\n- Example: > select $1=\"AJA*\" \n"
parseHelp "show" = "\n- Syntax: \"show\" \n- Function: Displays the selected cells.\n- Example: > show \n"
parseHelp "update" = "\n- Syntax: \"update x y z\" where x = row number, y = column description and z = new value.   \n- Function: Updates the appropriate fields with the new value\n- Example: > update 21 $3=\"*Cork*\" \"Kerry\" \n"
parseHelp "delete" = "\n- Syntax: \"delete x\" where x = row number. \n- Function: Deletes the row.\n- Example: > delete 21 \n"
parseHelp "insert" = "\n- Syntax: \"insert \" followed by entries in the form $1=\"...\". \n- Function: Inserts a new row with the specified data, prompting for missing fields.\n- Example: > insert $1=\"AJAX\" $2=\"Bull\" ... \n"
parseHelp "help" = "\n- Syntax: \"help x\" where x = \"\" or a command \n- Function: Displays all commands available, or details of one specified command.\n- Example: > help delete \n"
parseHelp "quit" = "\n- Syntax: \"quit\". \n- Function: Exits the program, but prompts the user if there is an unsaved modified file.\n- Example: > quit \n"
parseHelp other = "\n*** ERROR - invalid help command ***\n"