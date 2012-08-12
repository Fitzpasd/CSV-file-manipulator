
<h1> CS3016 Haskell .csv file manipulator </h1>

<p>
This is the final project for a third year computer science class 'Introduction to Functional Programming'.  Project spec can be found in <em><b> Project-2011.pdf </b></em>. 
<br/>
<br/>
<em>The program purpose is to enable a user to query and clean-up a spreadsheet,
made available using the comma-separated values format (CSV). The specific
spreadsheet of interest is one that contains data about maps, and the program
must work satisfactorily with this data.
The program will have a command-line interface, so the user can: load spread-
sheet files; perform simple queries; invoke some data cleanup; save cleaned-up files; and exit the program.</em>
<br/>
<br/>
<b>Note:</b> This was the first non-trivial Haskell program I wrote. As such there are numberous places thoughout the project where I do not use a standard function to implement some simple behaviour because I hadn't yet discovered it.  Furthermore, it is no longer an accurate portrayal of my coding ability in Haskel. 
</p>

<h1> List of commands </h1>

<ul>
<li>load | load CSV spreadsheet</li>
<li>save | save spreadsheet as CSV</li>
<li>report | run builtin report</li>
<li>count | count records satisfying a condition</li>
<li>list | show records satisfying a condition</li>
<li>distinct | report distinct items in a column</li>
<li>output | redirect output to file</li>
<li>date-fix | fix date data</li>
<li>grid-fix | fix grid-reference data</li>
<li>reformat | reformat column data</li>
<li>sort | sort spreadsheet</li>
<li>select | select sheet rows</li>
<li>show | show selected rows</li>
<li>update | update field</li>
<li>delete | delete row</li>
<li>insert | insert new row</li>
<li>help | help</li>
<li>quit | exit the program</li>
</ul>

<h1> Grade recieved </h1>

<p>
This project recieved a grade of <b>100/100</b> and was the only project in my year to do so.
</p>
