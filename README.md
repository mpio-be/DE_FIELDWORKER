## FIELDWORKER

__Fieldworker__ is a modular application for organizing fieldwork. 



```
📦FIELDWORKER
 ┣ 📂Admin
 ┃ ┗ 📜db_structure.SQL
 ┣ 📂DataEntry
 ┃ ┣ 📂AUTHORS
 ┃ ┣ 📂CAPTURES
 ┃ ┣ 📂EGGS
 ┃ ┣ 📂NESTS
 ┃ ┗ 📂RESIGHTINGS
 ┣ 📂gpxui
 ┃ ┣ 📜global.R
 ┃ ┣ 📜server.R
 ┃ ┗ 📜ui.R
 ┣ 📂main
 ┃ ┣ 📂R
 ┃ ┣ 📂www

```

The interface in `\main` is both :  
* a landing page that links to the [DataEntry](https://github.com/mpio-be/DataEntry), [gpxui](https://github.com/mpio-be/gpxui) interfaces and some other web apps.     
* a mapping, viewing and reporting interface.

The interfaces outside of `\main` are self-contained and can be run independently if needed. 
