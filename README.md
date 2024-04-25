## FIELDWORKER

__Fieldworker__ is a modular application for organizing fieldwork. 

### Directory structure

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


### How to install and run. 

1. Clone this repo locally.
2. Get access to a database (MySQL or MariaDB).
3. Open your DB and run `Admin/db_structure.SQL`. If this has been created for you, go to 4.
4. Install the required R packages in the `./global.R` files. 
5. Load the [dbo](https://github.com/mpio-be/dbo) package and then run `my.cnf()` to store your db credentials locally. Make sure the `SERVER` variable in all`./global.R` files reflect your `my.cnf()`.
6. Within `\main`, run `shiny::runApp('main')` to load the interface. 