# visualenrollment

App that visually recomends enrollment to UOC students.

## Installation

### 1. Install docker and git on your server.

### 2. Get the code:

```
git clone github.com/jrosell/visualenrollment
```

### 3. Configure your github access token in *.Renviron* file.

```
GITHUB_PAT=<Your personal acess tocken from user with read permisions on the repository>
```

### 3. Build the image:

```
source .Renviron; nohup docker build . -t visualenrollment --build-arg GITHUB_PAT=${GITHUB_PAT} &
```

### 4. Run the container:

```
docker run -d --rm -e "RENV_PATHS_CACHE=/opt/local/renv/cache" -v "/opt/local/renv/cache:/renv/cache" -p 4081:4081 --name visualenrollment visualenrollment
```

### 5. Deployment

Open your browser and go to http://localhost:4081

For public deployment, further configuration can be done like web server proxy, password protection or domain configuration.

For example, */etc/apache2/sites-available/visualenrollment.triggerbit.com.conf*

```
<VirtualHost *:80>
  ServerName visualenrollment.triggerbit.com
  ServerAlias visualenrollment.triggerbit.com
  ProxyPass / http://localhost:4081/
</VirtualHost>
```

### 6. Updates

Github repository may be updated. If you want to update the version of the current deployment, use the following commands:

```
git pull
source .Renviron; docker build . --no-cache -t visualenrollment --build-arg GITHUB_PAT=${GITHUB_PAT} 
docker run -d --rm -e "RENV_PATHS_CACHE=/opt/local/renv/cache" -v "/opt/local/renv/cache:/renv/cache" -p 4081:4081 --name visualenrollment visualenrollment
```

Open your browser and go to http://localhost:4081


## Directroy structure


* app.R: Run this file to run the app in development.
* R directory with all the functions that we use.

  * 000_global.R: First file loaded, used to set global variables.
  * zzz.R: Last file loadad, used to load libraries and set web resource paths.
  * visualenrollmentApp.R: Shiny app where we build the router and use informatica module.
  * informaticaUI.R: User interface for the informatica module.
  * informaticaServer.R: Business logic of the app (Valors reactius, mapa asignatures, calendari, expedient, resum matrícula, cercador, selectors admin, events)
  * asignatura.R: Helper functions related to subjects and semesters.
  * theme.R: Helper theme functions using fluent UI.
  * translations.R: Helper functions to show or translate translated messages from english.
  * setup.R: Helper functions to install required packages and run the app.

* inst/www/img/logo.png: logo image.
* inst/www/css/styles.css: all used CSS styles.
* inst/www/js/scripts.js: all used JavaScript code.
* data: all used data files (see Data section for details)


## Data

I data folder we have the following files for INFORMATICA:

### assignatures_INFORMATICA.csv

* ass: codi de l'assignatura.
* asem: a quin semstre consta al pla d'estudis.
* bsem: si es pot cursar en un semestre (1), l'altre (2) o tots dos (0).
* type: tipus (B, O, P, C, P, T).
* path: A quins plans d'estudi correspon, pots er més d'un.
* name: nom en castellà o NA.
* abrv: abreviatura que es reutilitza en tots els idiomes.

### noms_INFORMATICA.csv

* ass: codi de l'assignatura.
* name_en: nom de l'assignatura en anglès.
* name_es: en castellà.
* name_ca: en català.

### prerequisits_INFORMATICA.csv

* assignatura1: codi per l'asignatura previa
* assignatura2: codi per l'asgignatura posterior.
* prerequisit: distancia o importancia del requisit (?).

### recomanacions_INFORMATICA.csv

En proof-of-concept no hi havia header i s'ha deixat sense header (!).

* V1/idp: id del estudiant 000db405523a5cf78db519ff741da81c
* V1: Té el valor 31 i no sembla que s'utilitzi.
* V3: Té diferents valors pero no sembla que s'utilitzi.
* V4/sem: Sembla ser el número de semestre cursat per l'estudiant (?), sent 1 el primer semestre que cursa. 
* V5: Té valors 0  i 1, no sembla que s'utilitzi.
* V6: S'utilitza per seleccionar els estudiants que podem utilitzar en el recomanador (x$V6>=10).
* De V7 a V10 no veig que s'utilitzin.
* V11 o ass1: Codi de la primera asignatura cursada (?).
* V12 o ass2: Codi de la segona asignatura cursada simultàniament (?)
* V13 o nota1: Nota obtinguda en la primara asignatura cursada (?)
* V14 o nota2: Nota obtinguda en la segona asignatura cursada (?)

### solap1_INFORMATICA.csv i solap2_INFORMATICA.csv

Ara mateix tenim 2 fitxers per INFORMATICA amb les següents columnes:

* subject_code: codi d'assignatura
* ti: no s'utilitza
* tf: no s'utilitza
* sem: tot i que consta el semestre, no s'utilitza, sino que cada fitxer correspon al se semestre corresponent.
* X1-X128 valors d'activitats que s'utilitzarà per calcular solapaments d'activitats.



### tipologia_INFORMATICA.csv

* type: tipus (B, O, P, C, P, T).
* path:  A quins plans d'estudi correspon, pots er més d'un. Cal tenir una fila per opció, per exemple per 1_4 cal indicar 1, 4 i 1_4.
* tipologia_en: Nom del tipus d'asignatura en anglés.
* tipologia_ca: català.
* tipologia_es: castellà.

### translation/translation_*.csv

* en: Text en angles que s'utilitzará com a clau en el codi.
* *: Text en l'idioma corresponent que será la traducció de l'anglès.

