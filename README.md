# Proyecto para la asignatura Visualización de Datos

## Integrantes

- Andreu Bonet Pavía
- Anna Gil Moliner
- Laiqian Ji
- Ernesto Martínez Gómez

## Ejecución

Se puede descargar el proyecto sin todos los commits anteriores con el siguiente comando si simplemente se busca ejecutar la aplicación en su última versión localmente:

```bash
git clone https://github.com/andreucs/vis-proy.git --depth=1
```

Para ejecutar la aplicación es necesario tener instalado [R](https://www.r-project.org/) y [Quarto](https://quarto.org/docs/get-started/).

Una vez instalado, se puede ejecutar lo siguiente en la terminal:

```bash
quarto serve dashboard.qmd
```

## Despliegue

Para desplegar la aplicación, se ha usado la liberería `rsconnect`.

```R
install.packages("rsconnect")
rsconnect::deployApp()
```

Actualmente, el dashboard generado se encuentra desplegado en el siguiente enlace:

[https://km87dt-ernesto0mart0nez.shinyapps.io/VISDashboard/](https://km87dt-ernesto0mart0nez.shinyapps.io/VISDashboard/)