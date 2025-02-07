# Deploying R Shiny Apps with Docker and renv

This repository is intended as an end-to-end example of deploying an
[R Shiny](https://shiny.rstudio.com/) application using [Docker](https://www.docker.com/). It also employs
the [renv](https://rstudio.github.io/renv/articles/renv.html) R package to simplify package installation in the Docker image.

Example based on rocker/shiny:4.4.2

## Getting Started

If you don't have it already, install Docker.

Then, move to folder you want to work in and clone this repository.

```
git clone https://github.com/deliaspanigo/standard_shiny_full_01.git
```

Explore the files.

```
cd standard_shiny_full_01
ls
```
Note that there is a `Dockerfile` which contains all the instructions to build an image of the R Shiny server and our app.

The `app` folder contains the R Shiny app we want to deploy.

## Running the App Locally

To run the app locally, we must follow these steps:

1. Build the Docker image.
2. Start the container.
3. Open the app in the browser.

You will find docker sentences in file sentences_docker.sh
### Build the Docker Image

To build the image, navigate to the folder containing the Dockerfile and run the following command.

```
docker build -t dk_shiny_full_01_img .
```

### Start the Container

Wait for the build process to finish, then start the container by running the following.

```
docker run --name dk_shiny_full_01_container -p 3838:3838 --rm -d dk_shiny_full_01_img 
```

### Dev - Enviroment - Start the Container with volumen

Wait for the build process to finish, then start the container by running the following.

```
docker run --name dk_shiny_full_01_container -v $(pwd)/app:/app -p 3838:3838 --rm -d dk_shiny_full_01_img
```


### Open the App in the Browser

Open your web browser and go to http://localhost:3838/. The example app 'Old Faithful Geyser Data' is displayed.

# RMedic3_dev_docker
