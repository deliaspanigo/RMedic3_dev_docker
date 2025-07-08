
#!/bin/bash
# Remove the 'app' directory if it exists
# if [ -d "app" ]; then
#     rm -rf app
# fi

# git clone https://github.com/deliaspanigo/RMedic3.git app


# Build the Docker image with the name 'my_docker_image'
docker build -t legion949/dk_rmedic3_dev_img:3.2.2 .

docker run --name dk_rmedic3_dev_container -p 3838:3838 --rm legion949/dk_rmedic3_dev_img:3.2.2


# Run the Docker container with the name 'my_docker_container'
docker run --name dk_rmedic3_dev_container -p 3838:3838 --rm -d legion949/dk_rmedic3_dev_img:3.2.2

# Run the Docker container with the name 'my_docker_container' and mount the local directory 'example-app' to the container directory '/app'
docker run --name dk_rmedic3_dev_container -v $(pwd)/app:/app -p 3838:3838 --rm -d legion949/dk_rmedic3_dev_img:3.2.0


# Run exec command to access the container
docker exec -it dk_rmedic3_dev_container /bin/bash

########################################################################################
# Stop the Docker container
docker stop dk_rmedic3_dev_container

# Remove the Docker container
docker rm dk_rmedic3_dev_container

# Remove the Docker image
docker rmi legion949/dk_rmedic3_dev_img:0.0.1   


########################################################################################

# Docker Hub -------------------------------------------------

# Pushing the Docker image to Docker Hub
## Push the Docker image with version 0.0.1 to Docker Hub
docker push legion949/dk_rmedic3_dev_img:3.2.2

## Tag the Docker image with 'latest' for Docker Hub
docker tag legion949/dk_rmedic3_dev_img:3.2.2 legion949/dk_rmedic3_dev_img:latest

## Push the Docker image with 'latest' to Docker Hub
docker push legion949/dk_rmedic3_dev_img:latest

########################################################################################

# Run the Docker container with the image from Docker Hub
docker run --name dk_rmedic3_dev_container -p 3838:3838 --rm -d legion949/dk_rmedic3_dev_img:latest

# Run the Docker container with the image from Docker Hub and mount the local directory 'example-app' to the container directory '/app'
docker run --name dk_rmedic3_dev_container -v $(pwd)/app:/app -p 3838:3838 --rm -d legion949/dk_rmedic3_dev_img:latest

# Run exec command to access the container
docker exec -it dk_rmedic3_dev_container /bin/bash

########################################################################################

# Stop the Docker container
docker stop dk_rmedic3_dev_container

# Remove the Docker container
docker rm dk_rmedic3_dev_container

# Remove the Docker image
docker rmi legion949/dk_rmedic3_dev_img:0.0.1

# Remove the Docker image
docker rmi legion949/dk_rmedic3_dev_img:latest

########################################################################################

# Pull the Docker image from Docker Hub
docker pull legion949/dk_rmedic3_dev_img:0.0.1

# Run the Docker container with the pulled image
docker run --name dk_rmedic3_dev_container -p 3838:3838 --rm -d legion949/dk_rmedic3_dev_img:0.0.1

# Run the Docker container with the pulled image and mount the local directory 'example-app' to the container directory '/app'
docker run --name dk_rmedic3_dev_container -v $(pwd)/app:/app -p 3838:3838 --rm -d legion949/dk_rmedic3_dev_img:0.0.1

# Run exec command to access the container
docker exec -it dk_rmedic3_dev_container /bin/bash

########################################################################################
