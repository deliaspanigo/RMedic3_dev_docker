
#!/bin/bash
# Remove the 'app' directory if it exists
if [ -d "app" ]; then
    rm -rf RMedic3_dev_docker
fi

docker pull legion949/dk_rmedic3_dev_img:latest

git clone https://github.com/deliaspanigo/RMedic3_dev_docker.git

docker pull legion949/dk_rmedic3_dev_img:latest

# Run the Docker container with the image from Docker Hub and mount the local directory 'example-app' to the container directory '/app'
docker run --name dk_rmedic3_dev_container -v $(pwd)/app:/app -p 3838:3838 --rm -d legion949/dk_rmedic3_dev_img:latest

# Run exec command to access the container
docker exec -it dk_rmedic3_dev_container /bin/bash


########################################################################################

# Pull the Docker image from Docker Hub
docker pull legion949/dk_rmedic3_dev_img:latest

# Run the Docker container with the image from Docker Hub
docker run --name dk_rmedic3_dev_container -p 3838:3838 --rm -d legion949/dk_rmedic3_dev_img:latest

# Run the Docker container with the image from Docker Hub and mount the local directory 'example-app' to the container directory '/app'
docker run --name dk_rmedic3_dev_container -v $(pwd)/app:/app -p 3838:3838 --rm -d legion949/dk_rmedic3_dev_img:latest

# Run exec command to access the container
docker exec -it dk_rmedic3_dev_container /bin/bash

# Pull the Docker image from Docker Hub
docker pull legion949/dk_rmedic3_dev_img:0.0.1


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
