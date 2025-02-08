
#!/bin/bash
# Remove the 'app' directory if it exists
if [ -d "app" ]; then
    rm -rf app
fi

git clone https://github.com/deliaspanigo/RMedic3.git app


# Build the Docker image with the name 'my_docker_image'
docker build -t dk_rmedic3_dev_img .

# Run the Docker container with the name 'my_docker_container'
docker run --name dk_rmedic3_dev_container -p 3838:3838 --rm -d dk_rmedic3_dev_img 

# Run the Docker container with the name 'my_docker_container' and mount the local directory 'example-app' to the container directory '/app'
docker run --name dk_rmedic3_dev_container -v $(pwd)/app:/app -p 3838:3838 --rm -d dk_rmedic3_dev_img


# Run exec command to access the container
docker exec -it dk_rmedic3_dev_container /bin/bash

########################################################################################
# Stop the Docker container
docker stop dk_rmedic3_dev_container

# Remove the Docker container
docker rm dk_rmedic3_dev_container

# Remove the Docker image
docker rmi dk_rmedic3_dev_img   

