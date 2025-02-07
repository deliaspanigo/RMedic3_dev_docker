# Stop all running containers
docker stop $(docker ps -q)

# Remove all containers (running and stopped)
docker rm $(docker ps -a -q)

# Remove all images
docker rmi $(docker images -q)

# Remove unused volumes
docker volume prune -f

# Remove unused networks
docker network prune -f

# Clean the system (containers, images, volumes, networks) at once
docker system prune -a --volumes -f
