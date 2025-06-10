#!/bin/bash

# Detectar número de cores disponibles
NUM_CORES=$(grep -c ^processor /proc/cpuinfo)

# Para cargas con muchos usuarios, agregar un 50% más de procesos que cores
# Ajusta esta fórmula según tus necesidades
NUM_PROCESSES=$((NUM_CORES + NUM_CORES/2))

# Crear el archivo de configuración
cat > /etc/shiny-server/shiny-server.conf << EOL
server {
  listen 3838;
  
  # Usar el número de cores detectado automáticamente
  simple_scheduler ${NUM_PROCESSES};
  
  run_as shiny;
  
  app_idle_timeout 180;
  app_init_timeout 120;
  
  location / {
    app_dir /app;
    log_dir /var/log/shiny-server;
    log_level info;
    sanitize_errors false;
    directory_index on;
  }
}
EOL

# Asegurar permisos correctos
chmod 644 /etc/shiny-server/shiny-server.conf
