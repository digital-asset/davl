#!/usr/bin/env bash

set -euo pipefail

cat <<NGINX_CONFIG > /etc/nginx/nginx.conf
worker_processes auto;
pid /run/nginx.pid;

events {
  worker_connections 768;
}

http {
  sendfile on;
  tcp_nopush on;
  tcp_nodelay on;
  keepalive_timeout 65;
  types_hash_max_size 2048;
  include /etc/nginx/mime.types;
  default_type application/octet-stream;

  ssl_protocols TLSv1 TLSv1.1 TLSv1.2;
  ssl_prefer_server_ciphers on;

  access_log /var/log/nginx/access.log;
  error_log /var/log/nginx/error.log;

  gzip on;

  server {
    listen 80;
    server_name _;
    location /contracts {
      proxy_pass http://${LEDGER_IP_PORT}/;
    }
    location /command {
      proxy_pass http://${LEDGER_IP_PORT}/;
    }
    location /parties {
      proxy_pass http://${LEDGER_IP_PORT}/;
    }

    root /app/ui;
    index index.html;
    location / {
      try_files \$uri \$uri/ =404;
    }
  }
}
NGINX_CONFIG
