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

# <workaround>
# The UI currently does not support signing up, so we add a running Navigator
# to our setup. It will be served on 8080, so we also need to expose that port.
  server {
    listen 8080;
    server_name navigator;
    location / {
      proxy_pass http://${NAVIGATOR_IP_PORT};
    }
  }
# </workaround>

  # This serves https://davl.da-ext.net, with the load balancer doing TLS
  # termination.
  server {
    listen 80;
    server_name davl.da-ext.net;

    location /v1/stream {
      proxy_pass http://${LEDGER_IP_PORT};
      proxy_http_version 1.1;
      proxy_set_header Upgrade \$http_upgrade;
      proxy_set_header Connection "Upgrade";
    }

    location /v1 {
      proxy_pass http://${LEDGER_IP_PORT};
    }

    root /app/ui;
    index index.html;
    location / {
      try_files \$uri \$uri/ =404;
    }

    # Disable all forms of caching on all requests.
    # A real production setup would require more granularity, but caching is
    # hard and we currently don't have big enough volumes.
    add_header Last-Modified \$date_gmt;
    add_header Expires \$date_gmt;
    add_header Cache-Control "no-store, no-cache, must-revalidate, proxy-revalidate, max-age=0";
    add_header Pragme "no-cache";
  }

  # Load balancer redirects plain HTTP to this port.
  server {
    listen 8081;
    server_name _;
    return 307 https://davl.da-ext.net\$request_uri;
  }
}
NGINX_CONFIG
