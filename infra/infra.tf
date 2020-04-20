terraform {
  backend "gcs" {
    bucket = "davl-tfstate"
    prefix = "davl"
  }
}

variable "sandbox" {}
variable "ui" {}
variable "json" {}
variable "trigger" {}

provider "google" {
  project = "da-dev-pinacolada"
  region  = "us-east4"
  zone    = "us-east4-a"
  version = "2.17.0"
}

# Network created by the security team. Allows all traffic from offices and
# VPN.
data "google_compute_network" "network" {
  name = "da-pinacolada-vpc-1"
}

data "google_compute_subnetwork" "subnet" {
  name = "da-gcp-pinacolada-subnet-1"
}

resource "google_compute_firewall" "allow-internal" {
  name    = "allow-internal"
  network = "${data.google_compute_network.network.self_link}"
  allow {
    protocol = "tcp"
  }
  allow {
    protocol = "udp"
  }
  allow {
    protocol = "icmp"
  }
  source_ranges = ["${data.google_compute_subnetwork.subnet.ip_cidr_range}"]
}

resource "google_compute_firewall" "allow-http-load-balancers" {
  name    = "allow-lb"
  network = "${data.google_compute_network.network.self_link}"
  allow {
    protocol = "tcp"
    ports    = ["80", "8080", "8081"]
  }
  source_ranges = ["130.211.0.0/22", "35.191.0.0/16"]
  target_tags   = ["http-enabled"]
}

resource "google_compute_instance" "db" {
  name         = "db"
  machine_type = "n1-standard-2"

  boot_disk {
    initialize_params {
      image = "ubuntu-1804-lts"
    }
  }

  network_interface {
    network    = "${data.google_compute_network.network.self_link}"
    subnetwork = "${data.google_compute_subnetwork.subnet.self_link}"
    access_config {
      // auto-generate ephemeral IP
    }
  }

  metadata_startup_script = <<STARTUP
set -euxo pipefail

apt-get update
apt-get install -y docker.io

docker run -e POSTGRES_USER=davl \
           -e POSTGRES_PASSWORD=s3cr3t \
           -e POSTGRES_DB=davl-db \
           -e PGDATA=/var/lib/postgresql/data/pgdata \
           -v /db:/var/lib/postgresql/data \
           -p 5432:5432 \
           postgres:11.5-alpine

STARTUP

  lifecycle {
    prevent_destroy = true
  }
}

resource "google_storage_bucket" "db-backups" {
  name = "davl-db-backups"

  storage_class = "STANDARD"

  # keep 60 days of backups
  lifecycle_rule {
    action {
      type = "Delete"
    }
    condition {
      age = "60" # days
    }
  }
}

resource "google_compute_instance" "backed-up-db" {
  name         = "backed-up-db"
  machine_type = "n1-standard-2"

  boot_disk {
    initialize_params {
      image = "ubuntu-1804-lts"
    }
  }

  network_interface {
    network    = "${data.google_compute_network.network.self_link}"
    subnetwork = "${data.google_compute_subnetwork.subnet.self_link}"
    access_config {
      // auto-generate ephemeral IP
    }
  }

  service_account {
    scopes = ["https://www.googleapis.com/auth/devstorage.read_write"]
  }

  metadata_startup_script = <<STARTUP
set -euxo pipefail

apt-get update
apt-get install -y docker.io

log() {
  echo "$(date -Is -u) $1" >> /root/log 2>&1
}

log "boot"

LATEST_BACKUP_GCS=$(gsutil ls ${google_storage_bucket.db-backups.url} | sort | tail -1)
LATEST_BACKUP=/backup/$(basename $LATEST_BACKUP_GCS)
mkdir /backup
gsutil cp $LATEST_BACKUP_GCS $LATEST_BACKUP

log "fetched $LATEST_BACKUP_GCS"

docker run -d \
           --name pg \
           -e POSTGRES_USER=davl \
           -e POSTGRES_PASSWORD=s3cr3t \
           -e POSTGRES_DB=davl-db \
           -e PGDATA=/var/lib/postgresql/data/pgdata \
           -v /db:/var/lib/postgresql/data \
           -v /backup:/backup \
           -p 5432:5432 \
           postgres:11.5-alpine

log "started pg"

docker exec pg bash -c "while ! nc -z localhost:5432; do sleep 1; done"

log "pg ready"

# Note: because /backup is mounted on /backup, path will work inside the
# container too.
docker exec pg bash -c "cat $LATEST_BACKUP | gunzip | psql davl-db -U davl"

log "restored $LATEST_BACKUP"

cat <<CRON > /root/backup.sh
#!/usr/bin/env bash
set -euo pipefail
echo "\$(date -Is -u) start backup"

TMP=\$(mktemp)
BACKUP=\$(date -u +%Y%d%m%H%M%SZ).gz
docker exec pg pg_dump -U davl -d davl-db | gzip -9 > \$TMP
$(which gsutil) cp \$TMP ${google_storage_bucket.db-backups.url}/\$BACKUP
rm \$TMP

echo "\$(date -Is -u) end backup"
CRON

chmod +x /root/backup.sh

cat <<CRONTAB >> /etc/crontab
0 * * * * root /root/backup.sh >> /root/log 2>&1
CRONTAB

tail -f /root/log

STARTUP
}

resource "google_compute_instance" "ledger" {
  name         = "ledger"
  machine_type = "n1-standard-2"

  boot_disk {
    initialize_params {
      image = "ubuntu-1804-lts"
    }
  }

  network_interface {
    network    = "${data.google_compute_network.network.self_link}"
    subnetwork = "${data.google_compute_subnetwork.subnet.self_link}"
    access_config {
      // auto-generate ephemeral IP
    }
  }

  service_account {
    scopes = ["https://www.googleapis.com/auth/devstorage.read_only"]
  }

  metadata_startup_script = <<STARTUP
set -euxo pipefail

apt-get update
apt-get install -y docker.io

curl https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-270.0.0-linux-x86_64.tar.gz | tar xz
export PATH="$(pwd)/google-cloud-sdk/bin:$PATH"

gcloud components install docker-credential-gcr
gcloud auth configure-docker --quiet

# Wait for postgres machine to be listening
while ! nc -z ${google_compute_instance.db.network_interface.0.network_ip} 5432; do
  sleep 1
done

docker run --name sandbox -d -p 127.0.0.1:6865:6865 gcr.io/da-dev-pinacolada/sandbox:${var.sandbox} --sql-backend-jdbcurl 'jdbc:postgresql://${google_compute_instance.db.network_interface.0.network_ip}/davl-db?user=davl&password=s3cr3t'

# Wait for ledger to be ready
docker exec sandbox /bin/sh -c "while ! nc -z localhost:6865; do sleep 1; done"

# <workaround>
# sandbox currently ignores the DAR files given to it when it detects it's
# running against an existing database, so we need to manually deploy the DAR
# file after it has started. Since uploading a DAR file is idempotent, this
# should not break if/when the sandbox behaviour changes.
docker exec sandbox /bin/sh -c "for f in /app/released/*.dar; do /root/.daml/bin/daml ledger upload-dar --host=127.0.0.1 --port=6865 \$f; done"
# </workaround>

docker run --name json-api -d --link sandbox -p 7575:7575 gcr.io/da-dev-pinacolada/json-api:${var.json} --ledger-host sandbox --ledger-port 6865 --http-port 7575

docker run --name script --link sandbox --entrypoint="java" gcr.io/da-dev-pinacolada/trigger:${var.trigger} \
     -Dlogback.configurationFile=/app/daml-sdk/script-logback.xml \
     -jar \
     /app/daml-sdk/daml-sdk.jar \
     script \
     --dar \
     /app/automation.dar \
     --script-name "Automation:init" \
     --input-file /app/init.json \
     --wall-clock-time \
     --ledger-host sandbox\
     --ledger-port 6865
docker run --name automation -d --link sandbox gcr.io/da-dev-pinacolada/trigger:${var.trigger} --ledger-host sandbox --ledger-port 6865

# <workaround>
# The UI currently does not support signing up, so we add a running Navigator
# to our setup. It will be served on 8080, so we also need to expose that port.
# Note: this relies on the Docker image containing the whole SDK.
docker run --name navigator --link sandbox -p 8080:4000 --entrypoint /bin/sh -d gcr.io/da-dev-pinacolada/sandbox:${var.sandbox} -c "
cat <<EOF > /app/navigator.conf
users {
  DA {
    party = \"Digital Asset\"
  }
}
EOF

/root/.daml/bin/daml navigator server --port 4000 --config-file /app/navigator.conf sandbox 6865"
# </workaround>
STARTUP
}

resource "google_compute_instance" "proxy" {
  name         = "proxy-${var.ui}"
  machine_type = "n1-standard-2"

  boot_disk {
    initialize_params {
      image = "ubuntu-1804-lts"
    }
  }

  network_interface {
    network    = "${data.google_compute_network.network.self_link}"
    subnetwork = "${data.google_compute_subnetwork.subnet.self_link}"
    access_config {
      // auto-generate ephemeral IP
    }
  }

  tags = ["http-enabled"]

  service_account {
    scopes = ["https://www.googleapis.com/auth/devstorage.read_only"]
  }
  metadata_startup_script = <<STARTUP
set -euxo pipefail

apt-get update
apt-get install -y docker.io

curl https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-270.0.0-linux-x86_64.tar.gz | tar xz
export PATH="$(pwd)/google-cloud-sdk/bin:$PATH"

gcloud components install docker-credential-gcr
gcloud auth configure-docker --quiet

# <workaround>
# The UI currently does not support signing up, so we add a running Navigator
# to our setup. It will be served on 8080, so we also need to expose that port.
# Added -p 8080:8080 and -e NAVIGATOR_IP_PORT=...
docker run -p 8081:8081 -p 8080:8080 -e NAVIGATOR_IP_PORT=${google_compute_instance.ledger.network_interface.0.network_ip}:8080 -p 80:80 -e LEDGER_IP_PORT=${google_compute_instance.ledger.network_interface.0.network_ip}:7575 gcr.io/da-dev-pinacolada/ui:${var.ui}
# </workaround>


STARTUP
}

resource "google_compute_address" "proxy" {
  name         = "proxy"
  region       = "us-east4"
  network_tier = "STANDARD"
}

// Networking rules need to target a group, so we create on for the forntend
// machine.
resource "google_compute_instance_group" "frontend" {
  name      = "frontend"
  instances = ["${google_compute_instance.proxy.self_link}"]
  named_port {
    name = "http"
    port = "8081"
  }
  named_port {
    name = "https"
    port = "80"
  }
  named_port {
    name = "navigator"
    port = "8080"
  }
}

// The load balancers are created outside our network, and therefore do not
// inherit the security rules defined by the security team as firewall rules on
// the network. We still want (for now at least) the same level of isolation.
resource "google_compute_security_policy" "only-offices-and-vpns" {
  name = "only-offices-and-vpns"

  rule {
    action   = "deny(403)"
    priority = "2147483647"
    match {
      versioned_expr = "SRC_IPS_V1"
      config {
        src_ip_ranges = ["*"]
      }
    }
    description = "Default: deny all"
  }

  rule {
    action   = "allow"
    priority = 998
    match {
      versioned_expr = "SRC_IPS_V1"
      config {
        src_ip_ranges = [
          "188.142.210.213",
          "49.255.4.210",
          "172.85.43.2",
          "151.237.232.154",
          "146.4.45.122"
        ]
      }
    }
    description = "Allow offices"
  }

  // There is a limit of 5 ranges per rule.
  rule {
    action   = "allow"
    priority = 997
    match {
      versioned_expr = "SRC_IPS_V1"
      config {
        src_ip_ranges = [
          "52.204.127.83",
          "35.194.81.56",
          "35.189.40.124",
          "35.198.147.95",
          "52.73.38.90",
        ]
      }
    }
    description = "Allow first five VPNs"
  }

  rule {
    action   = "allow"
    priority = 999
    match {
      versioned_expr = "SRC_IPS_V1"
      config {
        src_ip_ranges = [
          "34.92.190.230"
        ]
      }
    }
    description = "Allow last VPN"
  }
}

// All our networking rules target the same machine so we use the same health
// check. Note: this always checks port 80 on the proxy machine.
resource "google_compute_http_health_check" "frontend" {
  name               = "health-check"
  request_path       = "/"
  check_interval_sec = 20
  timeout_sec        = 3
}

// FRONTEND: forward connections on port 80 to frontend machine port 8081, so
// they get redirected by nginx to HTTPS on port 443.
resource "google_compute_forwarding_rule" "frontend" {
  name         = "frontend"
  target       = "${google_compute_target_http_proxy.frontend.self_link}"
  ip_address   = "${google_compute_address.proxy.address}"
  port_range   = "80"
  network_tier = "STANDARD"
}

resource "google_compute_target_http_proxy" "frontend" {
  name    = "frontend"
  url_map = "${google_compute_url_map.frontend.self_link}"
}

resource "google_compute_url_map" "frontend" {
  name            = "frontend"
  default_service = "${google_compute_backend_service.frontend.self_link}"
}

resource "google_compute_backend_service" "frontend" {
  name            = "frontend"
  health_checks   = ["${google_compute_http_health_check.frontend.self_link}"]
  security_policy = "${google_compute_security_policy.only-offices-and-vpns.self_link}"
  port_name       = "http"
  backend {
    group = "${google_compute_instance_group.frontend.self_link}"
  }
}

// UI: serve application; redirect connections to port 443 of the external IP
// address to port 80 of the machine (after TLS termination)
resource "google_compute_forwarding_rule" "ui" {
  name         = "ui"
  target       = "${google_compute_target_https_proxy.ui.self_link}"
  ip_address   = "${google_compute_address.proxy.address}"
  port_range   = "443"
  network_tier = "STANDARD"
}

resource "google_compute_target_https_proxy" "ui" {
  name             = "ui"
  url_map          = "${google_compute_url_map.ui.self_link}"
  ssl_certificates = ["https://www.googleapis.com/compute/v1/projects/da-dev-pinacolada/global/sslCertificates/da-ext-wildcard"]
}

resource "google_compute_url_map" "ui" {
  name            = "ui"
  default_service = "${google_compute_backend_service.ui.self_link}"
}

resource "google_compute_backend_service" "ui" {
  name            = "ui"
  health_checks   = ["${google_compute_http_health_check.frontend.self_link}"]
  security_policy = "${google_compute_security_policy.only-offices-and-vpns.self_link}"
  port_name       = "https"
  backend {
    group = "${google_compute_instance_group.frontend.self_link}"
  }
  # Because GCP is... well, GCP, this timeout is not just for failed
  # connections, i.e. the maximum time the proxy would wait befire returning an
  # error to clients when the backend doesn't respond, it's also the maximum
  # connection time for _active_ websocket connections, regardless of how much
  # traffic is flowing through them. So we set an unusually large value.
  timeout_sec = 900
}

// Navigator: serve on 8080. Cannot serve as HTTPS as GCP restricts that to
// 443.
resource "google_compute_forwarding_rule" "navigator" {
  name         = "navigator"
  target       = "${google_compute_target_http_proxy.navigator.self_link}"
  ip_address   = "${google_compute_address.proxy.address}"
  port_range   = "8080"
  network_tier = "STANDARD"
}

resource "google_compute_target_http_proxy" "navigator" {
  name    = "navigator"
  url_map = "${google_compute_url_map.navigator.self_link}"
}

resource "google_compute_url_map" "navigator" {
  name            = "navigator"
  default_service = "${google_compute_backend_service.navigator.self_link}"
}

resource "google_compute_backend_service" "navigator" {
  name            = "navigator"
  health_checks   = ["${google_compute_http_health_check.frontend.self_link}"]
  security_policy = "${google_compute_security_policy.only-offices-and-vpns.self_link}"
  port_name       = "navigator"
  backend {
    group = "${google_compute_instance_group.frontend.self_link}"
  }
}

output "proxy-ip" {
  value = "${google_compute_address.proxy.address}"
}
