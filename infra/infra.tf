terraform {
  backend "gcs" {
    bucket = "davl-tfstate"
    prefix = "davl"
  }
}


locals {
  sandbox = "201911090036-2cce8b"
  json    = "201911072118-6e3b95"
  ui      = "201911080149-d538bc"
}

provider "google" {
  project = "da-dev-pinacolada"
  region  = "us-east4"
}

# Network created by the security team. Allows all traffic from offices and
# VPN.
data "google_compute_network" "network" {
  name = "da-pinacolada-vpc-1"
}

data "google_compute_subnetwork" "subnet" {
  name = "da-gcp-pinacolada-subnet-1"
}

resource "google_compute_firewall" "allow-external-ssh" {
  name    = "allow-external-ssh"
  network = "${data.google_compute_network.network.self_link}"
  allow {
    protocol = "tcp"
    ports    = ["22"]
  }
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

resource "google_compute_instance" "db" {
  name         = "db"
  machine_type = "n1-standard-2"
  zone         = "us-east4-a"

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
}

resource "google_compute_instance" "ledger" {
  name         = "ledger"
  machine_type = "n1-standard-2"
  zone         = "us-east4-a"

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

docker run --name sandbox -d -p 127.0.0.1:6865:6865 gcr.io/da-dev-pinacolada/sandbox:${local.sandbox} --sql-backend-jdbcurl 'jdbc:postgresql://${google_compute_instance.db.network_interface.0.network_ip}/davl-db?user=davl&password=s3cr3t'

# Wait for ledger to be ready
docker exec sandbox /bin/sh -c "while ! nc -z localhost:6865; do sleep 1; done"

# <workaround>
# sandbox currently ignores the DAR files given to it when it detects it's
# running against an existing database, so we need to manually deploy the DAR
# file after it has started. Since uploading a DAR file is idempotent, this
# should not break if/when the sandbox behaviour changes.
docker exec sandbox /bin/sh -c "/root/.daml/bin/daml ledger upload-dar --host=127.0.0.1 --port=6865 /app/daml.dar"
# </workaround>

docker run --name json-api -d --link sandbox -p 7575:7575 gcr.io/da-dev-pinacolada/json-api:${local.json} --ledger-host sandbox --ledger-port 6865 --http-port 7575

# <workaround>
# The UI currently does not support signing up, so we add a running Navigator
# to our setup. It will be served on 8080, so we also need to expose that port.
# Note: this relies on the Docker image containing the whole SDK.
docker run --name navigator --link sandbox -p 8080:4000 --entrypoint /bin/sh -d gcr.io/da-dev-pinacolada/sandbox:201911090036-2cce8b -c "
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
  name         = "proxy"
  machine_type = "n1-standard-2"
  zone         = "us-east4-a"

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
docker run -p 8080:8080 -e NAVIGATOR_IP_PORT=${google_compute_instance.ledger.network_interface.0.network_ip}:8080 -p 80:80 -e LEDGER_IP_PORT=${google_compute_instance.ledger.network_interface.0.network_ip}:7575 gcr.io/da-dev-pinacolada/ui:${local.ui}
# </workaround>


STARTUP
}

resource "google_compute_address" "frontend" {
  name   = "frontend"
  region = "us-east4"
}

resource "google_compute_target_instance" "frontend" {
  name     = "frontend"
  instance = "${google_compute_instance.proxy.self_link}"
  zone     = "us-east4-a"
}

resource "google_compute_forwarding_rule" "frontend-http" {
  name       = "frontend-http"
  target     = "${google_compute_target_instance.frontend.self_link}"
  ip_address = "${google_compute_address.frontend.address}"
  port_range = "80"
}

resource "google_compute_forwarding_rule" "navigator" {
  name       = "navigator"
  target     = "${google_compute_target_instance.frontend.self_link}"
  ip_address = "${google_compute_address.frontend.address}"
  port_range = "8080"
}

output "frontend-address" {
  value = "${google_compute_address.frontend.address}"
}
