terraform {
  backend "gcs" {
    bucket = "davl-tfstate"
    prefix = "davl"
  }
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

docker run --name sandbox -d -p 127.0.0.1:6865:6865 gcr.io/da-dev-pinacolada/sandbox:20191107-1954-d7dad6ce96 --sql-backend-jdbcurl 'jdbc:postgresql://${google_compute_instance.db.network_interface.0.network_ip}/davl-db?user=davl&password=s3cr3t'

# Wait for ledger to be ready
docker exec sandbox /bin/sh -c "while ! nc -z localhost:6865; do sleep 1; done"

docker run --name json-api -d --link sandbox -p 7575:7575 gcr.io/da-dev-pinacolada/json-api:20191107-1954-d7dad6ce96 --ledger-host sandbox --ledger-port 6865 --http-port 7575

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

docker run -p 80:80 -e LEDGER_IP_PORT=${google_compute_instance.ledger.network_interface.0.network_ip}:7575 gcr.io/da-dev-pinacolada/ui:20191107-1626-4e3f3cc81e

STARTUP
}
