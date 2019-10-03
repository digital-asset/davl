terraform {
  backend "gcs" {
    bucket = "davl-tfstate"
    prefix = "davl"
  }
}

provider "google" {
  project = "da-dev-pinacolada"
  region = "us-east4"
}

resource "google_compute_network" "network" {
  name = "davl-network"
  auto_create_subnetworks = true
}

resource "google_compute_firewall" "default" {
  name = "allow-ssh"
  network = "davl-network"
  allow {
    protocol = "tcp"
    ports = ["22"]
  }
}

resource "google_compute_instance" "demo-ledger" {
  name = "demo-ledger"
  machine_type = "n1-standard-2"
  zone = "us-east4-a"

  boot_disk {
    initialize_params {
      image = "ubuntu-1804-lts"
    }
  }

  network_interface {
    network = "davl-network"
    access_config {
      // auto-generate ephemeral IP
    }
  }

  metadata_startup_script = <<STARTUP
set -euxo pipefail

mkdir -p /home/ubuntu/.ssh
AUTH=/home/ubuntu/.ssh/authorized_keys
echo "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCz3adQiDsx63+7CfxviHQueCPgGtt0/q86E5FGW9zKIvFqKIN3UCKJw0dQmZoNB7nVuYmOMq8PL/p8trXZOamtwFJWyGiDqO+tjfGj6LptNnfWm7iBHMnqSbzgFmsI5/M9bm75qa7VYddRSnTTiXrMTq42kDdl8SHiFHrZa+Xi6MJspY8NZOxhw1lSXsClIEzpimv+sEgRHpDWNxC4rOvrJFZqIA63NkR5WVyODxCUUp0i3lyiyAsVjhDMS2P5+PB6hYSJMeztBkIbS6858npT/8FaoUtcPMQGuKrFxsjKTomaKcEytDrYSA6VXXXJvinkUPeXXOP60DSrd+IZtsrgFI9lNZo4NAK72OZTQbTqVVEjS2Fx34/l4eM2sbQte3JUXYo640miW6HwcdFxDaPDLfwqK+5gL2oFChbB/k5Bsq61KWx0x0kTYX+rKHa12p3zvtOMls2wqebEWeWpN/yuHyNF0ppi3wAB9Khcy8Ea8baUqi0Wy3EMUMWJItrQeirgMBf/6dNF/oXxmKClTD/VCznqtQUjxMEPziwOrpGE9KQCNTlSqQDHK/YsRtlbYtok4EOFG9DixRsa++XP9oBdCYkhwY5CX20IWJGEGdtgzr5hQ0LiNKbYaC51mkH6IUxGN77Q1uHr8QDKbGW8Bp4HUpyZAFxcfD5vQVX3+4+mRw== nick.chapman@digitalasset.com" >> $AUTH
echo "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDZiU8pGsaOzd+hNhK58bp93Zf8wAW3KDB5ucAg0kW1RvQjix99cK059EPSZednMXMujaVCQyMP1WUMyxMo3tBhlgnTCvgH1bdCa6RJ2woyCzj98sjnc+aaEnhZk3QBNhwVObiamlW9WzSddunHrxfthooi/AK/ZXnL0dsawpkBzA1LSW6UTNh87QtKdDjzsJJwRjSuQflMqKmPXsadBIUx0fj5GdUexifxQLTwpBW38A6p7dWYdiMyo56qoR67VzYhiM6rTbEDUF7M3fgch2f4YXccY3ugQivKMYpf7pBa4DEbJ7Q10+xRyw6Qjw2wp8aGjVQuoRvcb7UbrpkqSlNdJLUYfiJsZx4anr2KmepvHXOo0wXrGMfGv30HqsLa4pDNcqxLpHtJ9eRZXVnHWdvk+uricgVnP0K4KmdIDxr9X/Mb/0QV6opYnJYFGX4CBY1C2s+Uo/xc3l+4BaWVoEjHImoWp3z+OChK7FWAacSCPp40+uD1PK2Dvmqm6Ka01+LcUduDKmrsMMJaqYkIiDLlMI7qn2qIxfF+e0Otv6uU8GPwtltkvPKUVXsn0gaDpNKlc49b5mG0dASQUhyqHYOg2R0iuM6fav+oMtxVhDfxD5gDwDHtIVpXaCGvrOKouWMSYiy8jqz0QdGlkqcqtmN0JVc6T0knRmtmV3dyCLZ4oQ== gary.verhaegen@digitalasset.com" >> $AUTH
chown ubuntu:ubuntu $AUTH
chmod 0400 $AUTH

apt-get update
apt-get install -y openjdk-8-jdk

su -c 'curl https://get.daml.com | sh -s 0.13.27' ubuntu

STARTUP
}
