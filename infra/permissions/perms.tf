// This file is meant to document the permissions needed to apply the main
// Terraform file in the parent directory. In case a new project gets set up,
// this file should be applied first, by someone with full admin access to the
// project, and be used to give everyone else the appropriate accesses.

terraform {
  backend "gcs" {
    bucket = "davl-tfstate"
    prefix = "davl-permissions"
  }
}

provider "google" {
  project = "da-dev-pinacolada"
  region  = "us-east4"
  zone    = "us-east4-a"
}

// Service account to represent CI runner. Note that generating the API key
// that allows CI to log in as this account and making that key available to CI
// machines is outside the scope of this Terraform file.
resource "google_service_account" "ci" {
  account_id = "azure-pipelines"
}

// CI can write to the special GCS bucket that backs GCR.
// Note: this bucket gets created when the first image is uploaded, so that
// needs to be done manually first.
resource "google_storage_bucket_iam_member" "ci-write-images" {
  bucket = "artifacts.da-dev-pinacolada.appspot.com"
  role   = "roles/storage.admin"
  member = "serviceAccount:${google_service_account.ci.email}"
}

// CI can check the existence of the backup bucket (but doesn't need to do
// anything else with it).
resource "google_project_iam_custom_role" "ci-db-backups" {
  role_id = "checkBackupsBucketExistence"
  title   = "Minimal rights for CI TF to check backup bucket exists"
  permissions = [
    "storage.buckets.get",
  ]
}
resource "google_storage_bucket_iam_member" "ci-backup-bucket" {
  bucket = "davl-db-backups"
  role   = "projects/da-dev-pinacolada/roles/checkBackupsBucketExistence"
  member = "serviceAccount:${google_service_account.ci.email}"
}


// The next 2 roles are incrementally built to have the minimal set of rights
// to be able to deploy using Terraform.
resource "google_project_iam_custom_role" "tf-write-state" {
  role_id = "writeTerraformState"
  title   = "Read and write Terraform state"
  permissions = [
    "storage.objects.list",
    "storage.objects.get",
    "storage.objects.create",
    "storage.objects.delete"
  ]
}

resource "google_project_iam_custom_role" "tf-deploy" {
  role_id = "deployTerraform"
  title   = "Deploy new versions through Terraform"
  permissions = [
    "compute.securityPolicies.get",
    "compute.httpHealthChecks.get",
    "compute.addresses.get",
    "compute.networks.get",
    "compute.subnetworks.get",
    "compute.instances.get",
    "compute.firewalls.get",
    "compute.instanceGroups.get",
    "compute.backendServices.get",
    "compute.urlMaps.get",
    "compute.targetHttpProxies.get",
    "compute.targetHttpsProxies.get",
    "compute.forwardingRules.get",
    "compute.instances.delete",
    "compute.zones.get",
    "compute.images.get",
    "compute.images.getFromFamily",
    "compute.instances.create",
    "compute.disks.create",
    "compute.subnetworks.use",
    "compute.subnetworks.useExternalIp",
    "compute.instances.setMetadata",
    "compute.instances.setServiceAccount",
    "compute.instances.setTags",
    "compute.instanceGroups.update",
    "compute.instances.use",
  ]
}

resource "google_service_account_iam_member" "ci-deploy" {
  service_account_id = "projects/da-dev-pinacolada/serviceAccounts/837776980187-compute@developer.gserviceaccount.com"
  role               = "roles/iam.serviceAccountUser"
  member             = "serviceAccount:${google_service_account.ci.email}"
}

resource "google_storage_bucket_iam_member" "ci-deploy" {
  bucket     = "davl-tfstate"
  role       = "projects/da-dev-pinacolada/roles/writeTerraformState"
  member     = "serviceAccount:${google_service_account.ci.email}"
  depends_on = [google_project_iam_custom_role.tf-write-state]
}

resource "google_project_iam_member" "ci-deploy" {
  role   = "projects/da-dev-pinacolada/roles/deployTerraform"
  member = "serviceAccount:${google_service_account.ci.email}"
}
