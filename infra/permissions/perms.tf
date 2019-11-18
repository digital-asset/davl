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
