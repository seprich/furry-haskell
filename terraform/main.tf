provider "aws" {
  version = "~> 2.8"
  region  = "eu-north-1"
}

terraform {
  backend "s3" {
    bucket = "seprich-terraform-790309491566"
    key    = "terraform/state"
    region = "eu-north-1"
  }
}
