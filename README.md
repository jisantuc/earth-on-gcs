`earth-on-gcs`
===

A thin Servant proxy in front of Google Big Query's indices of geographic data.

Getting Started
-----

### Running the server

- set a `GCP_AUTH_KEY` environment variable that you can use to communicate with GCP
- `stack build`, `stack exec earth-on-gcs-exe`
- `http :8081/landsat cloudCoverMin==12.5 dateAcquiredMax==2018-05-13` etc.

### Running tests

There aren't any yet.
