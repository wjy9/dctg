#!/usr/bin/env bash
curl http://localhost:8080/ -X POST -d @config.json --header "Content-Type:application/json"

curl http://localhost:8080/status

curl http://localhost:8080/start