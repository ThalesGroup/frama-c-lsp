#!/bin/bash

npm config set proxy 
npm config set https-proxy 
npm set strict-ssl false
export NODE_TLS_REJECT_UNAUTHORIZED=1

export http_proxy=
export https_proxy=
export HTTP_PROXY=
export HTTPS_PROXY=

git config --global http.proxy "";
git config --global https.proxy "";
