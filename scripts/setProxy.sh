#!/bin/bash

npm config set proxy http://165.225.76.41:80;
npm config set https-proxy http://165.225.76.41:80;
npm set strict-ssl false;
export NODE_TLS_REJECT_UNAUTHORIZED=1;

export http_proxy=http://165.225.76.41:80;
export https_proxy=http://165.225.76.41:80;
export HTTP_PROXY=http://165.225.76.41:80;
export HTTPS_PROXY=http://165.225.76.41:80;

git config --global http.proxy "http://165.225.76.41:80";
git config --global https.proxy "http://165.225.76.41:80";
