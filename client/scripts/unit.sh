#!/usr/bin/env bash
# Lance les tests unitaires Mocha SANS VSCode (pas besoin de xvfb)

set -e

cd "$(dirname "$0")/.."   # on remonte à la racine du client

# S'assure que rewire est installé
if [ ! -d "node_modules/rewire" ]; then
    echo "[unit] Installation de rewire..."
    npm install --save-dev rewire @types/rewire
fi

echo "[unit] Lancement des tests unitaires..."
node_modules/.bin/mocha \
    --require ts-node/register \
    --timeout 10000 \
    --ui tdd \
    "out/test/suite/unit/**/*.test.js"
