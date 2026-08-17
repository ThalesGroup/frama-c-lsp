#!/usr/bin/env bash

export CODE_TESTS_PATH="$(pwd)/out/test"
export CODE_TESTS_WORKSPACE="$(pwd)/testFixture"
export VSCODE_TEST_VERSION="1.115.0"

node "$(pwd)/out/test/runTest"