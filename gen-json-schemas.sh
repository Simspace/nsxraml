#!/usr/bin/env bash

set -o errexit
set -o pipefail
[[ "${DEBUG}" == 'true' ]] && set -o xtrace

declare -r schemaDir='schemas'
# TODO: Not portable! :)
declare -r xsd2JsonDir='/Users/jason/git/personal/xsd2json'

convertXsdToJsonSchema() {
  local xsdFile=$1
  local jsonSchemaFile="$(dirname ${xsdFile})/$(basename ${xsdFile} .xsd).json"
  # On macOS, trying to run the xsd2json executable throws some weird ioctl
  # error, so we run it using SWI-Prolog directly:
  #   > https://github.com/fnogatz/xsd2json/issues/87
  swipl -g main ${xsd2JsonDir}/lib-pl/cli.pl -- ${xsdFile} > ${jsonSchemaFile}
}

declare -ar xsdFiles=($(find schemas -type f -name '*.xsd' -print))
for xsdFile in ${xsdFiles[@]}; do
  convertXsdToJsonSchema ${xsdFile}
done
