# POC Steps

1. Install `oas-raml-converter` tool: https://github.com/mulesoft/oas-raml-converter
1. Install SWI-Prolog (for `xsd2json`'s internal use): `brew install swi-prolog`
1. Install `xsd2json`: `npm install -g xsd2json`
1. Install `swagger-codegen`: `brew install swagger-codegen`
1. Convert all the XSD schemas to JSON schemas: `bash gen-json-schemas.sh`
1. Convert NSX RAML file to OAS 2.0 (a.k.a. swagger) file: `<path_to>/oas-raml-converter/lib/bin/converter.js --from RAML --to OAS20 api.raml > swagger.json`
1. Convert swagger file to Haskell code: `swagger-codegen generate -i swagger.json -l haskell -o temp/`

Note that the `nsxvapi.raml` file was manually modified for the POC:

* `advancedConfigUpdate` schema bit was changed from xsd to the generated json schema
* The api section using `advancedConfigUpdate` had its `example` removed (because it was XML and not JSON) and the content type was changed to `application/json`

The above 2 things should be scripted, but did it manually for POC pruposes.
