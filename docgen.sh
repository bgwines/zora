# Usage (from project root): ./docgen.sh <Repo Name> <Version> <Hackage Username> <Hackage Password>
# e.g. sudo bash docgen.sh Zora 1.1.17 bgwines Hacinco142857
#!/bin/bash
cabal configure && cabal build && cabal haddock --hyperlink-source \
                                    --html-location='http://hackage.haskell.org/package/$pkg/docs' \
                                    --contents-location='http://hackage.haskell.org/package/$pkg'
S=$?
if [ "${S}" -eq "0" ]; then
    cd "dist/doc/html"
    DDIR="${1}-${2}-docs"
    cp -r "${1}" "${DDIR}" && tar -c -v -z --format=ustar -f "${DDIR}.tar.gz" "${DDIR}"
    CS=$?
    if [ "${CS}" -eq "0" ]; then
        echo "Uploading to Hackage…"
        curl -X PUT -H 'Content-Type: application/x-tar' -H 'Content-Encoding: gzip' --data-binary "@${DDIR}.tar.gz" "http://${3}:${4}@hackage.haskell.org/package/${1}-${2}/docs"
        exit $?
    else
        echo "Error when packaging the documentation"
        exit $CS
    fi
else
    echo "Error when trying to build the package."
    exit $S
fi