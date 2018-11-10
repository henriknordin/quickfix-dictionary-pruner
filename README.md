# quickfix-dictionary-pruner

Utility for finding all field definitions in a QuickFIX data dictionary but not used by any of the message definitions.


## Build

    stack build

## Run

    stack exec -- quickfix-dictionary-pruner-exe -f FILENAME

## Development

In order to display errors/warnings, run ghcid in a separate window

    make ghcid

