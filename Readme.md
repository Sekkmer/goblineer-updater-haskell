# Goblineer Updater

Goblineer updater analyses the World of Warcraft auction house and calculates marketvalues.

## Building

Required haskell packages:

- aeson
- http-client
- cmdargs

You can build the project by running:
```
make build
```
or if make in not installed:
```
ghc -O2 Main.hs
```

## Usage

```
./Main --region=<region> --realm=<realm> --apikey=<apikey>
```

The output file will be `out.json`
