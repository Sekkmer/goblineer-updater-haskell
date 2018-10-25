FROM ubuntu:18.04

RUN apt-get update && apt-get install -y \
	haskell-platform \
	build-essential

RUN cabal install -y \
	aeson \
	http-client \
	cmdargs
