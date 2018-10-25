#!/bin/sh

sudo docker build -t gu-haskell .
sudo docker run --rm gu-haskell
