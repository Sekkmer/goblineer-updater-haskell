FROM andipeter/haskell-dev

COPY . /app
WORKDIR /app

RUN make build

CMD ["ghci Main.hs"]


