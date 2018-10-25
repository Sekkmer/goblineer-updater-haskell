build: Main.hs Downloader.hs MarketValue.hs
	ghc -O2 Main.hs

br: build.sh
	bash build.sh
