CC=ghc -dynamic

Main: Main.hs
	$(CC) $@ -o calc
