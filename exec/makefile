TARGET = strassen

$(TARGET):$(TARGET).ml
	rm -f strassen.exe
	dune build $(TARGET).exe
	mv _build/default/$(TARGET).exe ./