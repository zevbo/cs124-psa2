TARGET = strassen

$(TARGET):$(TARGET).ml
	dune build $(TARGET).exe
	rm -f strassen
	mv _build/default/$(TARGET).exe ./strassen