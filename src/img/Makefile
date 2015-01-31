indirect_inputs.png: indirect_inputs.svg
indirect_outputs.png: indirect_outputs.svg

INKSCAPE=/Applications/Inkscape.app/Contents/Resources/bin/inkscape
WIDTH=356
HEIGHT=226

%.png: %.svg
	$(INKSCAPE) --export-png $@ -w $(WIDTH) -h $(HEIGHT) $<
