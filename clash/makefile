-include ../build.cfg

##----------------------------------------------------------------------------##
#   Project Settings                                                           #
##----------------------------------------------------------------------------##

NAME = KitchenTimer
TOPENTITY = TopEntity

##----------------------------------------------------------------------------##
#   Build Rules                                                                #
##----------------------------------------------------------------------------##

default: iceblink

upload: iceblink-upload

iceblink-upload: ${BUILDDIR}/iceblink/${NAME}.bin
	${ICEDUDE} -U flash:w:$<

iceblink: ${BUILDDIR}/iceblink/${NAME}.bin

${BUILDDIR}/iceblink/${NAME}.bin: ${BUILDDIR}/iceblink/${NAME}.asc
	${ICEPACK} $< $@
	echo "Created $@"

${BUILDDIR}/iceblink/${NAME}.asc: iceblink.pcf ${BUILDDIR}/iceblink/${NAME}.blif
	${NEXTPNR} --no-tmdriv --hx1k --package vq100 --json ${BUILDDIR}/iceblink/${NAME}.json --pcf iceblink.pcf --asc $@

${BUILDDIR}/iceblink/${NAME}.blif: ${BUILDDIR}/verilog/${TOPENTITY}/${NAME}.v
	mkdir -p ${BUILDDIR}/iceblink
	$(eval VFILES = $(shell find ${BUILDDIR}/verilog/${TOPENTITY} -name '*.v' ! -name '*testbench*'))
	${YOSYS} -p "synth_ice40 -abc2 -top ${NAME} -json ${BUILDDIR}/iceblink/${NAME}.json" ${VFILES}

${BUILDDIR}/verilog/${TOPENTITY}/${NAME}.v: src/${TOPENTITY}.hs src/Control.hs
	mkdir -p ${BUILDDIR}/clash
	cd src && ${CLASH} -outputdir ../${BUILDDIR}/clash --verilog ${TOPENTITY}.hs
	rm -Rf ${BUILDDIR}/verilog/${TOPENTITY}
	mv ${BUILDDIR}/clash/verilog ${BUILDDIR}/

src/Control.hs:
	$(MAKE) -C ..
	echo ""

##----------------------------------------------------------------------------##
#   Cleanup                                                                    #
##----------------------------------------------------------------------------##

clean:
	rm -Rf ${BUILDDIR}

##----------------------------------------------------------------------------##
#   Special Targets                                                            #
##----------------------------------------------------------------------------##

.PHONY: clean iceblink upload iceblink-upload
.SECONDARY:
.SILENT:
