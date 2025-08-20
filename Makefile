base_dir=$(abspath ./)

CHISEL_VERSION=3.6.0
MODEL ?= TestHarness
PROJECT ?= freechips.rocketchip.system
CFG_PROJECT ?= $(PROJECT)
CONFIG ?= $(CFG_PROJECT).DefaultConfig
MILL ?= mill

verilog:
	cd $(base_dir) && $(MILL) emulator[freechips.rocketchip.system.TestHarness,$(CONFIG)].mfccompiler.compile

TestSuperscalar:
	cd $(base_dir) && $(MILL) unit[freechips.rocketchip.rocket.TestSuperscalar,freechips.rocketchip.system.DefaultConfig].mfccompiler.compile

clean:
	rm -rf out/
