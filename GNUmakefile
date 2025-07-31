#
# Copyright 2024 Oxide Computer Company
#

NM =		nm
NM_OPTS =	-pv -t x
OUTPUT_DIR =	obj

# --- END BUILD CONFIGURATION ---

#
# We need to use the GNU link-editor.  On illumos, it's usually `gld`, while
# on GNU/Linux it's usually ld (the default).  This can be extended to find
# a suitable link-editor on other hosts as well if needed.
#
ifneq (, $(findstring solaris, $(MAKE_HOST)))
ENV_FOR_CARGO +=	CARGO_BUILD_RUSTFLAGS="-C linker=gld"
NM_OPTS +=		-h
else
ifneq (, $(findstring illumos, $(MAKE_HOST)))
ENV_FOR_CARGO +=	CARGO_BUILD_RUSTFLAGS="-C linker=gld"
NM_OPTS +=		-h
endif
endif

CARGO_BUILD_FLAGS =	\
	-Z unstable-options	\
	--artifact-dir $(OUTPUT_DIR)

ELF = $(OUTPUT_DIR)/nanobl-rs.elf
SYMMAP = $(ELF:%.elf=%.map)

.PHONY: all
all: $(ELF) $(SYMMAP)

.PHONY: clean
clean:
	rm -f -- $(ELF) $(SYMMAP)
	rm -rf -- $(OUTPUT_DIR)

.PHONY: clobber
clobber: clean
	$(ENV_FOR_CARGO) cargo clean $(FLAGS_FOR_CARGO)

_always:

$(ELF): _always
	$(ENV_FOR_CARGO) cargo build $(CARGO_BUILD_FLAGS) $(FLAGS_FOR_CARGO)

$(SYMMAP): $(ELF)
	$(NM) $(NM_OPTS) $< > $@
