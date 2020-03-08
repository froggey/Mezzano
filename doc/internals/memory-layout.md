Virtual memory layout for the x86-64 target
========

0000000000000000 - 0000000000400000  Unmapped
0000000000400000 - 0000000080000000  Wired area
0000000080000000 - 0000004000000000  Unused
0000004000000000 - 0000006000000000  Card table (wired)
0000006000000000 - 0000007F80000000  Unused
0000007F80000000 - 0000008000000000  Wired function area
0000008000000000 - 0000008080000000  Function area
0000008080000000 - 0000010000000000  Unused
0000010000000000 - 0000100000000000  Pinned area
0000100000000000 - 0000120000000000  GC mark bits (one per 2 words)
0000120000000000 - 0000200000000000  Unused
0000200000000000 - 0000200080000000  Wired stack area
0000200080000000 - 0000204000000000  Unused (future expansion for wired stack area)
0000204000000000 - 0000208000000000  DMA buffer mapping area
0000208000000000 - 0000400000000000  Stack area
0000400000000000 - 0000480000000000  General young generation (semispace A)
0000480000000000 - 0000500000000000  General old generation (semispace A)
0000500000000000 - 0000580000000000  General young generation (semispace B)
0000580000000000 - 0000600000000000  General old generation (semispace B)
0000600000000000 - 0000680000000000  Cons young generation (semispace A)
0000680000000000 - 0000700000000000  Cons old generation (semispace A)
0000700000000000 - 0000680000000000  Cons young generation (semispace B)
0000780000000000 - 0000800000000000  Cons old generation (semispace B)
0000800000000000 - FFFF800000000000  x86-64 non-canonical region
FFFF800000000000 - FFFF808000000000  Linear map of physical memory
FFFF808000000000 - FFFF808100000000  Page frame information vector
FFFF808100000000 -10000000000000000  Unused

Everything below 0000008000000000 (512G) is assumed to be wired.

The function area
--------

The wired and normal function areas both begin at 0000008000000000 and are
expanded downwards for the wired area and upward for the normal area.
The total combined size of the function areas cannot exceed 2GB.
This allows wired & normal functions to reach each other through normal
direct calls, which have a maximum range of +/-2GB.
ARM64 branches have a limit of +/-128MB, so the total size of the function
area is a little more limited, but this still seems to be enough.

The base address is chosen so it stradles the wired & pinned zones.
This is not a specific requirement, but was done to minimize disruption
to code which assumes the location of wired memory.

The entire function area will eventually be subject to the full write barrier.
Supervisor code must not write to it.
NX will eventually be applied to all memory outside the function area, to
assist with catching wild jumps.

Mark bits
----------
Every two words of memory has a mark bit assigned to it so that objects
do not need to have a mark bit in their header.
