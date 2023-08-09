A simple Mega CD Mode 1 ROM, originally written to test my experimental Mega CD Error Handler, a custom Moduled Kosinski implementation for compressing sub CPU code in ROM, and initialization code. However, it can be used as a base for other Mode 1 programs.

Boots to green screen if initialization is successful, red screen if no Mega CD is found, and to blue screen if the Mega CD BIOS cannot be identified.

Includes code from [Devon's Mode 1 Library](https://github.com/DevsArchive/mcd-mode-1-library), [VladikComper's Error Handler](https://github.com/vladikcomper/md-modules), and FlameWing's [MDComp](https://github.com/flamewing/mdcomp).
Custom KosM compressor by Clownacy based on his [ClownLZSS](https://github.com/Clownacy/clownlzss), with an additional modification by myself.
