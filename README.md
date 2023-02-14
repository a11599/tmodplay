# Therapy MOD player (tmodplay)

This the modern remake of a multichannel [MOD](https://en.wikipedia.org/wiki/MOD_(file_format)) player I wrote back in the mid-90's for retro PC platforms. Why? Because programming for limited hardware with direct control over it is a challenging, but rewarding fun. Also this old modplayer of mine was lost back in the day in a sad hard drive accident and burned a hole in my heart. So it was about time to rebuild it and make it even better than it ever was to heal my wounds finally.

It requires a 386 or above running MS-DOS or Windows 9x/ME in real DOS mode - or an emulator such as DOSBox. It is written in full assembly and includes a small runtime library that implements [flat real mode](https://en.wikipedia.org/wiki/Unreal_mode). The MOD player supports ProTracker format and its close derivates up to 32 channels on the following sound cards:

* No sound (keeps the player running without actually playing anything)
* PC speaker up to 44.1 kHz sample rate
* LPT DAC variants (single, dual, stereo) up to 100 kHz sample rate
* Sound Blaster, Pro and 16 up to 22/44.1 kHz sample rate (depending on actual model)

An extremely optimized software wavetable with 16-bit mixing and optional linear interpolation upsampling is included for non-wavetable sound cards (all of them for now, but at least GUS support is planned in the future). Stereo is supported with hard pan or 75% crossfeed. True stereo panning via 8xx and E8x MOD commands is also available.

According to 86Box, this thing should be able to play [dope.mod](https://modarchive.org/index.php?request=view_by_moduleid&query=35344) at 33 kHz in stereo through a Sound Blaster 16 with linear interpolation on a 486 DX2/66.

The main development platform is a modern PC with DOSBox, but the result is always tested (also some parts are developed) on actual retro hardware:

* VIA MVP4 / Pentium MMX 233@250 MHz / Sound Blaster 16 Vibra16S (CT2800) ISA / Hercules S3 Trio64V+ PCI / MS-DOS 7.0
* SIS 650GX/962LUA / Pentium 4 2.66 GHz / Avance Logic ALS-4000 PCI / nVidia Geforce 440MX 64 MB AGP / MS-DOS 7.0

## Build pre-requisites

The app can be built under DOS and Windows. The build uses the following toolchain:

* [NASM](https://www.nasm.us/) to compile assembly source code to linkable objects.
* [Open Watcom](http://www.openwatcom.org/) to make the project and link the executable binary.
* (optional) [DOSBox-X](https://dosbox-x.com/) to test the build on a modern PC. Normal [DOSBox](https://www.dosbox.com/) should work as well, but the configuration file may need some adjustments.

The build toolchain is also available for Linux, but I only build under DOS and Windows.

Download and install the dependencies, then:

* Make sure Open Watcom binaries are available in `PATH` and the `WATCOM` environment variable is correctly set. The installer should do this for you, but if you just copied files over from somewhere else, you have to do this yourself.
* Copy `makeinit.sam` to `makeinit` and set correct paths for `nasm.exe` (NASM command line compiler) and `wlink.exe` (Watcom linker). Normally you don't need to change the path to `wlink.exe` as long as its available in `PATH`.
* Copy `test_bat.sam` to `test.bat` and adjust `file` and `args` environment variables according to your testing requirements. `file` should point to a MOD file and `args` contains parameters to the player. Run `tmodplay /h` to display the available command line parameters.
* (optional) When using DOSBox-X for development on a modern PC, copy `emu\env_bat.sam` to `emu\env.bat` and adjust the path of `dosbox-x.exe` in the file to your actual install directory.

## Building the executable

In the project root directory, run `wmake` to create a debug build of tmodplay to `dist\tmodplay.exe`. This will create `.obj` files, `tmodplay.lnk` linker directives and `tmodplay.map` segment/symbol map in the `obj` directory.

The following `wmake` targets are also available:

* `wmake debug`: Same as `wmake`. Creates an incremental debug build (recompiles only changed files).
* `wmake release`: Creates a release version without debug logging. Always a full compile.
* `wmake clean`: Removes files created during the build process.

To speed up the development process, two simple batch files are also provided:

* `make.bat`: Invokes `wmake` to create an incremental debug build and executes `test.bat` to test the executable. Requires MS-DOS, it doesn't work in Windows DOS box due to flat real mode incompatibility.
* `makedb.bat`: Invokes `wmake` to create an incremental debug build and executes `test.bat` using DOSBox-X to test the executable.

To start DOSBox-X with the same environment as `makedb.bat`, run `emu\dosbox.bat`. You can then manually test the build by running `test.bat` or executing `dist\tmodplay.exe` with your custom parameters.

## Why 386 and why flat real mode?

This project originally targeted the 286 which was very challenging. However it was soon realized that:

* The 286 is not really suitable for 16-bit mixing, and it's just not fast enough for acceptable (32 kHz+) sample rates unless it's a higher clock rate (12 MHz+) model anyways.
* There are large MODs (especially multichannel) out there and the 640 KB limit of conventional memory quickly became a bottleneck.
* There are also a few MODs with samples larger, than 64 KB and supporting this with segment:offset arithmetic (especially for loops) was annoying and slow.

I wanted this to have better sound quality and more compatibility than contemporary players with similar CPU usage and my goals just didn't seem to work out with the 286. And if the 286 is not good enough, why bottleneck 386 and later with 16-bit instructions?

Why flat real mode? Simply because it's faster, and it was easier to start with than protected mode. The lack of Windows DOS box compatibility is completely irrelevant for me - and this project is for my own satisfaction anyways. :)