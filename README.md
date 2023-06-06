# Therapy MOD player (tmodplay)

This is the modern remake of a multichannel [MOD](https://en.wikipedia.org/wiki/MOD_(file_format)) player I wrote back in the mid-90's for retro PC platforms. Why? Because programming for limited hardware with direct control over it is a challenging, but rewarding fun. Also this old modplayer of mine was lost back in the day in a sad hard drive accident and burned a hole in my heart. So it was about time to rebuild it and make it even better than it ever was to heal my wounds finally.

It requires a 386 or above running MS-DOS or Windows 9x/ME, or an emulator such as DOSBox. It is written in full assembly using [PMI, my own 32-bit protected mode host](https://github.com/a11599/pmi). The MOD player supports ProTracker format and its close derivates up to 32 channels on the following sound cards:

- No sound (keeps the player running without actually playing anything)
- PC speaker up to 29 kHz sample rate
- LPT DAC variants (single, dual, stereo) up to 100 kHz sample rate
- Sound Blaster, Pro and 16 up to 22/44.1 kHz sample rate (depending on actual model)

An extremely optimized software wavetable with 16-bit mixing, optional linear and trilinear Watte interpolation upsampling is included for non-wavetable sound cards (all of them for now, but at least GUS support is planned in the future). Stereo is supported with hard pan or 75% crossfeed. True stereo panning via 8xx and E8x MOD commands is also available.

According to 86Box, this thing should be able to play [dope.mod](https://modarchive.org/index.php?request=view_by_moduleid&query=35344) at 33 kHz in stereo through a Sound Blaster 16 with linear interpolation on a 486 DX2/66.

The main development platform is a modern PC with DOSBox, but the result is always tested (also some parts are developed) on actual retro hardware:

- VIA MVP4 / Pentium MMX 233@250 MHz / Sound Blaster 16 Vibra16S (CT2800) ISA / Hercules S3 Trio64V+ PCI / MS-DOS 7.0
- SIS 650GX/962LUA / Pentium 4 2.66 GHz / Avance Logic ALS-4000 PCI / nVidia Geforce 440MX 64 MB AGP / MS-DOS 7.0

## Building from source

### Pre-requisites

The app can be built under DOS and Windows. The build uses the following toolchain:

- [NASM](https://www.nasm.us/) to compile assembly source code to linkable objects.
- [Open Watcom](http://www.openwatcom.org/) to make the project and link the executable binary.
- (optional) [DOSBox-X](https://dosbox-x.com/) to test the build on a modern PC.

The build toolchain is also available for Linux, but I only build under DOS and Windows.

Download and install the dependencies, then:

- Make sure Open Watcom binaries are available in `PATH` and the `WATCOM` environment variable is correctly set. The installer should do this for you, but if you just copied files over from somewhere else, you have to do this yourself.
- Copy `makeinit.sam` to `makeinit` and set correct paths for `nasm.exe` (NASM command line compiler) and `wlink.exe` (Watcom linker). Normally you don't need to change the path to `wlink.exe` as long as its available in `PATH`.
- Copy `test_bat.sam` to `test.bat` and adjust `file` and `args` environment variables according to your testing requirements. `file` should point to a MOD file and `args` contains parameters to the player. Run `tmodplay /h` to display the available command line parameters.
- (optional) When using DOSBox-X for development on a modern PC, copy `emu\env_bat.sam` to `emu\env.bat` and adjust the path of `dosbox-x.exe` in the file to your actual install directory.
- Download [PMI](https://github.com/a11599/pmi) and extract it into the same parent as of tmodplay. The folder structure should look like this:

```
  |
  +-- pmi
  |   |
  |   +-- rtl
  |   +-- api
  |       ...
  |
  +-- tmodplay
      |
      +-- src
      +-- emu
          ...
```

### Building the executable

In the project root directory, run `wmake` to create a debug build of tmodplay to `bin\tmod_dbg.exe`. This will create `.obj` files, `tmod_dbg.lnk` linker directives and `tmod_dbg.map` segment/symbol map in the `obj` directory.

The following `wmake` targets are also available:

- `wmake debug`: Same as `wmake`. Creates an incremental debug build to `bin\tmod_dbg.exe` (recompiles only changed files).
- `wmake release`: Creates a release version to `bin\tmodplay.exe` without debug logging. Always a full compile.
- `wmake clean`: Removes files created during the build process.

To speed up the development process, two simple batch files are also provided:

- `make.bat`: Invokes `wmake` to create an incremental debug build and executes `test.bat` to test the executable. Requires MS-DOS, or Windows 9x/ME DOS box.
- `makedb.bat`: Invokes `wmake` to create an incremental debug build and executes `test.bat` using DOSBox-X to test the executable.

To start DOSBox-X with the same environment as `makedb.bat`, run `emu\dosbox.bat`. You can then manually test the build by running `test.bat` or executing `bin\tmodplay.exe` with your custom parameters.

## Why 386 and why protected mode?

This project originally targeted the 286 which was very challenging. However it was soon realized that:

- The 286 is not really suitable for 16-bit mixing, and it's just not fast enough for acceptable (32 kHz+) sample rates unless it's a higher clock rate (12 MHz+) model anyways.
- There are large MODs (especially those made on PCs and later expanded Amigas) out there and the 640 KB limit of conventional memory quickly became a bottleneck.
- There are also a few MODs with samples larger, than 64 KB and supporting this with segment:offset arithmetic (especially for loops) was annoying and slow.

I wanted this player to have better sound quality and more compatibility than contemporary players with similar CPU usage and my goals just didn't seem to work out with the 286. And if the 286 is not good enough, why bottleneck 386 and later with 16-bit instructions?

Why protected mode? For some time, tmodplay used [flat real mode](https://en.wikipedia.org/wiki/Unreal_mode) and it was working fine, but I eventually ran into several issues:

- The audio mixer code, which uses most of the CPU had a lot of 32-bit registers and addressing, thus generating operand and/or address size prefixes for almost every instruction. This consumed a significant amount of CPU cycles: after converting to protected mode, CPU usage was almost halved with Watte interpolation on a Pentium CPU.
- It was not compatible with EMM386. Although it could be made to work with it, it would have lost the simplicity and appeal of flat real mode.
- It would have never worked under Windows. While it was not a big concern and it was never a goal of this project, being compatible with Windows 9x DOS box is a nice thing to have.