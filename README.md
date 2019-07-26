# ghc-utils
There are utilities that I find useful while working on GHC. It's quite a mixed
bag; a few are for performance analysis, some for general development, and
others for release management. Some are useful; all are hacky.

# Usage

You can build the scripts using `nix-build .` and enter into a shell where
the scripts are available using `nix-shell`.

## Day-to-day maintenance

 * `land`: A script which I use to merge and clean up Phabricator differential
   revisions (e.g. `ghc-utils/land D1234`)
 * `push`: A script which I use to look over and push upstream my local branches
 * `review-submodules`: A script for reviewing and optionally reverting
   submodule changes in a commit
 * `add-upstream-remotes.py`: A script which adds git remotes (named `upstream`)
   to all submodules pointing to the upstream repository.

## Development tools

 * `make-ghc-tags.sh`: A script for generating emacs `TAGS` files for a GHC tree
   (e.g. `ghc-utils/make-ghc-tags.sh`)
 * `debug-ghc`: A script for running an in-place installation of GHC (e.g.
   `inplace/bin/ghc-stage2`) in `gdb`
 * `validate-all`: A script for validating a range of GHC commits.

## Understanding performance

 * `split-core2core.py`: An extremely useful script for splitting up the output
   of GHC's `-dverbose-core2core` dump option, placing the output of each pass
   in a separate file (e.g. `ghc-utils/split-core2core.py
   my-program.verbose-core2core`)
 * `compare-ticks/`: A hack for comparing reports from GHC's ticky-ticky
   profiler.
 * `compare-stats.py`: Utility for comparing output from GHC's `+RTS -t
   --machine-readable` output.


## Release engineering

 * `rel-eng`:
   * `source-release.sh`: The script that I use to build source distribution
     tarballs.
   * `bin-release.sh`: The script that I use to build binary distributions.
   * `upload.sh`: The script that I use to collect, hash, sign, and upload
     documentation and distribution tarballs.
   * `mkchroot-debian.sh`: A script for preparing clean Debian container
     environments for GHC building with `systemd-nspawn`.
   * `download`
     * `process.py`: The script that I use to generate GHC download pages
   * `update-autoconf.sh`: Update the autoconf sources in the GHC source tree
 * `library-versions`: A set of scripts used to generate the
   `Commentary/Libraries/VersionHistory` page on the GHC Wiki. See
   `library-versions/README.mkd`.
