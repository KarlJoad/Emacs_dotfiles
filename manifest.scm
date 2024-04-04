(use-modules (guix)
             (guix packages)
             (gnu packages)
             (gnu packages moreutils)
             (gnu packages bash)
             (gnu packages commencement)
             (gnu packages compression)
             (gnu packages autotools)
             (gnu packages base))

(packages->manifest
 (list autoconf automake
       coreutils moreutils
       bash
       gnu-make))
