## Test environments

 * Windows Server 2022 x64 (build 20348): x86_64-w64-mingw32 R Under development (unstable) (2025-10-03 r88899 ucrt)
 * macOS Sequoia 15.6.1: aarch64-apple-darwin20 R 4.5.1
 * macOS Ventura 13.7.6: aarch64-apple-darwin20 R 4.4.1
 * macOS Ventura 13.7.6: aarch64-apple-darwin20 R 4.5.1
 * Windows Server 2022 x64 (build 26100): x86_64-w64-mingw32 R version 4.5.1 (2025-06-13 ucrt)
 * Ubuntu 24.04.3 LTS: x86_64-pc-linux-gnu R version 4.5.1 (2025-06-13)
 * Ubuntu 24.04.3 LTS: x86_64-pc-linux-gnu R version 4.4.3 (2025-02-28)
 * Ubuntu 24.04.3 LTS: x86_64-pc-linux-gnu R Under development (unstable) (2025-10-02 r88897)

 * rhub::rhub_check()
    * Fedora Linux 38 (Container Image)
        * using R Under development (unstable) (2025-10-02 r88897)
        * using platform: x86_64-pc-linux-gnu
    * Ubuntu 24.04.3 LTS
        * using R Under development (unstable) (2025-10-02 r88897)
        * using platform: x86_64-pc-linux-gnu
    * macOS Sequoia 15.6.1
        * using R Under development (unstable) (2025-10-03 r88898)
        * using platform: aarch64-apple-darwin20
    * Windows Server 2022 x64 (build 26100)
        * using R Under development (unstable) (2025-10-02 r88897 ucrt)
        * using platform: x86_64-w64-mingw32

* There were no ERRORs or WARNINGs or NOTES.

## Downstream dependencies

* revdepcheck:revdep_check() found no problems with the only downstream dependency: `dataone`.

## Notes

Earlier I missed some broken URLs in the checks, which are now fixed.
