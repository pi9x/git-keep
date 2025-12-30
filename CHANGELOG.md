# Revision history for git-keep

## 0.1.0.1 -- 2025-12-30

* Maintenance release: small improvements and clarifications
  - Ensures `.gitkeep` files are not created inside the `.git` directory.
  - Respects Git ignore rules by using `git check-ignore` to skip ignored directories.
  - Recursive mode (`-r`) creates `.gitkeep` files in subdirectories.
  - CLI helpers: `-h` shows usage, `-v` prints the version.
  - Minor code cleanup and documentation updates (README).
