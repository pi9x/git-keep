# git-keep

A small git extension to create `.gitkeep` files in project folders.

`git-keep` is a tiny command-line tool (written in Haskell) that helps you ensure empty directories are tracked by Git by creating `.gitkeep` files. It skips non-directories and directories ignored by Git (and always ignores the `.git` directory).

---

## Features

- Create a `.gitkeep` file in the current directory.
- Optionally create `.gitkeep` files recursively in all subdirectories.
- Respects `.gitignore` (won't add `.gitkeep` in ignored paths).
- Simple, zero-configuration, single executable that can run as a Git subcommand (`git keep`).

---

## Usage

Run `git keep` with no arguments to create a `.gitkeep` file in the current directory (if the directory exists and is not ignored by Git).

    git keep

Options:

- `-r` — recursive: create `.gitkeep` files in all subdirectories
- `-h` — show a short usage message
- `-v` — print the version

Examples:

    # Create .gitkeep in the current directory only
    git keep

    # Create .gitkeep in current directory and all subdirectories
    git keep -r

    # Show help
    git keep -h

    # Print version
    git keep -v

Note: If the executable is named `git-keep` and is on your `PATH`, you may call it as `git keep` (Git automatically runs `git-<name>` when you use `git <name>`).

---

## Behavior details

- The tool canonicalizes paths before operating.
- If a path is not a directory, nothing is done.
- The tool uses `git check-ignore` to determine whether a directory is ignored; if a directory is ignored, `git-keep` will not create a `.gitkeep` inside it.
- The `.git` directory is always skipped.

---

## Troubleshooting

- If you see no `.gitkeep` files created:
  - Make sure you ran the command from a directory that exists.
  - If you expected files in subdirectories, ensure you used `-r`.
  - If a directory is listed in `.gitignore`, `git-keep` will not touch it.

- Ensure `git` is installed and available on your `PATH` (the tool invokes `git check-ignore`).

---

## Contributing

Contributions, bug reports, and suggestions are welcome. Open an issue or submit a pull request to the repository.
