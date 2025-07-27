# Skewed Emacs: Containerized MCP Setup for Emacs Lisp and Common Lisp

plus a ready-to-use emacs user configuration. 

![Skewed Emacs Logo](img/skewed-1-t.png)

This README assumes that you have access to a system with a [bash](https://www.gnu.org/software/bash/) shell
and [docker](https://www.docker.com/get-started) available. 

Skewed Emacs provides several capabilities from several angles. It
aims to be several things to several people and do all of them well.

## What Can This Do for Me

This Skewed Emacs repository houses essentially three things:

1.  a ready-to-go Emacs configuration
   
2.  a containerized emacs daemon version 

3.  a container orchestration to bring in supplemental compatible
    backend container services.


## Features

- **A Sampling of Included Emacs Packages:**
  - [Slime](https://en.wikipedia.org/wiki/SLIME) for Common Lisp / Swank
  - Paredit-mode, Flycheck-mode, Company-mode
  - Magit, Org-mode
  - Doom Color Themes, theme switching functions

- **Lisply-MCP (Model Context Protocol) Backend:**
  - allows AI agents to drive your contained emacs.
  - Defined in this repository and sideloaded from
    `dot-files/emacs.d/sideloaded/lisply-backend/`

- **Docker Integration:**
  - Local container image defined in `docker/Dockerfile` and `docker/build`.
    Images are pushed to tagged `gornskew/skewed-emacs` versions at Dockerhub.
  - Docker Compose orchestration for running configured skewed-emacs with
    other helpful containers such as lisply-mcp for the Lisply-MCP compatible middleware.


## Customizations for Windows

The Skewed Emacs repository also contains files to help setting up a
Windows machine for Emacs (with or without WSL)

  - For using Emacs with modern keyboards, it is recommended to [map
    CapsLock to Control](windows-keybindings/README.md).
  - AutoHotkey (.ahk) configuration for Emacs-style keybindings across
    Microsoft Edge, Chroms, Claude, and other applications as per your
    specification. Just double click the file in File Explorer.


## Containerized Runnings (recommended)

This is the easiest way to get everything started.

The steps below assume that you have access to a system with a
[bash](https://www.gnu.org/software/bash/) shell and [docker](https://www.docker.com/get-started) available.

1. Make a `~/projects/` directory if you don't already have one:

```bash

    cd
    mkdir -p projects/
    cd projects/
    
```

2. Clone this repo into `~/projects/`:

```bash

   git clone https://github.com/gornskew/skewed-emacs 
   cd skewed-emacs

```

3. Start the default container orchestra:

```
   ./compose-dev up
   
```

This will pull and starts four containers:
- **skewed-emacs**: Emacs with Benefits 
- **gendl-ccl**: Gendl system on Clozure CL
- **gendl-sbcl**: Gendl system on Steel Bank CL
- **lisply-mcp**: Node.js environment MCP & Claude


Now you can use the `eskew` or `egskew` aliases to launch a terminal-
or graphical-based emacs client. These aliases are defined in the
`dot-files/bash_profile`, so if you did not run the `~/.setup` on your
host machine, you can quickly define thes aliases for yourself with

```
  cd ~/projects/skewed-emacs/
  source dot-files/bash_profile
  
```

Then can do `eskew` or `egskew` to get a client attached to the
containerized skewed emacs daemon.

After you are in, see the "Getting Started" section near the top of
the default landing dashboard.


## Local Installation

This section is for setting up the .emacs.d and other so-called "dot
files" in your home directory. It does not download or install or run
any of the systems discussed in the Containerized Runnings section
above.

1. Make a `~/projects/` directory if you don't already have one:

```bash

    cd
    mkdir -p projects/
    cd projects/
    
```

2. Clone this repo into `~/projects/`:

```bash

   git clone https://github.com/gornskew/skewed-emacs 
   cd skewed-emacs

```


3. Run the setup script:
   ```bash
   
   cd ~/projects/skewed-emacs
   ./setup
   
   ```
   
   The setup script will create symbolic links of the salient
   "dot-files" (hidden files starting with `.` pointing to the
   corresponding files in the cloned repo, for example:
   
    `~/.emacs.d -> ~/skewed-emacs/dot-files/emacs.d`
   
   If you already have any of these dot files existing (as links or
   actual files/directories), the existing files will be backed up
   with names appended with `-pre-skewed-emacs`.


### Optional options for `setup`

- `--dry-run`: Shows what would happen without making any changes
- `--shadow-suffix=NAME` or `--shadow-suffix NAME`: Creates symlinks with a "-NAME" suffix
     (e.g., with `--shadow-suffix=test` or `--shadow-suffix test` creates ~/.emacs.d-test instead of ~/.emacs.d)
- `--scrub-shadow-suffix=NAME` or `--scrub-shadow-suffix NAME`: Removes all symlinks with the "-NAME" suffix
     (e.g., `--scrub-shadow-suffix=test` removes ~/.emacs.d-test, ~/.bash_profile-test, etc.)
- `--scrub-shadow-suffix=""` or `--scrub-shadow-suffix=`: Removes all default symlinks without a suffix (e.g., removes ~/.emacs.d, ~/.bash_profile, etc.)

The setup script will automatically detect and replace broken symlinks
and handle existing dotfiles by backing them up with a
`-pre-skewed-emacs` suffix. It also skips backup files ending with
tilde (~) in the dot-files directory.

##   Example with options:

```bash
   # Preview changes without modifying anything
   ./setup --dry-run
   
   # Install configuration files with regular names
   ./setup
   
   # Install configuration files with "-shadow" suffix
   # (useful for testing or for maintaining multiple configurations)
   ./setup --shadow-suffix=shadow
   
   # Install with a custom suffix
   ./setup --shadow-suffix=work
   
   # Preview shadow installation without making changes
   ./setup --dry-run --shadow-suffix=shadow
   
   # Preview custom suffix installation without making changes
   ./setup --dry-run --shadow-suffix=test
   
   # Remove all symlinks with the "-test" suffix
   ./setup --scrub-shadow-suffix=test
   
   # Preview removal of all symlinks with the "-shadow" suffix without making changes
   ./setup --dry-run --scrub-shadow-suffix=shadow
   
   # Remove all symlinks with the "-test" suffix and create new ones with "-work" suffix
   ./setup --scrub-shadow-suffix=test --shadow-suffix=work
   
   # Preview removing all symlinks with the "-test" suffix and creating
   
```


⚠️ **Warning**: The setup script may overwrite your existing
               `~/.emacs.d/` and `~/.bash_profile`. It is designed to
               back up this data, but in case of defects or failure it
               would be wise to back up your existing dot files before
               running the `./setup` script.
	       

## Requirements

 - Bash
 - Docker 
 - Git


## Configuration Structure

 - `dot-files/` - all dotfiles that will end up symlinked to
   your home directory
  - `emacs.d/` - Emacs configuration, to be linked to ~/.emacs.d/
    - `init.el` - Main Emacs configuration entry point
    - `etc/` - Modular configuration files
    - `sideloaded/` - Second-party packages
  - `bash_profile` - Bash configuration
  - `tmux.conf` - tmux configuration
  - `zshrc` - ZSH configuration


## Customization

For personal customizations that shouldn't be committed to this
repository, add them to a `~/.emacs-local` file, which will be loaded
at the end of the Emacs initialization process.




## License

This package is licensed under the GNU Affero General Public License
v3.0 (AGPL-3.0) which presumably is compatible with Gnu Emacs's GPL.

## MCP Server Registries

- [MCPHub](https://mcphub.com/mcp-servers/gornskew/skewed-emacs)
