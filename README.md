# Skewed Emacs: Containerized MCP Setup for Emacs Lisp and Common Lisp

Skewed Emacs is a containerized Emacs development environment with
Model Context Protocol (MCP) integration, enabling AI agents to
interact directly with Emacs and other Lisp REPLs for automated
development workflows.

![Skewed Emacs Logo](img/skewed-colorful.png)

This README assumes that you have access to a system with a
[bash](https://www.gnu.org/software/bash/) shell and
[docker](https://www.docker.com/get-started) available.

## What Can This Do for Me

This Skewed Emacs repository houses essentially three things:

1.  a ready-to-go Emacs configuration
   
2.  a containerized emacs server with pre-configured skewed emacs user 

3.  a container orchestration to bring in supplemental compatible
    backend container services for e.g. Model Context Protocol and
    Knowledge Based Engineering.


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



## Windows Keyboard Tweaks for Emacs

Skewed-emacs uses the traditional Emacs keybindings by default, which
make heavy use of the Control key ("C-" in emacs parlance). For this
reason, it can be convenient to bind a more ergonomic key such as
CapsLock to Control, on modern keyboards. (Older keyboards had Control
in the place of current CapsLock). The Skewed Emacs repository
contains [instructions](windows-keybindings/README.md) for mapping
CapsLock to Control (with or without WSL) using a free program called
SharpKeys.

If you enjoy the traditional emacs keychords and want more of them in
your life, you can replicate those across most Windows programs using
the free AutoHotkey program, for which we bundle a config, also
described in the [instructions](windows-keybindings/README.md).


## Containerized Runnings (recommended)

This is the easiest way to get everything started.

The steps below assume that you have access to a system with a
[bash](https://www.gnu.org/software/bash/) shell and [docker](https://www.docker.com/get-started) available.

### Initial Setup

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

By default this pulls missing images only (no overwrites of local builds).
To force pulling the latest images, use:

```
   ./compose-dev up --pull
```

Services are defined in `docker-compose.yml` and any other `.yml` files
in this directory (generated from `services.sexp`).


Now you can use the `eskew` or `egskew` aliases to launch a terminal-
or graphical-based emacs client. These aliases are defined in the
`dot-files/bash_profile`, so if you did not run the `~/.setup` on your
host machine, you can quickly define thes aliases for yourself with

```
  cd ~/projects/skewed-emacs/
  source dot-files/bash_profile
  
```

You can then do `eskew` or `egskew` to get a client attached to the
containerized skewed emacs daemon.

After you are in, see the "Getting Started" section near the top of
the default landing dashboard.

### Claude Desktop Integration

If you prefer to interact via Claude Desktop rather than directly with
Emacs, see [Claude Desktop Integration](docs/CLAUDE_DESKTOP.md) for
setup instructions. The MCP configuration is automatically generated
when you run `./compose-dev up`.

### Pulling Updates

`./compose-dev up` pulls missing images only by default. If you want
fresh images, use `./compose-dev up --pull` (or set `PULL_ALWAYS=1`).
It will not automatically pull git updates to your local cloned
skewed-emacs repository. To do that:

```
cd ~/projects/skewed-emacs
./compose-dev down
git pull
./compose-dev up --pull
```

As you can see, we bring down the docker composition before doing the
git pull, just in case there is a change in docker compose
configuration that might affect a shutdown.

### Custom Projects Directory

`./compose-dev` generates `.env` via `./generate-env.sh`. Do not edit
`.env` directly. To use a non-default projects directory, set
`PROJECTS_DIR` before running:

```
PROJECTS_DIR=/path/to/projects ./compose-dev up
```

## Terminal Icons Setup 

Skewed Emacs includes a flexible icon system for the dashboard and
org-mode agenda. By default we use colorful Unicode icons. If these do
not work in your terminal, or you'd like a more muted experience, we
recommend installing a **Nerd Font** in your terminal.

### Why Nerd Fonts?

With a Nerd Font installed, you get flat professional looking icons
rather than loud colorful gaudy ones.

### Quick Setup

1. **Download a Nerd Font** from [nerdfonts.com](https://www.nerdfonts.com/font-downloads)
   - Popular choices: **Hack**, **FiraCode**, **JetBrainsMono**, **Meslo**
   - Download the "Nerd Font" version (not the regular font)

2. **Install the font** on your system:
   - **Windows**: Right-click the `.ttf` files → "Install"
   - **macOS**: Double-click the `.ttf` files → "Install Font"
   - **Linux**: Copy to `~/.local/share/fonts/` and run `fc-cache -fv`

3. **Configure your terminal** to use the Nerd Font:
   - **Windows Terminal**: Settings → Profiles → Defaults → Appearance → Font face
   - **iTerm2**: Preferences → Profiles → Text → Font
   - **GNOME Terminal**: Preferences → Profile → Custom font
   - **Alacritty**: Edit `font.normal.family` in config

4. **Enable nerd icons in Skewed Emacs** by adding to your config or running:
   ```elisp
   (setq skewed-icons-style 'nerd)
   ```
   Or interactively: `M-x skewed-icons-set-style RET nerd RET`

### Available Icon Styles

| Style | Description | When to Use |
|-------|-------------|-------------|
| `ascii` | Pure ASCII characters | Dumb terminals, serial consoles |
| `unicode` | Safe geometric symbols | Default, works everywhere |
| `unicode-fancy` | Colorful Unicode + VS15 | Experimental, terminal support varies |
| `nerd` | Nerd Font icons | **Recommended** with Nerd Font installed |


### Troubleshooting Icons

- **Question marks in diamonds (�)**: Nerd Font not installed or not selected in terminal
- **Misaligned columns**: Switch from `unicode-fancy` to `unicode` or `nerd`
- **Icons look plain**: Install a Nerd Font and set `skewed-icons-style` to `'nerd`



### Troubleshooting

- *Dangling Containers*

If all containers do not shut down cleanly for some reason, you can
list them with

```
docker ps 
```

then forcibly remove one with

```
docker rm -f <container-name>
```

- *Dangling Network*

Sometimes a docker network "skewed-network" or "skewed-emacs-network"
can be left dangling, preventing a clean `./compose-dev up`. Such
cases can be cleaned up with e.g.

```
cd ~/projects/skewed-emacs/
./compose-dev down 
docker network rm skewed-emacs # if for some reason necessary
./compose-dev up 
```


## Local Installation

This section is for setting up the `.emacs.d` and other so-called "dot
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
