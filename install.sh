#!/usr/bin/env bash
set -euo pipefail

INSTALL_DIR="$HOME/.local/bin"
PATH_LINE='export PATH="$HOME/.local/bin:$PATH"'

detect_shell_rc() {
  if [[ -n "${ZSH_VERSION:-}" ]]; then
    echo "$HOME/.zshrc"
  elif [[ -n "${BASH_VERSION:-}" ]]; then
    echo "$HOME/.bashrc"
  elif [[ -f "$HOME/.zshrc" ]]; then
    echo "$HOME/.zshrc"
  else
    echo "$HOME/.bashrc"
  fi
}

SHELL_RC="$(detect_shell_rc)"

echo "==> Building project with Stack"
mkdir -p "$INSTALL_DIR"
stack build --copy-bins --local-bin-path "$INSTALL_DIR"

if [[ -f "$SHELL_RC" ]]; then
  if ! grep -Fq "$PATH_LINE" "$SHELL_RC"; then
    {
      echo ""
      echo "# Added by Stack install script"
      echo "$PATH_LINE"
    } >>"$SHELL_RC"
    echo "==> Added ~/.local/bin to PATH in $SHELL_RC"
  else
    echo "==> PATH already configured in $SHELL_RC"
  fi
else
  {
    echo "# Added by Stack install script"
    echo "$PATH_LINE"
  } >"$SHELL_RC"
  echo "==> Created $SHELL_RC and added ~/.local/bin to PATH"
fi

echo
echo "Installed binaries should now be in: $INSTALL_DIR"
echo "Reload your shell with:"
echo "  source \"$SHELL_RC\""
echo
echo "Then verify by running:"
echo "  tmc"
