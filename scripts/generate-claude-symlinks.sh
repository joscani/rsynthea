#!/usr/bin/env bash
# Crea symlink CLAUDE.md -> AGENTS.md en cada directorio que tenga un AGENTS.md.
# Útil si el proyecto tiene AGENTS.md en subcarpetas con reglas específicas (ej. src/, tests/).
#
# Uso (desde la raíz del proyecto):
#   bash scripts/generate-claude-symlinks.sh

set -euo pipefail

ROOT="$(pwd)"

echo "Buscando ficheros AGENTS.md bajo $ROOT ..."

count=0
while IFS= read -r -d '' agents_file; do
    dir="$(dirname "$agents_file")"
    claude_file="$dir/CLAUDE.md"

    if [ -L "$claude_file" ]; then
        # Ya es un symlink; re-crear por si apunta a otro sitio
        rm "$claude_file"
    elif [ -e "$claude_file" ]; then
        echo "  AVISO: $claude_file existe y NO es un symlink, se omite"
        continue
    fi

    ln -s "AGENTS.md" "$claude_file"
    echo "  [+] $claude_file -> AGENTS.md"
    count=$((count + 1))
done < <(find "$ROOT" -name "AGENTS.md" -not -path "*/node_modules/*" -not -path "*/.venv/*" -not -path "*/renv/library/*" -print0)

echo "Listo. Symlinks creados: $count"
