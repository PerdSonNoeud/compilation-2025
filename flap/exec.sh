RUNTIME="src/runtime.c"

for f in tests/06-Retrolix_to_x86-64/*.retrolix; do
  nom=$(basename "${f%.retrolix}")
  echo -n "Test $nom: "

  s_file="${f%.retrolix}.s"
  elf="${f%.retrolix}.elf"
  expected="${f%.retrolix}.expected"

  dune exec -- flap -t x86-64 "$f" > "$s_file" 2>/dev/null

  if [ -f "$s_file" ]; then
    gcc -no-pie -o "$elf" "$s_file" "$RUNTIME" 2>/dev/null
  fi

  if [ -f "$elf" ] && [ -f "$expected" ]; then
    sortie=$("$elf" 2>/dev/null)

    attendu=$(sed -n '/^STDOUT:$/,/^STDERR:$/p' "$expected" | sed '1d;$d')

    if [ "$sortie" = "$attendu" ]; then
      echo "✓ PASS"
    else
      echo "✗ FAIL"
      echo "  Attendu: '$(echo "$attendu" | cat -A)'"
      echo "  Obtenu:   '$(echo "$sortie" | cat -A)'"
    fi
  else
    echo "⚠ SKIP (Erreur de compilation ou fichier manquant)"
  fi
done
