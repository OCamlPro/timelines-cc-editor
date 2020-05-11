opam switch create . 4.07.1 --no-install
opam install . --deps-only --working-dir -y
eval $(opam env)
