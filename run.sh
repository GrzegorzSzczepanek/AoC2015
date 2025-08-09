# Build the C compiler (no GNU extensions needed)
clang -std=c11 -O2 -Wall -Wextra -o blinkc blinkc.c

# Example program
cat > example.blink <<'EOF'
let x := 10
mut y := 3
y = (x * 2) + 5
println(y |> inc |> neg)
EOF

# ARM64 macOS
./blinkc --target=arm64-macos example.blink -o out.s
clang -arch arm64 out.s -o a.out
./a.out

# x86_64 Linux
# ./blinkc --target=x86_64-linux example.blink -o out.s
# gcc out.s -o a.out
# ./a.out

