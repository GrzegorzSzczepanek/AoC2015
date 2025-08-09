.text
.globl _main
.p2align 2
_main:
  stp x29, x30, [sp, #-16]!
  mov x29, sp
  sub sp, sp, #16
.section __TEXT,__cstring
L_fmt: .asciz "16\n"
.text
  mov x0, #10
  str x0, [x29, -8]
  mov x0, #3
  str x0, [x29, -16]
  ldr x0, [x29, -8]
  sub sp, sp, #16
  str x0, [sp]
  mov x0, #2
  ldr x1, [sp]
  mul x0, x1, x0
  add sp, sp, #16
  sub sp, sp, #16
  str x0, [sp]
  mov x0, #5
  ldr x1, [sp]
  add x0, x1, x0
  add sp, sp, #16
  str x0, [x29, -16]
  ldr x0, [x29, -16]
  add x0, x0, #1
  neg x0, x0
  mov x1, x0
  adrp x0, L_fmt@PAGE
  add  x0, x0, L_fmt@PAGEOFF
  bl _printf
  mov w0, #0
  ldp x29, x30, [sp], #16
  ret
