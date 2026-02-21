  .global _start

  .text

 # perun_init_library(argc: i32, argv: u8**)
 # argc - %rdi
 # argv - %rsi

_start:
  .cfi_startproc
  .cfi_undefined %rip

  # Clear the frame pointer
  xorl %ebp, %ebp

  popq %rdi
  mov %rsp, %rsi

  and $~15, %rsp
  pushq %rax
  pushq %rsp

  call perun_init_library

  hlt
  .cfi_endproc
