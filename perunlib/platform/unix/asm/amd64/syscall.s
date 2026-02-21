  .global posix_write
  .global posix_exit

  .text
posix_write:
  mov $1, %rax
  syscall
  ret

posix_exit:
  mov $60, %rax
  syscall
  ret
