  .global posix_read
  .global posix_write
  .global posix_open
  .global posix_close
  .global posix_exit

  .text
posix_read:
  mov $0, %rax
  syscall
  ret

posix_write:
  mov $1, %rax
  syscall
  ret

posix_open:
  mov $2, %rax
  syscall
  ret

posix_close:
  mov $3, %rax
  syscall
  ret

posix_exit:
  mov $60, %rax
  syscall
  ret
