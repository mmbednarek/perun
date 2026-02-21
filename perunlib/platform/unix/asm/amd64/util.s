  .global offset_ptr
  .type offset_ptr, @function

  .text

# fn offset_ptr(in_ptr: rawptr, offset: i64) : rawptr
# rdi - in_ptr
# rsi - offset
offset_ptr:
    mov %rdi, %rax
    add %rsi, %rax
    ret
