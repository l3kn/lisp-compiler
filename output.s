  .intel_syntax noprefix
  .text
  .globl scheme_body
  .type scheme_body, @function
scheme_body:
  # sys-write
  mov [rbp], QWORD PTR 13
  mov [rbp + 8], QWORD PTR 104
  mov [rbp + 9], QWORD PTR 101
  mov [rbp + 10], QWORD PTR 108
  mov [rbp + 11], QWORD PTR 108
  mov [rbp + 12], QWORD PTR 111
  mov [rbp + 13], QWORD PTR 32
  mov [rbp + 14], QWORD PTR 119
  mov [rbp + 15], QWORD PTR 111
  mov [rbp + 16], QWORD PTR 114
  mov [rbp + 17], QWORD PTR 108
  mov [rbp + 18], QWORD PTR 100
  mov [rbp + 19], QWORD PTR 10
  mov [rbp + 20], QWORD PTR 13
  mov rax, rbp
  add rbp, 8
  add rbp, 16
  mov rdi, QWORD PTR 1
  mov rdx, [rax]
  add rax, 8
  mov rsi, rax
  mov rax, QWORD PTR 1
  syscall
  # end sys-write
  ret
  .text
  .globl scheme_entry
  .type scheme_entry, @function
scheme_entry:
  # store register contents in ctxt
  mov rcx, rdi
  mov [rcx + 8], rbx
  mov [rcx + 32], rsi
  mov [rcx + 40], rdi
  mov [rcx + 48], rbp
  mov [rcx + 56], rsp
  # load stack_base and heap addresses
  mov rsp, rsi
  mov rbp, rdx
  call scheme_body
  # store register contents in ctxt
  mov rbx, [rcx + 8]
  mov rsi, [rcx + 32]
  mov rdi, [rcx + 40]
  mov rbp, [rcx + 48]
  mov rsp, [rcx + 56]
  ret
