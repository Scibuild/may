.data
.balign 8
.static.1:
	.quad .static.2+0
	.int 0
	.int 1
/* end data */

.data
.balign 8
.static.2:
	.ascii "\n"
/* end data */

.data
.balign 8
.static.3:
	.quad .static.4+0
	.int 0
	.int 1
/* end data */

.data
.balign 8
.static.4:
	.ascii "\n"
/* end data */

.data
.balign 8
.static.5:
	.quad .static.6+0
	.int 0
	.int 3
/* end data */

.data
.balign 8
.static.6:
	.ascii " = "
/* end data */

.data
.balign 8
.static.7:
	.quad .static.8+0
	.int 0
	.int 4
/* end data */

.data
.balign 8
.static.8:
	.ascii "true"
/* end data */

.data
.balign 8
.static.9:
	.quad .static.10+0
	.int 0
	.int 5
/* end data */

.data
.balign 8
.static.10:
	.ascii "false"
/* end data */

.data
.balign 8
.static.11:
	.quad .static.12+0
	.int 0
	.int 1
/* end data */

.data
.balign 8
.static.12:
	.ascii "\n"
/* end data */

.data
.balign 8
.static.13:
	.quad .static.14+0
	.int 0
	.int 4
/* end data */

.data
.balign 8
.static.14:
	.ascii "a.id"
/* end data */

.data
.balign 8
.static.15:
	.quad .static.16+0
	.int 0
	.int 13
/* end data */

.data
.balign 8
.static.16:
	.ascii "a_array[0].id"
/* end data */

.data
.balign 8
.static.17:
	.quad .static.18+0
	.int 0
	.int 15
/* end data */

.data
.balign 8
.static.18:
	.ascii "Exchanging ...."
/* end data */

.data
.balign 8
.static.19:
	.quad .static.20+0
	.int 0
	.int 4
/* end data */

.data
.balign 8
.static.20:
	.ascii "a.id"
/* end data */

.data
.balign 8
.static.21:
	.quad .static.22+0
	.int 0
	.int 13
/* end data */

.data
.balign 8
.static.22:
	.ascii "a_array[0].id"
/* end data */

.text
g__std_may_0__String_print_0:
	pushq %rbp
	movq %rsp, %rbp
	callq internal_may_print_string
	leave
	ret
.type g__std_may_0__String_print_0, @function
.size g__std_may_0__String_print_0, .-g__std_may_0__String_print_0
/* end function g__std_may_0__String_print_0 */

.text
g__std_may_0__Int_print_2:
	pushq %rbp
	movq %rsp, %rbp
	callq internal_may_print_int
	leave
	ret
.type g__std_may_0__Int_print_2, @function
.size g__std_may_0__Int_print_2, .-g__std_may_0__Int_print_2
/* end function g__std_may_0__Int_print_2 */

.text
g__std_may_0__String_println_1:
	pushq %rbp
	movq %rsp, %rbp
	callq g__std_may_0__String_print_0
	leaq .static.1(%rip), %rdi
	callq g__std_may_0__String_print_0
	leave
	ret
.type g__std_may_0__String_println_1, @function
.size g__std_may_0__String_println_1, .-g__std_may_0__String_println_1
/* end function g__std_may_0__String_println_1 */

.text
g__std_may_0__Int_println_3:
	pushq %rbp
	movq %rsp, %rbp
	callq g__std_may_0__Int_print_2
	leaq .static.3(%rip), %rdi
	callq g__std_may_0__String_print_0
	leave
	ret
.type g__std_may_0__Int_println_3, @function
.size g__std_may_0__Int_println_3, .-g__std_may_0__Int_println_3
/* end function g__std_may_0__Int_println_3 */

.text
g__std_may_0__Int_printInfo_4:
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	pushq %rbx
	movq %rsi, %rbx
	callq g__std_may_0__String_print_0
	movq %rbx, %rdi
	movq %rdi, %rbx
	leaq .static.5(%rip), %rdi
	callq g__std_may_0__String_print_0
	movq %rbx, %rdi
	callq g__std_may_0__Int_println_3
	popq %rbx
	leave
	ret
.type g__std_may_0__Int_printInfo_4, @function
.size g__std_may_0__Int_printInfo_4, .-g__std_may_0__Int_printInfo_4
/* end function g__std_may_0__Int_printInfo_4 */

.text
g__std_may_0__Bool_print_5:
	pushq %rbp
	movq %rsp, %rbp
	cmpl $0, %edi
	jnz .Lbb12
	leaq .static.9(%rip), %rdi
	callq g__std_may_0__String_print_0
	jmp .Lbb13
.Lbb12:
	leaq .static.7(%rip), %rdi
	callq g__std_may_0__String_print_0
.Lbb13:
	leave
	ret
.type g__std_may_0__Bool_print_5, @function
.size g__std_may_0__Bool_print_5, .-g__std_may_0__Bool_print_5
/* end function g__std_may_0__Bool_print_5 */

.text
g__std_may_0__Bool_println_6:
	pushq %rbp
	movq %rsp, %rbp
	callq g__std_may_0__Bool_print_5
	leaq .static.11(%rip), %rdi
	callq g__std_may_0__String_print_0
	leave
	ret
.type g__std_may_0__Bool_println_6, @function
.size g__std_may_0__Bool_println_6, .-g__std_may_0__Bool_println_6
/* end function g__std_may_0__Bool_println_6 */

.text
g_main_7:
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	pushq %rbx
	pushq %r12
	pushq %r13
	movl $16, %edi
	callq malloc
	movq %rax, %rbx
	movl $8, %edi
	callq malloc
	movq %rax, %r13
	movq %r13, (%rbx)
	movl $0, 8(%rbx)
	movl $1, 12(%rbx)
	movl $16, %edi
	callq malloc
	movq %rax, %r12
	movq $0, (%r12)
	movl $1, %esi
	movq %r12, %rdi
	callq constructor_A
	movq %r12, (%r13)
	movl $16, %edi
	callq malloc
	movq %rax, %r12
	movq $0, (%r12)
	movl $2, %esi
	movq %r12, %rdi
	callq constructor_A
	movq 8(%r12), %rsi
	leaq .static.13(%rip), %rdi
	callq g__std_may_0__Int_printInfo_4
	movl 12(%rbx), %eax
	cmpq $0, %rax
	setg %al
	movzbl %al, %eax
	imull $1, %eax, %eax
	cmpl $0, %eax
	jnz .Lbb18
	movl $46, %ecx
	movl $12, %edx
	movl $45, %esi
	movl $12, %edi
	callq panic_index_out_of_bounds
	ud2
.Lbb18:
	movq (%rbx), %rax
	movl 8(%rbx), %ecx
	addq $0, %rcx
	movq (%rax, %rcx, 8), %rax
	movq 8(%rax), %rsi
	leaq .static.15(%rip), %rdi
	callq g__std_may_0__Int_printInfo_4
	leaq .static.17(%rip), %rdi
	callq g__std_may_0__String_println_1
	movl 12(%rbx), %eax
	cmpq $0, %rax
	setg %al
	movzbl %al, %eax
	imull $1, %eax, %eax
	cmpl $0, %eax
	jnz .Lbb20
	movl $17, %ecx
	movl $14, %edx
	movl $16, %esi
	movl $14, %edi
	callq panic_index_out_of_bounds
	ud2
.Lbb20:
	movq (%rbx), %rcx
	movl 8(%rbx), %eax
	movl $0, %edx
	addq %rax, %rdx
	movq (%rcx, %rdx, 8), %rax
	movq %r12, (%rcx, %rdx, 8)
	movq 8(%rax), %rsi
	leaq .static.19(%rip), %rdi
	callq g__std_may_0__Int_printInfo_4
	movl 12(%rbx), %eax
	cmpq $0, %rax
	setg %al
	movzbl %al, %eax
	imull $1, %eax, %eax
	cmpl $0, %eax
	jnz .Lbb22
	movl $46, %ecx
	movl $16, %edx
	movl $45, %esi
	movl $16, %edi
	callq panic_index_out_of_bounds
	ud2
.Lbb22:
	movq (%rbx), %rax
	movl 8(%rbx), %ecx
	addq $0, %rcx
	movq (%rax, %rcx, 8), %rax
	movq 8(%rax), %rsi
	leaq .static.21(%rip), %rdi
	callq g__std_may_0__Int_printInfo_4
	popq %r13
	popq %r12
	popq %rbx
	leave
	ret
.type g_main_7, @function
.size g_main_7, .-g_main_7
/* end function g_main_7 */

.text
constructor_A:
	pushq %rbp
	movq %rsp, %rbp
	movq %rsi, 8(%rdi)
	leaq vtable_A(%rip), %rax
	movq %rax, (%rdi)
	leave
	ret
.type constructor_A, @function
.size constructor_A, .-constructor_A
/* end function constructor_A */

.data
.balign 8
vtable_A:
	.quad 0
/* end data */

.text
.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	callq g_main_7
	leave
	ret
.type main, @function
.size main, .-main
/* end function main */

.section .note.GNU-stack,"",@progbits
