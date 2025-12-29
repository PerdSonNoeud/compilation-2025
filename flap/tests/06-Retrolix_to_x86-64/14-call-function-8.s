.data
.S_0:
	.string "%d %d %d %d %d %d %d\n"
	/* x */
	.quad 0
.text
	.globl main
.p2align 3, 144
main:
	/* Program entry point. */
	subq $8, %rsp
	call .I_154575915
	movq $0, %rdi
	call exit
	.extern printf
.p2align 3, 144
print_seven_int_for_real:
	/* Retrolix function print_seven_int_for_real. */
	pushq %rbp
	movq %rsp, %rbp
	subq $48, %rsp
	movq %rdi, -8(%rbp)
	movq %rsi, -16(%rbp)
	movq %rdx, -24(%rbp)
	movq %rcx, -32(%rbp)
	movq %r8, -40(%rbp)
	movq %r9, -48(%rbp)
	movq $.S_0, %rdi
	movq -8(%rbp), %rsi
	movq -16(%rbp), %rdx
	movq -24(%rbp), %rcx
	movq -32(%rbp), %r8
	movq -40(%rbp), %r9
	movq $0, %rax
	movq -48(%rbp), %rdi
	movq a7(%rip), %rsi
	call printf
	movq %rbp, %rsp
	popq %rbp
	ret
.p2align 3, 144
.I_154575915:
	/* Initializer for x. */
	pushq %rbp
	movq %rsp, %rbp
	movq $37, %rdi
	movq $73, %rsi
	movq $31, %rdx
	movq $13, %rcx
	movq $0, %r8
	movq $99, %r9
	movq $42, %rdi
	call print_seven_int_for_real
	movq $0, %rdi
	call exit
