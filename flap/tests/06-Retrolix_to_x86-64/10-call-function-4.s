.data
.S_0:
	.string "%d %d %d %d\n"
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
print_four_int:
	/* Retrolix function print_four_int. */
	pushq %rbp
	movq %rsp, %rbp
	subq $32, %rsp
	movq %rdi, -8(%rbp)
	movq %rsi, -16(%rbp)
	movq %rdx, -24(%rbp)
	movq %rcx, -32(%rbp)
	movq $.S_0, %rdi
	movq -8(%rbp), %rsi
	movq -16(%rbp), %rdx
	movq -24(%rbp), %rcx
	movq -32(%rbp), %r8
	movq $0, %rax
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
	call print_four_int
	movq $0, %rdi
	call exit
