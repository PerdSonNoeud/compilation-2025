.data
.S_0:
	.string "%d %d %d\n"
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
print_three_int:
	/* Retrolix function print_three_int. */
	pushq %rbp
	movq %rsp, %rbp
	subq $24, %rsp
	movq %rdi, -8(%rbp)
	movq %rsi, -16(%rbp)
	movq %rdx, -24(%rbp)
	movq $.S_0, %rdi
	movq -8(%rbp), %rsi
	movq -16(%rbp), %rdx
	movq -24(%rbp), %rcx
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
	movq $42, %rdx
	call print_three_int
	movq $0, %rdi
	call exit
