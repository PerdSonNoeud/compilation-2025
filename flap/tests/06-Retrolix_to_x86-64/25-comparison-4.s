.data
.S_1:
	.string "not lte\n"
.S_0:
	.string "lte\n"
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
.I_154575915:
	/* Initializer for x. */
	pushq %rbp
	movq %rsp, %rbp
	movq $1, %r15
	cmpq $1, %r15
	jle l17
	jmp l20
l17:
	movq $.S_0, %rdi
	call printf
	movq $0, %rdi
	call exit
l20:
	movq $.S_1, %rdi
	movq $0, %rax
	call printf
	movq $0, %rdi
	call exit
