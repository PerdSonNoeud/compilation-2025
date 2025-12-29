.data
.S_1:
	.string "street\n"
.S_0:
	.string "%d jump\n"
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
	movq $1, %rbx
l01:
	movq %rbx, %r15
	cmpq $22, %r15
	je l17
	jmp l11
l11:
	movq $.S_0, %rdi
	movq %rbx, %rsi
	movq $0, %rax
	call printf
	movq %rbx, %r15
	addq $1, %r15
	movq %r15, %rbx
	jmp l01
l17:
	movq $.S_1, %rdi
	movq $0, %rax
	call printf
	movq $0, %rdi
	call exit
