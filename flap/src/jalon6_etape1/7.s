.data
x:
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
.p2align 3, 144
fact:
	/* Retrolix function fact. */
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq %rdi, -8(%rbp)
	movq -8(%rbp), %r15
	cmpq $0, %r15
	je f2
	jmp f4
f2:
	movq $1, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
f4:
	movq -8(%rbp), %r15
	movq %r15, -16(%rbp)
	movq -8(%rbp), %r15
	subq $1, %r15
	movq %r15, %rdi
	call fact
	movq %rax, %r15
	imulq -16(%rbp), %r15
	movq %r15, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
.p2align 3, 144
.I_154575915:
	/* Initializer for x. */
	pushq %rbp
	movq %rsp, %rbp
	movq $5, %rdi
	call fact
	movq %rax, x(%rip)
	movq x(%rip), %rdi
	call observe_int
	movq $0, %rdi
	call exit
