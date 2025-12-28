.data
s:
	/* s */
	.quad 0
.text
	.globl main
.p2align 3, 144
main:
	/* Program entry point. */
	subq $8, %rsp
	call .I_624401798
	movq $0, %rdi
	call exit
.p2align 3, 144
.I_624401798:
	/* Initializer for s. */
	movq $1, %rdi
	movq $2, %rsi
	movq $3, %rdx
	movq $4, %rcx
	movq $5, %r8
	movq $6, %r9
	pushq $8
	pushq $7
	call add_eight_int
	addq $16, %rsp
	movq %rax, s(%rip)
	movq s(%rip), %rdi
	call observe_int
	movq $0, %rdi
	call exit
