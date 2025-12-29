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
double_int:
	/* Retrolix function double_int. */
	pushq %rbp
	movq %rsp, %rbp
	movq %rdi, %r15
	addq %rdi, %r15
	movq %r15, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
.p2align 3, 144
.I_154575915:
	/* Initializer for x. */
	pushq %rbp
	movq %rsp, %rbp
	movq $1, x(%rip)
	movq $4, %rbx
l02:
	movq %rbx, %r15
	cmpq $1, %r15
	jge l03
	jmp l08
l03:
	movq x(%rip), %rdi
	movq %rbx, %r15
	subq $1, %r15
	movq %r15, %rbx
	call double_int
	movq %rax, x(%rip)
	jmp l02
l08:
	movq x(%rip), %rdi
	call observe_int
	movq $0, %rdi
	call exit
