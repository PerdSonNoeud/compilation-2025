.data
.S_0:
	.string "%d\n"
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
print_one_int:
	/* Retrolix function print_one_int. */
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	movq %rdi, -8(%rbp)
	movq $.S_0, %rdi
	movq -8(%rbp), %rsi
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
	movq $37, %r15
	addq $73, %r15
	movq %r15, %rdi
	call print_one_int
	movq $0, %rdi
	call exit
