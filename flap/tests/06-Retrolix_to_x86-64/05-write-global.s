.data
.S_3:
	.string "%s\n"
.S_2:
	.string "Like an assignment that works, for instance!"
.S_1:
	.string "%s\n"
.S_0:
	.string "The universe is big, it's vast and complicated, and ridiculous and sometimes, very rarely, impossible things just happen and we call them miracles."
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
	.extern printf
.p2align 3, 144
.I_154575915:
	/* Initializer for x. */
	pushq %rbp
	movq %rsp, %rbp
	movq $.S_0, x(%rip)
	movq $.S_1, %rdi
	movq x(%rip), %rsi
	movq $0, %rax
	call printf
	movq $.S_2, x(%rip)
	movq $.S_3, %rdi
	movq x(%rip), %rsi
	movq $0, %rax
	call printf
	movq $0, %rdi
	call exit
