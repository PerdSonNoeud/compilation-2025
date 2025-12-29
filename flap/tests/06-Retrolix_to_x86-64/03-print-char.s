.data
.S_0:
	.string "%c\n"
.text
	.globl main
.p2align 3, 144
main:
	/* Program entry point. */
	subq $8, %rsp
	call .I_129913994
	movq $0, %rdi
	call exit
	.extern printf
.p2align 3, 144
.I_129913994:
	/* Initializer for . */
	pushq %rbp
	movq %rsp, %rbp
	movq $.S_0, %rdi
	movq $97, %rsi
	movq $0, %rax
	call printf
	movq $0, %rdi
	call exit
