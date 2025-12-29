.data
.S_3:
	.string "rdi <> 0, 1, 2, 3\n"
.S_2:
	.string "rdi = 2 or 3\n"
.S_1:
	.string "rdi = 1\n"
.S_0:
	.string "rdi = 0\n"
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
check:
	/* Retrolix function check. */
	pushq %rbp
	movq %rsp, %rbp
	movq %rdi, %r15
	cmpq $0, %rdi
	je c02
	cmpq $1, %rdi
	je c04
	cmpq $2, %rdi
	je c06
	cmpq $3, %rdi
	je c06
	jmp c08
c02:
	movq $.S_0, %rdi
	jmp c10
c04:
	movq $.S_1, %rdi
	jmp c10
c06:
	movq $.S_2, %rdi
	jmp c10
c08:
	movq $.S_3, %rdi
	jmp c10
c10:
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
	movq $0, %rdi
	call check
	movq $1, %rdi
	call check
	movq $2, %rdi
	call check
	movq $3, %rdi
	call check
	movq $4, %rdi
	call check
	movq $5, %rdi
	call check
	movq $73, %rdi
	call check
	movq $0, %rdi
	movq $0, %rdi
	call exit
