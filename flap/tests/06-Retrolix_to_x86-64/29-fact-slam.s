.data
.S_4:
	.string "%s(%d) = %d\n"
.S_3:
	.string "fact_tailrec"
.S_2:
	.string "fact_adapt"
.S_1:
	.string "fact_iter"
.S_0:
	.string "fact"
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
fact:
	/* Retrolix function fact. */
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	movq %rdi, %r15
	cmpq $1, %r15
	jle f2
	jmp f4
f2:
	movq $1, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
f4:
	movq %rdi, -8(%rbp)
	movq %rdi, %r15
	subq $1, %r15
	movq %r15, %rdi
	call fact
	movq %rax, %r15
	imulq -8(%rbp), %r15
	movq %r15, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
.p2align 3, 144
fact_tailrec_body:
	/* Retrolix function fact_tailrec_body. */
	pushq %rbp
	movq %rsp, %rbp
	movq %rdi, %r15
	cmpq $1, %r15
	jle ft2
	jmp ft4
ft2:
	movq %rsi, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
ft4:
	movq %rsi, %r15
	imulq %rdi, %r15
	movq %r15, %rsi
	movq %rdi, %r15
	subq $1, %r15
	movq %r15, %rdi
	movq %rbp, %rsp
	popq %rbp
	jmp fact_tailrec_body
.p2align 3, 144
fact_tailrec:
	/* Retrolix function fact_tailrec. */
	pushq %rbp
	movq %rsp, %rbp
	movq $1, %rsi
	movq %rbp, %rsp
	popq %rbp
	jmp fact_tailrec_body
.p2align 3, 144
fact_iter:
	/* Retrolix function fact_iter. */
	pushq %rbp
	movq %rsp, %rbp
	movq $1, %rax
y2:
	movq %rdi, %r15
	cmpq $1, %r15
	jle y6
	jmp y3
y3:
	movq %rax, %r15
	imulq %rdi, %r15
	movq %r15, %rax
	movq %rdi, %r15
	subq $1, %r15
	movq %r15, %rdi
	jmp y2
y6:
	movq %rbp, %rsp
	popq %rbp
	ret
.p2align 3, 144
fact_adapt:
	/* Retrolix function fact_adapt. */
	pushq %rbp
	movq %rsp, %rbp
	movq %rdi, %r15
	cmpq $0, %rdi
	je o11
	cmpq $1, %rdi
	je o11
	cmpq $2, %rdi
	je o13
	cmpq $3, %rdi
	je o15
	cmpq $4, %rdi
	je o17
	cmpq $5, %rdi
	je o19
	jmp oE
o11:
	movq $1, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
o13:
	movq $2, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
o15:
	movq $6, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
o17:
	movq $24, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
o19:
	movq %rbp, %rsp
	popq %rbp
	jmp fact
oE:
	movq %rbp, %rsp
	popq %rbp
	jmp fact_iter
.p2align 3, 144
test_fact_impl:
	/* Retrolix function test_fact_impl. */
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	movq %r12, -8(%rbp)
	movq %rdi, %rax
	movq %rsi, %rdi
	movq %rdx, %rbx
	movq %rsi, %r12
	call *%rax
	movq $.S_4, %rdi
	movq %rbx, %rsi
	movq %r12, %rdx
	movq %rax, %rcx
	movq $0, %rax
	call printf
	movq -8(%rbp), %r12
	movq %rbp, %rsp
	popq %rbp
	ret
.p2align 3, 144
.I_129913994:
	/* Initializer for . */
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq %r12, -8(%rbp)
	movq %r13, -16(%rbp)
	movq $30, %r12
	movq $1, %r13
x2:
	movq %r13, %r15
	cmpq %r12, %r15
	jle x3
	jmp xEND
x3:
	movq $fact, %rdi
	movq %r13, %rsi
	movq $.S_0, %rdx
	call test_fact_impl
	movq $fact_iter, %rdi
	movq %r13, %rsi
	movq $.S_1, %rdx
	call test_fact_impl
	movq $fact_adapt, %rdi
	movq %r13, %rsi
	movq $.S_2, %rdx
	call test_fact_impl
	movq $fact_tailrec, %rdi
	movq %r13, %rsi
	movq $.S_3, %rdx
	call test_fact_impl
	movq %r13, %r15
	addq $1, %r15
	movq %r15, %r13
	jmp x2
xEND:
	movq -8(%rbp), %r12
	movq -16(%rbp), %r13
	movq $0, %rdi
	call exit
