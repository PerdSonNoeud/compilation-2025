.data
n2:
	/* n2 */
	.quad 0
main37:
	/* main37 */
	.quad 0
m1:
	/* m1 */
	.quad 0
.text
	.globl main
.p2align 3, 144
main:
	/* Program entry point. */
	subq $8, %rsp
	call .I_833836167
	call .I_600030479
	call .I_325745432
	movq $0, %rdi
	call exit
.p2align 3, 144
.I_833836167:
	/* Initializer for m1. */
	pushq %rbp
	movq %rsp, %rbp
	movq $10, m1(%rip)
	movq %rbp, %rsp
	popq %rbp
	ret
.p2align 3, 144
.I_600030479:
	/* Initializer for n2. */
	pushq %rbp
	movq %rsp, %rbp
	movq $10, n2(%rip)
	movq %rbp, %rsp
	popq %rbp
	ret
.p2align 3, 144
ref:
	/* Retrolix function ref. */
	pushq %rbp
	movq %rsp, %rbp
	subq $64, %rsp
	movq %rdi, -64(%rbp)
	movq $1, -40(%rbp)
	movq -40(%rbp), %rdi
	call allocate_block
	movq %rax, -56(%rbp)
	movq -56(%rbp), %r15
	movq %r15, -16(%rbp)
	movq $0, -24(%rbp)
	movq -64(%rbp), %r15
	movq %r15, -32(%rbp)
	movq -16(%rbp), %rdi
	movq -24(%rbp), %rsi
	movq -32(%rbp), %rdx
	call write_block
	movq %rax, -48(%rbp)
	movq -56(%rbp), %r15
	movq %r15, -8(%rbp)
	movq -8(%rbp), %rax
	movq %rbp, %rsp
	popq %rbp
	ret
.p2align 3, 144
read:
	/* Retrolix function read. */
	pushq %rbp
	movq %rsp, %rbp
	subq $32, %rsp
	movq %rdi, -32(%rbp)
	movq -32(%rbp), %r15
	movq %r15, -16(%rbp)
	movq $0, -24(%rbp)
	movq -16(%rbp), %rdi
	movq -24(%rbp), %rsi
	call read_block
	movq %rax, -8(%rbp)
	movq -8(%rbp), %rax
	movq %rbp, %rsp
	popq %rbp
	ret
.p2align 3, 144
write:
	/* Retrolix function write. */
	pushq %rbp
	movq %rsp, %rbp
	subq $48, %rsp
	movq %rdi, -40(%rbp)
	movq %rsi, -48(%rbp)
	movq -40(%rbp), %r15
	movq %r15, -16(%rbp)
	movq $0, -24(%rbp)
	movq -48(%rbp), %r15
	movq %r15, -32(%rbp)
	movq -16(%rbp), %rdi
	movq -24(%rbp), %rsi
	movq -32(%rbp), %rdx
	call write_block
	movq %rax, -8(%rbp)
	movq -8(%rbp), %rax
	movq %rbp, %rsp
	popq %rbp
	ret
.p2align 3, 144
make_matrix:
	/* Retrolix function make_matrix. */
	pushq %rbp
	movq %rsp, %rbp
	subq $392, %rsp
	movq %rdi, -376(%rbp)
	movq %rsi, -384(%rbp)
	movq -376(%rbp), %r15
	movq %r15, -248(%rbp)
	movq -248(%rbp), %rdi
	call allocate_block
	movq %rax, -392(%rbp)
	movq -376(%rbp), %r15
	movq %r15, -232(%rbp)
	movq $1, -240(%rbp)
	movq -232(%rbp), %r15
	subq -240(%rbp), %r15
	movq %r15, -224(%rbp)
	movq -224(%rbp), %rdi
	call ref
	movq %rax, -360(%rbp)
l325:
	movq -360(%rbp), %r15
	movq %r15, -208(%rbp)
	movq -208(%rbp), %rdi
	call read
	movq %rax, -200(%rbp)
	movq $0, -216(%rbp)
	movq -200(%rbp), %r15
	cmpq -216(%rbp), %r15
	jge l280
	jmp l314
l280:
	movq -392(%rbp), %r15
	movq %r15, -160(%rbp)
	movq -360(%rbp), %r15
	movq %r15, -176(%rbp)
	movq -176(%rbp), %rdi
	call read
	movq %rax, -168(%rbp)
	movq -384(%rbp), %r15
	movq %r15, -192(%rbp)
	movq -192(%rbp), %rdi
	call allocate_block
	movq %rax, -184(%rbp)
	movq -160(%rbp), %rdi
	movq -168(%rbp), %rsi
	movq -184(%rbp), %rdx
	call write_block
	movq %rax, -336(%rbp)
	movq -384(%rbp), %r15
	movq %r15, -144(%rbp)
	movq $1, -152(%rbp)
	movq -144(%rbp), %r15
	subq -152(%rbp), %r15
	movq %r15, -136(%rbp)
	movq -136(%rbp), %rdi
	call ref
	movq %rax, -368(%rbp)
l247:
	movq -368(%rbp), %r15
	movq %r15, -120(%rbp)
	movq -120(%rbp), %rdi
	call read
	movq %rax, -112(%rbp)
	movq $0, -128(%rbp)
	movq -112(%rbp), %r15
	cmpq -128(%rbp), %r15
	jge l169
	jmp l236
l169:
	movq -392(%rbp), %r15
	movq %r15, -32(%rbp)
	movq -360(%rbp), %r15
	movq %r15, -48(%rbp)
	movq -48(%rbp), %rdi
	call read
	movq %rax, -40(%rbp)
	movq -32(%rbp), %rdi
	movq -40(%rbp), %rsi
	call read_block
	movq %rax, -24(%rbp)
	movq -368(%rbp), %r15
	movq %r15, -64(%rbp)
	movq -64(%rbp), %rdi
	call read
	movq %rax, -56(%rbp)
	movq -368(%rbp), %r15
	movq %r15, -88(%rbp)
	movq -88(%rbp), %rdi
	call read
	movq %rax, -80(%rbp)
	movq -360(%rbp), %r15
	movq %r15, -104(%rbp)
	movq -104(%rbp), %rdi
	call read
	movq %rax, -96(%rbp)
	movq -80(%rbp), %r15
	addq -96(%rbp), %r15
	movq %r15, -72(%rbp)
	movq -24(%rbp), %rdi
	movq -56(%rbp), %rsi
	movq -72(%rbp), %rdx
	call write_block
	movq %rax, -352(%rbp)
	movq -368(%rbp), %r15
	movq %r15, -304(%rbp)
	movq -368(%rbp), %r15
	movq %r15, -8(%rbp)
	movq -8(%rbp), %rdi
	call read
	movq %rax, -320(%rbp)
	movq $1, -16(%rbp)
	movq -320(%rbp), %r15
	subq -16(%rbp), %r15
	movq %r15, -312(%rbp)
	movq -304(%rbp), %rdi
	movq -312(%rbp), %rsi
	call write
	movq %rax, -344(%rbp)
	jmp l247
l236:
	/*  Exit of while loop */
	movq -360(%rbp), %r15
	movq %r15, -264(%rbp)
	movq -360(%rbp), %r15
	movq %r15, -288(%rbp)
	movq -288(%rbp), %rdi
	call read
	movq %rax, -280(%rbp)
	movq $1, -296(%rbp)
	movq -280(%rbp), %r15
	subq -296(%rbp), %r15
	movq %r15, -272(%rbp)
	movq -264(%rbp), %rdi
	movq -272(%rbp), %rsi
	call write
	movq %rax, -328(%rbp)
	jmp l325
l314:
	/*  Exit of while loop */
	movq -392(%rbp), %r15
	movq %r15, -256(%rbp)
	movq -256(%rbp), %rax
	movq %rbp, %rsp
	popq %rbp
	ret
.p2align 3, 144
transpose:
	/* Retrolix function transpose. */
	pushq %rbp
	movq %rsp, %rbp
	subq $376, %rsp
	movq %rdi, -352(%rbp)
	movq %rsi, -344(%rbp)
	movq %rdx, -360(%rbp)
	movq -360(%rbp), %r15
	movq %r15, -296(%rbp)
	movq -344(%rbp), %r15
	movq %r15, -304(%rbp)
	movq -296(%rbp), %rdi
	movq -304(%rbp), %rsi
	call make_matrix
	movq %rax, -368(%rbp)
	movq -360(%rbp), %r15
	movq %r15, -280(%rbp)
	movq $1, -288(%rbp)
	movq -280(%rbp), %r15
	subq -288(%rbp), %r15
	movq %r15, -272(%rbp)
	movq -272(%rbp), %rdi
	call ref
	movq %rax, -376(%rbp)
l590:
	movq -376(%rbp), %r15
	movq %r15, -256(%rbp)
	movq -256(%rbp), %rdi
	call read
	movq %rax, -248(%rbp)
	movq $0, -264(%rbp)
	movq -248(%rbp), %r15
	cmpq -264(%rbp), %r15
	jge l573
	jmp l579
l573:
	movq -344(%rbp), %r15
	movq %r15, -232(%rbp)
	movq $1, -240(%rbp)
	movq -232(%rbp), %r15
	subq -240(%rbp), %r15
	movq %r15, -224(%rbp)
	movq -224(%rbp), %rdi
	call ref
	movq %rax, -336(%rbp)
l556:
	movq -336(%rbp), %r15
	movq %r15, -208(%rbp)
	movq -208(%rbp), %rdi
	call read
	movq %rax, -200(%rbp)
	movq $0, -216(%rbp)
	movq -200(%rbp), %r15
	cmpq -216(%rbp), %r15
	jge l450
	jmp l545
l450:
	movq -368(%rbp), %r15
	movq %r15, -104(%rbp)
	movq -376(%rbp), %r15
	movq %r15, -120(%rbp)
	movq -120(%rbp), %rdi
	call read
	movq %rax, -112(%rbp)
	movq -104(%rbp), %rdi
	movq -112(%rbp), %rsi
	call read_block
	movq %rax, -96(%rbp)
	movq -336(%rbp), %r15
	movq %r15, -136(%rbp)
	movq -136(%rbp), %rdi
	call read
	movq %rax, -128(%rbp)
	movq -352(%rbp), %r15
	movq %r15, -160(%rbp)
	movq -336(%rbp), %r15
	movq %r15, -176(%rbp)
	movq -176(%rbp), %rdi
	call read
	movq %rax, -168(%rbp)
	movq -160(%rbp), %rdi
	movq -168(%rbp), %rsi
	call read_block
	movq %rax, -152(%rbp)
	movq -376(%rbp), %r15
	movq %r15, -192(%rbp)
	movq -192(%rbp), %rdi
	call read
	movq %rax, -184(%rbp)
	movq -152(%rbp), %rdi
	movq -184(%rbp), %rsi
	call read_block
	movq %rax, -144(%rbp)
	movq -96(%rbp), %rdi
	movq -128(%rbp), %rsi
	movq -144(%rbp), %rdx
	call write_block
	movq %rax, -328(%rbp)
	movq -336(%rbp), %r15
	movq %r15, -56(%rbp)
	movq -336(%rbp), %r15
	movq %r15, -80(%rbp)
	movq -80(%rbp), %rdi
	call read
	movq %rax, -72(%rbp)
	movq $1, -88(%rbp)
	movq -72(%rbp), %r15
	subq -88(%rbp), %r15
	movq %r15, -64(%rbp)
	movq -56(%rbp), %rdi
	movq -64(%rbp), %rsi
	call write
	movq %rax, -320(%rbp)
	jmp l556
l545:
	/*  Exit of while loop */
	movq -376(%rbp), %r15
	movq %r15, -16(%rbp)
	movq -376(%rbp), %r15
	movq %r15, -40(%rbp)
	movq -40(%rbp), %rdi
	call read
	movq %rax, -32(%rbp)
	movq $1, -48(%rbp)
	movq -32(%rbp), %r15
	subq -48(%rbp), %r15
	movq %r15, -24(%rbp)
	movq -16(%rbp), %rdi
	movq -24(%rbp), %rsi
	call write
	movq %rax, -312(%rbp)
	jmp l590
l579:
	/*  Exit of while loop */
	movq -368(%rbp), %r15
	movq %r15, -8(%rbp)
	movq -8(%rbp), %rax
	movq %rbp, %rsp
	popq %rbp
	ret
.p2align 3, 144
check:
	/* Retrolix function check. */
	pushq %rbp
	movq %rsp, %rbp
	subq $456, %rsp
	movq %rdi, -424(%rbp)
	movq %rsi, -432(%rbp)
	movq %rdx, -440(%rbp)
	movq %rcx, -448(%rbp)
	movq $0, -376(%rbp)
	movq -376(%rbp), %rdi
	call ref
	movq %rax, -416(%rbp)
	movq -440(%rbp), %r15
	movq %r15, -360(%rbp)
	movq $1, -368(%rbp)
	movq -360(%rbp), %r15
	subq -368(%rbp), %r15
	movq %r15, -352(%rbp)
	movq -352(%rbp), %rdi
	call ref
	movq %rax, -456(%rbp)
l944:
	movq -456(%rbp), %r15
	movq %r15, -336(%rbp)
	movq -336(%rbp), %rdi
	call read
	movq %rax, -328(%rbp)
	movq $0, -344(%rbp)
	movq -328(%rbp), %r15
	cmpq -344(%rbp), %r15
	jge l927
	jmp l933
l927:
	movq -448(%rbp), %r15
	movq %r15, -312(%rbp)
	movq $1, -320(%rbp)
	movq -312(%rbp), %r15
	subq -320(%rbp), %r15
	movq %r15, -304(%rbp)
	movq -304(%rbp), %rdi
	call ref
	movq %rax, -408(%rbp)
l910:
	movq -408(%rbp), %r15
	movq %r15, -288(%rbp)
	movq -288(%rbp), %rdi
	call read
	movq %rax, -280(%rbp)
	movq $0, -296(%rbp)
	movq -280(%rbp), %r15
	cmpq -296(%rbp), %r15
	jge l804
	jmp l899
l804:
	movq -424(%rbp), %r15
	movq %r15, -184(%rbp)
	movq -456(%rbp), %r15
	movq %r15, -200(%rbp)
	movq -200(%rbp), %rdi
	call read
	movq %rax, -192(%rbp)
	movq -184(%rbp), %rdi
	movq -192(%rbp), %rsi
	call read_block
	movq %rax, -176(%rbp)
	movq -408(%rbp), %r15
	movq %r15, -216(%rbp)
	movq -216(%rbp), %rdi
	call read
	movq %rax, -208(%rbp)
	movq -176(%rbp), %rdi
	movq -208(%rbp), %rsi
	call read_block
	movq %rax, -168(%rbp)
	movq -432(%rbp), %r15
	movq %r15, -240(%rbp)
	movq -456(%rbp), %r15
	movq %r15, -256(%rbp)
	movq -256(%rbp), %rdi
	call read
	movq %rax, -248(%rbp)
	movq -240(%rbp), %rdi
	movq -248(%rbp), %rsi
	call read_block
	movq %rax, -232(%rbp)
	movq -408(%rbp), %r15
	movq %r15, -272(%rbp)
	movq -272(%rbp), %rdi
	call read
	movq %rax, -264(%rbp)
	movq -232(%rbp), %rdi
	movq -264(%rbp), %rsi
	call read_block
	movq %rax, -224(%rbp)
	movq -168(%rbp), %r15
	cmpq -224(%rbp), %r15
	je l734
	jmp l775
l734:
	movq -416(%rbp), %r15
	movq %r15, -112(%rbp)
	movq -416(%rbp), %r15
	movq %r15, -136(%rbp)
	movq -136(%rbp), %rdi
	call read
	movq %rax, -128(%rbp)
	movq $1, -144(%rbp)
	movq -128(%rbp), %r15
	addq -144(%rbp), %r15
	movq %r15, -120(%rbp)
	movq -112(%rbp), %rdi
	movq -120(%rbp), %rsi
	call write
	movq %rax, -400(%rbp)
	jmp l782
l775:
	movq -416(%rbp), %r15
	movq %r15, -160(%rbp)
	movq -160(%rbp), %rdi
	call read
	movq %rax, -152(%rbp)
	movq -152(%rbp), %rdi
	call print_int
	movq %rax, -400(%rbp)
l782:
	/*  Join control point */
	movq -408(%rbp), %r15
	movq %r15, -72(%rbp)
	movq -408(%rbp), %r15
	movq %r15, -96(%rbp)
	movq -96(%rbp), %rdi
	call read
	movq %rax, -88(%rbp)
	movq $1, -104(%rbp)
	movq -88(%rbp), %r15
	subq -104(%rbp), %r15
	movq %r15, -80(%rbp)
	movq -72(%rbp), %rdi
	movq -80(%rbp), %rsi
	call write
	movq %rax, -392(%rbp)
	jmp l910
l899:
	/*  Exit of while loop */
	movq -456(%rbp), %r15
	movq %r15, -32(%rbp)
	movq -456(%rbp), %r15
	movq %r15, -56(%rbp)
	movq -56(%rbp), %rdi
	call read
	movq %rax, -48(%rbp)
	movq $1, -64(%rbp)
	movq -48(%rbp), %r15
	subq -64(%rbp), %r15
	movq %r15, -40(%rbp)
	movq -32(%rbp), %rdi
	movq -40(%rbp), %rsi
	call write
	movq %rax, -384(%rbp)
	jmp l944
l933:
	/*  Exit of while loop */
	movq -416(%rbp), %r15
	movq %r15, -24(%rbp)
	movq -24(%rbp), %rdi
	call read
	movq %rax, -16(%rbp)
	movq -16(%rbp), %rdi
	call print_int
	movq %rax, -8(%rbp)
	movq -8(%rbp), %rax
	movq %rbp, %rsp
	popq %rbp
	ret
.p2align 3, 144
.I_325745432:
	/* Initializer for main37. */
	pushq %rbp
	movq %rsp, %rbp
	subq $136, %rsp
	movq m1(%rip), %r15
	movq %r15, -96(%rbp)
	movq n2(%rip), %r15
	movq %r15, -104(%rbp)
	movq -96(%rbp), %rdi
	movq -104(%rbp), %rsi
	call make_matrix
	movq %rax, -120(%rbp)
	movq -120(%rbp), %r15
	movq %r15, -72(%rbp)
	movq m1(%rip), %r15
	movq %r15, -80(%rbp)
	movq n2(%rip), %r15
	movq %r15, -88(%rbp)
	movq -72(%rbp), %rdi
	movq -80(%rbp), %rsi
	movq -88(%rbp), %rdx
	call transpose
	movq %rax, -128(%rbp)
	movq -128(%rbp), %r15
	movq %r15, -48(%rbp)
	movq n2(%rip), %r15
	movq %r15, -56(%rbp)
	movq m1(%rip), %r15
	movq %r15, -64(%rbp)
	movq -48(%rbp), %rdi
	movq -56(%rbp), %rsi
	movq -64(%rbp), %rdx
	call transpose
	movq %rax, -136(%rbp)
	movq -120(%rbp), %r15
	movq %r15, -16(%rbp)
	movq -136(%rbp), %r15
	movq %r15, -24(%rbp)
	movq m1(%rip), %r15
	movq %r15, -32(%rbp)
	movq n2(%rip), %r15
	movq %r15, -40(%rbp)
	movq -16(%rbp), %rdi
	movq -24(%rbp), %rsi
	movq -32(%rbp), %rdx
	movq -40(%rbp), %rcx
	call check
	movq %rax, -112(%rbp)
	movq $0, -8(%rbp)
	movq -8(%rbp), %rdi
	call print_int
	movq %rax, main37(%rip)
	movq %rbp, %rsp
	popq %rbp
	ret
