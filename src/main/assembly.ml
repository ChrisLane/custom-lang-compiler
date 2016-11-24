let codegenx86_prefix = "
.LC0:
	.string	\"%d\\n\"
	.globl	print
	.type	print, @function
print:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	movl	%eax, %esi
	leaq	.LC0(%rip), %rdi
	movl	$0, %eax
	call	printf@PLT
	movl	$0, %edi
	call	exit@PLT
	// Begin injected program

"
let codegenx86_suffix = "
	// End injected program
	popq	%rdi
	call	print
	movl	$1, %eax
	leave
	ret
"
