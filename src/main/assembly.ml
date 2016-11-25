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
  leave
  ret
	// Begin injected program

"
let codegenx86_suffix = "
	// End injected program
	movl	$0, %edi
	call	exit@PLT
"
