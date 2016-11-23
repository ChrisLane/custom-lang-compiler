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
	movl	$.LC0, %edi
	movl	$0, %eax
	call	printf
	movl	$0, %edi
	call	exit
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	// Begin injected code

"

let codegenx86_suffix = "
	// End injected code
	popq	%rdi
	call	print
	movl	$1, %eax
	leave
	ret
"
