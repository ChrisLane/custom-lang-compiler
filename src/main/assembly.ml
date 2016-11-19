let codegenx86_prefix = "
	.file	\"template.c\"
	.section 		.rodata
.LC0:
	.string	\"%d\\n\"
	.text
	.globl	print
	.type	print, @function
print:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	movl	%eax, %esi
	leaq	.LC0(%rip), %rdi
	movl	$0, %eax
	call	printf@PLT
	movl	$0, %edi
	call	exit@PLT
	.cfi_endproc
.LFE0:
	.size	print, .-print
	.globl	main
	.type	main, @function
main:
.LFB1:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	$260, -4(%rbp)
	movl	-4(%rbp), %eax
	// Begin injected code

"

let codegenx86_suffix = "
	// End injected code
	popq	%rdi
	call	print
	movl	$1, %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	main, .-main
	.ident	\"GCC: (Alpine 5.3.0) 5.3.0\"
	.section		.note.GNU-stack,\"\",@progbits
"
