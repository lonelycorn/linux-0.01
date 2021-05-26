/*
 * page.s contains the low-level page-exception code.
 * the real work is done in mm.c
 */

.globl _page_fault

//页异常中断处理，中断号14
//分两种情况，一是缺页引起的->_do_no_page，二是写保护导致的->_do_wp_page
//error_code是cpu自动生成，并压在栈上了
//导致异常的线性地址会被cpu自动存放在CR2中
_page_fault:
	xchgl %eax,(%esp) //取出错码到eax
        //保存处理函数会用到的寄存器
	pushl %ecx
	pushl %edx
	push %ds
	push %es
	push %fs
        //置内核数据段选择符
	movl $0x10,%edx
	mov %dx,%ds
	mov %dx,%es
	mov %dx,%fs
	movl %cr2,%edx //取引起页面异常的线性地址
        //参数压栈
	pushl %edx
	pushl %eax
        //根据出错码调用对应处理函数
	testl $1,%eax
	jne 1f
	call _do_no_page
	jmp 2f
1:	call _do_wp_page
2:	addl $8,%esp //丢弃压栈给处理函数的参数
        //恢复寄存器
	pop %fs
	pop %es
	pop %ds
	popl %edx
	popl %ecx
	popl %eax
	iret
