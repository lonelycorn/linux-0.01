//采用ATT汇编格式,GNU的gas86编译，gld86链接
/*
 *  head.s contains the 32-bit startup code.
 *
 * NOTE!!! Startup happens at absolute address 0x00000000, which is also where
 * the page directory will exist. The startup code will be overwritten by
 * the page directory.
 */
.text
.globl _idt,_gdt,_pg_dir
_pg_dir:
//这里最后存放的是页目录(开启分页后使用)

startup_32:
	movl $0x10,%eax //mov long word 0x00000010 -> eax
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	mov %ax,%gs
//ds=es=fs=gs=0x10h=0b 0001 0000 都是数据段选择子寄存器
//selector bit:
// 0-1: RPL, requested privilege level
//   2: TI, table indicator
//3-15: index
//RPL=0,GDT,index=0b10=2, the 2nd descriptor in GDT
// 15 - 3 |  2 | 1 - 0
//  index | TI |  RPL
	lss _stack_start,%esp   //defined in kernel/sched.c, 设置堆栈位置
	call setup_idt 		//设置中断描述符
	call setup_gdt 		//设置全局描述符
	movl $0x10,%eax		# reload all the segment registers
	mov %ax,%ds		# after changing gdt. CS was already
	mov %ax,%es		# reloaded in 'setup_gdt'
	mov %ax,%fs
	mov %ax,%gs
//同上，设定数据段选择子寄存器
	lss _stack_start,%esp   //defined in kernel/sched.c, 设置堆栈位置

	xorl %eax,%eax          //eax=0
1:	incl %eax		# check that A20 really IS enabled
	movl %eax,0x000000      //eax--->0x000000
	cmpl %eax,0x100000      //eax==[0x100000](1MB处的内存)
	je 1b                   // if equal j backward to 1:
                                // so, if A20 not enabled, 会down机

//用于检查协处理器是否存在。ET bit in CR0.
	movl %cr0,%eax		# check math chip
	andl $0x80000011,%eax	# Save PG,ET,PE
	testl $0x10,%eax
	jne 1f			# ET is set - 387 is present
	orl $4,%eax		# else set emulate bit (EM bit in CR0)
1:	movl %eax,%cr0
	jmp after_page_tables

/*
 *  setup_idt
 *
 *  sets up a idt with 256 entries pointing to
 *  ignore_int, interrupt gates. It then loads
 *  idt. Everything that wants to install itself
 *  in the idt-table may do so themselves. Interrupts
 *  are enabled elsewhere, when we can be relatively
 *  sure everything is ok. This routine will be over-
 *  written by the page tables.
 */
setup_idt:
	lea ignore_int,%edx     //ignore_int表示的地址--->edx
	movl $0x00080000,%eax
	movw %dx,%ax		/* selector = 0x0008 = cs */
	movw $0x8E00,%dx	/* interrupt gate - dpl=0, present */
//eax=0x0008,(ignore_int表示的地址低2字节)
//edx=(ignore_int表示的地址高2字节), 0x8E00
//interrupt descriptor (8Bytes=64bits):
//63-48: isr 入口地址高2字节
//15-0:  isr 入口地址低2字节
//31-16: 段选择符
//47:    P标志,该段是否存在
//46-45: DPL
//44:    描述符类型, 0=system, 1=data/code
//43-40: "D110=1110" 系统描述符类型,110表示中断描述符
//       d:(调用)门的大小,1=32bit,0=16bit,即isr入口地址是32bit or 16bit?
//39-32: reserved, must be 0
//
//       63 - 48  |  47 | 46 45 | 44 43 42 41 40 | 39 - 32
//  edx: isr high | P=1 | DPL=0 |  0  1  1  1  0 |    0
//           31 - 16     |  15 - 0
//  eax: selector=0x0008 | isr low
// selector bits:
//   15 - 3 |   2  | 1 - 0
//  index=1 | TI=0 | RPL=0
//  对应描述符是全局描述符表中的第二项,index=1,此描述符说了:base=0x0000
//  就是说，isr代码在0x0000+ignore_int处

	lea _idt,%edi          //_idt表示的地址--->edi, _idt是中断描述符表
	mov $256,%ecxi         //循环256次，中断描述符表有256个中断描述符
rp_sidt:
	movl %eax,(%edi)       //每个中断描述符由eax和edx的内容组成
	movl %edx,4(%edi)
	addl $8,%edi           //移动到新地址，填充下一个中断描述符
	dec %ecx
	jne rp_sidt
	lidt idt_descr         //idt_descr处内容加载到中断描述符表寄存器
//源操作数指定 6 字节内存位置，它包含全局描述符表格 (GDT) 或中断描述符表格 (IDT) 的基址（线性地址）与限制（表格大小，以字节计）。
	ret

/*
 *  setup_gdt
 *
 *  This routines sets up a new gdt and loads it.
 *  Only two entries are currently built, the same
 *  ones that were built in init.s. The routine
 *  is VERY complicated at two whole lines, so this
 *  rather long comment is certainly needed :-).
 *  This routine will beoverwritten by the page tables.
 */
setup_gdt:
	lgdt gdt_descr //加载全局描述符表到GDT寄存器
	ret

.org 0x1000
pg0: //页表0

.org 0x2000
pg1: //页表1

.org 0x3000
pg2: //页表2	# This is not used yet, but if you
		# want to expand past 8 Mb, you'll have
		# to use it.

//0x4000以上地址最终会被页表填充
.org 0x4000
after_page_tables:
	pushl $0		# These are the parameters to main :-)
	pushl $0
	pushl $0
//envp=0, argc=0, argv=0
	pushl $L6		# return address for main, if it decides to.
	pushl $_main
	jmp setup_paging
//注意，这里是JMP不是CALL，但是setup_paging返回的时候是ret
//所以直接跳转到_main函数执行
L6:
	jmp L6			# main should never return here, but
				# just in case, we know what happens.

/* This is the default interrupt "handler" :-) */
//哑中断isr
.align 2
ignore_int:
	incb 0xb8000+160		# put something on the screen
	movb $2,0xb8000+161		# so that we know something
	iret				# happened


/*
 * Setup_paging
 *
 * This routine sets up paging by setting the page bit
 * in cr0. The page tables are set up, identity-mapping
 * the first 8MB. The pager assumes that no illegal
 * addresses are produced (ie >4Mb on a 4Mb machine).
 *
 * NOTE! Although all physical memory should be identity
 * mapped by this routine, only the kernel page functions
 * use the >1Mb addresses directly. All "normal" functions
 * use just the lower 1Mb, or the local data space, which
 * will be mapped to some other place - mm keeps track of
 * that.
 *
 * For those with more memory than 8 Mb - tough luck. I've
 * not got it, why should you :-) The source is here. Change
 * it. (Seriously - it shouldn't be too difficult. Mostly
 * change some constants etc. I left it at 8Mb, as my machine
 * even cannot be extended past that (ok, but it was cheap :-)
 * I've tried to show which constants to change by having
 * some kind of marker at them (search for "8Mb"), but I
 * won't guarantee that's all :-( )
 */
//大于1MB的内存空间被用于主内存，由mm模块管理。内核中其他函数(normal functions above)
//用低于1MB的内存。主内存区的页面用get_free_page()取得。

//物理地址0x0开始，放1页页目录和2页页表(内核专用,映射到8MB的范围)。页表2，即第三页页表，
//并没有使用。新开的进程，在主内存区申请页面存放页表。
//32Bit系统使用2级页表
.align 2 //4字节对齐
setup_paging:
	movl $1024*3,%ecx //设置计数器，清空3页内容
	xorl %eax,%eax   //eax=0
	xorl %edi,%edi	 //edi=0        /* pg_dir is at 0x000 */
	cld;rep;stosl //edi正方向增长，eax--->es:edi
//内核只用了2个页表，只对页目录设置2项
//每个页表项(entry)占4Bytes,一张页表占1页(4KB),所以可以放1024项
//页表项定义:
//  31 - 12  | 11 - 9 | 8 |  7  | 6 | 5 |  4  |  3  | 2 | 1 | 0
//  页帧地址     AVL    G   PAT   D   A   PCD   PWT  U/S R/W  P
//  P: page exist?
//  R/W: 0=RO, 1=writable
//  U/S: 0=super 1=user
//  PWT: 1=write through 0=write back (cache system)
//  PCD: 1=disable cache
//  A: accessed recently 1=yes (TLB system)
//  D: dirty
//  PAT:On the x86 with Pentium III and higher, this bit is called the Page Attribute Table (PAT)
//      while earlier architectures such as the Pentium II had this bit reserved.
//      The PAT bit is used to indicate the size of the page the PTE is referencing.
//      In a PGD entry, this same bit is instead called the Page Size Exception (PSE) bit
//      so obviously these bits are meant to be used in conjunction.
//      在普通的4KB分页机制中, 处理器建议将其置0.
//      页表项中为0.
//  G:用来指示该表项所指向的页是否为全局性质的. 如果页是全局的, 那么, 它将在高速缓存中一直保存(也就意味着地址转换速度会很快). 因为页高速缓存容量有限, 只能存放频繁使用的那些表项. 而且, 当因任务切换等原因改变CR3寄存器的内容时, 整个页高速缓存的内容都会被刷新.(TLB system)
//  AVL位被处理器忽略, 软件可以使用.

// Line Address ---> Physical Address
// LA             31 - 22           |            21 - 12             |         11 - 0
//        Index to Page Directory          Index to Page Table                 Offset

	movl $pg0+7,_pg_dir		/* set present bit/user r/w */
	movl $pg1+7,_pg_dir+4		/*  --------- " " --------- */
//页目录如下:
//addr      pg_dir
//0x0000    ($pg0+7) 0x00001007        // 页帧地址=0xFFFFF000 & 0x00001007 = 0x00001000
                                       // U/S=1, R/W=1, P=1
//0x0004    ($pg1+7) 0x00002007        // 页帧地址=0xFFFFF000 & 0x00001007 = 0x00002000
                                       // U/S=1, R/W=1, P=1
//以下填充页表(pg0,pg1)的页表项
	movl $pg1+4092,%edi            // 最后一页最后一项对应地址(0x00002ffc)--->edi
	movl $0x007ff007,%eax	/*  8Mb - 4096 + 7 (r/w user,p) */
                                //物理内存地址8M-4KB的起始位置(0x007ff000)
	std                     //方向置位，edi往地址减少方向
1:	stosl			/* fill pages backwards - more efficient :-) */
                                //eax--->es:edi
	subl $0x1000,%eax       //eax-=0x1000,每填好一项，物理地址减0x1000
	jge 1b                  //当eax>=0x1000时,页表未填充完毕,jmp backward to 1:
                                //当eax<0x1000时,(此时eax为0x00000007)表示填好,不跳转
//填充后页表内容:
//addr      pg0                 phy addr
//0x1000    0x00000007   --->   0x00000000 - 0x00000fff
//0x1004    0x00001007   --->   0x00001000 - 0x00001fff
// ...
//0x1FF8    0x003fe007   --->   0x003fe000 - 0x003fefff
//0x1FFC    0x003ff007   --->   0x003ff000 - 0x003fffff
//addr      pg1
//0x2000    0x00400007   --->   0x00400000 - 0x00400fff
//0x2004    0x00401007   --->   0x00401000 - 0x00401fff
// ...
//0x2FF8    0x004fe007   --->   0x004fe000 - 0x004fefff
//0x2FFC    0x004ff007   --->   0x004ff000 - 0x004fffff

	xorl %eax,%eax		/* pg_dir is at 0x0000 */ //eax=0
	movl %eax,%cr3		/* cr3 - page directory start */
                                //页目录地址(0x00000000)--->CR3
	movl %cr0,%eax
	orl $0x80000000,%eax    //开启分页
	movl %eax,%cr0		/* set paging (PG) bit */
	ret			/* this also flushes prefetch-queue */

.align 2
.word 0
idt_descr:
	.word 256*8-1		# idt contains 256 entries //中断描述符表大小in bytes
	.long _idt
.align 2
.word 0
gdt_descr:
	.word 256*8-1		# so does gdt (not that that's any
	.long _gdt		# magic number, but it works for me :^)

	.align 3
_idt:	.fill 256,8,0		# idt is uninitialized

_gdt:	.quad 0x0000000000000000	/* NULL descriptor */
	.quad 0x00c09a00000007ff	/* 8Mb */ //与boot.s中设置的一样
	.quad 0x00c09200000007ff	/* 8Mb */ //与boot.s中设置的一样
	.quad 0x0000000000000000	/* TEMPORARY - don't use */
	.fill 252,8,0			/* space for LDT's and TSS's etc */
