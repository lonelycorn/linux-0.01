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
/*
在保护模式下，处理器不是用的中断向量表来处理中断的，取而代之的是中断描述符表（Interrupt Descriptor Table，IDT），中断描述符表存放的是中断门，陷阱门和任务门。
其中中断门和陷阱门是只能放在IDT中。和IVT不一样的是，IDT不要求必须位于内存的最低端。在处理器内部，有一个48位的中断描述符表寄存器（Interrupt Descriptor Table Register，IDTR），
保存着中断描述符表在内存中的线性基地址和界限，IDTR只有一个，和GDTR的储存格式是一样的。

中断描述符表IDT可以位于内存的任何地方，只要IDTR指向了它，整个终端系统就可以正常的工作。为了利用高速缓存使处理器的工作性能最大化，处理器建议IDT的基地址是8字节对齐的。
处理器复位的时候，IDTR的基地址部分是0，界限部分是0xFFFF（和GDTR是一样的）。处理器只识别256个中断，所以LDT通常只用2KB。和GDT不一样的是，IDT的第一个槽位可以不是0描述符。

在保护模式下处理器执行中断的时候，先根据相应的中断号乘以8加上IDT的基地址得到相应的中段描述符的位置（如果有页映射也是根据页的映射规则来找到相应的描述符），和通过调用门的控制转移一样，
处理器也要对中断和异常处理程序进行特权级的保护。但是在中断和异常的特权级检查中有特殊的情况。处理器在进入中断或者异常处理程序的时候，或者通过task gate发起任务切换的时候，不检查RPL。
和普通的门调用一样，CPL要在数值上小于等于目标代码段的DPL才可以执行代码段的切换，但是对于门的DPL的检查中，除了软中断int n和单步中断int3以及into引发的中断和异常外，处理器不对门的DPL进行特权级检查，
如果是以上三种中断命令引发的中断，则要求CPL<=门描述符的DPL。（主要是为了防止软中断引发的越权操作）。

如果发生了特权级的转变（比如从局部空间转移到了全局空间）。那么要进行栈切换。压栈顺序如下：

根据处理器的特权级别，从当前任务的TSS中取得栈段选择子和栈指针。处理器把旧的栈的选择子和栈指针压入新栈。如果中断处理程序的特权级别和当前特权级别一致。则不用转换栈。
处理器把EFLGAS压入栈，然后把CS压栈，然后再压栈EIP。
如果有错误代码的异常，处理器还要将错误代码压入新栈，紧挨着EIP之后。

中断门和陷阱门的区别就是对IF位的处理不同。通过中断门进入中断处理程序的收，EFLAGS寄存器的IF位被处理器自动清零。以禁止嵌套的中断，当中断返回的时候，从栈中恢复EFLAGS的原始状态。陷阱中断的优先级比较低，当通过陷阱门进入中断处理程序的时候，EFLAGS寄存器的IF位不变，以允许其他中断的优先处理。EFLAGS寄存器的IF位仅影响硬件中断，对NMI，异常和int n形式的中断不起作用。

和GDT一样，如果要访问的位置超过了IDT的界限，那么就会产生常规保护异常（#GP）。
*/

/*
在实地址模式下，80486 CPU的中断响应是根据中断源提供的中断类型号，查找中断向量表，获取中断向量，继而转去执行中断处理。

中断向量表位于内存底端的1KB RAM区，地址范围为0000H～03FFH。256种中断类型由中断类型号0～255（00H～FFH）表示。中断类型号n与其对应中断向量表的地址V的关系是：V＝4n。
约定4n＋0和4n＋1单元存放中断服务子程序的偏移地址IP的值，4n＋2和4n＋3单元存放中断服务子程序的段基址CS的值。
CPU响应中断请求后，由中断源自动给出中断类型号n送入CPU，由CPU自动完成向量表地址4n的运算，从向量表中取出中断服务子程序的入口地址送入CS和IP中，将执行的流程控制转移到中断服务子程序。

在保护模式下，为每一个中断和异常定义了一个中断描述符，来说明中断和异常服务程序的入口地址的属性，所有的中断描述符都集中存放在中断描述符表（IDT）中，由中断描述符表取代实地址模式下的中断向量表。

当CPU响应外部中断请求或执行某条指令产生异常时，根据中断或异常的类型号n，从中断描述符表（IDT）中找到相应的中断门，
由中断描述符中的段选择符指向全局描述符表（GDT）或局部描述符表（LDT）中的目标段描述符，
此目标段描述符内的段基址指向中断服务子程序代码段的基地址，由该基地址与中断描述符中的偏移量之和形成中断服务子程序的入口地址。
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
//argc, *argv[], *env[]
	pushl $0		# These are the parameters to main :-)
	pushl $0
	pushl $0
//envp=0, argc=0, argv=0
	pushl $L6		# return address for main, if it decides to.
	pushl $_main            # return address of setup_paging
	jmp setup_paging        # notice this is jmp not call!
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
.align 2 //2字节对齐
setup_paging:
	movl $1024*3,%ecx //设置计数器，清空3页内容, 0x0000-0x3FFF set to 0
	xorl %eax,%eax   //eax=0
	xorl %edi,%edi	 //edi=0        /* pg_dir is at 0x000 */
	cld;rep;stosl //edi正方向增长，eax--->es:edi
//内核只用了2个页表，只对页目录设置2项
//每个页表项(entry)占4Bytes,一张页表占1页(4KB),所以可以放1024项
//页表项定义:
//  31 - 12  | 11 - 9 | 8 |  7  | 6 | 5 |  4  |  3  | 2 | 1 | 0
//  页帧地址     AVL    G   PAT   D   A   PCD   PWT  U/S R/W  P
//  P: page exist?vl $1024,%%ecx\n
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

//填充页目录
	movl $pg0+7,_pg_dir		/* set present bit/user r/w */
	movl $pg1+7,_pg_dir+4		/*  --------- " " --------- */
//填充后页目录如下:
//addr      pg_dir
//0x0000    ($pg0+7) 0x00001007        // 页帧地址=0xFFFFF000 & 0x00001007 = 0x00001000
                                       // U/S=1, R/W=1, P=1
//0x0004    ($pg1+7) 0x00002007        // 页帧地址=0xFFFFF000 & 0x00002007 = 0x00002000
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
/*(Example: LA: 0b 0000 0000 01  00 0000 0010  0000 0000 1011)
                0x    0    0    8      0    2     0    0    B
                0b 0000 0000 01 -> page directory 1(2nd) (content=0x00002007) -> 页帧/页表地址：& 0xFFFFF000 = 0x00002000
                0b 00 0000 0010 -> page entry 2 (3rd) (content=0x00403007) -> 目标页物理地址：& 0xFFFFF000 = 0x00403000

                PA = 0x00403000 + 0x0080800B & 0x00000FFF = 0x00403 00B
                                 
*/
//填充后页表内容: (页表项内容与(&)上0xFFFFF000得到目标页所在的Physical Addr)
//addr      pg0                 phy addr range
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
	.long _idt              # address of _idt
/*
IDT Reg:
bits:             47 - 16                   |        15 - 0
      32-bit Linear Base Address of IDT         Table limit/size
*/

.align 2
.word 0
gdt_descr:
	.word 256*8-1		# so does gdt (not that that's any
	.long _gdt		# magic number, but it works for me :^)
/*
GDT Reg:
bits:             47 - 16                   |        15 - 0
      32-bit Linear Base Address of GDT         Table limit/size
*/
	.align 3
_idt:	.fill 256,8,0		# idt is uninitialized

_gdt:	.quad 0x0000000000000000	/* NULL descriptor */
	.quad 0x00c09a00000007ff	/* 8Mb */ //与boot.s中设置的一样
	.quad 0x00c09200000007ff	/* 8Mb */ //与boot.s中设置的一样
	.quad 0x0000000000000000	/* TEMPORARY - don't use */
	.fill 252,8,0			/* space for LDT's and TSS's etc */

/*
head.s 运行完毕后，内存组织如下
0x00000000 - 0x00004000 page table
0x00004000 : after_page_tables
             ignore_int
             setup_paging
             padding

0x00004??? : (word=2bytes) 0
             (word=2bytes) 256*8-1
             (long=8bytes) address of _idt
             padding

             (word=2bytes) 0
             (word=2bytes) 256*8-1
             (long=8bytes) address of _gdt             
             padding

       _idt  : 0 (occupy 256*8bytes)
       _gdt  : 4 descriptors above，occupy 4*8bytes
               0 (occupy 252*8bytes)
             
*/
