//利用iret实现内核态到用户态转换
#define move_to_user_mode() \
__asm__ ("movl %%esp,%%eax\n\t" \    //eax=esp，保存堆栈指针，因为以下代码一直操作堆栈，iret以后堆栈指针应该跟现时一样
	"pushl $0x17\n\t" \          //SS堆栈选择符入栈
	"pushl %%eax\n\t" \          //已保存的堆栈指针值入栈
	"pushfl\n\t" \               //标志寄存器内容入栈
	"pushl $0x0f\n\t" \          //Task0代码段选择符入栈
	"pushl $1f\n\t" \            //下面标号1的偏移地址入栈
	"iret\n" \                   //执行中断返回，跳到标号1处执行
	"1:\tmovl $0x17,%%eax\n\t" \ //eax=0x17
	"movw %%ax,%%ds\n\t" \       //数据段寄存器全部指向局部描述符表的数据段
	"movw %%ax,%%es\n\t" \       //
	"movw %%ax,%%fs\n\t" \       //
	"movw %%ax,%%gs" \           //
	:::"ax")

#define sti() __asm__ ("sti"::)
#define cli() __asm__ ("cli"::)
#define nop() __asm__ ("nop"::)

#define iret() __asm__ ("iret"::)

//设置门描述符宏
//根据参数中的中断或异常处理例程addr、门描述符类型type和特权信息dpl，设置位于地址gate_addr处的描述符
#define _set_gate(gate_addr,type,dpl,addr) \
__asm__ ("movw %%dx,%%ax\n\t" \
	"movw %0,%%dx\n\t" \
	"movl %%eax,%1\n\t" \
	"movl %%edx,%2" \
	: \ //无输出操作数
	: "i" ((short) (0x8000+(dpl<<13)+(type<<8))), \ //立即整数操作数
	"o" (*((char *) (gate_addr))), \                //同一段内内存操作数
	"o" (*(4+(char *) (gate_addr))), \
	"d" ((char *) (addr)),"a" (0x00080000))         //edx=(char *) (addr), eax=0x00080000

#define set_intr_gate(n,addr) \
	_set_gate(&idt[n],14,0,addr)

#define set_trap_gate(n,addr) \
	_set_gate(&idt[n],15,0,addr)

#define set_system_gate(n,addr) \
	_set_gate(&idt[n],15,3,addr)

//设置段描述符宏
#define _set_seg_desc(gate_addr,type,dpl,base,limit) {\
	*(gate_addr) = ((base) & 0xff000000) | \
		(((base) & 0x00ff0000)>>16) | \
		((limit) & 0xf0000) | \
		((dpl)<<13) | \
		(0x00408000) | \
		((type)<<8); \
	*((gate_addr)+1) = (((base) & 0x0000ffff)<<16) | \
		((limit) & 0x0ffff); }

//在gdt中设置tss或ldt描述符宏
#define _set_tssldt_desc(n,addr,type) \
__asm__ ("movw $104,%1\n\t" \
	"movw %%ax,%2\n\t" \
	"rorl $16,%%eax\n\t" \
	"movb %%al,%3\n\t" \
	"movb $" type ",%4\n\t" \
	"movb $0x00,%5\n\t" \
	"movb %%ah,%6\n\t" \
	"rorl $16,%%eax" \
	::"a" (addr), "m" (*(n)), "m" (*(n+2)), "m" (*(n+4)), \
	 "m" (*(n+5)), "m" (*(n+6)), "m" (*(n+7)) \
	)

#define set_tss_desc(n,addr) _set_tssldt_desc(((char *) (n)),addr,"0x89")
#define set_ldt_desc(n,addr) _set_tssldt_desc(((char *) (n)),addr,"0x82")
