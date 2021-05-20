/*
 *  'fork.c' contains the help-routines for the 'fork' system call
 * (see also system_call.s), and some misc functions ('verify_area').
 * Fork is rather simple, once you get the hang of it, but the memory
 * management can be a bitch. See 'mm/mm.c': 'copy_page_tables()'
 */
#include <errno.h>

#include <linux/sched.h>
#include <linux/kernel.h>
#include <asm/segment.h>
#include <asm/system.h>

extern void write_verify(unsigned long address);

long last_pid=0;

/// check if [addr, addr + size) is writable
void verify_area(void * addr,int size)
{
	unsigned long start;

	start = (unsigned long) addr;
	size += start & 0xfff; // offset within the page
	start &= 0xfffff000; /// start now points to page head
	start += get_base(current->ldt[2]); // converts to linear address using Local Descriptor Table
	while (size>0) {
		size -= 4096;
		write_verify(start);
		start += 4096;
	}
}

//nr是新任务号；p是新任务数据结构指针。该函数位新任务在线性地址空间中设置代码段和数据段基址、限长，并复制页表
//Linux采用copy-on-write技术，所以这里仅为新进程设置自己的页目录和页表项，没有实际为新进程分配物理内存页面。
//新任务与父任务共享所有内存页面。
int copy_mem(int nr,struct task_struct * p)
{
	unsigned long old_data_base,new_data_base,data_limit;
	unsigned long old_code_base,new_code_base,code_limit;
//首先取当前进程LDT中代码段描述符和数据段描述符项中的段限长
//0x0f是代码段选择子，0x17是数据段选择子
	code_limit=get_limit(0x0f);
	data_limit=get_limit(0x17);
//取当前进程LDT中代码段描述符和数据段描述符项中的段基址
//LDT表和GDT表在0.01是同一个表，第
	old_code_base = get_base(current->ldt[1]);
	old_data_base = get_base(current->ldt[2]);
//在0.01中，代码段和数据段是一起的，所以如果基址不同就panic
	if (old_data_base != old_code_base)
		panic("We don't support separate I&D");
//数据段长小于代码段长panic
	if (data_limit < code_limit)
		panic("Bad data_limit");
//设置创建中的新任务在线性地址空间中的基地址位64MB*任务号
//内核强制每个进程的虚拟地址空间为64m,一共就可以拥有4g/64m=64个进程
//有点滥用虚拟线性地址空间,但如果不这么做，则各任务间会打架
//根本原因是cr3寄存器在所有任务中都是0，指向同一块页目录
//页目录一样，代表并没有实现虚拟地址分隔。这里是求其次的做法。
//要实现虚存，必须每个任务都有自己的页目录和页表项。
//现在是只有自己的页表项，没有单独的页目录项。页表项的分割是
//通过耗费虚拟地址实现的。而且，不同进程之间是互相可以看到对方
//的页表的。进程隔离完全没有实现。

//以下摘录自网络，术语不完全正确，但道理是对的
//内核将不同的进程映射 到不同的“线性空间”，然后再通过不同的页目录项映射到物理内存，这里有必要说一下线性空间的概念，intel的分页机制使得cpu拥有4g大小的线性空间，从0到4g，而程序中用到的地址是虚拟地址而不是线性地址，比如进程0和进程1 的虚拟地址空间都是0到64m，但是由于所有进程共享这4g的线性空间，则进程0拥有0到63m的线性空间，而进程1拥有接下来64m到127m的线性空 间，分段机制把地址从虚拟空间映射到线性空间而分页机制将地址从线性空间映射到物理地址空间，0.01内核就是完全按照这种映射接力来完成的，不知道这是不是intel的意愿。现在，不管是linux还是windows都是使用平坦模式来绕开了分段，这样每个进程都可以独享4g的虚拟地址空间了，intel提供了一种机制可以使得cpu寻址4g空间，而且提供cr3页目录寄存器，这就是说intel的本意是让进程独享4g空间的，可是估计当时 李纳斯认为没有那需求，而且共享页表可以省去切换开销，所以就选择了这种现在看来很奇怪的方式。

	new_data_base = new_code_base = nr * 0x4000000;
//用该值设置新进程LDT中代码段和数据段描述符中的基地址
	set_base(p->ldt[1],new_code_base);
	set_base(p->ldt[2],new_data_base);
//复制当前进程(父进程)的页目录表项和页表项。此时子进程共享父进程的内存页面。
	if (copy_page_tables(old_data_base,new_data_base,data_limit)) {
		free_page_tables(new_data_base,data_limit);
		return -ENOMEM;
	}
	return 0;
}

/*
 *  Ok, this is the main fork-routine. It copies the system process
 * information (task[nr]) and sets up the necessary registers. It
 * also copies the data segment in it's entirety.
 */
//nr是任务号，由find_empty_process()找出，由_sys_fork压入栈传入
int copy_process(int nr,long ebp,long edi,long esi,long gs,long none,
		long ebx,long ecx,long edx,
		long fs,long es,long ds,
		long eip,long cs,long eflags,long esp,long ss)
{
	struct task_struct *p;
	int i;
	struct file *f;

	p = (struct task_struct *) get_free_page();
	if (!p)
		return -EAGAIN;
	//copy整个task_struct内容
	*p = *current;	/* NOTE! this doesn't copy the supervisor stack */
	//对复制进来的内容进行必要修改
	//注意cr3 field没有修改，默认是0，所以所有任务的页目录都是一样的，在0x0处
	p->state = TASK_RUNNING; //设置新进程状态为running以便可以被调度执行
	p->pid = last_pid;
	p->father = current->pid;
	p->counter = p->priority;
	p->signal = 0;
	p->alarm = 0;
	p->leader = 0;		/* process leadership doesn't inherit */
	p->utime = p->stime = 0;
	p->cutime = p->cstime = 0;
	p->start_time = jiffies;
	p->tss.back_link = 0;
	//由于系统给任务结构p分配了1页新内存，所以PAGE_SIZE + (long) p让esp0刚好指向该页顶端。ss0:esp0用作程序在内核态执行是的栈
	p->tss.esp0 = PAGE_SIZE + (long) p; //任务内核态栈指针，每个任务都有自己独立的内核态栈 (esp0对应cpl=0)
	p->tss.ss0 = 0x10; //内核态栈的段选择符，0x10 = 0b10 000，索引是b10=0x2，GDT中第3个，与内核数据段相同，段基址是0
	p->tss.eip = eip;
	p->tss.eflags = eflags;
	p->tss.eax = 0; //fork()返回时，新进程的pid=0
	p->tss.ecx = ecx;
	p->tss.edx = edx;
	p->tss.ebx = ebx;
	p->tss.esp = esp; //设置cpl=3时候的栈的指针
	p->tss.ebp = ebp;
	p->tss.esi = esi;
	p->tss.edi = edi;
	p->tss.es = es & 0xffff; //段寄存器仅16位有效
	p->tss.cs = cs & 0xffff;
	p->tss.ss = ss & 0xffff; //设置cpl=3时候的栈段选择符
	p->tss.ds = ds & 0xffff;
	p->tss.fs = fs & 0xffff;
	p->tss.gs = gs & 0xffff;
	p->tss.ldt = _LDT(nr); //任务局部描述符表的选择符(LDT描述符在GDT内)，每个任务都有自己的局部描述符，当段选择子中TI=1时，使用LDT
	//CPU切换任务时，会自动从TSS中把LDT段描述符的选择子加载到LDTR中
	p->tss.trace_bitmap = 0x80000000;
	if (last_task_used_math == current)
		__asm__("fnsave %0"::"m" (p->tss.i387));
	//复制页表
	if (copy_mem(nr,p)) {
		free_page((long) p);
		return -EAGAIN;
	}
	//父进程如有文件打开，修改计数
	for (i=0; i<NR_OPEN;i++)
		if (f=p->filp[i])
			f->f_count++;
	if (current->pwd)
		current->pwd->i_count++;
	if (current->root)
		current->root->i_count++;
	//GDT表中设置新任务TSS段和LDT段描述符项
	//gdt+(nr<<1)+FIRST_TSS_ENTRY 是新进程的TSS描述符在GDT表中位置
	//gdt+(nr<<1)+FIRST_LDT_ENTRY 是新进程的LDT描述符在GDT表中位置
	set_tss_desc(gdt+(nr<<1)+FIRST_TSS_ENTRY,&(p->tss));
	set_ldt_desc(gdt+(nr<<1)+FIRST_LDT_ENTRY,&(p->ldt));
	//将新分配的一页，存放刚刚初始化好的任务结构放入task数组
	task[nr] = p;	/* do this last, just in case */
	return last_pid;
}

//为新进程取得不重复的进程号last_pid
int find_empty_process(void)
{
	int i;

	repeat: //你他妈用while true会死啊？
		if ((++last_pid)<0) last_pid=1;
		for(i=0 ; i<NR_TASKS ; i++)
			if (task[i] && task[i]->pid == last_pid) goto repeat;
	for(i=1 ; i<NR_TASKS ; i++)
		if (!task[i])
			return i;
	return -EAGAIN;
}
