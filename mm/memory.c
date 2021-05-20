#include <signal.h>

#include <linux/config.h>
#include <linux/head.h>
#include <linux/kernel.h>
#include <asm/system.h>

//本文件的函数都是给内核用的，必须运行在内核态, CPL=0
//memory layout
//---------------------------------
//|内核程序 | 高速缓冲 | 主内存区 |
//| 0K-640K |  640K-2M |   2M-8M  |
//---------------------------------
//buffer,高速缓冲区是用于临时存放数据的地方，比如磁盘读进来的数据，1K粒度
//主内存区是按页划分
//
int do_exit(long code);

//重新加载页目录寄存器，页目录在0x0处
#define invalidate() \
__asm__("movl %%eax,%%cr3"::"a" (0))

//refer to include/linux/config.h
//HIGH_MEMORY 是 8MB，BUFFER_END 是 2MB。BUFFER_END 是缓冲区末端。
#if (BUFFER_END < 0x100000)
#define LOW_MEM 0x100000    //LOW_MEM 位于 2MB 处
#else
#define LOW_MEM BUFFER_END  //LOW_MEM 位于 buffer 后
#endif

/* these are not to be changed - thay are calculated from the above */
#define PAGING_MEMORY (HIGH_MEMORY - LOW_MEM)
#define PAGING_PAGES (PAGING_MEMORY/4096)
//将主内存区分为 4K 为一段来管理，mem_map 的一个 entry 项代表一个 4K 的页，当其值为 0 时，表示该页没有被占用，大于 0 表示被占用，大于 1 表示页面被共享的次数
//MAP_NR 求给定地址 addr 所在页面在 mem_map 数组的索引。
static unsigned short mem_map [ PAGING_PAGES ] = {0,};
#define MAP_NR(addr) (((addr)-LOW_MEM)>>12)

#if (PAGING_PAGES < 10)
#error "Won't work" //系统内存小于10页编译不过
#endif

//从from处复制一页到to处
#define copy_page(from,to) \
__asm__("cld ; rep ; movsl"::"S" (from),"D" (to),"c" (1024):"cx","di","si")

//注意，页目录，页表1，页表2都已经在head.s中填充
//也就是0-8MB的物理内存已经映射完毕可以由内核管理
//而且从内核角度看，虚拟地址与物理地址完全是一一对应的

/*
 * Get physical address of first (actually last :-) free page, and mark it
 * used. If no free pages left, return 0.
 */
/* 在主内存区中申请一页空闲物理页面
 * input: %1(ax=0);
 *        %2(LOW_MEM)内存字节位图管理的起始位置, "i"表示立即数;
 *        %3(cx = PAGING_PAGES);
 *        %4(edi = mem_map + PAGING_PAGES - 1) 指向mem_map[]内存字节位图的最后一个字节。
 * output: %0 (ax = 物理页面起始地址)
 */
unsigned long get_free_page(void)
{
//定义 __res 变量，register 关键字指示编译器将 __res 变量放在寄存器中，后面的 asm("ax") 指定寄存器为 ax 寄存器
register unsigned long __res asm("ax");

__asm__("std ; repne ; scasw\n\t"
//recall: 串操作指令中，源操作数和目的操作数分别使用寄存器 (e)si 和 (e)di 进行间接寻址，每执行一次串操作，源指针 (e)si 和目的指针 (e)di 将自动进行修改：±1、±2、±4，其对应的分别是字节操作、字操作和双字操作。
//这里的 ±1、±2、±4 是加还是减就是取决于 DF 方向标志位，当 DF=1 时为减，当 DF=0 时为加（与 std 指令相对的 cld 指令）。

//REPNE 指令一般用来扫描字符串，它用来重复后一个指令，这里是 scasw 指令，REPNE 的重复条件是 ZF=0 且 ecx > 0，每循环执行一次，ecx 的值自动减 1
//所以如果当它后面的指令扫描字符串时，当它扫到字符串与需要检测的字符串相等时或者扫描了 ecx 次之后停止扫描. 扫描到相等则会置 ZF=1

//最后一个指令 scasw，最后那个 w 表示字，两个字节，这个指令减 ax 寄存器和 di 寄存器的值对比，每比较一次，di 寄存器依赖 DF 的值自动增加或减小，增加或减小的值根据指令是对word还是对byte操作决定
//如果指令是 scasw 则为 2，如果是 scasb 则为 1。

//ax 寄存器的初始赋值=0，"0" (0) 中 "0"(或者“”)表示使用输出操作数中同一位置的寄存器，即寄存器 ax，(0) 表示赋值为 0；di 寄存器的初始赋值："D" (mem_map+PAGING_PAGES-1) 中的 "D" 表示引用寄存器 edi，后面的 (mem_map+PAGING_PAGES-1) 表示赋值为 mem_map 数组的最后一个索引，所以查找空闲页面是从最后一页开始查找的。
//所以这行是在mem_map[]中，从后向前找0，从而确定没有被使用的页面在mem_map[]中的索引(ecx)

	"jne 1f\n\t"                   // jne 1f 表示 ZF=0 则跳转到 1 标签，ZF=0 表示没有找到空闲页，跳转到 1 标签，结束嵌入汇编，然后直接返回 __res，也就是 0
	"movw $1,2(%%edi)\n\t"         // 1 => [2 + edi]，edi 寄存器加 2 是因为执行完 scasw 指令之后 edi 会减 2，加 2 回到找到的空闲页的地址，这里找到空闲页，将对应的 mem_map 的值置为 1。
	"sall $12,%%ecx\n\t"           // 将 ecx 左移 12 位，ecx 的值会随着 repne 指令变化，找到空闲页之后，ecx 为空闲页的 mem_map 数组索引，左移 12 位即是 *4096, 得到空闲页面相对于主内存区起始位置的起始地址
	"movl %%ecx,%%edx\n\t"         // edx=ecx
	"addl %2,%%edx\n\t"            // edx 加上 LOW_MEM(主内存区起始地址) 则为空闲页面的实际物理地址, 由于内核是线性一一映射，所以 edx 既是物理地址也是线性地址。
	"movl $1024,%%ecx\n\t"         // ecx = 1024, 
	"leal 4092(%%edx),%%edi\n\t"   // leal 指令将 edi 指向空闲页的最后倒数第 4 个字节，stosl 指令将 eax 中的值保存到 es:edi 指向的地址中
	                               // 这里 es:edi，es 表示的是将要访问哪一个数据段，在保护模式下只有一个数据段，所以 es 的值默认和 ds 一样，如果是在实模式中，要自己设置 es 的值，
				       // 由于 eax 的值为0，所以这段代码目的是将物理页面内容清零。

	"rep ; stosl\n\t"              // 重复stosl ecx次，stosl: eax(=0)--->es:edi, 进行清0操作(不会有缺页异常发生，因为已经映射到内核的页目录/页表中)
	"movl %%edx,%%eax\n"           // 设置 eax 的值为空闲页面的物理地址，也就是设置了 __res 的值
	"1:"
	:"=a" (__res)
	:"0" (0),"i" (LOW_MEM),"c" (PAGING_PAGES),
	"D" (mem_map+PAGING_PAGES-1)
	:"di","cx","dx");
return __res;
//这段代码完全可以用C写，不知道发什么神经, 无外乎就是算出空闲页的实际物理地址，清零
}

/*
 * Free a page of memory at physical address 'addr'. Used by
 * 'free_page_tables()'
 */
void free_page(unsigned long addr)
{
	if (addr<LOW_MEM) return; //如果要释放的地址小于 LOW_MEM，表示要释放的地址不位于主内存区，对此不做处理。
	if (addr>HIGH_MEMORY)     //如果要释放的地址大于 HIGH_MEMORY（等于 HIGH_MEMORY 也是不合法的, bug, should be >=)，表示要释放的地址不在于我们所认知的物理地址范围之内，所以 panic
		panic("trying to free nonexistent page");
	addr -= LOW_MEM; // 求出 addr 在 mem_map 数组中的索引，也就是 MAP_NR 宏实现的功能，不清楚这里为啥不直接使用这个宏。
	addr >>= 12;     // 是不是有病？
	if (mem_map[addr]--) return; //将 mem_map 中对应的索引的值减 1，即该页面数的引用减少 1。
	mem_map[addr]=0; //如果跑到这一行则表示原本 mem_map[addr] == 0，但由于上一行 if 语句中将其减了 1， 所以这里将其直接还原成 0，然后 panic
	panic("trying to free free page");
}

/*
 * This function frees a continuos block of page tables, as needed
 * by 'exit()'. As does copy_page_tables(), this handles only 4Mb blocks.
 */
int free_page_tables(unsigned long from,unsigned long size)
{
	unsigned long *pg_table;
	unsigned long * dir, nr;
//线性内存的起始地址 from，from 至少 4M 对齐，因为这个函数要释放的是整个页表，一个页表最大能表示 4M 的地址，所以要释放从 from 起的连续 4M 线性地址空间，那么 from 就要 4M 对齐。
	if (from & 0x3fffff)
		panic("free_page_tables called with wrong alignment");
//如果 from 是 0 的话，那就是相当于要释放掉内核段所在的 4M 线性空间，那当然是不行的了。
	if (!from)
		panic("Trying to free up swapper memory space");
//size 向上取整到 4M 对齐，并且除以 4M，譬如 size 传参为 5M，那么向上取整就是 8M，就是 2 个 4M，所以 size 的值就是 2.
	size = (size + 0x3fffff) >> 22;
//这行代码涉及到分页系统中，线性地址如何通过页目录和页表定位到物理地址。
//dir 最终得到的值是 from 对应的页目录的索引的地址
//线性地址的高 10 位指示页目录项，所以将 from 右移 22 位就得到了在页目录的第几项，又因每项占 4 个字节，所以还得乘以 4，也就是还要左移 2 位，才能得到 from 在页目录表中的对应索引的地址
//总的来说，就是先右移 22 位，再左移 2 位，这个过程相当于 dir = (unsigned long *) ((from>>20) & 0xffc)
	dir = (unsigned long *) ((from>>20) & 0xffc); /* _pg_dir = 0 */
//size-->0 is (size > 0), size --
	for ( ; size-->0 ; dir++) {
		if (!(1 & *dir)) //Present bit.
			continue;
		//*dir 取得页目录项里面的内容，也就是页表基址和页表属性，低 12 位是页表属性，所以要与上 0xfffff000
		//pg_table就是页表所在的虚拟地址也是物理地址
		pg_table = (unsigned long *) (0xfffff000 & *dir);
                //
		for (nr=0 ; nr<1024 ; nr++) {
			if (1 & *pg_table) //p位为1
				free_page(0xfffff000 & *pg_table); //释放该页表项对应的那一页
			*pg_table = 0; //清除页表项
			pg_table++;
		}
		free_page(0xfffff000 & *dir); //释放页表所在的那一页
		*dir = 0;
	}
	invalidate(); //因为修改了原本有效的页表，所以需要刷新 TLB：重新加载 cr3 控制寄存器。
	return 0;
}

/*
 *  Well, here is one of the most complicated functions in mm. It
 * copies a range of linerar addresses by copying only the pages.
 * Let's hope this is bug-free, 'cause this one I don't want to debug :-)
 *
 * Note! We don't copy just any chunks of memory - addresses have to
 * be divisible by 4Mb (one page-directory entry), as this makes the
 * function easier. It's used only by fork anyway.
 *
 * NOTE 2!! When from==0 we are copying kernel space for the first
 * fork(). Then we DONT want to copy a full page-directory entry, as
 * that would lead to some serious memory waste - we just copy the
 * first 160 pages - 640kB. Even that is more than we need, but it
 * doesn't take any more memory - we don't copy-on-write in the low
 * 1 Mb-range, so the pages can be shared with the kernel. Thus the
 * special case for nr=xxxx.
 */
//这个函数主要是供 fork() 函数使用，给定映射的源线性地址 from 、目的线性地址 to 和线性空间大小 size
int copy_page_tables(unsigned long from,unsigned long to,long size)
{
	unsigned long * from_page_table;
	unsigned long * to_page_table;
	unsigned long this_page;
	unsigned long * from_dir, * to_dir;
	unsigned long nr;
//检测源地址from和目的地址to的有效性。源地址和目的地址都需要在4MB的内存边界上。
//因为一个页表的1024项可以管理4MB的内存。
//只有满足这个条件才能保证从一个页表的第1项开始复制页表项并且新页表的最初所有项都是有效的。
	if ((from&0x3fffff) || (to&0x3fffff))
		panic("copy_page_tables called with wrong alignment");
//取得源地址在页目录的索引的地址
	from_dir = (unsigned long *) ((from>>20) & 0xffc); /* _pg_dir = 0 */
//取得目的地址在页目录的索引的地址
	to_dir = (unsigned long *) ((to>>20) & 0xffc);
//计算出size大小字节对应的内存块占用的页表数（即目录项数）
	size = ((unsigned) (size+0x3fffff)) >> 22;
//开始对每个页目录项一次申请1页内存来保存对应的页表，并且开始页表项复制操作。
	for( ; size-->0 ; from_dir++,to_dir++) {
		//如果目的目录项指定的页表存在，panic
		if (1 & *to_dir)
			panic("copy_page_tables: already exist");
		//如果源目录项无效，跳过，进入下一个循环处理下一目录项
		if (!(1 & *from_dir))
			continue;
		//取得源目录项中页表对应的地址
		from_page_table = (unsigned long *) (0xfffff000 & *from_dir);
		//为了保存目的目录项对应的页表，在主内存区申请1页空闲内存页，即新申请的页是拿来存页表的
		if (!(to_page_table = (unsigned long *) get_free_page()))
			return -1;	/* Out of memory, see freeing */
		//置属性位，7表示该页表映射的内存页面是用户级的，可读，可写，presented
		*to_dir = ((unsigned long) to_page_table) | 7;
		//如果 from 是 0 的话，表示要复制的是内核段，这段最多只要 640K 内存肯定完全覆盖内核了，所以只要复制 640K / 4K = 160 = 0xA0 项，而其他的要复制 1024 项
		nr = (from==0)?0xA0:1024;
		for ( ; nr-- > 0 ; from_page_table++,to_page_table++) {
			//this_page是页面起始地址和属性位
			this_page = *from_page_table;
			//如果该页无效
			if (!(1 & this_page))
				continue;
			this_page &= ~2; //~2=0xFFFFFFFD, 把属性位第2位置0,该页面只读
			*to_page_table = this_page; //在新页表中填充页表项，该页表项mapping到源地址from对应的页面，但是这里是只读的。
                                                    //写时复制(copy-on-write)，简单的说， fork 的时候子进程和父进程数据段共享一个物理页，它们的页面属性设置为只读，
						    //只有当写的时候，重新分配一页，然后设置属性为可读写。
			                            //对该页写触发写保护异常，trap触发处理

			//低于 LOW_MEM 是内核使用的，一直都是共享的，不用修改这个属性。mem_map 数组是管理 LOW_MEM 以上内存的。
			if (this_page > LOW_MEM) {
				*from_page_table = this_page;
				this_page -= LOW_MEM; //有宏不用
				this_page >>= 12;     //有jb病
				mem_map[this_page]++;
			}
		}
	}
	invalidate();//刷新TLB
	return 0;
}

/*
 * This function puts a page in memory at the wanted address.
 * It returns the physical address of the page gotten, 0 if
 * out of memory (either when trying to access page-table or
 * page.)
 */
unsigned long put_page(unsigned long page,unsigned long address)
{
	unsigned long tmp, *page_table;

/* NOTE !!! This uses the fact that _pg_dir=0 */

	if (page < LOW_MEM || page > HIGH_MEMORY)
		printk("Trying to put page %p at %p\n",page,address);
	if (mem_map[(page-LOW_MEM)>>12] != 1)
		printk("mem_map disagrees with %p at %p\n",page,address);
	page_table = (unsigned long *) ((address>>20) & 0xffc);
	if ((*page_table)&1)
		page_table = (unsigned long *) (0xfffff000 & *page_table);
	else {
		if (!(tmp=get_free_page()))
			return 0;
		*page_table = tmp|7;
		page_table = (unsigned long *) tmp;
	}
	page_table[(address>>12) & 0x3ff] = page | 7;
	return page;
}

void un_wp_page(unsigned long * table_entry)
{
	unsigned long old_page,new_page;

	old_page = 0xfffff000 & *table_entry;
	if (old_page >= LOW_MEM && mem_map[MAP_NR(old_page)]==1) {
		*table_entry |= 2;
		return;
	}
	if (!(new_page=get_free_page()))
		do_exit(SIGSEGV);
	if (old_page >= LOW_MEM)
		mem_map[MAP_NR(old_page)]--;
	*table_entry = new_page | 7;
	copy_page(old_page,new_page);
}	

/*
 * This routine handles present pages, when users try to write
 * to a shared page. It is done by copying the page to a new address
 * and decrementing the shared-page counter for the old page.
 */
void do_wp_page(unsigned long error_code,unsigned long address)
{
	un_wp_page((unsigned long *)
		(((address>>10) & 0xffc) + (0xfffff000 &
		*((unsigned long *) ((address>>20) &0xffc)))));

}

void write_verify(unsigned long address)
{
	unsigned long page;

	if (!( (page = *((unsigned long *) ((address>>20) & 0xffc)) )&1))
		return;
	page &= 0xfffff000;
	page += ((address>>10) & 0xffc);
	if ((3 & *(unsigned long *) page) == 1)  /* non-writeable, present */
		un_wp_page((unsigned long *) page);
	return;
}

void do_no_page(unsigned long error_code,unsigned long address)
{
	unsigned long tmp;

	if (tmp=get_free_page())
		if (put_page(tmp,address))
			return;
	do_exit(SIGSEGV);
}

void calc_mem(void)
{
	int i,j,k,free=0;
	long * pg_tbl;

	for(i=0 ; i<PAGING_PAGES ; i++)
		if (!mem_map[i]) free++;
	printk("%d pages free (of %d)\n\r",free,PAGING_PAGES);
	for(i=2 ; i<1024 ; i++) {
		if (1&pg_dir[i]) {
			pg_tbl=(long *) (0xfffff000 & pg_dir[i]);
			for(j=k=0 ; j<1024 ; j++)
				if (pg_tbl[j]&1)
					k++;
			printk("Pg-dir[%d] uses %d pages\n",i,k);
		}
	}
}
