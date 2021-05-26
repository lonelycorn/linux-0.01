#include <errno.h>
#include <sys/stat.h>
#include <a.out.h>

#include <linux/fs.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/mm.h>
#include <asm/segment.h>

extern int sys_exit(int exit_code);
extern int sys_close(int fd);

/*
 * MAX_ARG_PAGES defines the number of pages allocated for arguments
 * and envelope for the new program. 32 should suffice, this gives
 * a maximum env+arg of 128kB !
 */
#define MAX_ARG_PAGES 32

#define cp_block(from,to) \
__asm__("pushl $0x10\n\t" \
	"pushl $0x17\n\t" \
	"pop %%es\n\t" \
	"cld\n\t" \
	"rep\n\t" \
	"movsl\n\t" \
	"pop %%es" \
	::"c" (BLOCK_SIZE/4),"S" (from),"D" (to) \
	:"cx","di","si")

/*
 * read_head() reads blocks 1-6 (not 0). Block 0 has already been
 * read for header information.
 */
int read_head(struct m_inode * inode,int blocks)
{
	struct buffer_head * bh;
	int count;

	if (blocks>6)
		blocks=6;
	for(count = 0 ; count<blocks ; count++) {
		if (!inode->i_zone[count+1])
			continue;
		if (!(bh=bread(inode->i_dev,inode->i_zone[count+1])))
			return -1;
		cp_block(bh->b_data,count*BLOCK_SIZE);
		brelse(bh);
	}
	return 0;
}

int read_ind(int dev,int ind,long size,unsigned long offset)
{
	struct buffer_head * ih, * bh;
	unsigned short * table,block;

	if (size<=0)
		panic("size<=0 in read_ind");
	if (size>512*BLOCK_SIZE)
		size=512*BLOCK_SIZE;
	if (!ind)
		return 0;
	if (!(ih=bread(dev,ind)))
		return -1;
	table = (unsigned short *) ih->b_data;
	while (size>0) {
		if (block=*(table++))
			if (!(bh=bread(dev,block))) {
				brelse(ih);
				return -1;
			} else {
				cp_block(bh->b_data,offset);
				brelse(bh);
			}
		size -= BLOCK_SIZE;
		offset += BLOCK_SIZE;
	}
	brelse(ih);
	return 0;
}

/*
 * read_area() reads an area into %fs:mem.
 */
int read_area(struct m_inode * inode,long size)
{
	struct buffer_head * dind;
	unsigned short * table;
	int i,count;

	if ((i=read_head(inode,(size+BLOCK_SIZE-1)/BLOCK_SIZE)) ||
	    (size -= BLOCK_SIZE*6)<=0)
		return i;
	if ((i=read_ind(inode->i_dev,inode->i_zone[7],size,BLOCK_SIZE*6)) ||
	    (size -= BLOCK_SIZE*512)<=0)
		return i;
	if (!(i=inode->i_zone[8]))
		return 0;
	if (!(dind = bread(inode->i_dev,i)))
		return -1;
	table = (unsigned short *) dind->b_data;
	for(count=0 ; count<512 ; count++)
		if ((i=read_ind(inode->i_dev,*(table++),size,
		    BLOCK_SIZE*(518+count))) || (size -= BLOCK_SIZE*512)<=0)
			return i;
	panic("Impossibly long executable");
}

/*
 * create_tables() parses the env- and arg-strings in new user
 * memory and creates the pointer tables from them, and puts their
 * addresses on the "stack", returning the new stack pointer value.
 */
static unsigned long * create_tables(char * p,int argc,int envc)
{
	unsigned long *argv,*envp;
	unsigned long * sp;

	sp = (unsigned long *) (0xfffffffc & (unsigned long) p);
	sp -= envc+1;
	envp = sp;
	sp -= argc+1;
	argv = sp;
	put_fs_long((unsigned long)envp,--sp);
	put_fs_long((unsigned long)argv,--sp);
	put_fs_long((unsigned long)argc,--sp);
	while (argc-->0) {
		put_fs_long((unsigned long) p,argv++);
		while (get_fs_byte(p++)) /* nothing */ ;
	}
	put_fs_long(0,argv);
	while (envc-->0) {
		put_fs_long((unsigned long) p,envp++);
		while (get_fs_byte(p++)) /* nothing */ ;
	}
	put_fs_long(0,envp);
	return sp;
}

/*
 * count() counts the number of arguments/envelopes
 */
static int count(char ** argv)
{
	int i=0;
	char ** tmp;

	if (tmp = argv)
		while (get_fs_long((unsigned long *) (tmp++)))
			i++;

	return i;
}

/*
 * 'copy_string()' copies argument/envelope strings from user
 * memory to free pages in kernel mem. These are in a format ready
 * to be put directly into the top of new user memory.
 */
static unsigned long copy_strings(int argc,char ** argv,unsigned long *page,
		unsigned long p)
{
	int len,i;
	char *tmp;

	while (argc-- > 0) {
		if (!(tmp = (char *)get_fs_long(((unsigned long *) argv)+argc)))
			panic("argc is wrong");
		len=0;		/* remember zero-padding */
		do {
			len++;
		} while (get_fs_byte(tmp++));
		if (p-len < 0)		/* this shouldn't happen - 128kB */
			return 0;
		i = ((unsigned) (p-len)) >> 12;
		while (i<MAX_ARG_PAGES && !page[i]) {
			if (!(page[i]=get_free_page()))
				return 0;
			i++;
		}
		do {
			--p;
			if (!page[p/PAGE_SIZE])
				panic("nonexistent page in exec.c");
			((char *) page[p/PAGE_SIZE])[p%PAGE_SIZE] =
				get_fs_byte(--tmp);
		} while (--len);
	}
	return p;
}

static unsigned long change_ldt(unsigned long text_size,unsigned long * page)
{
	unsigned long code_limit,data_limit,code_base,data_base;
	int i;

	//见sched.h
	//ldt[1]是代码段描述符,ldt[2]是代码段描述符
	//ldt[0]保留不用
	//代码段向上一页取整对齐页大小
	code_limit = text_size+PAGE_SIZE -1;
	code_limit &= 0xFFFFF000;
	//数据段直接64mb
	data_limit = 0x4000000;
	code_base = get_base(current->ldt[1]);
	data_base = code_base;
	//修改ldt
	set_base(current->ldt[1],code_base);
	set_limit(current->ldt[1],code_limit);
	set_base(current->ldt[2],data_base);
	set_limit(current->ldt[2],data_limit);
/* make sure fs points to the NEW data segment */
	//fs中放入ldt数据段描述符的选择符，0x17=10 111b,
	//10b=2=ldt[2]=ds descriptor
	__asm__("pushl $0x17\n\tpop %%fs"::);
	//将已经存放好参数和环境变量的页面放到数据段末端
	//做好映射
	data_base += data_limit;
	for (i=MAX_ARG_PAGES-1 ; i>=0 ; i--) {
		data_base -= PAGE_SIZE;
		if (page[i])
			put_page(page[i],data_base);
	}
	return data_limit;
}

/*
 * 'do_execve()' executes a new program.
 */
//envp,argv,filename对应edx,ecx,ebx寄存器，已压栈
//tmp是系统中断中在调用_sys_execve时的返回地址，这里没有用到
//在system_call.s中用了call sys_call_table(,%eax,4)，所以返回地址被压栈，这里就是tmp
//eip调用系统中断的程序代码指针
int do_execve(unsigned long * eip,long tmp,char * filename,
	char ** argv, char ** envp)
{
	struct m_inode * inode;
	struct buffer_head * bh;
	struct exec ex;
	unsigned long page[MAX_ARG_PAGES]; //准备了32个页面来放置可执行文件的命令行参数和环境变量字符串
	int i,argc,envc;
	unsigned long p; //用来指示32个页面128kb空间中的当前位置

	//eip[1]是cs，是调用本次系统调用的原用户程序代码段寄存器的值
	//其中的段选择符必须是当前任务代码段的选择符(0x000f)，否则
	//只能是内核段代码段的选择符(0x0008)，这绝对不允许，因为内核段
	//代码是常驻内存不能被替换调的。
	if ((0xffff & eip[1]) != 0x000f)
		panic("execve called from supervisor mode");
	//清除页表项，page是32个页面对应的页表项
	for (i=0 ; i<MAX_ARG_PAGES ; i++)	/* clear page-table */
		page[i]=0;
	if (!(inode=namei(filename)))		/* get executables inode */
		return -ENOENT;
	if (!S_ISREG(inode->i_mode)) {	/* must be regular file */
		iput(inode);
		return -EACCES;
	}
	i = inode->i_mode;
	//根据进程的euid和egid和执行文件的访问属性进行比较。
	if (current->uid && current->euid) {
		if (current->euid == inode->i_uid)
			i >>= 6;
		else if (current->egid == inode->i_gid)
			i >>= 3;
	} else if (i & 0111)
		i=1;
	//没有执行权限，出错返回
	if (!(i & 1)) {
		iput(inode);
		return -ENOEXEC;
	}
	//执行到此处，代表当前进程有运行指定文件的权限。
	//取出执行文件的头部数据，依据其中信息来分析设置运行环境
	//首先读取被执行文件第一块数据到高速缓冲块
	if (!(bh = bread(inode->i_dev,inode->i_zone[0]))) {
		iput(inode);
		return -EACCES;
	}
	//并复制数据到ex中。
	ex = *((struct exec *) bh->b_data);	/* read exec-header */
	//释放高速缓冲块。
	brelse(bh);
	//对ex中的数据，被执行文件的inode zone[0]，进行解析
	//0.01仅仅支持ZMAGIC执行文件格式。见a.out.h定义
	//并且执行文件代码都从逻辑地址0开始执行，所以不支持含有代码段或数据重定位信息的执行文件
	//这里判断文件magic number，代码段重定位信息，数据段重定位信息
	//代码段+数据段+堆>50MB,
	//代码段+数据段+符号表+执行头部分>文件长度
	//任何不合法的信息导致出错返回
	if (N_MAGIC(ex) != ZMAGIC || ex.a_trsize || ex.a_drsize ||
		ex.a_text+ex.a_data+ex.a_bss>0x3000000 ||
		inode->i_size < ex.a_text+ex.a_data+ex.a_syms+N_TXTOFF(ex)) {
		iput(inode);
		return -ENOEXEC;
	}
	//zmagic文件的代码部分是从执行文件的1024字节偏移处开始
	if (N_TXTOFF(ex) != BLOCK_SIZE)
		panic("N_TXTOFF != BLOCK_SIZE. See a.out.h.");

	argc = count(argv); //命令行参数个数
	envc = count(envp); //环境字符串变量个数
	//复制参数
	p = copy_strings(envc,envp,page,PAGE_SIZE*MAX_ARG_PAGES-4);
	p = copy_strings(argc,argv,page,p);
	//p=0代表参数把32页128kb空间都用完了
	if (!p) {
		for (i=0 ; i<MAX_ARG_PAGES ; i++)
			free_page(page[i]);
		iput(inode);
		return -1;
	}
/* OK, This is the point of no return */
	//前面设置了需要运行的程序的命令行参数和环境变量
	//现在要位其初始化进程任务结构信息，建立页表等。
	//由于被执行文件使用当前进程的躯壳，所以应该释放当前进程占用的资源
	//包括已经打开的文件，占用的页表和页面
	//再根据被执行文件的头部信息修改ldt，重新设置代码段，数据段的描述符限长
	//再本次调用的返回地址eip指向被执行文件的代码起始处

	//关闭文件
	for (i=0 ; i<32 ; i++)
		current->sig_fn[i] = NULL;
	for (i=0 ; i<NR_OPEN ; i++)
		if ((current->close_on_exec>>i)&1)
			sys_close(i);
	current->close_on_exec = 0;

	//释放原来程序的代码段和数据段所对应的内存页表指定的物理内存页面以及页表本身
	//此时次年执行文件没有占用主内存区任何页面
	//处理器真正运行文件代码的时候，会引起缺页异常，内存管理程序会执行
	//缺页处理，为新文件申请内存，设置页表，并把相关执行文件页面读入内存。即所谓按需加载
	free_page_tables(get_base(current->ldt[1]),get_limit(0x0f));
	free_page_tables(get_base(current->ldt[2]),get_limit(0x17));
	if (last_task_used_math == current)
		last_task_used_math = NULL;
	current->used_math = 0;
	//根据新执行文件的头部结构中的代码长度字段a_text的值修改ldt中描述符的基址和段长
	//并将32页的参数放置在数据段末端。p最后更改成以数据段起始处为原点的偏移值，但依然
	//指向参数数据开始处，也就是转换为了栈指针值。create_table在栈空间中创建参数指针表，
	//新执行文件main函数会用到，返回该栈指针。
	p += change_ldt(ex.a_text,page)-MAX_ARG_PAGES*PAGE_SIZE;
	p = (unsigned long) create_tables((char *)p,argc,envc);
	//修改任务数据结构为新执行文件的信息
	//代码尾字段end_code等于执行文件的代码段长度
	//数据尾字段end_data等于代码段+数据段长度，不包括bss
	//brk字段为代码加数据加bss段(未初始化全局数据段),brk用于指出当前数据段包括未初始化部分的末端位置
	current->brk = ex.a_bss +
		(current->end_data = ex.a_data +
		(current->end_code = ex.a_text));
	current->start_stack = p & 0xfffff000;
	i = read_area(inode,ex.a_text+ex.a_data);
	iput(inode);
	if (i<0)
		sys_exit(-1);
	i = ex.a_text+ex.a_data;
	while (i&0xfff)
		put_fs_byte(0,(char *) (i++));
	//最后将原调用系统中断的程序在栈上的代码指针替换为新程序的入口点，并将栈指针替换为新执行文件的
	//栈指针。因此返回的时候，就执行了新的文件。
	eip[0] = ex.a_entry;		/* eip, magic happens :-) */
	eip[3] = p;			/* stack pointer */
	return 0;
}
