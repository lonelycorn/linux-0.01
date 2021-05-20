#define __LIBRARY__
#include <unistd.h>
#include <time.h>

/*
 * we need this inline - forking from kernel space will result
 * in NO COPY ON WRITE (!!!), until an execve is executed. This
 * is no problem, but for the stack. This is handled by not letting
 * main() use the stack at all after fork(). Thus, no function
 * calls - which means inline code for fork too, as otherwise we
 * would use the stack upon exit from 'fork()'.
 *
 * Actually only pause and fork are needed inline, so that there
 * won't be any messing with the stack from main(), but we define
 * some others too.
 */
static inline _syscall0(int,fork) //定义了fork()函数，unistd.h内定义了_syscall宏，这里展开后就是fork函数
static inline _syscall0(int,pause)
static inline _syscall0(int,setup)
static inline _syscall0(int,sync)

#include <linux/tty.h>
#include <linux/sched.h>
#include <linux/head.h>
#include <asm/system.h>
#include <asm/io.h>

#include <stddef.h>
#include <stdarg.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>

#include <linux/fs.h>

static char printbuf[1024];

extern int vsprintf();
extern void init(void);
extern void hd_init(void);
extern long kernel_mktime(struct tm * tm);
extern long startup_time;

/*
 * Yeah, yeah, it's ugly, but I cannot find how to do this correctly
 * and this seems to work. I anybody has more info on the real-time
 * clock I'd be interested. Most of this was trial and error, and some
 * bios-listing reading. Urghh.
 */

#define CMOS_READ(addr) ({ \
outb_p(0x80|addr,0x70); \
inb_p(0x71); \
})

#define BCD_TO_BIN(val) ((val)=((val)&15) + ((val)>>4)*10)

static void time_init(void)
{
	struct tm time;

	do {
		time.tm_sec = CMOS_READ(0);
		time.tm_min = CMOS_READ(2);
		time.tm_hour = CMOS_READ(4);
		time.tm_mday = CMOS_READ(7);
		time.tm_mon = CMOS_READ(8)-1;
		time.tm_year = CMOS_READ(9);
	} while (time.tm_sec != CMOS_READ(0));
	BCD_TO_BIN(time.tm_sec);
	BCD_TO_BIN(time.tm_min);
	BCD_TO_BIN(time.tm_hour);
	BCD_TO_BIN(time.tm_mday);
	BCD_TO_BIN(time.tm_mon);
	BCD_TO_BIN(time.tm_year);
	startup_time = kernel_mktime(&time);
}

void main(void)		/* This really IS void, no error here. */
{			/* The startup routine assumes (well, ...) this */
/*
 * Interrupts are still disabled. Do necessary setups, then
 * enable them
 */
	time_init();
	tty_init();
	trap_init();
	sched_init(); //调度初始化，其中设置第一个任务task0的ldt,tss
	buffer_init(); //高速缓冲区初始化
	hd_init();
	sti();
	move_to_user_mode(); //由内核态进入用户态
	//任务0数据段，代码段直接是映射到内核的代码和数据空间。
	if (!fork()) {		/* we count on this going ok */
		//fork() -> static inline _syscall0(int,fork) (expanding marco in unistd.h)
		//       -> _system_call -> _sys_call_table -> _sys_fork
		init(); //于task1中执行
	}
/*
 *   NOTE!!   For any other task 'pause()' would mean we have to get a
 * signal to awaken, but task0 is the sole exception (see 'schedule()')
 * as task 0 gets activated at every idle moment (when no other tasks
 * can run). For task0 'pause()' just means we go check if some other
 * task can run, and if not we return here.
 */
	for(;;) pause();
}

static int printf(const char *fmt, ...)
{
	va_list args;
	int i;

	va_start(args, fmt);
	write(1,printbuf,i=vsprintf(printbuf, fmt, args));
	va_end(args);
	return i;
}

static char * argv[] = { "-",NULL };
static char * envp[] = { "HOME=/usr/root", NULL };

void init(void) //在task1中执行，也就是所谓init()进程
{
	int i,j;

	setup(); //读取硬盘参数表, 装载分区，见hd.c sys_setup()
	if (!fork()) //开一个task来执行update,后续linux版本被取消
		_exit(execve("/bin/update",NULL,NULL));
	(void) open("/dev/tty0",O_RDWR,0);
	(void) dup(0);
	(void) dup(0);
	printf("%d buffers = %d bytes buffer space\n\r",NR_BUFFERS,
		NR_BUFFERS*BLOCK_SIZE);
	printf(" Ok.\n\r");
	if ((i=fork())<0) //fork出task2
		printf("Fork failed in init\r\n");
	else if (!i) { //task2执行shell, 没有shell就88
		close(0);close(1);close(2);
		setsid();
		(void) open("/dev/tty0",O_RDWR,0);
		(void) dup(0);
		(void) dup(0);
		_exit(execve("/bin/sh",argv,envp));
	}
	j=wait(&i);
	printf("child %d died with code %04x\n",j,i);
	sync();
	_exit(0);	/* NOTE! _exit, not exit() */
}

//memory layout
//--------------------------------
//|内核程序 | 高速缓冲 | 主内存区|
//--------------------------------
//buffer,高速缓冲区是用于临时存放数据的地方，比如磁盘读进来的数据，1K粒度
//主内存区是按页划分

//fork()创建新task/进程是通过完全复制父进程代码段，数据段和栈段的方式实现。
//为了保证新进程用户态栈没有父进程的信息，不能通过一般调用函数的方式来执行fork
//因为这会导致压栈操作，修改了栈。所以要通过inline方式实现。int 0x80软中断后
//进入内核态，切换为内核栈，这个时候就可以继续call了

//task0和task1/init进程实际上同时使用内核代码区相同的代码和数据物理内存页面，
//也同时使用相同的用户堆栈。在为新进程init复制父进程(task0)的页目录和页表项时，
//进程0的页表项没有被改动过，依然是RW，但是进程1的页表项是只读的。
//当进程1开始执行时，对用户态栈的操作导致页面写保护异常，内存管理模块会处理，
//为进程1在主内存区分配页面。
