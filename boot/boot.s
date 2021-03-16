//本主引导记录采用intel汇编格式,as86编译,ld86链接
| Power on ---> FFFF:0000 (0xFFFFFFF0 in 32bit) (0xFFFF0 in 20bit) --->
| jmp to (0xFFFFE05B) ---> POST ---> setup isr (13(disk), 10(display),
| 16(keyboard), 19(find mbr)) ---> int 0x19h (load mbr(1st sec)to 07C0:0000)
| ---> jmp to 0x7C00(32KB).


|
|	boot.s
|
| boot.s is loaded at 0x7c00 by the bios-startup routines, and moves itself
| out of the way to address 0x90000, and jumps there.
|
| It then loads the system at 0x10000, using BIOS interrupts. Thereafter
| it disables all interrupts, moves the system down to 0x0000, changes
| to protected mode, and calls the start of system. System then must
| RE-initialize the protected mode in it's own tables, and enable
| interrupts as needed.
|
| NOTE! currently system is at most 8*65536 bytes long. This should be no
| problem, even in the future. I want to keep it simple. This 512 kB
| kernel size should be enough - in fact more would mean we'd have to move
| not just these start-up routines, but also do something about the cache-
| memory (block IO devices). The area left over in the lower 640 kB is meant
| for these. No other memory is assumed to be "physical", ie all memory
| over 1Mb is demand-paging. All addresses under 1Mb are guaranteed to match
| their physical addresses.
|
| NOTE1 abouve is no longer valid in it's entirety. cache-memory is allocated
| above the 1Mb mark as well as below. Otherwise it is mainly correct.
|
| NOTE 2! The boot disk type must be set at compile-time, by setting
| the following equ. Having the boot-up procedure hunt for the right
| disk type is severe brain-damage.
| The loader has been made as simple as possible (had to, to get it
| in 512 bytes with the code to move to protected mode), and continuos
| read errors will result in a unbreakable loop. Reboot by hand. It
| loads pretty fast by getting whole sectors at a time whenever possible.

| 1.44Mb disks:
sectors = 18
| 1.2Mb disks:
| sectors = 15
| 720kB disks:
| sectors = 9

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

BOOTSEG = 0x07c0
INITSEG = 0x9000
SYSSEG  = 0x1000			| system loaded at 0x10000 (65536).
ENDSEG	= SYSSEG + SYSSIZE
//SYSSIZE 在编译时候由编译工具按照编译脚本生成, 见makefile

entry start
start:
	mov	ax,#BOOTSEG
	mov	ds,ax
	mov	ax,#INITSEG
	mov	es,ax
        | ds=07c0, es=9000
	mov	cx,#256
	sub	si,si
	sub	di,di
        | si=0, di=0
	rep
	movw
        | copy 1 word (16byte) ds:si -> es:di (07c0:0000 -> 9000:0000) (section:offset)
        | until cx==0. 256*16bit from 0x7c00(32KB) to 0x90000 (576KB)
        //bios只加载了mbr(=512Bytes)到7c00, 只要挪256个word即可
	jmpi	go,INITSEG
        | jmp to INITSEG:go (9000:offset of "go")
        | therefore cs=INITSEG=0x9000
go:	mov	ax,cs
	mov	ds,ax
	mov	es,ax
	mov	ss,ax
        | ss=es=ds=cs=0x9000
	mov	sp,#0x400		| arbitrary value >>512
        | stack pointer = 9000:0400
	mov	ah,#0x03	| read cursor pos
	xor	bh,bh
	int	0x10            | display isr
	mov	cx,#24
	mov	bx,#0x0007	| page 0, attribute 7 (normal)
	mov	bp,#msg1
	mov	ax,#0x1301	| write string, move cursor, ah = 0x13 (show)
	int	0x10            | display isr
| ok, we've written the message, now
| we want to load the system (at 0x10000)

	mov	ax,#SYSSEG
	mov	es,ax		| segment of 0x010000, es = 0x1000
	call	read_it
	call	kill_motor

| if the read went well we get current cursor position ans save it for
| posterity.
//读当前光标位置--->cs:510，bios 0x10中断，功能号0x03
	mov	ah,#0x03	| read cursor pos
	xor	bh,bh
	int	0x10		| save it in known place, con_init fetches
	mov	[510],dx	| it from 0x90510.
		
| now we want to move to protected mode ...

	cli			| no interrupts allowed !

| first we move the system to it's rightful place

	mov	ax,#0x0000
	cld			| 'direction'=0, movs moves forward, si+=2 di+=2 after movsw
//将标志寄存器Flag的方向标志位DF清零.
//在字串操作中使变址寄存器SI或DI的地址指针自动增加，字串处理由前往后。
do_move:
//移动整个system模块，从0x10000(64KB)到0x00000(0KB)处，当时假设整个系统
//模块不会超过512KB(0x80000)，所以现在其末端地址不超过0x90000.
//Boot:7c00(32KB)--->90000(576KB)，加载整个系统Floopy--->0x10000.
//Whole system (0x10000---0x8FFFF) ---> (0x00000---0x7FFFF)
	mov	es,ax		| destination segment
	add	ax,#0x1000
	cmp	ax,#0x9000
	jz	end_move
	mov	ds,ax		| source segment
	sub	di,di
	sub	si,si
	mov 	cx,#0x8000 //移动0x8000个字, 刚好64KB,一个段的大小
	rep
	movsw //源地址是DS:SI,目的地址是ES:DI
	j	do_move

| then we load the segment descriptors

end_move:
//代码现在依旧运行在0x9000段，cs=0x9000
//这里将数据段寄存器ds也设置为与cs一样
//目的是加载全局描述符和中断描述符, 保护模式要用到
	mov	ax,cs		| right, forgot this at first. didn't work :-)
	mov	ds,ax
	lidt	idt_48		| load idt with 0,0
	lgdt	gdt_48		| load gdt with whatever appropriate

| that was painless, now we enable A20
// 键盘相关的最重要的硬件是两个芯片。一个是 intel 8042 芯片，位于主板上，CPU 通过 IO 端口直接和这个芯片通信，获得按键的扫描码或者发送各种键盘命令。另一个是 intel 8048 芯片或者其兼容芯片，位于键盘中，这个芯片主要作用是从键盘的硬件中得到被按的键所产生的扫描码，与 i8042 通信，控制键盘本身。
// 8042的P2端口位1是A20地址线控制位。A20 on之后就可以寻址1MB以上空间
/*
很多稀奇古怪的东西都是由于系统升级时，为了保持向下兼容而产生的，A20Gate就是其中之一。

在8086/8088中，只有20根地址总线，所以可以访问的地址是2^20=1M，但由于8086/8088是16位地址模式，能够表示的地址范围是0-64K，所以为了在8086/8088下能够访问1M内存，Intel采取了分段的模式：16位段基地址:16位偏移。其绝对地址计算方法为：16位基地址左移4位+16位偏移=20位地址。

但这种方式引起了新的问题，通过上述分段模式，能够表示的最大内存为：FFFFh:FFFFh=FFFF0h+FFFFh=10FFEFh=1M+64K-16Bytes（1M多余出来的部分被称做高端内存区HMA）。但8086/8088只有20位地址线，如果访问100000h~10FFEFh之间的内存，则必须有第21根地址线。所以当程序员给出超过1M（100000H-10FFEFH）的地址时，系统并不认为其访问越界而产生异常，而是自动从重新0开始计算，也就是说系统计算实际地址的时候是按照对1M求模的方式进行的，这种技术被称为wrap-around。

到了80286，系统的地址总线发展为24根，这样能够访问的内存可以达到2^24=16M。Intel在设计80286时提出的目标是，在实模式下，系统所表现的行为应该和8086/8088所表现的完全一样，也就是说，在实模式下，80286以及后续系列，应该和8086/8088完全兼容。但最终，80286芯片却存在一个BUG：如果程序员访问100000H-10FFEFH之间的内存，系统将实际访问这块内存，而不是象过去一样重新从0开始。

为了解决上述问题，IBM使用键盘控制器上剩余的一些输出线来管理第21根地址线（从0开始数是第20根），被称为A20Gate：如果A20 Gate被打开，则当程序员给出100000H-10FFEFH之间的地址的时候，系统将真正访问这块内存区域；如果A20Gate被禁止，则当程序员给出100000H-10FFEFH之间的地址的时候，系统仍然使用8086/8088的方式。绝大多数IBM PC兼容机默认的A20Gate是被禁止的。由于在当时没有更好的方法来解决这个问题，所以IBM使用了键盘控制器来操作A20 Gate，但这只是一种黑客行为，毕竟A20Gate和键盘操作没有任何关系。在许多新型PC上存在着一种通过芯片来直接控制A20 Gate的BIOS功能。从性能上，这种方法比通过键盘控制器来控制A20Gate要稍微高一点。

上面所述的内存访问模式都是实模式，在80286以及更高系列的PC中，即使A20Gate被打开，在实模式下所能够访问的内存最大也只能为10FFEFH，尽管它们的地址总线所能够访问的能力都大大超过这个限制。为了能够访问10FFEFH以上的内存，则必须进入保护模式。（其实所谓的实模式，就是8086/8088的模式，这种模式存在的唯一理由就是为了让旧的程序能够继续正常的运行在新的PC体系上）

1. A20 Gate inProtected Mode

从80286开始，系统出现了一种新的机制，被称为保护模式。到了80386，保护模式得到了进一步的完善和发展，并且对于80386以后的芯片，保护模式的变化就非常小了。

我们在上一节已经谈到，如果要访问更多的内存，则必须进入保护模式，那么，在保护模式下，A20Gate对于内存访问有什么影响呢？

为了搞清楚这一点，我们先来看一看A20的工作原理。A20，从它的名字就可以看出来，其实它就是对于20-bit（从0开始数）的特殊处理(也就是对第21根地址线的处理)。如果A20Gate被禁止，对于80286来说，其地址为24bit，其地址表示为EFFFFF；对于80386极其随后的32-bit芯片来说，其地址表示为FFEFFFFF。这种表示的意思是如果A20Gate被禁止，则其第20-bit在CPU做地址访问的时候是无效的，永远只能被作为0；如果A20 Gate被打开，则其第20-bit是有效的，其值既可以是0，又可以是1。

所以，在保护模式下，如果A20Gate被禁止，则可以访问的内存只能是奇数1M段，即1M,3M,5M…，也就是00000-FFFFF,200000-2FFFFF,300000-3FFFFF…。如果A20 Gate被打开，则可以访问的内存则是连续的。

2. How to Enable A20Gate

多数PC都使用键盘控制器（8042芯片）来处理A20Gate。

从理论上讲，打开A20Gate的方法是通过设置8042芯片输出端口（64h）的2nd-bit，但事实上，当你向8042芯片输出端口进行写操作的时候，在键盘缓冲区中，或许还有别的数据尚未处理，因此你必须首先处理这些数据。

流程如下：

　1. 禁止中断；
　2. 等待，直到8042 Inputbuffer为空为止；
　3. 发送禁止键盘操作命令到8042Input buffer；
　4. 等待，直到8042 Inputbuffer为空为止；
　5. 发送读取8042 OutputPort命令；
　6. 等待，直到8042 Outputbuffer有数据为止；
　7. 读取8042 Outputbuffer，并保存得到的字节；
　8. 等待，直到8042 Inputbuffer为空为止；
　9. 发送Write 8042Output Port命令到8042 Input buffer；
　10. 等待，直到8042 Inputbuffer为空为止；
　11. 将从8042 OutputPort得到的字节的第2位置1（OR 2），然后写入8042 Input buffer；
　12. 等待，直到8042 Inputbuffer为空为止；
　13. 发送允许键盘操作命令到8042Input buffer；
　14. 打开中断。

以上描述的是一种和IBMPC完全兼容的，通过键盘控制器控制A20 Gate的方法。但是，正象我们在前面所提到的，A20 Gate与键盘操作完全没有关系，IBM之所以将A20Gate的功能控制放在键盘控制器上，完全是一种为了弥补Intel 80286与Intel8086/8088不完全兼容的缺陷，而采取的Hacker行为，所以在许多新型PC上存在着一种通过芯片来直接控制A20 Gate的BIOS功能，我们在RealMode下只需要调用BIOS中断就可以实现A20 Gate的控制功能。

这个BIOS中断为 INT 15h, AX=2401h。被称为Fast A20。

movw $0x2401, %ax
int $0x15

3. How to Detect ifA20 Gate has been Enabled?

我们在之前已经提到，如果A20Gate被打开了，则在实模式下，程序员可以直接访问100000H~10FFEFH之间的内存，如果A20Gate被禁止，则在实模式下，若程序员访问100000H~10FFEFH之间的内存，则会被硬件自动转换为0H~0FFEFH之间的内存，所以我们可以利用这个差异来检测A20Gate是否被打开。见head.s
*/
	call	empty_8042
	mov	al,#0xD1		| command write
	out	#0x64,al
	call	empty_8042
	mov	al,#0xDF		| A20 on
	out	#0x60,al
	call	empty_8042

| well, that went ok, I hope. Now we have to reprogram the interrupts :-(
| we put them right after the intel-reserved hardware interrupts, at
| int 0x20-0x2F. There they won't mess up anything. Sadly IBM really
| messed this up with the original PC, and they haven't been able to
| rectify it afterwards. Thus the bios puts interrupts at 0x08-0x0f,
| which is used for the internal hardware interrupts as well. We just
| have to reprogram the 8259's, and it isn't fun.
//给8259中断控制器重新编程
	mov	al,#0x11		| initialization sequence
	out	#0x20,al		| send it to 8259A-1
	.word	0x00eb,0x00eb		| jmp $+2, jmp $+2
	out	#0xA0,al		| and to 8259A-2
	.word	0x00eb,0x00eb
	mov	al,#0x20		| start of hardware int's (0x20),主芯片起始中断号
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x28		| start of hardware int's 2 (0x28)，从芯片起始中断号
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x04		| 8259-1 is master
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x02		| 8259-2 is slave
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x01		| 8086 mode for both
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0xFF		| mask off all interrupts for now
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al

| well, that certainly wasn't fun :-(. Hopefully it works, and we don't
| need no steenking BIOS anyway (except for the initial loading :-).
| The BIOS-routine wants lots of unnecessary data, and it's less
| "interesting" anyway. This is how REAL programmers do it.
|
| Well, now's the time to actually move into protected mode. To make
| things as simple as possible, we do no register set-up or anything,
| we let the gnu-compiled 32-bit programs do that. We just jump to
| absolute address 0x00000, in 32-bit protected mode.
	mov	ax,#0x0001	| protected mode (PE) bit
//将源操作数加载到机器状态字
	lmsw	ax		| This is it!
//只有源操作数的低 4 位（也就是 PE、MP、EM 及 TS 标志）会加载到 CR0。CR0 的 PG、CD、NW、AM、WP、NE 及 ET 标志不受影响。
//PE=1, enter protection mode, 特权级CPL=0
	jmpi	0,8		| jmp offset 0 of segment 8 (cs)
//已经进入保护模式, cs=8,保护模式下的选择子selector=8, 2B=16bit长度
//selector bit:
// 0-1: RPL, requested privilege level
//   2: TI, table indicator
//3-15: index
//0b 0000, 0000, 0000, 1000
//   ^-----------------^:   index = 1, the 2nd descriptor
//                      ^:  TI, select the GDTR
//                       ^^:RPL = 0, 最高特权级
//对应的全局描述符base address=0x0000, 偏移是jmpi第一个操作数
//所以跳到0x0000绝对地址开始执行

| This routine checks that the keyboard command queue is empty
| No timeout is used - if this hangs there is something wrong with
| the machine, and we probably couldn't proceed anyway.
empty_8042:
//测试8042键盘控制器寄存器状态，等待输入buffer空
	.word	0x00eb,0x00eb //JMP $+2 $:current IP
//相当于两条NOP空操作, 但是耗时比NOP更高，NOP=3 ticks, 0x00eb=7-10 ticks
	in	al,#0x64	| 8042 status port
	test	al,#2		| is input buffer full?
	jnz	empty_8042	| yes - loop
	ret

| This routine loads the system at address 0x10000, making sure
| no 64kB boundaries are crossed. We try to load it as fast as
| possible, loading whole tracks whenever we can.
|
| in:	es - starting address segment (normally 0x1000)
|
| This routine has to be recompiled to fit another drive type,
| just change the "sectors" variable at the start of the file
| (originally 18, for a 1.44Mb drive)
|
sread:	.word 1			| sectors read of current track
//注意，不是从0扇区开始。0扇区对应的是MBR主引导记录，也就是本boot.s程序
head:	.word 0			| current head
track:	.word 0			| current track
read_it:
//从软盘读取SYSSIZE(当时假定不超过0x80000,512KB)数据去ES:BX
//由于是从扇区1开始读的，所以直接就是head.s开始的程序了
	mov ax,es               // es is 0x1000 now
	test ax,#0x0fff         // bit-and and set Z flag (set if result is 0)
die:	jne die			// es must be at 64kB boundary (jmp if Z flag is set)
                                // es:bx must be a product of an integer and 64KB
	xor bx,bx		// bx is starting address within segment
rp_read:
//判读是否已经读入全部数据。通过比较当前段是否就是系统数据末端所处段。
	mov ax,es
	cmp ax,#ENDSEG		| have we loaded all yet?
	jb ok1_read
	ret
ok1_read:
//计算当前磁道需要读取的扇区数--->ax
	mov ax,#sectors         // 1.44MB=18sector per track
	sub ax,sread
	mov cx,ax               // cx=# of unread sectors
	shl cx,#9               // cx=*512==bytes unread
//当前磁道还未读取的字节数加上段内数据开始处偏移位置
	add cx,bx               // cx=bytes read into this sector(es:bx) after this reading op
//不超过64KB的段内限制
	jnc ok2_read            // j if not carry, (cx<0x10000) 64kb
	je ok2_read             // j if equal (cx == 0x10000) 64kb
//超过则反算扇区数
	xor ax,ax               // ax=0
	sub ax,bx               // ax=64kb'compliment of bx
	shr ax,#9               // ax>>9
ok2_read:
	call read_track
	mov cx,ax  //返回值-->cx=# of sector read
	add ax,sread // ax=+sread, 加上当前磁道已读扇区数
	cmp ax,#sectors //当前磁道所有扇区已读？
	jne ok3_read //否，j ok3_read
	mov ax,#1 //已读，ax=1
	sub ax,head //ax=-head, ax此时是下面要读取的数据对应的磁头号
                    //head==0, ax=1; head==1,ax=0
	jne ok4_read //jmp not equal, if ax==0，当前磁道磁头0面已读，换到1面读
	inc track // ax==1,双面已读，换下一磁道
ok4_read:
	mov head,ax //保存要读的磁头号
	xor ax,ax //ax=0
ok3_read:
// 如果当前磁道还有未读扇区，先保存当前磁道已读扇区数量，调整数据存放的起始位置(bx)
// 若小于64KB, j rp_read. 否，调整段,j rp_read
	mov sread,ax //保存当前磁道已读扇区数量
	shl cx,#9 //cx是上次已读扇区数, cx>>9 变成已读字节数
	add bx,cx //调整当前段内数据起始位置
	jnc rp_read //j if 没有超过段限长
	mov ax,es // es--->ax
	add ax,#0x1000 //指向下一个段, +64KB
	mov es,ax // ax--->es
	xor bx,bx // bx=0
	jmp rp_read

read_track:
//读取当前磁道上指定开始扇区和需要读取的扇区数的数据到es:bx处 (0x1000:0000开始写入system模块)
	push ax
	push bx
	push cx
	push dx

	mov dx,track //当前磁道号->dx
	mov cx,sread //已读扇区数->cx
	inc cx       //要读取的扇区号
	mov ch,dl    //ch=当前磁道号
	mov dx,head  //dx=当前磁头号
	mov dh,dl    //dh=当前磁头号
	mov dl,#0    //dl=驱动器号,0=A:\>
	and dx,#0x0100 //磁头号不大于1,软盘最多2个头
	mov ah,#2     //中断功能号，2=读扇区
	int 0x13      //bios磁盘中断
	jc bad_rt     //carry位表示读到坏区

	pop dx
	pop cx
	pop bx
	pop ax
	ret
bad_rt:	mov ax,#0      //reset driver
	mov dx,#0
	int 0x13
	pop dx
	pop cx
	pop bx
	pop ax
	jmp read_track

/*
 * This procedure turns off the floppy drive motor, so
 * that we enter the kernel in a known state, and
 * don't have to worry about it later.
 */
kill_motor: //关马达
	push dx
	mov dx,#0x3f2   //io address ---> FDC
			//bit7-4(A,B,C,D盘on/off), bit3-2(DMA,irq on/off), bit1-0(selector, which FD)
	mov al,#0       //irq, dma, FDC ---> off
	outb //IO op
	pop dx
	ret

gdt:
//全局描述符表
//第一个描述符reserved.
	.word	0,0,0,0		| dummy
//代码段和数据段在此描述符表内都是同一段内存,开始于0x00000000绝对地址
//text segment descriptor
	.word	0x07FF		| 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		| base address=0
	.word	0x9A00		| code read/exec
	.word	0x00C0		| granularity=4096, 386
//data segment descriptor
	.word	0x07FF		| 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		| base address=0
	.word	0x9200		| data read/write
	.word	0x00C0		| granularity=4096, 386

idt_48:
//中断描述符表,空。被加载到IDT寄存器
	.word	0			| idt limit=0
	.word	0,0			| idt base=0L
/*
Interrupt descriptor table (IDT) is an x86 system table that holds descriptors for Interrupt Service Routines (ISRs) or simply interrupt handlers.

In real mode, there is an IVT (interrupt vector table) which is located by the fixed address 0x0 and contains “interrupt handler pointers” in the form of CS and IP registers values. This is really inflexible and relies on segmented memory management, and since 80286, there is an IDT for protected mode.

IDT is the table in memory, created and filled by OS that is pointed by idtr system register which is loaded with lidt instruction. You can use IDT only in protected mode. IDT entries contain gate descriptors - not only addresses of interrupts handlers (ISRs) in 32-bit form but also flags and protection levels. IDT entries are descriptors that describe interrupt gates, and so in this sense, it resembles GDT and its segment descriptors.
*/
gdt_48:
//lgdt操作数是6字节，这里的数据将会加载进GDT寄存器
	.word	0x800		| gdt limit=2048, 256 GDT entries
//GDT长度=0x800, 2KB. 2KB/8B(per descriptor) = 256个描述符
	.word	gdt,0x9		| gdt base = 0X9xxxx
//描述符表地址=0x9<<16+(gdt)标签的偏移地址
	
msg1:
	.byte 13,10
	.ascii "Loading system ..."
	.byte 13,10,13,10

.text
endtext:
.data
enddata:
.bss
endbss:
