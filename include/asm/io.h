/** IO Ports
 * Ports of an IBM PC (compatiable) are mapped to I/O addresses; for example,
 * address range 0x60 - 0x6F is used to communicate with the keyboard.
 * 
 * @sa AT Tech Ref, 1-28
 */

/** GCC Extended Asm
 * GCC offers extended asm so we can read and write C variables from assembler,
 * and perform jumps from assembler code to C labels.
 * In short, colons (':') are used to delimit the operand parameters after the
 * assembler template.
 *
 * @sa https://gcc.gnu.org/onlinedocs/gcc/Extended-Asm.html#Extended-Asm
 */

/// write 1 byte @p value to @p port
#define outb(value,port) \
__asm__ ("outb %%al,%%dx"::"a" (value),"d" (port))


/// read 1 byte @p value from @p port
#define inb(port) ({ \
unsigned char _v; \
__asm__ volatile ("inb %%dx,%%al":"=a" (_v):"d" (port)); \
_v; \
})

/// write 1 byte @p value to @p port; pauses until completes
#define outb_p(value,port) \
__asm__ ("outb %%al,%%dx\n" \
		"\tjmp 1f\n" \
		"1:\tjmp 1f\n" \
		"1:"::"a" (value),"d" (port))

/// read 1 byte @p value from @p port; pauses until completes
#define inb_p(port) ({ \
unsigned char _v; \
__asm__ volatile ("inb %%dx,%%al\n" \
	"\tjmp 1f\n" \
	"1:\tjmp 1f\n" \
	"1:":"=a" (_v):"d" (port)); \
_v; \
})
