#include <errno.h>

#include <linux/sched.h>
#include <linux/kernel.h>

extern int tty_read(unsigned minor,char * buf,int count);
extern int tty_write(unsigned minor,char * buf,int count);

static int rw_ttyx(int rw,unsigned minor,char * buf,int count);
static int rw_tty(int rw,unsigned minor,char * buf,int count);

typedef (*crw_ptr)(int rw,unsigned minor,char * buf,int count);

#define NRDEVS ((sizeof (crw_table))/(sizeof (crw_ptr)))

/// basically a map device_type --> rw_function
static crw_ptr crw_table[]={
	NULL,		/* nodev */
	NULL,		/* /dev/mem */
	NULL,		/* /dev/fd */
	NULL,		/* /dev/hd */
	rw_ttyx,	/* /dev/ttyx */
	rw_tty,		/* /dev/tty */
	NULL,		/* /dev/lp */
	NULL};		/* unnamed pipes */

/// r/w of a specific tty
static int rw_ttyx(int rw,unsigned minor,char * buf,int count)
{
	return ((rw==READ)?tty_read(minor,buf,count):
		tty_write(minor,buf,count));
}

/// r/w of current task's tty
static int rw_tty(int rw,unsigned minor,char * buf,int count)
{
	if (current->tty<0)
		return -EPERM;
	return rw_ttyx(rw,current->tty,buf,count);
}

/** Read/write a character device
 * @param [in] rw: 1 for read, 0 for write
 * @param [in] dev: unique id for the device
 * @param [out] buf:
 * @param [in] count:
 */
int rw_char(int rw,int dev, char * buf, int count)
{
	crw_ptr call_addr;

	if (MAJOR(dev)>=NRDEVS)
		panic("rw_char: dev>NRDEV");
	if (!(call_addr=crw_table[MAJOR(dev)])) {
		printk("dev: %04x\n",dev);
		panic("Trying to r/w from/to nonexistent character device");
	}
	return call_addr(rw,MINOR(dev),buf,count);
}
