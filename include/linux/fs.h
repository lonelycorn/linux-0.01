/*
 * This file has definitions for some important file table
 * structures etc.
 */

#ifndef _FS_H
#define _FS_H

#include <sys/types.h>

/* devices are as follows: (same as minix, so we can use the minix
 * file system. These are major numbers.)
 *
 * 0 - unused (nodev)
 * 1 - /dev/mem
 * 2 - /dev/fd
 * 3 - /dev/hd
 * 4 - /dev/ttyx
 * 5 - /dev/tty
 * 6 - /dev/lp
 * 7 - unnamed pipes
 */

#define IS_BLOCKDEV(x) ((x)==2 || (x)==3)

#define READ 0
#define WRITE 1

void buffer_init(void);

/// extract type of a device from device id
#define MAJOR(a) (((unsigned)(a))>>8)
/// which instance of the type of device
#define MINOR(a) ((a)&0xff)

/// max length of a filename
#define NAME_LEN 14

#define I_MAP_SLOTS 8
#define Z_MAP_SLOTS 8
#define SUPER_MAGIC 0x137F

#define NR_OPEN 20
#define NR_INODE 32
#define NR_FILE 64
#define NR_SUPER 8
#define NR_HASH 307
#define NR_BUFFERS nr_buffers
#define BLOCK_SIZE 1024
#ifndef NULL
#define NULL ((void *) 0)
#endif

#define INODES_PER_BLOCK ((BLOCK_SIZE)/(sizeof (struct d_inode)))
#define DIR_ENTRIES_PER_BLOCK ((BLOCK_SIZE)/(sizeof (struct dir_entry)))

/** When a block is stored in memory, it is stored in a buffer.
 * buffer_block is the memory representation of the block, and buffer_head is
 * the relevant metadata for the buffer.
 * NOTE: buffer_block doesn't seem to be used anywhere.
 */
typedef char buffer_block[BLOCK_SIZE];

struct buffer_head {
	char * b_data;			/* pointer to data block (1024 bytes) */
	unsigned short b_dev;		/* device (0 = free) */
	unsigned short b_blocknr;	/* logical block number */
	unsigned char b_uptodate;
	unsigned char b_dirt;		/* 0-clean,1-dirty */
	unsigned char b_count;		/* users using this block */
	unsigned char b_lock;		/* 0 - ok, 1 -locked */
	struct task_struct * b_wait; /// the task that's waiting on this buffer
	struct buffer_head * b_prev;
	struct buffer_head * b_next;
	struct buffer_head * b_prev_free;
	struct buffer_head * b_next_free;
};

/** MINIX File System Physical Layout
 * Each block is 1k bytes
 *
 * From the beginning of the file system
 * - blank block, 1 block, reserved for MBR
 * - super block, 1 block, info about the file system
 * - inode map, # inodes / 1k, usage of each inodes
 * - zone map, # zones / 1k, usage of each zones
 * - inode table, inodes / 1k * 16, all records of inodes
 * - data zones, remaining space, actual data
 *
 * see http://ohm.hgesser.de/sp-ss2012/Intro-MinixFS.pdf for details
 */

/// inode that exists on disk (aka MINIX inode)
struct d_inode {
	unsigned short i_mode;
	unsigned short i_uid;
	unsigned long i_size;
	unsigned long i_time;
	unsigned char i_gid;
	unsigned char i_nlinks;
	unsigned short i_zone[9];
};

/// inode that exists in memory (aka memory inode). Must be compatible with d_inode
struct m_inode {
	unsigned short i_mode; /// @sa S_ISREG, S_ISDIR, S_ISCHR, S_ISBLK, S_ISFIFO
	unsigned short i_uid; /// user ID
	unsigned long i_size; /// size of file in bytes
	unsigned long i_mtime; /// when inode is modified (wall-clock)
	unsigned char i_gid; /// group ID
	unsigned char i_nlinks; /// number of links (symlink)
    /** Seems to be some type-dependent data
     * for character devices, i_zone[0] == device id (major, minor)
     * for block devices, i_zone[0] == device id
     * i_zone[] stores the blocks in a file:
     * - for small files (i.e < 7k), the block ID's are directly stored in i_zone[0]...i_zone[6].
     * - for files whose size is between [7k, 519k), i_zone[7] points to a block where the ID's of
     *   those extra blocks are stored. i_zone[7] is aka "indirect block"
     * - for files whose size if between [519k, 262M), i_zone[8] points to a block of 512 block ID's,
     *   where each block ID points to a block where ID's of those extra blocks are stored. i_zone[8]
     *   is aka "double-indirect block"
     */
	unsigned short i_zone[9];
/* these are in memory also */
	struct task_struct * i_wait; /// task that's waiting for this inode
	unsigned long i_atime; /// last access time
	unsigned long i_ctime; /// when inode is created (wall-clock)
	unsigned short i_dev; /// which device this inode is from
	unsigned short i_num; /// index of this inode
	unsigned short i_count; /// number of references (e.g. reader, writer)
	unsigned char i_lock; /// 1 if inode is locked (spinlock)
	unsigned char i_dirt; /// 1 if inode dirty (modified but not written to disk)
	unsigned char i_pipe; /// 1 if this is a PIPE
	unsigned char i_mount;
	unsigned char i_seek;
	unsigned char i_update;
};

/// pipe as defined here is a ring-buffer, with (PAGE_SIZE - 1) bytes
#define PIPE_HEAD(inode) (((long *)((inode).i_zone))[0])
#define PIPE_TAIL(inode) (((long *)((inode).i_zone))[1])
#define PIPE_SIZE(inode) ((PIPE_HEAD(inode)-PIPE_TAIL(inode))&(PAGE_SIZE-1))
#define PIPE_EMPTY(inode) (PIPE_HEAD(inode)==PIPE_TAIL(inode))
#define PIPE_FULL(inode) (PIPE_SIZE(inode)==(PAGE_SIZE-1))
#define INC_PIPE(head) \
__asm__("incl %0\n\tandl $4095,%0"::"m" (head))

struct file {
	unsigned short f_mode; /// 1: read; 2: write
	unsigned short f_flags; /// flags used when opening the file
	unsigned short f_count; /// number of references; 0 if unused
	struct m_inode * f_inode; /// which inode this file points to
	off_t f_pos; /// offset into this inode
};

/// info about the file system
struct super_block {
	unsigned short s_ninodes; /// number of inodes
	unsigned short s_nzones; /// number of data zones
	unsigned short s_imap_blocks; /// number of blocks used by inode map
	unsigned short s_zmap_blocks; /// number of blocks used by zone map
	unsigned short s_firstdatazone; /// ID of the first block of the "data" part
	unsigned short s_log_zone_size; /// number of blocks of the "data" part is (1024 << s_log_zone_size)
	unsigned long s_max_size; /// max number of bytes of a file
	unsigned short s_magic; /// magic number for the file system
/* These are only in memory */
	struct buffer_head * s_imap[8];
	struct buffer_head * s_zmap[8];
	unsigned short s_dev;
	struct m_inode * s_isup;
	struct m_inode * s_imount;
	unsigned long s_time;
	unsigned char s_rd_only;
	unsigned char s_dirt;
};

/// directory
struct dir_entry {
    /// ID of the inode
	unsigned short inode;
    /// name of the directory
	char name[NAME_LEN];
};

extern struct m_inode inode_table[NR_INODE];
extern struct file file_table[NR_FILE];
extern struct super_block super_block[NR_SUPER];
extern struct buffer_head * start_buffer;
extern int nr_buffers;

extern void truncate(struct m_inode * inode);
extern void sync_inodes(void);
extern void wait_on(struct m_inode * inode);
extern int bmap(struct m_inode * inode,int block);
extern int create_block(struct m_inode * inode,int block);
extern struct m_inode * namei(const char * pathname);
extern int open_namei(const char * pathname, int flag, int mode,
	struct m_inode ** res_inode);
/// free the inode (in memory)
extern void iput(struct m_inode * inode);
extern struct m_inode * iget(int dev,int nr);
extern struct m_inode * get_empty_inode(void);
extern struct m_inode * get_pipe_inode(void);
extern struct buffer_head * get_hash_table(int dev, int block);
extern struct buffer_head * getblk(int dev, int block);
extern void ll_rw_block(int rw, struct buffer_head * bh);
extern void brelse(struct buffer_head * buf);
extern struct buffer_head * bread(int dev,int block);
extern int new_block(int dev);
extern void free_block(int dev, int block);
extern struct m_inode * new_inode(int dev);
extern void free_inode(struct m_inode * inode);

extern void mount_root(void);

extern inline struct super_block * get_super(int dev)
{
	struct super_block * s;

	for(s = 0+super_block;s < NR_SUPER+super_block; s++)
		if (s->s_dev == dev)
			return s;
	return NULL;
}

#endif
