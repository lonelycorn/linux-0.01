#include <signal.h>

#include <linux/sched.h>
#include <linux/mm.h>	/* for get_free_page */
#include <asm/segment.h>

int read_pipe(struct m_inode * inode, char * buf, int count)
{
	char * b=buf;

    /// wait until there's any data in the pipe
	while (PIPE_EMPTY(*inode)) {
		wake_up(&inode->i_wait);
		if (inode->i_count != 2) /* are there any writers left? */
			return 0;
		sleep_on(&inode->i_wait);
	}
	while (count>0 && !(PIPE_EMPTY(*inode))) {
		count --;
		put_fs_byte(((char *)inode->i_size)[PIPE_TAIL(*inode)],b++);
		INC_PIPE( PIPE_TAIL(*inode) );
	}
	wake_up(&inode->i_wait);
	return b-buf;
}
	
int write_pipe(struct m_inode * inode, char * buf, int count)
{
	char * b=buf;

    /// check if there's any task waiting for this file
	wake_up(&inode->i_wait);
	if (inode->i_count != 2) { /* no readers */
		current->signal |= (1<<(SIGPIPE-1));
		return -1;
	}
	while (count-->0) {
        /// pipe is full; wake up the reader task and let it clear the pipe
		while (PIPE_FULL(*inode)) {
			wake_up(&inode->i_wait);
			if (inode->i_count != 2) {
				current->signal |= (1<<(SIGPIPE-1));
				return b-buf;
			}
			sleep_on(&inode->i_wait);
		}
        /// TRICK: if a is an array and i is an index into the array, then a[i] <-> i[a]
		((char *)inode->i_size)[PIPE_HEAD(*inode)] = get_fs_byte(b++);
		INC_PIPE( PIPE_HEAD(*inode) );
		wake_up(&inode->i_wait);
	}
	wake_up(&inode->i_wait);
	return b-buf;
}

int sys_pipe(unsigned long * fildes)
{
	struct m_inode * inode;
	struct file * f[2];
	int fd[2];
	int i,j;

    /** find 2 unused file descriptors inside kernel
     * file_table stores all defined file descriptors, and is defined in
     * fs/file_table.c
     */
	j=0;
	for(i=0;j<2 && i<NR_FILE;i++)
		if (!file_table[i].f_count)
			(f[j++]=i+file_table)->f_count++;
	if (j==1)
		f[0]->f_count=0;
	if (j<2)
		return -1;

    /** find 2 unused file descriptors owned by current task.
     * current refers to the task that called this function, and is defined in
     * kernel/sched.c
     */
	j=0;
	for(i=0;j<2 && i<NR_OPEN;i++)
		if (!current->filp[i]) {
			current->filp[ fd[j]=i ] = f[j]; /// fd is unique per task
			j++;
		}
	if (j==1)
		current->filp[fd[0]]=NULL;
	if (j<2) {
		f[0]->f_count=f[1]->f_count=0;
		return -1;
	}
    /** create a PIPE inode
     */
	if (!(inode=get_pipe_inode())) {
		current->filp[fd[0]] =
			current->filp[fd[1]] = NULL;
		f[0]->f_count = f[1]->f_count = 0;
		return -1;
	}
	f[0]->f_inode = f[1]->f_inode = inode;
	f[0]->f_pos = f[1]->f_pos = 0;
	f[0]->f_mode = 1;		/* read */
	f[1]->f_mode = 2;		/* write */
	put_fs_long(fd[0],0+fildes);
	put_fs_long(fd[1],1+fildes);
	return 0;
}
