/*
 * system calls: process <-> OS, userspace -> kernel space
 * but why some of sys calls here are called by kernel modules????
 * sys calls brackets:
 * 1. Process control,
 * example: kill, fork, exec, get/set process attributes, memory allocation
 * wait for time/event/signal, pause
 * 2. File control(including device control)
 * create/delete file, open/close, read/write,
 * get/set permissions(protection)
 * link/unlink
 * 3. System information
 * example: get/set system information, timer
 * 4. Communication
 * example: IPC(shared memory or File control, send/receive messages)
 */

/* kernel/hd.c setup hardrive */
/* called by init/main.c???? */
extern int sys_setup();
/* in kernel/exit.c, calling do_exit */
extern int sys_exit();
/* system_call.s _sys_fork() */
extern int sys_fork();
/* fs/read_write.c */
extern int sys_read();
extern int sys_write();
/* fs/open.c */
extern int sys_open();
extern int sys_close();
/* kernel/exit.c */
extern int sys_waitpid();
/* fs/open.c */
extern int sys_creat();
/* fs/namei.c */
extern int sys_link();
extern int sys_unlink();
extern int sys_execve();
/* fs/open.c */
extern int sys_chdir();
/* system bracket */
extern int sys_time();
/* not implemented, create a special or ordinary file, i.e.: name_pipe */
extern int sys_mknod();
/* fs/open.c */
extern int sys_chmod();
extern int sys_chown();
/* not implemented, set program break */
extern int sys_break();
/* fs/stat.c return the inode info */
extern int sys_stat();
/* fs/read_write.c, move file pointer pos to specified location */
extern int sys_lseek();
/* kernel sched.c */
extern int sys_getpid();
extern int sys_mount();
extern int sys_umount();
extern int sys_setuid();
extern int sys_getuid();
extern int sys_stime();
/* not implemented */
/* get process trace, for debug propose */
extern int sys_ptrace();
/* kernel/sched.c, set the alarm seconds for current task */
extern int sys_alarm();
/* fd instead of filename */
extern int sys_fstat();
/* kernel/sched.c, set the task state to TASK_INTERRUPTIBLE */
extern int sys_pause();
/* fs/open.c, update the actime(access)/modtime(modify) for filename */
extern int sys_utime();
/* set/get mode of tty */
extern int sys_stty();
extern int sys_gtty();
/* fs/open.c */
extern int sys_access();
/* kernel/sched.c, current->priority -= increment */
extern int sys_nice();
/* not implemented, not sure the difference with sys_time */
extern int sys_ftime();
/* fs/buffer.c, update fs inodes to disk */
extern int sys_sync();
/* kernel/exit.c do_kill of pid, sig */
extern int sys_kill();
/* not implemented, rename a file, oldpath->newpath */
extern int sys_rename();
/* fs/namei.c */
extern int sys_mkdir();
/* fs/namei.c */
extern int sys_rmdir();
/* fs/fcntl.c, duplicate a fd to a new one */
extern int sys_dup();
/* fs/pipe.c setup the pipe and return fd[0] fd[1] */
extern int sys_pipe();
/* see kernel/sys.c */
extern int sys_times();
/* not implemented, profiling the system???? */
extern int sys_prof();
/* see kernel/sys.c */
extern int sys_brk();
extern int sys_setgid();
extern int sys_getgid();
/* kernel/sched.c, bind sig_fn(signal function) and sig_restorer to the function entries */
extern int sys_signal();
extern int sys_geteuid();
extern int sys_getegid();
/* not implemented, enable/disable process accounting,
 * summarize the commands the process has ran
 */
extern int sys_acct();
/* not existing anymore :) not sure what it means */
extern int sys_phys();
/* not implemented, what about unlock?,
 * newer kernel is using mlock/munlock --> memory lock/unlock
 */
extern int sys_lock();
/* fs/ioctl.c, well, only for tty_ioctl */
extern int sys_ioctl();
/* fs/fcntl.c, file control*/
extern int sys_fcntl();
/* not existing anymore */
extern int sys_mpx();
/* set process group id */
extern int sys_setpgid();
/* not existing anymore */
extern int sys_ulimit();
/* see kernel/sys.c */
extern int sys_uname();
/* see kernel/sys.c */
extern int sys_umask();
/* fs/open.c, change root of current task to the filename */
extern int sys_chroot();
/* not implemented, get filesystem statistics */
extern int sys_ustat();
/* fs/fcntl.c, duplicate a fd to an aimed one */
extern int sys_dup2();
extern int sys_getppid();
extern int sys_getpgrp();
extern int sys_setsid();

fn_ptr sys_call_table[] = { sys_setup, sys_exit, sys_fork, sys_read,
sys_write, sys_open, sys_close, sys_waitpid, sys_creat, sys_link,
sys_unlink, sys_execve, sys_chdir, sys_time, sys_mknod, sys_chmod,
sys_chown, sys_break, sys_stat, sys_lseek, sys_getpid, sys_mount,
sys_umount, sys_setuid, sys_getuid, sys_stime, sys_ptrace, sys_alarm,
sys_fstat, sys_pause, sys_utime, sys_stty, sys_gtty, sys_access,
sys_nice, sys_ftime, sys_sync, sys_kill, sys_rename, sys_mkdir,
sys_rmdir, sys_dup, sys_pipe, sys_times, sys_prof, sys_brk, sys_setgid,
sys_getgid, sys_signal, sys_geteuid, sys_getegid, sys_acct, sys_phys,
sys_lock, sys_ioctl, sys_fcntl, sys_mpx, sys_setpgid, sys_ulimit,
sys_uname, sys_umask, sys_chroot, sys_ustat, sys_dup2, sys_getppid,
sys_getpgrp,sys_setsid};
