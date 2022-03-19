# diamond
☁️ Easily host reasonably contained applications with sane limits.
  
Diamond is an alternative for when you can't or don't want to use Docker/Podman. It allows you to make management of hosted services easy and allows you to "contain" them to some extent.
  
Here are the steps it takes for this -- let's assume you want to host a service called `foo`.
Diamond will:
* Create a Linux user `foo` and it's home directory, `/var/apps/foo`
* Create a disk image called `foo.img` with the size you desire
* Mount the image in `/var/apps/foo` and clone the source there
* Create a SystemD service file with CPU/RAM limitations in `/etc/systemd/system/foo.service`
  
Since the service will have it's own unprivileged Linux user, it's possible to limit damage caused if that specific service is compromised. The reason for the creation of the disk image is simple -- if the service allows saving arbitrary files to disk, it should not be possible for the service to fill the host disk to the brim and crash the system; it also has a positive side-effect of making the service and it's files more portable.
  
Diamond also allows you to automatically mount all the disk images and start all the services automatically this way:
```bash
sudo -E diamond --mount-all
sudo -E diamond --start-all
```
  
and allows checking their status using `sudo -E diamond --list`.

# Downloads  
## arm64
**Linux**: https://nightly.link/uditkarode/diamond/workflows/arm64/master/diamond-arm64.zip  

## x86_64
**Linux**: https://nightly.link/uditkarode/diamond/workflows/x86_64/master/diamond-Linux.zip  

> NOTE: untested on macOS  
**macOS**: https://nightly.link/uditkarode/diamond/workflows/x86_64/master/diamond-macOS.zip
