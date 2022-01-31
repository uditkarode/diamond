FROM archlinux

# set env vars
ENV LANG=en_US.UTF-8
ENV IN_TESTING_ENV=1

# set work dir
WORKDIR /diamond-test

# update and install needed packages
RUN pacman -Syu --noconfirm
RUN pacman -S sudo git nodejs yarn wget --noconfirm

# add a non-root user
RUN useradd -m udit -s /bin/bash
RUN usermod -aG wheel udit

# also export the test envvar for the user
RUN echo 'export IN_TESTING_ENV=1' >> ~udit/.bashrc
# allow the non root user to run sudo commands without password
RUN echo 'udit ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

# download diamond binary from locally running server
RUN echo 'wget http://localhost:19132/diamond -O /diamond-test/diamond' >> ~/.bashrc
RUN echo 'chmod +x /diamond-test/diamond' >> ~/.bashrc
RUN echo 'chown udit diamond' >> ~/.bashrc
RUN echo clear >> ~/.bashrc

# drop into the user shell on start and exit when done
RUN echo 'sudo -su udit' >> ~/.bashrc
RUN echo exit >> ~/.bashrc
