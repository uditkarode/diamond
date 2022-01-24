FROM archlinux

ENV LANG=en_US.UTF-8
ENV IN_TESTING_ENV=1

WORKDIR /diamond-test

RUN pacman -Syu --noconfirm
RUN pacman -S wget --noconfirm
RUN echo 'wget http://localhost:19132/diamond -O /diamond-test/diamond' >> ~/.bashrc
RUN echo 'chmod +x /diamond-test/diamond' >> ~/.bashrc
