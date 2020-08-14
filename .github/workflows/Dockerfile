FROM fedora:latest
ARG GITUID
RUN dnf --assumeyes install rpmdevtools && \
 dnf --assumeyes install dnf-plugins-core && \
 dnf --assumeyes install fedora-gnat-project-common && \
 dnf --assumeyes install gcc-gnat && \
 dnf --assumeyes install gprbuild && \
 dnf --assumeyes install make && \
 dnf --assumeyes install git && \
 dnf --assumeyes install libxml2-devel && \
 dnf --assumeyes install aws-devel && \
 dnf --assumeyes install matreshka-devel && \
useradd -G wheel --uid $GITUID user
USER user
COPY update.sh /home/user/
