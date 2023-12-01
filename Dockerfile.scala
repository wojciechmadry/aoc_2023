FROM ubuntu:22.04

ENV USER_NAME wojtek

RUN apt update \
    && apt install -y sudo

RUN echo "%sudo   ALL=(ALL:ALL) NOPASSWD:ALL" >> /etc/sudoers

RUN useradd -ms /bin/bash ${USER_NAME} \
    && usermod -aG sudo ${USER_NAME}

ENV DEBIAN_FRONTEND=noninteractive
RUN ln -snf /usr/share/zoneinfo/$CONTAINER_TIMEZONE /etc/localtime && echo $CONTAINER_TIMEZONE > /etc/timezone

USER ${USER_NAME}

WORKDIR /home/${USER_NAME}

RUN sudo apt update \
    && sudo apt install -y curl \
    && sudo apt upgrade -y \
    && sudo DEBIAN_FRONTEND=noninteractive \
    sudo apt install -y \
    clang \
    gcc \
    make \
    cmake \
    git \
    openssh-server \
    cppcheck \
    npm \
    flake8 \
    python3 \
    python3-setuptools \
    python3-pip \
    python3-venv \
    nodejs \
    debianutils \
    x11-xkb-utils \
    bash-completion \
    wget \
    unzip \
    composer \
    luarocks \
    xclip \
    ripgrep \
    fd-find \
    gettext \
    curl

RUN pip3 install \
    cpplint \
    neovim

RUN  npm install \
    tree-sitter-cli \
    && sudo ln -s ~/node_modules/tree-sitter-cli/tree-sitter /usr/bin/ \
    && sudo npm install -g neovim

RUN sudo apt purge cmake -y \
    && pip3 install cmake \
    && export PATH="$PATH:/home/${USER_NAME}/.local/bin" \
    && mkdir -p ~/git \
    && cd ~/git \
    && git clone -b v0.9.1 https://github.com/neovim/neovim.git \
    && cd neovim \
    && make distclean \
    && make deps \
    && make CMAKE_BUILD_TYPE=Release -j \
    && sudo make install

RUN git clone https://github.com/wojciechmadry/linux_conf_files.git \
    && cd linux_conf_files \
    && make install

# Java
RUN sudo apt update \
    && sudo apt install -y default-jdk htop
# Scala
RUN mkdir -p tmp \
    && cd tmp \
    && wget https://github.com/coursier/coursier/releases/latest/download/cs-x86_64-pc-linux.gz \
    && gzip -d cs-x86_64-pc-linux.gz \
    && chmod +x cs-x86_64-pc-linux \
    && yes | ./cs-x86_64-pc-linux setup \
    && echo "export PATH='$PATH:/home/${USER_NAME}/.local/share/coursier/bin'" >> ~/.bashrc \
    && cd ~ \
    && rm -rf tmp


RUN /home/${USER_NAME}/.local/share/coursier/bin/coursier install metals

ADD home/.config /home/${USER_NAME}/.config

