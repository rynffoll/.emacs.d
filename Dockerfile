FROM silex/emacs

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Europe/Moscow

RUN apt update && \
        apt install -y \
        tzdata \
        git \
        httpie \
        direnv \
        hunspell \
        hunspell-en-us \
        hunspell-ru \
        cmake \
        && rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8
ENV TERM=xterm-256color

RUN useradd --create-home --shell /bin/bash rynffoll

USER rynffoll
WORKDIR /home/rynffoll

COPY --chown=rynffoll:rynffoll . .emacs.d/

RUN emacs --user rynffoll --batch --kill
