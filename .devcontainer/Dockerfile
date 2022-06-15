# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.236.0/containers/codespaces-linux/.devcontainer/base.Dockerfile

FROM mcr.microsoft.com/vscode/devcontainers/universal:2-focal

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_GHC_VERSION=9.0.2 BOOTSTRAP_HASKELL_INSTALL_STACK=1 BOOTSTRAP_HASKELL_INSTALL_HLS=1 sh \
    && echo "source ~/.ghcup/env" >> ~/.zshrc \
