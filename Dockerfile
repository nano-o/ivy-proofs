FROM python:3.11-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    python3 python3-pip g++ cmake python3-ply python3-pygraphviz git \
    python3-tk tix pkg-config libssl-dev libreadline-dev sudo \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

RUN useradd -m -s /bin/bash ivyuser && echo "ivyuser ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/ivyuser
USER ivyuser
WORKDIR /home/ivyuser/

RUN git clone --recurse-submodules https://github.com/kenmcmil/ivy.git
WORKDIR ivy
RUN python3 build_submodules.py
RUN python3 setup.py install --user
# Ensure the user-installed binaries are accessible
ENV PATH="/home/ivyuser/.local/bin:${PATH}"

WORKDIR /home/ivyuser/
RUN git clone https://github.com/nano-o/ivy-proofs.git
WORKDIR /home/ivyuser/ivy-proofs/
RUN git checkout impl_temp

CMD ["/bin/bash"]
