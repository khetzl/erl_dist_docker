# Erlang distribution between Docker containers

Using built-in Erlang distribution between Erlang nodes deployed in Docker containers. Project is aiming to use all functionality of built-in distribution including RPC calls, MNesia replication.

### Distributed port mapping and Docker

* Erlang nodes will start distribution when a node name was passed with -name or -sname option. The nodes will look for epmd on 127.0.0.1:4369 and fail with error nodistribution if epmd is not listening at that address and port.
* To set up a distributed cluster of Erlang nodes, they have to have their cookie and epmd port number matched, and each node must have a unique node name.
* Docker containers running epmd can't expose their port 4369 as it will lead to a port collision, when containers run on a single host.
* Erlang nodes started in containers should start their epmd inside their containers.
* To enable networking without restrictions between two Docker containers, the containers need to be part of the same user defined Docker network. (Using networks provided by Docker such as none, host, bridge won't work in this context.)
* Communication between two Erlang nodes in two Docker containers:
  - Erlang nodes need to be started with shortname (``-sname``) option. In this case the hostname will be dynamically distributed and will inherit the Docker container ID. If the hostname is set, two nodes will not see each other.
 - Limitation: Since the dynamic binding, manual intervention is obligatory so the nodes will discover each other. This means that in one's shell ```net_adm:ping(OthersFullName).``` needs to be run.


* current problems, possible fields to explore:
  - connection between an Erlang node in Docker container and a stand-alone Erlang node is not possible - they are not on the same Docker network
  - two Docker containers with Erlang nodes on two hosts - same Docker network problem - Kubernetes will be needed to overcome this limitation

### Minimal success example
```bash
docker network create erlnet
docker run -it --rm --net erlnet erlang sh -c 'erl -sname snode -setcookie cookie'
```

or using Dockerfile provided in this project

```bash
docker network create erlnet
cd docker_erlang
docker build -t erl . erldock
docker run -it --rm --net erlnet erldock sh -c 'erl -sname snode -setcookie cookie'
```

### Building and running the example Erlang project

```bash
cd node
rebar3 compile
rebar3 shell
```
