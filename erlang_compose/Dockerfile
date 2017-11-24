FROM erlang:20

RUN git clone https://github.com/khetzl/erl_dist_docker.git
WORKDIR erl_dist_docker/node/
RUN rebar3 release

# perhaps dev_mode should be turned off and a complete release could be
# downloaded, irl scenario, or at least built correctly here

CMD ["_build/default/rel/node/bin/node", "foreground"]
