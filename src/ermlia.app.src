{application, ermlia, [
  {description, "ermlia - Erlang implementation of Kademlia."},
  {vsn, "%VSN%"},
  {modules, [
    ermlia, ermlia_app, ermlia_sup,
    ermlia_kbukets_sup, ermlia_kbukets,
    ermlia_data_store_sup, ermlia_data_store,
    ermlia_timeout_nodes_sup, ermlia_timeout_nodes,
    ermlia_ets_server_sup, ermlia_ets_server,
    ermlia_facade,
    ermlia_node_pipe, udp_server,
    ermlia_web,
    list_utils
  ]},
  {registered, [
    ermlia_sup,
    ermlia_kbukets_sup, ermlia_kbukets,
    ermlia_data_store_sup, ermlia_data_store,
    ermlia_timeout_nodes_sup, ermlia_timeout_nodes,
    ermlia_node_pipe,
    ermlia_web
  ]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {ermlia_app, []}},
  {start_phases, []},
  {env, []}
]}.
