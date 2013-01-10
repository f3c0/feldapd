-record(feldapdTransportState, {
	server_name,
	socket_type,
	bind_address,
	port,
	listen_socket,
	accept_loop_pid = null,
	fd
}).

-record(feldapdState, {
	root_username,
	root_password,
	schema = [],
	next_session_id = 1,
	sessions = [],
	data_ets_tid,
	data_dirty = false,
	counter = 0,
	data_module,
	data_file
}).

-record(feldapdSession, {
	id,
	binded = ""
}).

-record(
	r_FELDAPD_Node, 
	{
		baseObject = "",
		children = [],
		attributes = []
	}
).

