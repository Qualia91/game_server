extends Node

###############################################################################
### Signals
###############################################################################

signal error_connecting(reason)
signal connection_closed(was_clean)
signal error_connection_closed(was_clean)
signal error(reason)
signal server_connected
signal game_ended
signal user_joined(user_name)
signal lobby_created(lobby_name, is_private)
signal lobby_ready
signal restart_game
signal lobbies_waiting(lobbies)
signal moves_ready(moves)
signal other_data_recieved(data)

###############################################################################
### Exports
###############################################################################

# The URL we will connect to
export var websocket_url = "ws://<ENTER_URL_HERE>:8080/game_handler"
export var server_version_number = 0

###############################################################################
### Public Variables
###############################################################################

var player_index = -1
var player_names = ["", ""]

###############################################################################
### Private Variables
###############################################################################

# Our WebSocketClient instance
var _client = WebSocketClient.new()
var _player_name = ""
var _is_private = false
var _local_restart_ready = false
var _network_restart_ready = false
var _local_moves_ready = false
var _network_moves_ready = false
var _moves = [null, null]
var _is_local = false

###############################################################################
### Node functions
###############################################################################

# Called when the node enters the scene tree for the first time.
func _ready() -> void:
	# Connect base signals to get notified of connection open, close, and errors.
	_client.connect("connection_closed", self, "_closed")
	_client.connect("connection_error", self, "_error_closed")
	_client.connect("connection_established", self, "_connected")
	_client.connect("data_received", self, "_on_data")
	
# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta : float) -> void:
	_client.poll()

###############################################################################
### API
###############################################################################

func init(player_name : String) -> void:
	self._player_name = player_name
	
func connect_to_server():
	disconnect_from_server()
	set_process(true)
	# Initiate connection to the given URL.
	var err = _client.connect_to_url(websocket_url)
	if err != OK:
		emit_signal("error_connecting", err)
		set_process(false)
	
func disconnect_from_server():
	_client.disconnect_from_host(200, "")
		
func reconnect_to_server():
	_client.disconnect_from_host(200, "")
	connect_to_server()
		
func create_lobby(is_private : bool) -> void:
	self._is_private = is_private
	self.player_index = 0
	var visibility_type = "public"
	self.player_names[0] = self._player_name
	if self._is_private:
		visibility_type = "private"
	_send_message('{"msg": "create_game", "type": "' + visibility_type + '", "user_name": "' + self._player_name + '"}')
	
func join_lobby(is_private : bool, lobby_name : String) -> void:
	self._is_private = is_private
	self.player_index = 1
	var visibility_type = "public"
	self.player_names[1] = self._player_name
	if self._is_private:
		visibility_type = "private"
	_send_message('{"msg": "join_game", "type": "' + visibility_type + '", "lobby_name": "' + lobby_name + '", "user_name": "' + self._player_name + '"}')
	
func submit_moves(moves) -> void:
	self._local_moves_ready = true
	self._moves[self.player_index] = moves
	var move_dict = {"msg": "move", "moves": moves}
	_send_message(JSON.print(move_dict))
	_check_ready_to_move()
	
func request_public_lobbies() -> void:
	_send_message(JSON.print({"msg": "get_public_games"}))
	
func send_other_data(data : Dictionary) -> void:
	_send_message(JSON.print(data))
	
func restart() -> void:
	_send_message(JSON.print({"restart": true}))
	self._local_restart_ready = true
	_check_ready_to_restart()
	
func fake_lobby_init(player_names : Array) -> void:
	self._is_local = true
	self.player_index = 0
	self.player_names = player_names
	self._player_name = player_names[0]
	emit_signal("lobby_ready")
	
func fake_network_message(dict : Dictionary) -> void:
	_apply_json(dict)

###############################################################################
### WS Client Signal Functions
###############################################################################

func _closed(was_clean : bool = false) -> void:
	_reset_data()
	emit_signal("connection_closed", was_clean)
	set_process(false)
	
func _error_closed(was_clean : bool = false) -> void:
	_reset_data()
	emit_signal("error_connection_closed", was_clean)
	set_process(false)

func _connected(proto = "") -> void:
	_reset_data()
	emit_signal("server_connected")
	var version_msg = '{"version": ' + str(server_version_number) + '}'
	_send_message(version_msg)

func _on_data() -> void:
	var msg = _client.get_peer(1).get_packet().get_string_from_utf8()
	if msg == "ping":
		_send_message("pong")
	else:
		var json = JSON.parse(msg)
		if json.error != OK:
			return
		_apply_json(json.result)

###############################################################################
### Internal Functions
###############################################################################

func _reset_data():
	self.player_index = -1
	self.player_names = ["", ""]
	self._local_restart_ready = false
	self._network_restart_ready = false
	self._local_moves_ready = false
	self._network_moves_ready = false
	self._moves = [null, null]
	self._is_local = false

func _send_message(msg : String) -> void:
	if not self._is_local:
		_client.get_peer(1).put_packet(msg.to_utf8())

func _apply_json(dict):
	if dict.has("version_match"):
		if not dict["version_match"]:
			emit_signal("error", "local server version did not match server version")
	elif dict.has("error"):
		emit_signal("error", "Server error: " + dict["error"])
	elif dict.has("game_ended"):
		emit_signal("game_ended")
	elif dict.has("lobby_created"):
		emit_signal("lobby_created", dict["lobby_created"], _is_private)
	elif dict.has("user_joined"):
		emit_signal("user_joined", dict["user_joined"])
		if self.player_index == 0:
			_send_message(JSON.print({"host_name": player_names[0]}))
			self.player_names[1] = dict["user_joined"]
			emit_signal("lobby_ready")
		else:
			self.player_names[0] = dict["user_joined"]
	elif dict.has("host_name"):
		self.player_names[0] = dict["host_name"]
		emit_signal("lobby_ready")
	elif dict.has("restart") and dict["user_index"] != self.player_index:
		self._network_restart_ready = true
		_check_ready_to_restart()
	elif dict.has("games"):
		emit_signal("lobbies_waiting", dict["games"])
	elif dict.has("moves"):
		if dict["user_index"] != self.player_index:
			self._network_moves_ready = true
			self._moves[dict["user_index"]] = dict["moves"]
			_check_ready_to_move()
	else:
		emit_signal("other_data_recieved", dict)

func _check_ready_to_restart() -> void:
	if self._network_restart_ready and self._local_restart_ready:
		self._network_restart_ready = false
		self._local_restart_ready = false
		emit_signal("restart_game")

func _check_ready_to_move() -> void:
	if self._network_moves_ready and self._local_moves_ready:
		self._network_moves_ready = false
		self._local_moves_ready = false
		emit_signal("moves_ready", self._moves)
