{application, 'openwerewolves', [
	{description, "Open Werewolves Game Server"},
	{vsn, "0.1.0"},
	{modules, ['openwerewolves_app','openwerewolves_sup','websocket_handler']},
	{registered, [openwerewolves_sup]},
	{applications, [kernel,stdlib,cowboy,jsone]},
	{mod, {openwerewolves_app, []}},
	{env, []}
]}.