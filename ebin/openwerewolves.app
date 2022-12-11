{application, 'openwerewolves', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['openwerewolves_app','openwerewolves_sup']},
	{registered, [openwerewolves_sup]},
	{applications, [kernel,stdlib]},
	{mod, {openwerewolves_app, []}},
	{env, []}
]}.