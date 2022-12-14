{application, 'whospy', [
	{description, "Who Is Spy Game"},
	{vsn, "0.1.0"},
	{modules, ['client','player','player_sup','whospy_app','whospy_sup']},
	{registered, [whospy_sup]},
	{applications, [kernel,stdlib,cowboy,jsone]},
	{mod, {whospy_app, []}},
	{env, []}
]}.