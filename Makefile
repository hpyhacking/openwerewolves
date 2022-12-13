PROJECT = openwerewolves
PROJECT_DESCRIPTION = Open Werewolves Game Server
PROJECT_VERSION = 0.1.0

DEPS = cowboy
dep_cowboy_commit = 2.9.0

BUILD_DEPS += relx
SHELL_OPTS = -eval 'application:ensure_all_started(openwerewolves)'

include erlang.mk
