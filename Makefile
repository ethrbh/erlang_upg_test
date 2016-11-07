PROJECT = erlang_upg_test
PROJECT_DESCRIPTION = Example for how to upgrade Erlang/OTP application
PROJECT_VERSION = 0.0.4

# List of Erlang/OTP applications this project depends on, excluding erts, 
# kernel and stdlib, or list of dependencies local to this repository.
LOCAL_DEPS = sasl tools observer runtime_tools

include erlang.mk
