-module(sim_code).

-export([bench_args/2, run/3]).

bench_args(_, _) -> 
    [[wrangler]].


run([wrangler], _, Conf) ->
    {_,DataDir} = lists:keyfind(datadir, 1, Conf),
    _Res = sim_code_v23:sim_code_detection(["c:/cygwin"++DataDir++"/wrangler"],
                                           3, 40, 2, 4, 0.8,[],8),
    ok;
    
run([test_suites], _, Conf) ->
    {_,DataDir} = lists:keyfind(datadir, 1, Conf),
    _Res = sim_code_v23:sim_code_detection(["c:/cygwin"++DataDir++"/test_suites"],
                                           5, 40, 2, 4, 0.8,[],8),
    ok.
    
