-module(sim_code).

-export([bench_args/2, run/3]).

bench_args(_, _) -> 
    [[wrangler]].

run(wrangler, _, Conf) ->
    {_,DataDir} = lists:keyfind(datadir, 1, Conf),
    [] = dialyzer:run([{analysis_type, plt_build},
		{report_mode, normal},
		{files_rec, [DataDir ++ "/plt"]},
		{timing, true},
		{output_plt, DataDir ++ "/the.plt"}]),
		ok;

run([wrangler], _, Conf) ->
    {_,DataDir} = lists:keyfind(datadir, 1, Conf),
    _Res = sim_code_v23:sim_code_detection(["c:/cygwin"++DataDir++"/wrangler"],
                                           5, 40, 2, 4, 0.8,[],8),
    ok.
    
