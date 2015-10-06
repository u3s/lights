%%% @author  <matt@liliput>
%%% @copyright (C) 2015, 
%%% @doc
%%%
%%% @end
%%% Created :  1 Sep 2015 by  <matt@liliput>
-ifndef(TL_HRL).
-define(TL_HRL, true).
-record(lamps, {
	  red    :: off | on,
	  yellow :: off | on | blinking,
	  green  :: off | on
	  }).

-record(robot, {
	  id,
	  pid  :: pid(),        % fsm process pid
	  spid :: pid()         % start_cycle process Pid
		  }).
-endif.
