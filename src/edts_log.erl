%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Wrapper for lager.
%%% @end
%%% @author Thomas Järvstrand <tjarvstrand@gmail.com>
%%% @copyright
%%% Copyright 2012 Thomas Järvstrand <tjarvstrand@gmail.com>
%%%
%%% This file is part of EDTS.
%%%
%%% EDTS is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% EDTS is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public License
%%% along with EDTS. If not, see <http://www.gnu.org/licenses/>.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(edts_log).

%%%_* Exports ==================================================================

%% API
-export([debug/2,
         info/2,
         notice/2,
         warning/2,
         error/2,
         critical/2,
         alert/2,
         emergency/2,

         set_log_level/1]).

-export([debug/1,
         info/1,
         notice/1,
         warning/1,
         error/1,
         critical/1,
         alert/1,
         emergency/1]).

%%%_* Includes =================================================================

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================
-spec debug(string(),[any()]) -> ok.
debug(    Fmt, Args) -> lager:debug(    Fmt, Args).
-spec info(string(),[any()]) -> ok.
info(     Fmt, Args) -> lager:info(     Fmt, Args).
-spec notice(string(),[any()]) -> ok.
notice(   Fmt, Args) -> lager:notice(   Fmt, Args).
-spec warning(string(),[any()]) -> ok.
warning(  Fmt, Args) -> lager:notice(   Fmt, Args).
-spec error(string(),[any()]) -> ok.
error(    Fmt, Args) -> lager:error(    Fmt, Args).
-spec critical(string(),[any()]) -> ok.
critical( Fmt, Args) -> lager:critical( Fmt, Args).
-spec alert(string(),[any()]) -> ok.
alert(    Fmt, Args) -> lager:alert(    Fmt, Args).
-spec emergency(string(),[any()]) -> ok.
emergency(Fmt, Args) -> lager:emergency(Fmt, Args).

-spec debug(string()) -> ok.
debug(    Fmt) -> lager:debug(    Fmt).
-spec info(string()) -> ok.
info(     Fmt) -> lager:info(     Fmt).
-spec notice(string()) -> ok.
notice(   Fmt) -> lager:notice(   Fmt).
-spec warning(string()) -> ok.
warning(  Fmt) -> lager:notice(   Fmt).
-spec error(string()) -> ok.
error(    Fmt) -> lager:error(    Fmt).
-spec critical(string()) -> ok.
critical( Fmt) -> lager:critical( Fmt).
-spec alert(string()) -> ok.
alert(    Fmt) -> lager:alert(    Fmt).
-spec emergency(string()) -> ok.
emergency(Fmt) -> lager:emergency(Fmt).

-spec set_log_level(_) -> ok.
set_log_level(Level) -> lager:set_loglevel(lager_console_backend, Level).

%%%_* Internal functions =======================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
