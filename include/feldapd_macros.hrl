-define(report(Severity, Label, Content), feldapd_trace:report_event(Severity, Label, ?MODULE, ?LINE, Content)).

-define(report_important(Label, Content), ?report(10, Label, Content)).
-define(report_verbose(Label, Content), ?report(20, Label, Content)).
-define(report_debug(Label, Content), ?report(30, Label, Content)).
-define(report_trace(Label, Content), ?report(40, Label, Content)).
	
-define(hlri(Label, Content), ?report_important(Label, Content)).
-define(hlrv(Label, Content), ?report_verbose(Label, Content)).
-define(hlrd(Label, Content), ?report_debug(Label, Content)).
-define(hlrt(Label, Content), ?report_trace(Label, Content)).
