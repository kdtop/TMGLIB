TMGLM0 ; ; 29-MAY-2017
 ;; ;
EN ; -- main entry point for TMG TEST 1
 D EN^VALM("TMG TEST 1")
 Q
 ;
HDR ; -- header code
 S VALMHDR(1)="This is a test header for TMG TEST 1."
 S VALMHDR(2)="This is the second line"
 Q
 ;
INIT ; -- init variables and list array
 F LINE=1:1:30 D SET^VALM10(LINE,LINE_"     Line number "_LINE)
 S VALMCNT=30
 Q
 ;
HELP ; -- help code
 S X="?" D DISP^XQORM1 W !!
 Q
 ;
EXIT ; -- exit code
 Q
 ;
EXPND ; -- expand code
 Q
 ;
