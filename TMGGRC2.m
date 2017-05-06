TMGGRC2	       ;TMG/kst-Work with Growth Chart Data ;10/5/10 ; 9/27/11 9:41am
	       ;;1.0;TMG-LIB;**1,17**;10/5/10;Build 38
	       ;
	       ;"Code for working with pediatric growth chart data.
	       ;"This helps generate javascript code to pass back to WebBrowser
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
	       ;"=======================================================================
	       ;" RPC -- Public Functions.
	       ;"=======================================================================
	       ;"TMGGRAPH(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --Height %tile (child)
	       ;"TMGGCLNI(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --Length %tile (infant)
	       ;"TMGGCHTC(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)-- Height %tile (child)
	       ;"TMGGCHDC(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --Head circ %tile
	       ;"TMGGCWTI(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --Wt %tile (infant)
	       ;"TMGGCWTC(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --Wt %tile (child)
	       ;"TMGGCBMI(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --BMI percentile (infant) <-- not used (no LMS data avail from CDC)
	       ;"TMGGCBMC(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --BMI %tile (child)
	       ;"TMGGCWHL(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --Wt %tile for Length (infant)
	       ;"TMGGCWHS(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --Wt %tile for Stature (child)
	       ;"<============= WHO GRAPH ENTRY POINTS =================>
	       ;"TMGWHOBA(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO BMI for Age.
	       ;"TMGWBAB2(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO BMI Birth To 2 Years
	       ;"TMGWBAB5(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO BMI Birth To 5 Years
	       ;"TMGWBA25(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO BMI 2 To 5 Years
	       ;"TMGWB519(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO BMI 5 To 19 Years
	       ;"
	       ;"TMGWHOHA(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Height for Age.
	       ;"TMGWHAB6(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Height Birth to 6 Months.
	       ;"TMGWHAB2(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Height Birth to 2 Years.
	       ;"TMGWHA62(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Height 6 Months to 2 Years.
	       ;"TMGWHA25(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Height 2 Years to 5 Years.
	       ;"TMGWHAB5(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Height Birth to 5 Years.
	       ;"TMGWH519(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Height 5 to 19 Years.
	       ;"
	       ;"TMGWHOWA(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Weight for Age.
	       ;"TMGWWAB6(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Weight Birth to 6 Months.
	       ;"TMGWWAB2(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Weight Birth to 2 Years.
	       ;"TMGWWA62(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Weight 6 Months to 2 Years.
	       ;"TMGWWAB5(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Weight Birth to 5 Years.
	       ;"TMGWWA25(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Weight 2 Years to 5 Years.
	       ;"TMGWW519(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Weight 5 Years to 19 Years.
	       ;"
	       ;"TMGWHOHC(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Head Circumference for Age.
	       ;"TMGWHCBT(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Head Circumference Birth to Thirteen.
	       ;"TMGWHCB2(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Head Circumference Birth to 2 Years.
	       ;"TMGWHCB5(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Head Circumference Birth to 5 Years.
	       ;"
	       ;"TMGWHOWL(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Weight for Length.
	       ;"TMGWHOWS(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Weight from Stature.
	       ;"<============= WHO Z-SCORE GRAPH ENTRY POINTS =================>
	       ;"TMGZBAB2(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Z-Score BMI Birth To 2 Years
	       ;"TMGZBAB5(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Z-Score BMI Birth To 5 Years
	       ;"TMGZBA25(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Z-Score BMI 2 To 5 Years
               ;"TMGZB519(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Z-Score BMI 5 To 19 Years
	       ;"
	       ;"TMGZHAB6(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Z-Score Length Birth to 6 Months.
	       ;"TMGZHAB2(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Z-Score Length Birth to 2 Years.
	       ;"TMGZHA62(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Z-Score Length 6 Months to 2 Years.
	       ;"TMGZHA25(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Z-Score Height 2 Years to 5 Years.
	       ;"TMGZHAB5(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Z-Score Length Birth to 5 Years.
               ;"TMGZH519(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Z-Score Height 5 Years to 19 Years.
	       ;"
	       ;"TMGZWAB6(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Z-Score Weight Birth to 6 Months.
	       ;"TMGZWAB2(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Z-Score Weight Birth to 2 Years.
	       ;"TMGZWA62(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Z-Score Weight 6 Months to 2 Years.
	       ;"TMGZWAB5(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Z-Score Weight Birth to 5 Years.
	       ;"TMGZWA25(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Z-Score Weight 2 Years to 5 Years.
               ;"TMGZW510(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Z-Score Weight 5 Years To 10 Years.
	       ;"
	       ;"TMGZWLB2(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Z-Score Weight for Length.
	       ;"TMGZWH25(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Z-Score Weight for Height.
	       ;"
	              ;"TMGZHCBT(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Z-Score Head Circumference Birth to 13 Weeks
	              ;"TMGZHCB2(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Z-Score Head Circumference Birth to 2 Years
	              ;"TMGZHCB5(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --WHO Z-Score Head Circumference Birth to 5 Years
	       ;
	       ;"ADDRPT -- install (add) the TMG GROWTH CHART MENU to ORWRP REPORT LIST.
	       ;
	       ;"=======================================================================
	       ;
TMGGRAPH(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"Input: ROOT -- Pass by NAME.  This is where output goes
	       ;"       DFN -- Patient DFN ; ICN for foriegn sites
	       ;"       ID --
	       ;"       ALPHA -- Start date (lieu of DTRANGE)
	       ;"       OMEGA -- End date (lieu of DTRANGE)
	       ;"       DTRANGE -- # days back from today
	       ;"       REMOTE --
	       ;"       MAX    --
	       ;"       ORFHIE --
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"CH-HT")
	       QUIT
	       ;
TMGGCLNI(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"         For Length percentile for age (infant)
	       ;"Input: (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       ;
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"INF-LN")
	       QUIT
	       ;
TMGGCWTI(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"         For Weight percentile for age (infant)
	       ;"Input: (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"INF-WT")
	       QUIT
	       ;
TMGGCHDC(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"         For Head Circumference percentile for age
	       ;"Input: (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"INF-HC")
	       QUIT
	       ;
TMGGCBMI(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"         For BMI percentile for age (infant)
	       ;"Input: (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"INF-BMI")
	       QUIT
	       ;
TMGGCWHL(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"         For Weight percentile for Length (infant)
	       ;"Input: (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"INF-WT4L")
	       QUIT
	       ;
TMGGCHTC(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"         For Height percentile for age (child)
	       ;"Input: (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"CH-HT")
	       QUIT
	       ;
TMGGCWTC(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"         For Weight percentile for age (child)
	       ;"Input: (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"CH-WT")
	       QUIT
	       ;
TMGGCBMC(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"         For BMI percentile for age (child)
	       ;"Input: (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"CH-BMI")
	       QUIT
	       ;
TMGGCWHS(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"         For Weight percentile for stature (child)
	       ;"Input: (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"CH-WT4S")
	       QUIT
	       ;
	       ;"WHO - BMI ENTRY POINTS
TMGWHOBA(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO BMI by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-BA")
	       QUIT
	       ;
TMGWBAB2(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO BMI by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-BA-B2")
	       QUIT
	       ;
TMGWBAB5(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO BMI by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-BA-B5")
	       QUIT
	       ;
TMGWBA25(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO BMI by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-BA-25")
	       QUIT
	       ;
TMGWB519(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO BMI by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-BA-519")
	       QUIT
	       ;
	       ;"WHO - Height for Age Entry Points
TMGWHOHA(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Height by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-HA")
	       QUIT
	       ;
TMGWHAB6(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Height by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-HA-B6")
	       QUIT
	       ;
TMGWHAB2(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Height by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-HA-B2")
	       QUIT
	       ;
TMGWHA62(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Height by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-HA-62")
	       QUIT
	       ;
TMGWHA25(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Height by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-HA-25")
	       QUIT
	       ;
TMGWH519(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Height by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-HA-519")
	       QUIT
	       ;
TMGWHAB5(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Height by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-HA-B5")
	       QUIT
	       ;
	       ;"WHO - Weight for age Entry Points
TMGWHOWA(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Weight by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-WA")
	       QUIT
	       ;
TMGWWAB6(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Weight by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-WA-B6")
	       QUIT
	       ;
TMGWWAB2(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Weight by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-WA-B2")
	       QUIT
	       ;
TMGWWA62(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Weight by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-WA-62")
	       QUIT
	       ;
TMGWWAB5(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Weight by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-WA-B5")
	       QUIT
	       ;
TMGWWA25(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Weight by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-WA-25")
	       QUIT
	       ;
TMGWW510(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Weight by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-WA-510")
	       QUIT
	       ;
	       ;"WHO - Head Circumference Entry Points
TMGWHOHC(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Head Circumference by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-HC")
	       QUIT
	       ;
TMGWHCBT(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Head Circumference by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-HC-BT")
	       QUIT
	       ;
TMGWHCB2(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Head Circumference by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-HC-B2")
	       QUIT
	       ;
TMGWHCB5(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Head Circumference by Age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-HC-B5")
	       QUIT
	       ;
TMGWHOWL(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Weight for Length
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-WH") ;"temp solution
	       QUIT
	       ;
TMGWHOWS(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	       ;
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Weight for Stature
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"WHO-WL") ;"temp solution
	       QUIT
	       ;  ====>Z-SCORE ENTRY POINTS =====
TMGZBAB2(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Z-Score BMI for age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"ZWO-BA-B2")
	       QUIT
	       ;
TMGZBAB5(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Z-Score BMI for age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"ZWO-BA-B5")
	       QUIT
	       ;
TMGZBA25(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Z-Score BMI for age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"ZWO-BA-25")
	       QUIT
	       ;
TMGZB519(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Z-Score BMI for age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"ZWO-BA-519")
	       QUIT
	       ;
TMGZHAB6(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Z-Score Length for age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"ZWO-HA-B6")
	       QUIT
	       ;
TMGZHAB2(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Z-Score Length for age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"ZWO-HA-B2")
	       QUIT
	       ;
TMGZHA62(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Z-Score Length for age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"ZWO-HA-62")
	       QUIT
	       ;
TMGZHA25(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Z-Score Height for age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"ZWO-HA-25")
	       QUIT
	       ;
TMGZH519(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	   
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Z-Score Weight for height
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"ZWO-HA-519")
	       QUIT
	              ;
TMGZHAB5(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Z-Score Length for age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"ZWO-HA-B5")
	       QUIT
	       ;
TMGZWAB6(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Z-Score weight for age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"ZWO-WA-B6")
	       QUIT
	       ;
TMGZWAB2(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Z-Score Weight for age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"ZWO-WA-B2")
	       QUIT
	       ;
TMGZWA62(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Z-Score Weight for age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"ZWO-WA-62")
	       QUIT
	       ;
TMGZWAB5(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Z-Score Weight for age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"ZWO-WA-B5")
	       QUIT
	       ;
TMGZWA25(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Z-Score Weight for age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"ZWO-WA-25")
	       QUIT
	       ;
TMGZW510(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Z-Score Weight for age
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"ZWO-WA-510")
	       QUIT
	       ;
TMGZWLB2(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Z-Score Weight for length
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"ZWO-WL-B2")
	       QUIT
	       ;
TMGZWH25(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	   
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Z-Score Weight for height
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"ZWO-WH-25")
	       QUIT
	              ;
TMGZHCBT(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	   
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Z-Score Head Circumference
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"ZWO-HC-BT")
	       QUIT
	              ;
TMGZHCB2(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	   
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Z-Score Head Circumference
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"ZWO-HC-B2")
	       QUIT
	              ;
TMGZHCB5(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)	   
	       ;"Purpose: Entry point, as called from CPRS REPORT system
	       ;"           For WHO Z-Score Head Circumference
	       ;"Input (Same as TMGGRAPH, see above)
	       ;"Result: None.  Output goes into @ROOT
	       DO TMGCOMGR^TMGGRC2A(.ROOT,"ZWO-HC-B5")
	       QUIT
	       ;
ADDRPT	        ;
	              DO ADDRPT^TMGGRC2C
	       DO INSTALL^TMGGRC0
	              QUIT
