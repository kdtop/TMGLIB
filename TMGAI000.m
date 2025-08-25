TMGAI000 ;TMG/kst/OS Specific functions ;4/11/22
         ;;1.0;TMG-LIB;**1**;4/11/22
 ;
 ;"TMG OPENAI functions. 
 ;"I.e. functions that are OS specific.
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 4/11/2022  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;" Private  Functions.
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
TESTMEDS ;
  NEW TMGDFN SET TMGDFN=65244
  DO MEDARR^TMGTIUOJ(.TEMP,TMGDFN,.ARR)  ;"//kt 5/7/18
  NEW QARR,IDX SET IDX=0
  SET QARR($I(IDX))="Below is a list of medications as might be found in a patients medical note. "
  SET QARR($I(IDX))="Each line is a separate medication.  Please construct a json object "
  SET QARR($I(IDX))="with a top level structure of 'OTC' for over-the-counter meds, and 'RX' for "
  SET QARR($I(IDX))="prescriptions ones.  Then inside each section, have an entry for each medication. "
  SET QARR($I(IDX))="Include the following properties. "
  SET QARR($I(IDX))="name:  with subproperties of brand (if known) and generic (if known) "
  SET QARR($I(IDX))="dose/form:  (e.g. 5 mg tab) "
  SET QARR($I(IDX))="sig:  for instructions "
  SET QARR($I(IDX))="comments:  for comments about med (see more below) "
  SET QARR($I(IDX))="end of json description. "
  SET QARR($I(IDX))="We often use the following words for meds.  Please take these into account when parsing "
  SET QARR($I(IDX))="If the word is prefixed with STOP or HOLD, this would be a comment. "
  SET QARR($I(IDX))="If NOT TAKING, this would be a comment that patient is not taking. "
  SET QARR($I(IDX))="If there is a text arrow, e.g. -> or --> or -- >, then this indicates a desired change "
  SET QARR($I(IDX))="and this change should be noted in the comment field. "  
  SET QARR($I(IDX))="Also, could you covert all medical abbreviations to full words, as possible. "
  SET QARR($I(IDX))="E.g. 'QD' would become 'daily', and 'PRN' would become 'as needed'. "
  SET QARR($I(IDX))="End of instructions.  Medication list to follow. "
  SET QARR($I(IDX))=" "
  NEW COUNT SET COUNT=0
  NEW JDX SET JDX=0
  FOR  SET JDX=$ORDER(ARR(JDX)) QUIT:(JDX'>0)!(COUNT>5)  DO
  . SET QARR($I(IDX))=$GET(ARR(JDX))
  . SET COUNT=COUNT+1
  NEW QUEST SET QUEST=$$ARR2STR^TMGSTUT2(.QARR," ")
  NEW OUT
  NEW ZZDEBUG SET ZZDEBUG=0
  IF ($GET(ZZSESSIONFLAG)=1)!(ZZDEBUG=1) DO  GOTO TM1
  .  MERGE OUT=^TMG("TMP","TESTMEDS^TMGAI000")
 
  DO OLLAMA(.OUT,QUEST)
  ;"IF $DATA(OUT) ZWR OUT
  KILL ^TMG("TMP","TESTMEDS^TMGAI000")
  MERGE ^TMG("TMP","TESTMEDS^TMGAI000")=OUT
TM1 ;  
  NEW STR SET STR=$GET(OUT(1))
      
  SET STR=$$REPLSTR^TMGSTUT3(STR,"\n","")   
  SET STR=$$REPLSTR^TMGSTUT3(STR,"\""","""")  
  SET STR=$$REPLSTR^TMGSTUT3(STR,"\u003e",">")        
 
  NEW STR2 SET STR2=$PIECE(STR,"```",2)
  SET STR2=$$REPLSTR^TMGSTUT3(STR2,"json{","{") 
  NEW PARTA,PARTB SET PARTA=$PIECE(STR,"```",1),PARTB=$PIECE(STR,"```",3,99)
  NEW DATA,ERR DO JSON2ARR^TMGJSON(PARTA_"..."_PARTB,"DATA",,.ERR)
  NEW DATA2 DO JSON2ARR^TMGJSON(STR2,"DATA2",,.ERR)
  SET STR=$GET(DATA("response"))
  QUIT
  ;
TESTOL ;
  NEW QUEST SET QUEST="What is the capital of France?"
  NEW OUT DO OLLAMA(.OUT,QUEST) 
  NEW OLLAMA DO JSON2ARR^TMGJSON($GET(OUT(1)),"OLLAMA")
  IF $DATA(OLLAMA) ZWR OLLAMA
  QUIT
  ;
OLLAMA(OUT,QUEST) ;"  Query ollama, running on local hardware, for answers.  
  ;"Example of call from BASH
  ;"  curl -X POST http://localhost:11434/api/generate -d '{
  ;"   "model": "llama2",
  ;"   "prompt": "What is the capital of France?",
  ;"   "stream": false
  ;"   }'          
  ;
  NEW ARR,HEADERS,DATA
  ;
  ;" 1. Define the URL
  NEW URL SET URL="http://192.168.3.98:11434/api/generate"
  ;
  ;" 2. Define the 'ARR' parameters (for curl flags like -X POST)
  ;"    Note: The LINUXCURL code appends ARR(x) directly.
  ;"    So you'll need the -X POST flag here.
  SET ARR(1)="-X POST "  ;" Add a trailing space for separation
  ;
  ;" 3. Define the 'DATA' payload (the JSON content)
  SET DATA("model")="llama3"
  SET DATA("prompt")=$GET(QUEST)
  SET DATA("stream")="false"   
  ;
  ;" 4. Call LINUXCURL
  ;"    OUT will hold the response from curl
  ;"    URL is the endpoint
  ;"    .ARR passes the array of curl arguments by reference
  ;"    HEADERS is omitted (can be passed as .HEADERS if needed)
  ;"    .DATA passes the data payload by reference
  DO LINUXCURL^TMGKERNL(.OUT,URL,.ARR,,.DATA) ;" Note the two commas for the omitted HEADERS
  QUIT
 
 ;"======================================================================
AICONSOLE ;
AIL1 ;
  NEW ARR,IDX SET IDX=1
  NEW INPUT,OUT
  FOR  DO  QUIT:INPUT=""
  . READ ">",INPUT WRITE !
  . IF INPUT="" QUIT
  . SET ARR(IDX)=INPUT,IDX=IDX+1
  IF $DATA(ARR)'>0 GOTO AIDN
  DO OPENAICURL(.ARR,.OUT)
  ZWR ARR
  W "---",!
  IF $DATA(OUT("choices",1,"text")) WRITE OUT("choices",1,"text"),!
  SET IDX=0
  FOR  SET IDX=$ORDER(OUT("choices",1,"text",IDX)) QUIT:IDX'>0  DO
  . WRITE $GET(OUT("choices",1,"text",IDX)),!
  GOTO AIL1
AIDN ;  
  QUIT
  ;          
OPENAI ;
  NEW ARR,OUT
  SET ARR(1)="Tell a sarcastic joke about smashing pumpkins"
  ;"SET ARR(1)="Q: What orbits the Earth, with sarcastic reply"
  DO OPENAICURL(.ARR,.OUT,1)    
  QUIT
  ;
OPENAICURL(PROMPTARR,RESULT,VERBOSE)  ;
  NEW TMGOUT,ARR,HDR,DATA,TMGERR
  SET URL="https://api.openai.com/v1/engines/text-davinci-002/completions"
  SET KEY=$GET(^TMG("TMP","OPENAI","KEY"))
  ;
  SET HDR(1)="Content-Type: application/json"
  SET HDR(2)="Authorization: Bearer "_KEY
  ;  
  SET DATA("prompt")=$$ARR2STR^TMGSTUT2(.PROMPTARR,"\n")
  SET DATA("temperature")=0.5
  SET DATA("max_tokens")=2000
  ;
  DO LINUXCURL^TMGKERNL(.TMGOUT,URL,.ARR,.HDR,.DATA)
  DO DECODE^%webjson("TMGOUT","RESULT","TMGERR")
  ;
  IF $GET(VERBOSE) ZWRITE RESULT
  QUIT
  ;