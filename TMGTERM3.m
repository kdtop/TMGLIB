TMGTERM3  ;TMG/kst/Terminal interface (Unicode ARROW drawing) ;9/19/2024
         ;;1.0;TMG-LIB;**1,17**;9/19/24
  ;
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 9/19/2024  Kevin S. Toppenberg MD
  ;"
  ;"This file is part of the TMG LIBRARY, and may only be used in accordence
  ;" to license terms outlined in separate file TMGLICNS.m, which should 
  ;" always be distributed with this file.
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ; 

WRITEARROW(DIR,ARR,OPTION) ;"Print unicode arrow. 
  ;"INPUT:  DIR - 'UP','DOWN','LEFT','RIGHT'
  ;"             or NAME from listing in ARROWREF
  ;"        ARR -- OPTIONAL.  PASS BY REFERENCE.  If not passed, then loaded with items from ARROWREF
  ;"        OPTION -- OPTIONAL. PASS BY REFERENCE.  Future expansion
  IF $DATA(ARR)=0 DO GETARROWCODES(.ARR)
  SET DIR=$GET(DIR)
  NEW CODE SET CODE=$GET(ARR(DIR_"WARDS ARROW"))
  IF CODE="" SET CODE=$GET(ARR(DIR))
  IF CODE'="" DO
  . DO UTF8WRITE^TMGSTUTL(CODE)
  QUIT
  ;
GETARROWCODES(ARR) ;"Set up array of line drawing codes
  ;"Input: ARR -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
  ;"         ARR(<NAME>)=<CODE>
  ;"        e.g. ARR("LEFTWARDS ARROW")="$2190"
  ;"Result: None. 
  NEW CODE,DONE,IDX SET DONE=0
  FOR IDX=1:1 DO  QUIT:DONE
  . NEW LINE SET LINE=$$TRIM^XLFSTR($TEXT(ARROWREF+IDX))
  . IF LINE="" SET DONE=1 QUIT
  . SET LINE=$PIECE(LINE,";;",2,99)
  . IF LINE["<DONE>" SET DONE=1 QUIT
  . IF LINE["@@" QUIT  ;"ignore title lines
  . SET CODE=$PIECE(LINE,"^",1),LINE=$PIECE(LINE,"^",2,99)   
  . NEW NAME SET NAME=$$UP^XLFSTR(LINE)
  . SET ARR(NAME)=CODE
  QUIT;
  ;
ARROWREF ;"NOTE: When using names below for lookup, CHANGE TO ALL UPPER CASE. 
  ;;"@@---Simple arrows---    
  ;;$2190^Leftwards Arrow
  ;;$2191^Upwards Arrow
  ;;$2192^Rightwards Arrow
  ;;$2193^Downwards Arrow
  ;;$2194^Left-Right Arrow
  ;;$2195^Up-Down Arrow
  ;;$2196^Up-Left Arrow
  ;;$2197^Up-Right Arrow    
  ;;$2198^Down-Right Arrow    
  ;;$2199^Down-Left Arrow
  ;;"@@---Arrows with modifications---
  ;;$219A^Leftwards Arrow with Stroke
  ;;$219B^Rightwards Arrow with Stroke
  ;;$219C^Leftwards Wave Arrow
  ;;$219D^Rightwards Wave Arrow
  ;;$219E^Leftwards Two Headed Arrow
  ;;$219F^Upwards Two Headed Arrow
  ;;$21A0^Rightwards Two Headed Arrow
  ;;$21A1^Downwards Two Headed Arrow
  ;;$21A2^Leftwards Arrow with Tail
  ;;$21A3^Rightwards Arrow with Tail
  ;;$21A4^Leftwards Arrow from Bar
  ;;$21A5^Upwards Arrow from Bar
  ;;$21A6^Rightwards Arrow from Bar
  ;;$21A7^Downwards Arrow from Bar
  ;;$21A8^Up Down Arrow with Base
  ;;$21A9^Right Arrow Curving Left
  ;;$21AA^Left Arrow Curving Right
  ;;$21AB^Leftwards Arrow with Loop
  ;;$21AC^Rightwards Arrow with Loop
  ;;$21AD^Left Right Wave Arrow
  ;;$21AE^Left Right Arrow with Stroke
  ;;$21AF^Downwards Zigzag Arrow
  ;;"@@---Arrows with bent tips---
  ;;$21B0^Upwards Arrow with Tip Leftwards
  ;;$21B1^Upwards Arrow with Tip Rightwards
  ;;$21B2^Downwards Arrow with Tip Leftwards
  ;;$21B3^Downwards Arrow with Tip Rightwards
  ;;"@@---Keyboard symbols and circle arrows---
  ;;$21B4^Rightwards Arrow with Corner Downwards
  ;;$21B5^Downwards Arrow with Corner Leftwards
  ;;$21B6^Anticlockwise Top Semicircle Arrow
  ;;$21B7^Clockwise Top Semicircle Arrow
  ;;$21B8^North West Arrow To Long Bar
  ;;$21B9^Leftwards Arrow To Bar Over Rightwards Arrow To Bar
  ;;$21BA^Anticlockwise Open Circle Arrow
  ;;$21BB^Clockwise Open Circle Arrow
  ;;"@@---Harpoons---
  ;;$21BC^Leftwards Harpoon with Barb Upwards
  ;;$21BD^Leftwards Harpoon with Barb Downwards
  ;;$21BE^Upwards Harpoon with Barb Rightwards
  ;;$21BF^Upwards Harpoon with Barb Leftwards
  ;;$21C0^Rightwards Harpoon with Barb Upwards
  ;;$21C1^Rightwards Harpoon with Barb Downwards
  ;;$21C2^Downwards Harpoon with Barb Rightwards
  ;;$21C3^Downwards Harpoon with Barb Leftwards
  ;;"@@---Paired arrows and harpoons---
  ;;$21C4^Rightwards Arrow Over Leftwards Arrow
  ;;$21C5^Upwards Arrow Leftwards of Downwards Arrow
  ;;$21C6^Leftwards Arrow Over Rightwards Arrow
  ;;$21C7^Leftwards Paired Arrows
  ;;$21C8^Upwards Paired Arrows
  ;;$21C9^Rightwards Paired Arrows
  ;;$21CA^Downwards Paired Arrows
  ;;$21CB^Leftwards Harpoon Over Rightwards Harpoon
  ;;$21CC^Rightwards Harpoon Over Leftwards Harpoon
  ;;"@@---Double arrows---
  ;;$21CD^Leftwards Double Arrow with Stroke
  ;;$21CE^Left Right Double Arrow with Stroke
  ;;$21CF^Rightwards Double Arrow with Stroke
  ;;$21D0^Leftwards Double Arrow
  ;;$21D1^Upwards Double Arrow
  ;;$21D2^Rightwards Double Arrow
  ;;$21D3^Downwards Double Arrow
  ;;$21D4^Left Right Double Arrow
  ;;$21D5^Up Down Double Arrow
  ;;$21D6^North West Double Arrow
  ;;$21D7^North East Double Arrow
  ;;$21D8^South East Double Arrow
  ;;$21D9^South West Double Arrow
  ;;"@@---Miscellaneous arrows and keyboard symbols---
  ;;$21DA^Leftwards Triple Arrow
  ;;$21DB^Rightwards Triple Arrow
  ;;$21DC^Leftwards Squiggle Arrow
  ;;$21DD^Rightwards Squiggle Arrow
  ;;$21DE^Upwards Arrow with Double Stroke
  ;;$21DF^Downwards Arrow with Double Stroke
  ;;$21E0^Leftwards Dashed Arrow
  ;;$21E1^Upwards Dashed Arrow
  ;;$21E2^Rightwards Dashed Arrow                                
  ;;$21E3^Downwards Dashed Arrow
  ;;$21E4^Leftwards Arrow To Bar                      
  ;;$21E5^Rightwards Arrow To Bar
  ;;"@@---White arrows and keyboard symbols---
  ;;$21E6^Leftwards White Arrow
  ;;$21E7^Upwards White Arrow
  ;;$21E8^Rightwards White Arrow
  ;;$21E9^Downwards White Arrow
  ;;$21EA^Upwards White Arrow from Bar
  ;;$21EB^Upwards White Arrow On Pedestal
  ;;$21EC^Upwards White Arrow On Pedestal with Horizontal Bar
  ;;$21ED^Upwards White Arrow On Pedestal with Vertical Bar
  ;;$21EE^Upwards White Double Arrow
  ;;$21EF^Upwards White Double Arrow On Pedestal
  ;;$21F0^Rightwards White Arrow from Wall
  ;;$21F1^North West Arrow To Corner
  ;;$21F2^South East Arrow To Corner
  ;;$21F3^Up Down White Arrow
  ;;"@@---Miscellaneous arrows---
  ;;$21F4^Right Arrow with Small Circle
  ;;$21F5^Downwards Arrow Leftwards of Upwards Arrow
  ;;$21F6^Three Rightwards Arrows
  ;;$21F7^Leftwards Arrow with Vertical Stroke
  ;;$21F8^Rightwards Arrow with Vertical Stroke
  ;;$21F9^Left Right Arrow with Vertical Stroke
  ;;$21FA^Leftwards Arrow with Double Vertical Stroke
  ;;$21FB^Rightwards Arrow with Double Vertical Stroke
  ;;$21FC^Left Right Arrow with Double Vertical Stroke
  ;;$21FD^Leftwards Open-Headed Arrow
  ;;$21FE^Rightwards Open-Headed Arrow
  ;;$21FF^Left Right Open-Headed Arrow
  ;;<DONE>
