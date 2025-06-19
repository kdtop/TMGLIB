TMGPRE01 ;TMG/kst/Pre-visit HTML stuff ;6/17/25
      ;;1.0;TMG-LIB;**1**;06/17/25
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/17/25  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
TESTGQ ;
  NEW OUT DO GETQUEST(74592,.OUT)   ;"DFN for ZZTEST,BABY test patient.  
  NEW DIR SET DIR=$$GETDIRNM^TMGIOUTL("Select output directory","/mnt/")
  DO ARR2HFS^TMGIOUT3("OUT",DIR,"index.html")
  QUIT
  ;
GETQUEST(TMGDFN,OUT)  ;"Get patient questionaire. 
  NEW TMP DO PREPDATA(TMGDFN,.TMP)
  NEW INFO MERGE INFO("DATA","BODY")=TMP
  DO HTMT2DOC^TMGHTM3("OUT","PTQUESTMPL","TMGPRE01",.INFO)  ;"Combine template with INFO data
  QUIT
  ;
PREPDATA(TMGDFN,OUT)  ;
  ;"TO DO... ADD PATIENT NAME....
  NEW IDX,INDENT SET INDENT=1
  DO ADDLN(.OUT,.IDX,.INDENT,"<h1>Family Physicians of Greeneville</h1>")
  DO ADDLN(.OUT,.IDX,.INDENT,"<p><b>Patient:</b> "_$$GETNAME(TMGDFN)_"<br></p>")
  DO WHYSEE(.OUT,.IDX,.INDENT)  ;"Why seeing doctor?
  DO NEWHX(.OUT,.IDX,.INDENT)  ;"Any new history?
  DO ADDLN(.OUT,.IDX,.INDENT,"<h1>Review of Systems</h1>")
  DO ROS(.OUT,.IDX,.INDENT)  ;"Review of system. 
  QUIT
  ;
WHYSEE(OUT,IDX,INDENT) ;"Why are you seeing doctor today?  
  DO STARTCATEGORY(.OUT,.IDX,.INDENT) ;"Start Category section
  DO ADDLN(.OUT,.IDX,.INDENT,"<h2>Why are you seeing the doctor today?</h2>")
  NEW LST SET LST="Physical^Recheck^Sick^New Problem"
  NEW PREFIX SET PREFIX="visit_reason"
  DO ADDCBLST(.OUT,.IDX,.INDENT,PREFIX,LST)
  DO ADDDETAILS(.OUT,.IDX,.INDENT,PREFIX) ;"Add Details box  
  DO ENDDIV(.OUT,.IDX,.INDENT) ;"End Category section
  QUIT
  ;
NEWHX(OUT,IDX,INDENT)  ;"Any new history?
  DO STARTCATEGORY(.OUT,.IDX,.INDENT) ;"Start Category section
  DO ADDLN(.OUT,.IDX,.INDENT,"<h2>Since your last visit, have you had any of the following?</h2>")
  NEW PRE SET PRE="hx_change_"
  DO ADDQSTGRP(.OUT,.IDX,.INDENT,PRE_"new_prob","New medical problems?")  ;"Add question group  
  DO ADDQSTGRP(.OUT,.IDX,.INDENT,PRE_"other_provider","Seen any other doctors (e.g. specialists, ER doctor, chiropracter, others etc)?")  ;"Add question group  
  DO ADDQSTGRP(.OUT,.IDX,.INDENT,PRE_"surgery","Had any new surgeries?")  ;"Add question group  
  DO ADDQSTGRP(.OUT,.IDX,.INDENT,PRE_"social","Any change in use of tobacco or alcohol? Any significant changes in social support / employment / living arrangements?")  ;"Add question group  
  DO ADDQSTGRP(.OUT,.IDX,.INDENT,PRE_"family","Any family members with new diseases (e.g. heart attack, diabetes, cancer etc)?")  ;"Add question group
  ;
  DO ADDQSTGRP(.OUT,.IDX,.INDENT,PRE_"tests","Have you had any recent medical tests elsewhere?")  ;"Add question group  
  DO ADDLN(.OUT,.IDX,.INDENT,"<div id='testing_list_container'>") SET INDENT=INDENT+1
  NEW LST SET LST="blood work^mammogram^xrays^MRI^CT scan^colon or stomach scope^ultrasound^echocardiogram^cardiac stress test^Holter monitor^ECG^bone density"
  DO ADDCBLST(.OUT,.IDX,.INDENT,"testing",LST)  ;"Add checkbox list. 
  DO ADDDETAILS(.OUT,.IDX,.INDENT,"testing") ;"Add Details box  
  DO ENDDIV(.OUT,.IDX,.INDENT) ;"End <div id='testing_list_container'>
  DO ENDDIV(.OUT,.IDX,.INDENT) ;"End category section 
  QUIT
  ;
ROS(OUT,IDX,INDENT) ;"Review of Systems
  NEW ROS,JDX SET JDX=0
  ;
  SET ROS($I(JDX),"section")="constitutional"
  SET ROS(JDX,"LST")="Chills^Fatigue^Fever^Weight gain^Weight loss"  ;//ADD MORE LATER
  ;
  SET ROS($I(JDX),"section")="heent"
  SET ROS(JDX,"TEXT")="HEAD, EARS, EYES, THROAT"
  SET ROS(JDX,"LST")="Hearing loss^Sinus pressure^Visual changes"  ;//ADD MORE LATER
  ;
  SET ROS($I(JDX),"section")="respiratory"
  SET ROS(JDX,"LST")="Cough^Shortness of breath^Wheezing"  ;//ADD MORE LATER
  ;
  SET ROS($I(JDX),"section")="cardiovascular"
  SET ROS(JDX,"LST")="Chest pain^Pain while walking^Edema^Palpitations"  ;//ADD MORE LATER   
  ;
  SET ROS($I(JDX),"section")="gastrointestinal"
  SET ROS(JDX,"LST")="Abdominal pain^Blood in stool^Constipation^Diarrhea^Heartburn^Loss of appetite^Nausea^Vomiting"  ;//ADD MORE LATER   
  ;
  SET ROS($I(JDX),"section")="genitourinary"
  SET ROS(JDX,"LST")="Painful urination (Dysuria)^Excessive amount of urine (Polyuria)^Urinary frequency"  ;//ADD MORE LATER   
  ;
  SET ROS($I(JDX),"section")="metabolic"
  SET ROS(JDX,"TEXT")="METABOLIC/ENDOCRINE"
  SET ROS(JDX,"LST")="Cold intolerance^Heat intolerance^Excessive thirst (Polydipsia)^Excessive hunger (Polyphagia)"  ;//ADD MORE LATER   
  ;
  SET ROS($I(JDX),"section")="neurological"
  SET ROS(JDX,"LST")="Dizziness^Extremity numbness^Extremity weakness^Headaches^Seizures^Tremors"  ;//ADD MORE LATER   
  ;
  SET ROS($I(JDX),"section")="psychiatric"
  SET ROS(JDX,"LST")="Anxiety^Depression"  ;//ADD MORE LATER   
  ;
  SET ROS($I(JDX),"section")="musculoskeletal"
  SET ROS(JDX,"LST")="Back pain^Joint pain^Joint swelling^Neck pain"  ;//ADD MORE LATER   
  ;
  SET ROS($I(JDX),"section")="hematologic"
  SET ROS(JDX,"LST")="Easily bleeds^Easily bruises^Lymphedema^Issues with blood clots"  ;//ADD MORE LATER   
  ;
  SET ROS($I(JDX),"section")="immunologic"
  SET ROS(JDX,"LST")="Food allergies^Seasonal allergies"  ;//ADD MORE LATER   
  ;
  SET JDX=0
  FOR  SET JDX=$ORDER(ROS(JDX)) QUIT:JDX'>0  DO
  . NEW SECT SET SECT=$GET(ROS(JDX,"section")) QUIT:SECT=""
  . NEW LST SET LST=$GET(ROS(JDX,"LST")) QUIT:LST=""
  . NEW TXT SET TXT=$GET(ROS(JDX,"TEXT")) IF TXT="" SET TXT=$$UP^XLFSTR(SECT)
  . DO ADDROSSECT(.OUT,.IDX,.INDENT,SECT,TXT,LST) 
  ;
  QUIT
  ;
STARTCATEGORY(OUT,IDX,INDENT) ;"Start Category section
  DO ADDLN(.OUT,.IDX,.INDENT,"<div class='category-section'>")
  SET INDENT=INDENT+1
  QUIT
  ;
ENDDIV(OUT,IDX,INDENT) ;"End DIV
  DO CLOSETAG(.OUT,.IDX,.INDENT,"div")
  QUIT  
  ;
CLOSETAG(OUT,IDX,INDENT,TAG) ;"CLOSE TAG
  SET INDENT=INDENT-1
  DO ADDLN(.OUT,.IDX,.INDENT,"</"_TAG_">")
  QUIT  
  ;
ADDCBLST(OUT,IDX,INDENT,PREFIX,LST)  ;"Add a checkbox list, from LST
  ;"LST format:  item^item^item^....
  DO ADDLN(.OUT,.IDX,.INDENT,"<ul>") SET INDENT=INDENT+1
  NEW JDX FOR JDX=1:1:$LENGTH(LST,"^") DO
  . NEW ITEM SET ITEM=$PIECE(LST,"^",JDX) QUIT:ITEM=""
  . NEW LCITEM SET LCITEM=$$LOW^XLFSTR(ITEM)
  . SET LCITEM=$TRANSLATE(LCITEM," ","_")
  . NEW NAME SET NAME=PREFIX_"_"_LCITEM
  . NEW STR SET STR="<li><label><input type='checkbox' name='"_NAME_"' class='sr-only'><span class='custom-checkbox-text'>"_ITEM_"</span></label></li>"
  . DO ADDLN(.OUT,.IDX,.INDENT,STR)
  DO CLOSETAG(.OUT,.IDX,.INDENT,"ul")
  QUIT
  ;
ADDDETAILS(OUT,IDX,INDENT,PREFIX) ;"Add Details box  
  DO ADDLN(.OUT,.IDX,.INDENT,"<div class='details-input-group'>") SET INDENT=INDENT+1
  NEW NAME SET NAME=PREFIX_"_details"
  DO ADDLN(.OUT,.IDX,.INDENT,"<label for='"_NAME_"'>Other -- Enter Details (if needed):</label>")
  DO ADDLN(.OUT,.IDX,.INDENT,"<textarea id='"_NAME_"' name='"_NAME_"'></textarea>")
  DO CLOSETAG(.OUT,.IDX,.INDENT,"div")
  QUIT
  ;
ADDROSSECT(OUT,IDX,INDENT,PREFIX,TEXT,LST) ;
  DO STARTCATEGORY(.OUT,.IDX,.INDENT) ;"Start Category section
  DO ADDLN(.OUT,.IDX,.INDENT,"<h2>"_TEXT_"</h2>") 
  DO ADDCBLST(.OUT,.IDX,.INDENT,PREFIX,LST)  ;"Add a checkbox list, from LST
  DO ADDDETAILS(.OUT,.IDX,.INDENT,PREFIX) ;"Add Details box  
  DO CLOSETAG(.OUT,.IDX,.INDENT,"div")  ;"close category section
  QUIT
  ;
ADDQSTGRP(OUT,IDX,INDENT,PREFIX,TEXT)  ;"Add question group  
  DO ADDLN(.OUT,.IDX,.INDENT,"<div class='question-group'>") SET INDENT=INDENT+1
  DO ADDLN(.OUT,.IDX,.INDENT,"<label class='main-question-label'>"_TEXT_"</label>")
  DO ADDLN(.OUT,.IDX,.INDENT,"<div class='details-options-row'>") SET INDENT=INDENT+1
  DO ADDLN(.OUT,.IDX,.INDENT,"<label class='none-option-label'>") SET INDENT=INDENT+1
  DO ADDLN(.OUT,.IDX,.INDENT,"<input type='checkbox' name='"_PREFIX_"_none' class='sr-only none-toggle-checkbox ")
  DO ADDLN(.OUT,.IDX,INDENT+1,"data-hide-target-ids='"_PREFIX_"_details_label,"_PREFIX_"_textarea_container'>")
  DO ADDLN(.OUT,.IDX,.INDENT,"<span class='custom-checkbox-text none-checkbox-text'>NONE</span>")
  DO CLOSETAG(.OUT,.IDX,.INDENT,"label")
  ;   
  DO ADDLN(.OUT,.IDX,.INDENT,"<span class='details-label' id='"_PREFIX_"_details_label'>Details:</span>")
  DO CLOSETAG(.OUT,.IDX,.INDENT,"div")
  DO ADDLN(.OUT,.IDX,.INDENT,"<div class='details-textarea-container' id='"_PREFIX_"_textarea_container'>") SET INDENT=INDENT+1
  DO ADDLN(.OUT,.IDX,.INDENT,"<textarea id='"_PREFIX_"' name='"_PREFIX_"'></textarea>")
  DO CLOSETAG(.OUT,.IDX,.INDENT,"div")
  DO CLOSETAG(.OUT,.IDX,.INDENT,"div")
  QUIT
  ;
ADDLN(ARR,IDX,INDENT,STR) ;
  IF $GET(IDX)'>0 SET IDX=$ORDER(ARR(""),-1)
  SET IDX=IDX+1
  SET INDENT=$GET(INDENT)
  NEW LEN SET LEN=$LENGTH(STR)+(INDENT*2)
  NEW TMP SET TMP=$$RJ^XLFSTR(STR,LEN," ")
  SET ARR(IDX)=TMP
  QUIT
  ;
GETNAME(TMGDFN) ;
  SET TMGDFN=+$GET(TMGDFN)
  NEW ZN SET ZN=$GET(^DPT(TMGDFN,0))
  NEW NAME SET NAME=$PIECE(ZN,"^",1)
  NEW DOB SET DOB=$PIECE(ZN,"^",3)
  SET DOB=$$FMTE^XLFDT(DOB,"5D")
  QUIT NAME_" ("_DOB_")"
  ;  
  ;"===========================================================================
  ;"===========================================================================
  ;
PTQUESTMPL ;" Patient questionaire HTML Template.    
  ;;<!DOCTYPE html>
  ;;<html lang="en-US">
  ;;<head>
  ;;  <meta charset="utf-8">
  ;;  <title>Review of Systems</title>
  ;;  <style>
  ;;  body {
  ;;    font-family: Arial, sans-serif;
  ;;    line-height: 1.6;
  ;;    margin: 20px auto;
  ;;    max-width: 800px;
  ;;    padding: 0 15px;
  ;;    color: #333;
  ;;  }
  ;;
  ;;  h1 {
  ;;    text-align: center;
  ;;    color: #2c3e50;
  ;;    margin-bottom: 30px;
  ;;  }
  ;;
  ;;  h2 {
  ;;    color: #2c3e50;
  ;;    border-bottom: 2px solid #3498db;
  ;;    padding-bottom: 5px;
  ;;    margin-top: 30px;
  ;;    margin-bottom: 15px;
  ;;  }
  ;;
  ;;  ul {
  ;;    list-style: none;
  ;;    padding: 0;
  ;;    margin-bottom: 20px;
  ;;    display: flex;
  ;;    flex-wrap: wrap;
  ;;    gap: 10px;
  ;;  }
  ;;
  ;;  li {
  ;;    margin-bottom: 0;
  ;;  }
  ;;
  ;;  /* --- Custom Checkbox Styling --- */
  ;;  .sr-only {
  ;;    position: absolute;
  ;;    width: 1px;
  ;;    height: 1px;
  ;;    padding: 0;
  ;;    margin: -1px;
  ;;    overflow: hidden;
  ;;    clip: rect(0, 0, 0, 0);
  ;;    white-space: nowrap;
  ;;    border-width: 0;
  ;;  }
  ;;
  ;;  .custom-checkbox-text {
  ;;    display: inline-block;
  ;;    padding: 7px 12px;
  ;;    border-radius: 12px;
  ;;    background-color: #f0f0f0;
  ;;    color: #555;
  ;;    transition: background-color 0.2s ease, color 0.2s ease, transform 0.1s ease;
  ;;    cursor: pointer;
  ;;    user-select: none;
  ;;  }
  ;;
  ;;  label:hover .custom-checkbox-text {
  ;;    background-color: #e2e2e2;
  ;;  }
  ;;
  ;;  input[type="checkbox"]:checked + .custom-checkbox-text {
  ;;    background-color: #3498db; /* Default checked color (blue) */
  ;;    color: white;
  ;;    transform: translateY(-1px);
  ;;    box-shadow: 0 2px 5px rgba(0,0,0,0.2);
  ;;  }
  ;;
  ;;  /* Specific style for 'NONE' checkbox when checked */
  ;;  input.none-toggle-checkbox:checked + .none-checkbox-text {
  ;;    background-color: #e74c3c; /* Reddish color for NONE when checked */
  ;;    color: white;
  ;;  }
  ;;
  ;;  /* Styles for general labels (like for custom checkboxes) */
  ;;  label {
  ;;    display: flex;
  ;;    align-items: center;
  ;;    width: fit-content;
  ;;  }
  ;;
  ;;  /* --- Details Textarea Styling (General) --- */
  ;;  .details-input-group {
  ;;    /* This general style applies to sections where NONE is not used this way */
  ;;    margin-top: 15px;
  ;;    margin-bottom: 25px;
  ;;  }
  ;;
  ;;  .details-input-group label {
  ;;    display: block;
  ;;    margin-bottom: 5px;
  ;;    font-weight: bold;
  ;;    color: #444;
  ;;  }
  ;;
  ;;  .details-input-group textarea {
  ;;    width: 100%;
  ;;    min-height: 30px;
  ;;    padding: 8px 10px;
  ;;    border: 1px solid #ccc;
  ;;    border-radius: 4px;
  ;;    font-size: 1em;
  ;;    box-sizing: border-box;
  ;;    resize: vertical;
  ;;  }
  ;;
  ;;  /* --- NEW: Specific Styling for 'Since your last visit' section --- */
  ;;  .question-group {
  ;;    margin-bottom: 25px; /* Space between each question group */
  ;;  }
  ;;
  ;;  .main-question-label { /* Style for the main question label */
  ;;    display: block; /* Ensures it's on its own line */
  ;;    margin-bottom: 10px; /* Space below the main question */
  ;;    font-weight: bold; /* Make the main question prominent */
  ;;    color: #333;
  ;;  }
  ;;
  ;;  .details-options-row {
  ;;    display: flex; /* Use flexbox to align "NONE" and "Details:" side-by-side */
  ;;    align-items: center; /* Vertically align items */
  ;;    gap: 15px; /* Space between "NONE" and "Details:" */
  ;;    margin-bottom: 10px; /* Space above textarea */
  ;;  }
  ;;
  ;;  .details-label { /* Style for the "Details:" label in this specific context */
  ;;      font-weight: bold;
  ;;      color: #444;
  ;;      white-space: nowrap; /* Prevent "Details:" from wrapping */
  ;;    }
  ;;
  ;;    .details-textarea-container {
  ;;      margin-top: 5px; /* Space below the details/none row */
  ;;    }
  ;;
  ;;    .details-textarea-container textarea {
  ;;      width: 100%;
  ;;      min-height: 30px;
  ;;      padding: 8px 10px;
  ;;      border: 1px solid #ccc;
  ;;      border-radius: 4px;
  ;;      font-size: 1em;
  ;;      box-sizing: border-box;
  ;;      resize: vertical;
  ;;    }
  ;;
  ;;    /* Class to hide elements with JavaScript */
  ;;    .hidden {
  ;;      display: none !important; /* Use !important to ensure it overrides other display properties */
  ;;    }
  ;;
  ;;    /* Responsive adjustments */
  ;;    @media (max-width: 768px) {
  ;;      body {
  ;;        margin: 15px;
  ;;      }
  ;;      ul {
  ;;        gap: 8px;
  ;;      }
  ;;      .custom-checkbox-text {
  ;;        padding: 6px 10px;
  ;;        font-size: 0.95em;
  ;;      }
  ;;      .details-options-row {
  ;;        flex-direction: column; /* Stack "NONE" and "Details:" vertically on small screens */
  ;;        align-items: flex-start;
  ;;        gap: 5px;
  ;;      }
  ;;    }
  ;;  </style>
  ;;</head>
  ;;<body>
  ;;  {@@@[BODY]@@@}
  ;;
  ;;  <script>
  ;;    document.addEventListener('DOMContentLoaded', function() {
  ;;      const noneCheckboxes = document.querySelectorAll('.none-toggle-checkbox');
  ;;
  ;;      noneCheckboxes.forEach(checkbox => {
  ;;        const toggleVisibility = (isChecked) => {
  ;;          // Get the comma-separated string of IDs to hide/show
  ;;          const targetIdsString = checkbox.dataset.hideTargetIds;
  ;;          if (!targetIdsString) return;
  ;;
  ;;          const targetIds = targetIdsString.split(','); // Split into an array of IDs
  ;;
  ;;          targetIds.forEach(id => {
  ;;            const targetElement = document.getElementById(id.trim()); // Trim whitespace
  ;;            if (targetElement) {
  ;;              if (isChecked) {
  ;;                targetElement.classList.add('hidden'); // Hide the element
  ;;                // If the element contains checkboxes or a textarea, clear/uncheck them
  ;;                const checkboxes = targetElement.querySelectorAll('input[type="checkbox"]');
  ;;                checkboxes.forEach(cb => {
  ;;                  cb.checked = false; // Uncheck all checkboxes within the hidden section
  ;;                });
  ;;                const textarea = targetElement.querySelector('textarea');
  ;;                if (textarea) {
  ;;                  textarea.value = ''; // Clear textarea
  ;;                }
  ;;              } else {
  ;;                targetElement.classList.remove('hidden'); // Show the element
  ;;              }
  ;;            }
  ;;          });
  ;;        };
  ;;
  ;;        // Add event listener for changes
  ;;        checkbox.addEventListener('change', function() {
  ;;          toggleVisibility(this.checked);
  ;;        });
  ;;
  ;;        // Initial check on page load in case checkboxes are pre-checked by browser
  ;;        toggleVisibility(checkbox.checked);
  ;;      });
  ;;    });
  ;;  </script>
  ;;</body>
  ;;</html>
  ;;#DONE_WITH_HTML  
  ;
  
  ;"=== Example of expected output below ====  DELETE LATER...
    ;"<!DOCTYPE html>
    ;"<html lang="en-US">
    ;"<head>
    ;"    <meta charset="utf-8">
    ;"    <title>Review of Systems</title>
    ;"    <style>
    ;"        body {
    ;"            font-family: Arial, sans-serif;
    ;"            line-height: 1.6;
    ;"            margin: 20px auto;
    ;"            max-width: 800px;
    ;"            padding: 0 15px;
    ;"            color: #333;
    ;"        }
    ;"
    ;"        h1 {
    ;"            text-align: center;
    ;"            color: #2c3e50;
    ;"            margin-bottom: 30px;
    ;"        }
    ;"
    ;"        h2 {
    ;"            color: #2c3e50;
    ;"            border-bottom: 2px solid #3498db;
    ;"            padding-bottom: 5px;
    ;"            margin-top: 30px;
    ;"            margin-bottom: 15px;
    ;"        }
    ;"
    ;"        ul {
    ;"            list-style: none;
    ;"            padding: 0;
    ;"            margin-bottom: 20px;
    ;"            display: flex;
    ;"            flex-wrap: wrap;
    ;"            gap: 10px;
    ;"        }
    ;"
    ;"        li {
    ;"            margin-bottom: 0;
    ;"        }
    ;"
    ;"        /* --- Custom Checkbox Styling --- */
    ;"        .sr-only {
    ;"            position: absolute;
    ;"            width: 1px;
    ;"            height: 1px;
    ;"            padding: 0;
    ;"            margin: -1px;
    ;"            overflow: hidden;
    ;"            clip: rect(0, 0, 0, 0);
    ;"            white-space: nowrap;
    ;"            border-width: 0;
    ;"        }
    ;"
    ;"        .custom-checkbox-text {
    ;"            display: inline-block;
    ;"            padding: 7px 12px;
    ;"            border-radius: 12px;
    ;"            background-color: #f0f0f0;
    ;"            color: #555;
    ;"            transition: background-color 0.2s ease, color 0.2s ease, transform 0.1s ease;
    ;"            cursor: pointer;
    ;"            user-select: none;
    ;"        }
    ;"
    ;"        label:hover .custom-checkbox-text {
    ;"            background-color: #e2e2e2;
    ;"        }
    ;"
    ;"        input[type="checkbox"]:checked + .custom-checkbox-text {
    ;"            background-color: #3498db; /* Default checked color (blue) */
    ;"            color: white;
    ;"            transform: translateY(-1px);
    ;"            box-shadow: 0 2px 5px rgba(0,0,0,0.2);
    ;"        }
    ;"
    ;"        /* Specific style for 'NONE' checkbox when checked */
    ;"        input.none-toggle-checkbox:checked + .none-checkbox-text {
    ;"            background-color: #e74c3c; /* Reddish color for NONE when checked */
    ;"            color: white;
    ;"        }
    ;"
    ;"        /* Styles for general labels (like for custom checkboxes) */
    ;"        label {
    ;"            display: flex;
    ;"            align-items: center;
    ;"            width: fit-content;
    ;"        }
    ;"
    ;"        /* --- Details Textarea Styling (General) --- */
    ;"        .details-input-group {
    ;"            /* This general style applies to sections where NONE is not used this way */
    ;"            margin-top: 15px;
    ;"            margin-bottom: 25px;
    ;"        }
    ;"
    ;"        .details-input-group label {
    ;"            display: block;
    ;"            margin-bottom: 5px;
    ;"            font-weight: bold;
    ;"            color: #444;
    ;"        }
    ;"
    ;"        .details-input-group textarea {
    ;"            width: 100%;
    ;"            min-height: 30px;
    ;"            padding: 8px 10px;
    ;"            border: 1px solid #ccc;
    ;"            border-radius: 4px;
    ;"            font-size: 1em;
    ;"            box-sizing: border-box;
    ;"            resize: vertical;
    ;"        }
    ;"
    ;"        /* --- NEW: Specific Styling for 'Since your last visit' section --- */
    ;"        .question-group {
    ;"            margin-bottom: 25px; /* Space between each question group */
    ;"        }
    ;"
    ;"        .main-question-label { /* Style for the main question label */
    ;"            display: block; /* Ensures it's on its own line */
    ;"            margin-bottom: 10px; /* Space below the main question */
    ;"            font-weight: bold; /* Make the main question prominent */
    ;"            color: #333;
    ;"        }
    ;"
    ;"        .details-options-row {
    ;"            display: flex; /* Use flexbox to align "NONE" and "Details:" side-by-side */
    ;"            align-items: center; /* Vertically align items */
    ;"            gap: 15px; /* Space between "NONE" and "Details:" */
    ;"            margin-bottom: 10px; /* Space above textarea */
    ;"        }
    ;"
    ;"        .details-label { /* Style for the "Details:" label in this specific context */
    ;"            font-weight: bold;
    ;"            color: #444;
    ;"            white-space: nowrap; /* Prevent "Details:" from wrapping */
    ;"        }
    ;"
    ;"        .details-textarea-container {
    ;"            margin-top: 5px; /* Space below the details/none row */
    ;"        }
    ;"
    ;"        .details-textarea-container textarea {
    ;"            width: 100%;
    ;"            min-height: 30px;
    ;"            padding: 8px 10px;
    ;"            border: 1px solid #ccc;
    ;"            border-radius: 4px;
    ;"            font-size: 1em;
    ;"            box-sizing: border-box;
    ;"            resize: vertical;
    ;"        }
    ;"
    ;"        /* Class to hide elements with JavaScript */
    ;"        .hidden {
    ;"            display: none !important; /* Use !important to ensure it overrides other display properties */
    ;"        }
    ;"
    ;"        /* Responsive adjustments */
    ;"        @media (max-width: 768px) {
    ;"            body {
    ;"                margin: 15px;
    ;"            }
    ;"            ul {
    ;"                gap: 8px;
    ;"            }
    ;"            .custom-checkbox-text {
    ;"                padding: 6px 10px;
    ;"                font-size: 0.95em;
    ;"            }
    ;"            .details-options-row {
    ;"                flex-direction: column; /* Stack "NONE" and "Details:" vertically on small screens */
    ;"                align-items: flex-start;
    ;"                gap: 5px;
    ;"            }
    ;"        }
    ;"    </style>
    ;"</head>
    ;"<body>
    ;"    <h1>Family Physicians of Greeneville</h1>
    ;"    <div class="category-section">
    ;"        <h2>Why are you seeing the doctor today?</h2>
    ;"        <ul>
    ;"            <li><label><input type="checkbox" name="visit_reason_physical" class="sr-only"><span class="custom-checkbox-text">Physical</span></label></li>
    ;"            <li><label><input type="checkbox" name="visit_reason_recheck" class="sr-only"><span class="custom-checkbox-text">Recheck</span></label></li>
    ;"            <li><label><input type="checkbox" name="visit_reason_sick" class="sr-only"><span class="custom-checkbox-text">Sick</span></label></li>
    ;"            <li><label><input type="checkbox" name="visit_reason_new_problem" class="sr-only"><span class="custom-checkbox-text">New problem</span></label></li>
    ;"        </ul>
    ;"        <div class="details-input-group">
    ;"            <label for="visit_reason_details">Other -- Enter Details (if needed):</label>
    ;"            <textarea id="visit_reason_details" name="visit_reason_details"></textarea>
    ;"        </div>
    ;"    </div>
    ;"
    ;"    <div class="category-section">
    ;"        <h2>Since your last visit, have you had any of the following?</h2>
    ;"
    ;"        <div class="question-group">
    ;"            <label class="main-question-label">New medical problems?</label>
    ;"            <div class="details-options-row">
    ;"                <label class="none-option-label">
    ;"                    <input type="checkbox" name="hx_change_new_prob_none" class="sr-only none-toggle-checkbox"
    ;"                           data-hide-target-ids="hx_change_new_prob_details_label,hx_change_new_prob_textarea_container">
    ;"                    <span class="custom-checkbox-text none-checkbox-text">NONE</span>
    ;"                </label>
    ;"                <span class="details-label" id="hx_change_new_prob_details_label">Details:</span>
    ;"            </div>
    ;"            <div class="details-textarea-container" id="hx_change_new_prob_textarea_container">
    ;"                <textarea id="hx_change_new_prob" name="hx_change_new_prob"></textarea>
    ;"            </div>
    ;"        </div>
    ;"
    ;"        <div class="question-group">
    ;"            <label class="main-question-label">Seen any other doctors (e.g. specialists, ER doctor, chiropracter, others etc)?</label>
    ;"            <div class="details-options-row">
    ;"                <label class="none-option-label">
    ;"                    <input type="checkbox" name="hx_change_other_providers_none" class="sr-only none-toggle-checkbox"
    ;"                           data-hide-target-ids="hx_change_other_providers_details_label,hx_change_other_providers_textarea_container">
    ;"                    <span class="custom-checkbox-text none-checkbox-text">NONE</span>
    ;"                </label>
    ;"                <span class="details-label" id="hx_change_other_providers_details_label">Details:</span>
    ;"            </div>
    ;"            <div class="details-textarea-container" id="hx_change_other_providers_textarea_container">
    ;"                <textarea id="hx_change_other_providers" name="hx_change_other_providers"></textarea>
    ;"            </div>
    ;"        </div>
    ;"
    ;"        <div class="question-group">
    ;"            <label class="main-question-label">Had any new surgeries?</label>
    ;"            <div class="details-options-row">
    ;"                <label class="none-option-label">
    ;"                    <input type="checkbox" name="hx_change_surgery_none" class="sr-only none-toggle-checkbox"
    ;"                           data-hide-target-ids="hx_change_surgery_details_label,hx_change_surgery_textarea_container">
    ;"                    <span class="custom-checkbox-text none-checkbox-text">NONE</span>
    ;"                </label>
    ;"                <span class="details-label" id="hx_change_surgery_details_label">Details:</span>
    ;"            </div>
    ;"            <div class="details-textarea-container" id="hx_change_surgery_textarea_container">
    ;"                <textarea id="hx_change_surgery" name="hx_change_surgery"></textarea>
    ;"            </div>
    ;"        </div>
    ;"
    ;"        <div class="question-group">
    ;"            <label class="main-question-label">Any change in use of tobacco or alcohol? Any significant changes in social support / employment / living arrangements?</label>
    ;"            <div class="details-options-row">
    ;"                <label class="none-option-label">
    ;"                    <input type="checkbox" name="hx_change_social_none" class="sr-only none-toggle-checkbox"
    ;"                           data-hide-target-ids="hx_change_social_details_label,hx_change_social_textarea_container">
    ;"                    <span class="custom-checkbox-text none-checkbox-text">NONE</span>
    ;"                </label>
    ;"                <span class="details-label" id="hx_change_social_details_label">Details:</span>
    ;"            </div>
    ;"            <div class="details-textarea-container" id="hx_change_social_textarea_container">
    ;"                <textarea id="hx_change_social" name="hx_change_social"></textarea>
    ;"            </div>
    ;"        </div>
    ;"
    ;"        <div class="question-group">
    ;"            <label class="main-question-label">Any family members with new diseases (e.g. heart attack, diabetes, cancer etc)?</label>
    ;"            <div class="details-options-row">
    ;"                <label class="none-option-label">
    ;"                    <input type="checkbox" name="hx_change_family_none" class="sr-only none-toggle-checkbox"
    ;"                           data-hide-target-ids="hx_change_family_details_label,hx_change_family_textarea_container">
    ;"                    <span class="custom-checkbox-text none-checkbox-text">NONE</span>
    ;"                </label>
    ;"                <span class="details-label" id="hx_change_family_details_label">Details:</span>
    ;"            </div>
    ;"            <div class="details-textarea-container" id="hx_change_family_textarea_container">
    ;"                <textarea id="hx_change_family" name="hx_change_family"></textarea>
    ;"            </div>
    ;"        </div>
    ;"
    ;"        <div class="question-group">
    ;"            <label class="main-question-label">Have you had any medical tests? (If ordered elsewhere we WON'T have results)</label>
    ;"            <div class="details-options-row">
    ;"                <label class="none-option-label">
    ;"                    <input type="checkbox" name="testing_none" class="sr-only none-toggle-checkbox"
    ;"                           data-hide-target-ids="testing_list_container">
    ;"                    <span class="custom-checkbox-text none-checkbox-text">NONE</span>
    ;"                </label>
    ;"            </div>
    ;"            <div id="testing_list_container">
    ;"                <ul>
    ;"                    <li><label><input type="checkbox" name="testing_blood_work" class="sr-only"><span class="custom-checkbox-text">blood work</span></label></li>
    ;"                    <li><label><input type="checkbox" name="testing_mammogram" class="sr-only"><span class="custom-checkbox-text">mammogram</span></label></li>
    ;"                    <li><label><input type="checkbox" name="testing_xrays" class="sr-only"><span class="custom-checkbox-text">xrays</span></label></li>
    ;"                    <li><label><input type="checkbox" name="testing_MRI" class="sr-only"><span class="custom-checkbox-text">MRI</span></label></li>
    ;"                    <li><label><input type="checkbox" name="testing_CT_scan" class="sr-only"><span class="custom-checkbox-text">CT scan</span></label></li>
    ;"                    <li><label><input type="checkbox" name="testing_GI_scope" class="sr-only"><span class="custom-checkbox-text">colon or stomach scope</span></label></li>
    ;"                    <li><label><input type="checkbox" name="testing_ultrasound" class="sr-only"><span class="custom-checkbox-text">ultrasound</span></label></li>
    ;"                    <li><label><input type="checkbox" name="testing_echo" class="sr-only"><span class="custom-checkbox-text">echocardiogram</span></label></li>
    ;"                    <li><label><input type="checkbox" name="testing_stress_test" class="sr-only"><span class="custom-checkbox-text">cardiac stress test</span></label></li>
    ;"                    <li><label><input type="checkbox" name="testing_Holter" class="sr-only"><span class="custom-checkbox-text">Holter monitor</span></label></li>
    ;"                    <li><label><input type="checkbox" name="testing_ECG" class="sr-only"><span class="custom-checkbox-text">ECG</span></label></li>
    ;"                    <li><label><input type="checkbox" name="testing_bone_density" class="sr-only"><span class="custom-checkbox-text">bone density</span></label></li>
    ;"                </ul>
    ;"                <label for="testing_other_test">Other test (if any):</label>
    ;"                <div class="details-textarea-container" id="change_testing_other_test">
    ;"                    <textarea id="testing_other_test" name="testing_other_test"></textarea>
    ;"                </div>
    ;"
    ;"
    ;"            </div>
    ;"        </div>
    ;"    </div>
    ;"
    ;"
    ;"    <h1>Review of Systems</h1>
    ;"
    ;"    <div class="category-section">
    ;"        <h2>CONSTITUTIONAL</h2>
    ;"        <ul>
    ;"            <li><label><input type="checkbox" name="constitutional_chills" class="sr-only"><span class="custom-checkbox-text">Chills</span></label></li>
    ;"            <li><label><input type="checkbox" name="constitutional_fatigue" class="sr-only"><span class="custom-checkbox-text">Fatigue</span></label></li>
    ;"            <li><label><input type="checkbox" name="constitutional_fever" class="sr-only"><span class="custom-checkbox-text">Fever</span></label></li>
    ;"            <li><label><input type="checkbox" name="constitutional_weight_gain" class="sr-only"><span class="custom-checkbox-text">Weight gain</span></label></li>
    ;"            <li><label><input type="checkbox" name="constitutional_weight_loss" class="sr-only"><span class="custom-checkbox-text">Weight loss</span></label></li>
    ;"        </ul>
    ;"        <div class="details-input-group">
    ;"            <label for="constitutional_details">Details (if needed):</label>
    ;"            <textarea id="constitutional_details" name="constitutional_details"></textarea>
    ;"        </div>
    ;"    </div>
    ;"
    ;"    <div class="category-section">
    ;"        <h2>HEENT</h2>
    ;"        <ul>
    ;"            <li><label><input type="checkbox" name="heent_hearing_loss" class="sr-only"><span class="custom-checkbox-text">Hearing loss</span></label></li>
    ;"            <li><label><input type="checkbox" name="heent_sinus_pressure" class="sr-only"><span class="custom-checkbox-text">Sinus pressure</span></label></li>
    ;"            <li><label><input type="checkbox" name="heent_visual_changes" class="sr-only"><span class="custom-checkbox-text">Visual changes</span></label></li>
    ;"        </ul>
    ;"        <div class="details-input-group">
    ;"            <label for="heent_details">Details (if needed):</label>
    ;"            <textarea id="heent_details" name="heent_details"></textarea>
    ;"        </div>
    ;"    </div>
    ;"
    ;"    <div class="category-section">
    ;"        <h2>RESPIRATORY</h2>
    ;"        <ul>
    ;"            <li><label><input type="checkbox" name="respiratory_cough" class="sr-only"><span class="custom-checkbox-text">Cough</span></label></li>
    ;"            <li><label><input type="checkbox" name="respiratory_shortness_of_breath" class="sr-only"><span class="custom-checkbox-text">Shortness of breath</span></label></li>
    ;"            <li><label><input type="checkbox" name="respiratory_wheezing" class="sr-only"><span class="custom-checkbox-text">Wheezing</span></label></li>
    ;"        </ul>
    ;"        <div class="details-input-group">
    ;"            <label for="respiratory_details">Details (if needed):</label>
    ;"            <textarea id="respiratory_details" name="respiratory_details"></textarea>
    ;"        </div>
    ;"    </div>
    ;"
    ;"    <div class="category-section">
    ;"        <h2>CARDIOVASCULAR</h2>
    ;"        <ul>
    ;"            <li><label><input type="checkbox" name="cardiovascular_chest_pain" class="sr-only"><span class="custom-checkbox-text">Chest pain</span></label></li>
    ;"            <li><label><input type="checkbox" name="cardiovascular_pain_walking" class="sr-only"><span class="custom-checkbox-text">Pain while walking (Claudication)</span></label></li>
    ;"            <li><label><input type="checkbox" name="cardiovascular_edema" class="sr-only"><span class="custom-checkbox-text">Edema</span></label></li>
    ;"            <li><label><input type="checkbox" name="cardiovascular_palpitating" class="sr-only"><span class="custom-checkbox-text">Palpitations</span></label></li>
    ;"        </ul>
    ;"        <div class="details-input-group">
    ;"            <label for="cardiovascular_details">Details (if needed):</label>
    ;"            <textarea id="cardiovascular_details" name="cardiovascular_details"></textarea>
    ;"        </div>
    ;"    </div>
    ;"
    ;"    <div class="category-section">
    ;"        <h2>GASTROINTESTINAL</h2>
    ;"        <ul>
    ;"            <li><label><input type="checkbox" name="gastrointestinal_abdominal_pain" class="sr-only"><span class="custom-checkbox-text">Abdominal pain</span></label></li>
    ;"            <li><label><input type="checkbox" name="gastrointestinal_blood_in_stool" class="sr-only"><span class="custom-checkbox-text">Blood in stool</span></label></li>
    ;"            <li><label><input type="checkbox" name="gastrointestinal_constipation" class="sr-only"><span class="custom-checkbox-text">Constipation</span></label></li>
    ;"            <li><label><input type="checkbox" name="gastrointestinal_diarrhea" class="sr-only"><span class="custom-checkbox-text">Diarrhea</span></label></li>
    ;"            <li><label><input type="checkbox" name="gastrointestinal_heartburn" class="sr-only"><span class="custom-checkbox-text">Heartburn</span></label></li>
    ;"            <li><label><input type="checkbox" name="gastrointestinal_loss_of_appetite" class="sr-only"><span class="custom-checkbox-text">Loss of appetite</span></label></li>
    ;"            <li><label><input type="checkbox" name="gastrointestinal_nausea" class="sr-only"><span class="custom-checkbox-text">Nausea</span></label></li>
    ;"            <li><label><input type="checkbox" name="gastrointestinal_vomiting" class="sr-only"><span class="custom-checkbox-text">Vomiting</span></label></li>
    ;"        </ul>
    ;"        <div class="details-input-group">
    ;"            <label for="gastrointestinal_details">Details (if needed):</label>
    ;"            <textarea id="gastrointestinal_details" name="gastrointestinal_details"></textarea>
    ;"        </div>
    ;"    </div>
    ;"
    ;"    <div class="category-section">
    ;"        <h2>GENITOURINARY</h2>
    ;"        <ul>
    ;"            <li><label><input type="checkbox" name="genitourinary_painful_urination" class="sr-only"><span class="custom-checkbox-text">Painful urination (Dysuria)</span></label></li>
    ;"            <li><label><input type="checkbox" name="genitourinary_excessive_urine" class="sr-only"><span class="custom-checkbox-text">Excessive amount of urine (Polyuria)</span></label></li>
    ;"            <li><label><input type="checkbox" name="genitourinary_urinary_frequency" class="sr-only"><span class="custom-checkbox-text">Urinary frequency</span></label></li>
    ;"        </ul>
    ;"        <div class="details-input-group">
    ;"            <label for="genitourinary_details">Details (if needed):</label>
    ;"            <textarea id="genitourinary_details" name="genitourinary_details"></textarea>
    ;"        </div>
    ;"    </div>
    ;"
    ;"    <div class="category-section">
    ;"        <h2>METABOLIC/ENDOCRINE</h2>
    ;"        <ul>
    ;"            <li><label><input type="checkbox" name="metabolic_cold_intolerance" class="sr-only"><span class="custom-checkbox-text">Cold intolerance</span></label></li>
    ;"            <li><label><input type="checkbox" name="metabolic_heat_intolerance" class="sr-only"><span class="custom-checkbox-text">Heat intolerance</span></label></li>
    ;"            <li><label><input type="checkbox" name="metabolic_excessive_thirst" class="sr-only"><span class="custom-checkbox-text">Excessive thirst (Polydipsia)</span></label></li>
    ;"            <li><label><input type="checkbox" name="metabolic_excessive_hunger" class="sr-only"><span class="custom-checkbox-text">Excessive hunger (Polyphagia)</span></label></li>
    ;"        </ul>
    ;"        <div class="details-input-group">
    ;"            <label for="metabolic_details">Details (if needed):</label>
    ;"            <textarea id="metabolic_details" name="metabolic_details"></textarea>
    ;"        </div>
    ;"    </div>
    ;"
    ;"    <div class="category-section">
    ;"        <h2>NEUROLOGICAL</h2>
    ;"        <ul>
    ;"            <li><label><input type="checkbox" name="neurological_dizziness" class="sr-only"><span class="custom-checkbox-text">Dizziness</span></label></li>
    ;"            <li><label><input type="checkbox" name="neurological_extremity_numbness" class="sr-only"><span class="custom-checkbox-text">Extremity numbness</span></label></li>
    ;"            <li><label><input type="checkbox" name="neurological_extremity_weakness" class="sr-only"><span class="custom-checkbox-text">Extremity weakness</span></label></li>
    ;"            <li><label><input type="checkbox" name="neurological_headaches" class="sr-only"><span class="custom-checkbox-text">Headaches</span></label></li>
    ;"            <li><label><input type="checkbox" name="neurological_seizures" class="sr-only"><span class="custom-checkbox-text">Seizures</span></label></li>
    ;"            <li><label><input type="checkbox" name="neurological_tremors" class="sr-only"><span class="custom-checkbox-text">Tremors</span></label></li>
    ;"        </ul>
    ;"        <div class="details-input-group">
    ;"            <label for="neurological_details">Details (if needed):</label>
    ;"            <textarea id="neurological_details" name="neurological_details"></textarea>
    ;"        </div>
    ;"    </div>
    ;"
    ;"    <div class="category-section">
    ;"        <h2>PSYCHIATRIC</h2>
    ;"        <ul>
    ;"            <li><label><input type="checkbox" name="psychiatric_anxiety" class="sr-only"><span class="custom-checkbox-text">Anxiety</span></label></li>
    ;"            <li><label><input type="checkbox" name="psychiatric_depression" class="sr-only"><span class="custom-checkbox-text">Depression</span></label></li>
    ;"        </ul>
    ;"        <div class="details-input-group">
    ;"            <label for="psychiatric_details">Details (if needed):</label>
    ;"            <textarea id="psychiatric_details" name="psychiatric_details"></textarea>
    ;"        </div>
    ;"    </div>
    ;"
    ;"    <div class="category-section">
    ;"        <h2>MUSCULOSKELETAL</h2>
    ;"        <ul>
    ;"            <li><label><input type="checkbox" name="musculoskeletal_back_pain" class="sr-only"><span class="custom-checkbox-text">Back pain</span></label></li>
    ;"            <li><label><input type="checkbox" name="musculoskeletal_joint_pain" class="sr-only"><span class="custom-checkbox-text">Joint pain</span></label></li>
    ;"            <li><label><input type="checkbox" name="musculoskeletal_joint_swelling" class="sr-only"><span class="custom-checkbox-text">Joint swelling</span></label></li>
    ;"            <li><label><input type="checkbox" name="musculoskeletal_neck_pain" class="sr-only"><span class="custom-checkbox-text">Neck pain</span></label></li>
    ;"        </ul>
    ;"        <div class="details-input-group">
    ;"            <label for="musculoskeletal_details">Details (if needed):</label>
    ;"            <textarea id="musculoskeletal_details" name="musculoskeletal_details"></textarea>
    ;"        </div>
    ;"    </div>
    ;"
    ;"    <div class="category-section">
    ;"        <h2>HEMATOLOGIC</h2>
    ;"        <ul>
    ;"            <li><label><input type="checkbox" name="hematologic_easily_bleeds" class="sr-only"><span class="custom-checkbox-text">Easily bleeds</span></label></li>
    ;"            <li><label><input type="checkbox" name="hematologic_easily_bruises" class="sr-only"><span class="custom-checkbox-text">Easily bruises</span></label></li>
    ;"            <li><label><input type="checkbox" name="hematologic_lymphedema" class="sr-only"><span class="custom-checkbox-text">Lymphedema</span></label></li>
    ;"            <li><label><input type="checkbox" name="hematologic_blood_clots" class="sr-only"><span class="custom-checkbox-text">Issues with blood clots</span></label></li>
    ;"        </ul>
    ;"        <div class="details-input-group">
    ;"            <label for="hematologic_details">Details (if needed):</label>
    ;"            <textarea id="hematologic_details" name="hematologic_details"></textarea>
    ;"        </div>
    ;"    </div>
    ;"
    ;"    <div class="category-section">
    ;"        <h2>IMMUNOLOGIC</h2>
    ;"        <ul>
    ;"            <li><label><input type="checkbox" name="immunologic_food_allergies" class="sr-only"><span class="custom-checkbox-text">Food allergies</span></label></li>
    ;"            <li><label><input type="checkbox" name="immunologic_seasonal_allergies" class="sr-only"><span class="custom-checkbox-text">Seasonal allergies</span></label></li>
    ;"        </ul>
    ;"        <div class="details-input-group">
    ;"            <label for="immunologic_details">Details (if needed):</label>
    ;"            <textarea id="immunologic_details" name="immunologic_details"></textarea>
    ;"        </div>
    ;"    </div>
    ;"
    ;"    <script>
    ;"        document.addEventListener('DOMContentLoaded', function() {
    ;"            const noneCheckboxes = document.querySelectorAll('.none-toggle-checkbox');
    ;"
    ;"            noneCheckboxes.forEach(checkbox => {
    ;"                const toggleVisibility = (isChecked) => {
    ;"                    // Get the comma-separated string of IDs to hide/show
    ;"                    const targetIdsString = checkbox.dataset.hideTargetIds;
    ;"                    if (!targetIdsString) return;
    ;"
    ;"                    const targetIds = targetIdsString.split(','); // Split into an array of IDs
    ;"
    ;"                    targetIds.forEach(id => {
    ;"                        const targetElement = document.getElementById(id.trim()); // Trim whitespace
    ;"                        if (targetElement) {
    ;"                            if (isChecked) {
    ;"                                targetElement.classList.add('hidden'); // Hide the element
    ;"                                // If the element contains checkboxes or a textarea, clear/uncheck them
    ;"                                const checkboxes = targetElement.querySelectorAll('input[type="checkbox"]');
    ;"                                checkboxes.forEach(cb => {
    ;"                                    cb.checked = false; // Uncheck all checkboxes within the hidden section
    ;"                                });
    ;"                                const textarea = targetElement.querySelector('textarea');
    ;"                                if (textarea) {
    ;"                                    textarea.value = ''; // Clear textarea
    ;"                                }
    ;"                            } else {
    ;"                                targetElement.classList.remove('hidden'); // Show the element
    ;"                            }
    ;"                        }
    ;"                    });
    ;"                };
    ;"
    ;"                // Add event listener for changes
    ;"                checkbox.addEventListener('change', function() {
    ;"                    toggleVisibility(this.checked);
    ;"                });
    ;"
    ;"                // Initial check on page load in case checkboxes are pre-checked by browser
    ;"                toggleVisibility(checkbox.checked);
    ;"            });
    ;"        });
    ;"    </script>
    ;"</body>
    ;"</html>  