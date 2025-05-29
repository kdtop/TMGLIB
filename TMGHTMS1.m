TMGHTMS1 ;TMG/kst/HTML Template source ;5/4/25
          ;;1.0;TMG-LIB;**1**;05/4/25
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 5/4/25  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;
  ;"========================================================================
  ;"This is the template that will be used by the TOPIC report in CPRS
  ;"  See PREPINFO^TMGTOPIC
  ;"  Notice tags {@@@[TOC]@@@} 
  ;"              {@@@[TABLE]@@@}
  ;"
TOPICRPT ;" Template for HTML.  See sample output in code below this part.  
  ;;<!DOCTYPE html>
  ;;<html>
  ;;<head>
  ;;    <meta charset="UTF-8">
  ;;    <meta http-equiv="X-UA-Compatible" content="IE=edge">
  ;;    <meta name="viewport" content="width=device-width, initial-scale=1.0">
  ;;    <title>Float Layout Tests</title>
  ;;    <style>
  ;;        body {
  ;;            margin: 0;
  ;;            font-family: Arial, sans-serif;
  ;;            flex-direction: column; /* Stack the test divs above the main content */
  ;;        }
  ;;        .test-container {
  ;;            width: 100%;
  ;;            overflow: hidden; /*clear flots */
  ;;        }
  ;;        .test-div {
  ;;            width: 50%;
  ;;            float: left; /* Float divs to the left */
  ;;            border: 3px solid black;
  ;;            height: 50px;
  ;;            box-sizing: border-box; /* Include border in size calculation */
  ;;        }
  ;;        #toc {
  ;;            width: 20%;
  ;;            border-right: 1px solid #ccc;
  ;;            overflow-y: auto;
  ;;            padding: 10px;  
  ;;            float: left; /* Float toc to the left */
  ;;            box-sizing: border-box; 
  ;;            height: 100vh; /* Ensure full height for scrolling */
  ;;        }
  ;;        #content {
  ;;           width: 75%;
  ;;           float: left; /* Float content to the left */
  ;;           overflow-y: auto;
  ;;           padding: 20px;
  ;;           border-left: 2px solid #ccc;
  ;;           box-sizing: border-box;
  ;;           height: 100vh; /* Ensure full height for scrolling */
  ;;       }
  ;;       #debug {
  ;;           width: 20%;
  ;;           float: left; /* Float debug to the left */
  ;;           background-color: #f9f9f9;
  ;;           border-left: 1px solid #ccc;
  ;;           padding: 10px;
  ;;           box-sizing: border-box;
  ;;           height: 100vh; /* Ensure full height for scrolling */
  ;;       } 
  ;; 
  ;;        a {
  ;;            text-decoration: none;
  ;;            color: blue;
  ;;        }
  ;;        a:hover {
  ;;            text-decoration: underline;
  ;;        }
  ;;        th {
  ;;            background-color: #f2f2f2;
  ;;            padding: 8px;
  ;;        }
  ;;        td {
  ;;            border: 1px solid #ddd;
  ;;            padding: 8px;
  ;;            text-align: left;
  ;;        }
  ;;        table {
  ;;            width: 100%;
  ;;            border-collapse: collapse;
  ;;        }
  ;;    </style>
  ;;</head>
  ;;<body>
  ;;
  ;;    <!--  
  ;;    <div id="debug">Debug messages will appear here.</div>
  ;;    -->
  ;;
  ;;    <div id="toc">
  ;;       {@@@[TOC]@@@}
  ;;    </div>
  ;;
  ;;    <div id="content">
  ;;        {@@@[TABLE]@@@}
  ;;    </div>    
  ;;
  ;;    <script>
  ;;        function setDebugMessage(message) {
  ;;            var debugDiv = document.getElementById('debug');
  ;;            if (debugDiv) {
  ;;                debugDiv.innerText = message;
  ;;            }
  ;;        }
  ;;               
  ;;        function navigateTo(event, id) {     
  ;;            setDebugMessage('id=' + id);
  ;;
  ;;            // Prevent the default link behavior and stop event propagation
  ;;            if (event.preventDefault) {
  ;;                event.preventDefault();
  ;;                event.stopPropagation();     
  ;;            } else {
  ;;                event.returnValue = false; // For IE
  ;;            }
  ;;            // handling errors
  ;;            try {
  ;;              var targetElement = document.getElementById(id);
  ;;              var contentDiv = document.getElementById('content');  
  ;;              // if (targetElement) {
  ;;              //     targetElement.scrollIntoView();
  ;;              // }  
  ;;              if (targetElement && contentDiv) {
  ;;                  // Calculate the offset of the target element relative to the content div
  ;;                  var offsetTop = targetElement.offsetTop;
  ;;                  setDebugMessage('Trying to set contentDiv.scrollTop to offsetTop: ' + offsetTop.toString());
  ;;                  contentDiv.scrollTop = offsetTop;
  ;;              }    
  ;;            } catch (error) {
  ;;              setDebugMessage('Error: ' + error.message);
  ;;            }
  ;;              
  ;;        }
  ;;    </script>
  ;;</body>
  ;;</html>
  ;;#DONE_WITH_HTML        
        
  
  ;"TOPICRPT SAMPLE OUTPUT
  ;"<!DOCTYPE html>
  ;"<html>
  ;"<head>
  ;"    <meta charset="UTF-8">
  ;"    <meta name="viewport" content="width=device-width, initial-scale=1.0">
  ;"    <title>Float Layout Tests</title>
  ;"    <style>
  ;"        body {
  ;"            margin: 0;
  ;"            font-family: Arial, sans-serif;
  ;"            flex-direction: column; /* Stack the test divs above the main content */
  ;"        }
  ;"        .test-container {
  ;"            width: 100%;
  ;"            overflow: hidden; /*clear flots */
  ;"        }
  ;"        .test-div {
  ;"            width: 50%;
  ;"            float: left; /* Float divs to the left */
  ;"            border: 3px solid black;
  ;"            height: 50px;
  ;"            box-sizing: border-box; /* Include border in size calculation */
  ;"        }
  ;"        #toc {
  ;"            width: 20%;
  ;"            border-right: 1px solid #ccc;
  ;"            overflow-y: auto;
  ;"            padding: 10px;  
  ;"            float: left; /* Float toc to the left */
  ;"            box-sizing: border-box; 
  ;"            height: 100vh; /* Ensure full height for scrolling */
  ;"        }
  ;"        #content {
  ;"           width: 75%;
  ;"           float: left; /* Float content to the left */
  ;"           overflow-y: auto;
  ;"           padding: 20px;
  ;"           border-left: 2px solid #ccc;
  ;"           box-sizing: border-box;
  ;"           height: 100vh; /* Ensure full height for scrolling */
  ;"       }
  ;"       #debug {
  ;"           width: 20%;
  ;"           float: left; /* Float debug to the left */
  ;"           background-color: #f9f9f9;
  ;"           border-left: 1px solid #ccc;
  ;"           padding: 10px;
  ;"           box-sizing: border-box;
  ;"           height: 100vh; /* Ensure full height for scrolling */
  ;"       } 
  ;" 
  ;"        a {
  ;"            text-decoration: none;
  ;"            color: blue;
  ;"        }
  ;"        a:hover {
  ;"            text-decoration: underline;
  ;"        }
  ;"        th {
  ;"            background-color: #f2f2f2;
  ;"            padding: 8px;
  ;"        }
  ;"        td {
  ;"            border: 1px solid #ddd;
  ;"            padding: 8px;
  ;"            text-align: left;
  ;"        }
  ;"        table {
  ;"            width: 100%;
  ;"            border-collapse: collapse;
  ;"        }
  ;"    </style>
  ;"</head>
  ;"<body>
  ;"
  ;"    <!--  
  ;"    <div id="debug">Debug messages will appear here.</div>
  ;"    -->
  ;"
  ;"    <div id="toc">
  ;"        <h3>Table of Contents</h3>
  ;"        <ul>
  ;"            <li><a href="#1" onclick="navigateTo(event, '1')">Section 1</a></li>
  ;"            <li><a href="#2" onclick="navigateTo(event, '2')">Section 2</a></li>
  ;"            <li><a href="#3" onclick="navigateTo(event, '3')">Section 3</a></li>
  ;"        </ul>
  ;"    </div>
  ;"
  ;"    <div id="content">
  ;"        <table>
  ;"            <tr>
  ;"                <th id="1">Section 1</th>
  ;"            </tr> 
  ;"            <tr>
  ;"                <td>
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                </td>
  ;"            </tr>
  ;"            <tr>
  ;"                <th id="2">Section 2</th>
  ;"            </tr>
  ;"            <tr>
  ;"                <td>
  ;"                  This is the content for Section 2. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 2. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 2. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 2. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 2. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 2. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 2. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 2. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 2. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 2. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 2. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 2. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                </td>
  ;"            </tr>
  ;"            <tr>
  ;"                <th id="3">Section 3</th>
  ;"            </tr>
  ;"            <tr>
  ;"                <td>
  ;"                  This is the content for Section 3. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 3. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 3. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 3. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 3. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 3. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 3. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 3. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 3. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 3. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 3. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                  This is the content for Section 3. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  ;"                </td>
  ;"            </tr>
  ;"        </table>
  ;"    </div>    
  ;"
  ;"    <script>
  ;"        function setDebugMessage(message) {
  ;"            var debugDiv = document.getElementById('debug');
  ;"            if (debugDiv) {
  ;"                debugDiv.innerText = message;
  ;"            }
  ;"        }
  ;"               
  ;"        function navigateTo(event, id) {     
  ;"            setDebugMessage('id=' + id);
  ;"
  ;"            // Prevent the default link behavior and stop event propagation
  ;"            if (event.preventDefault) {
  ;"                event.preventDefault();
  ;"                event.stopPropagation();     
  ;"            } else {
  ;"                event.returnValue = false; // For IE
  ;"            }
  ;"            // handling errors
  ;"            try {
  ;"              var targetElement = document.getElementById(id);
  ;"              var contentDiv = document.getElementById('content');  
  ;"              // if (targetElement) {
  ;"              //     targetElement.scrollIntoView();
  ;"              // }  
  ;"              if (targetElement && contentDiv) {
  ;"                  // Calculate the offset of the target element relative to the content div
  ;"                  var offsetTop = targetElement.offsetTop;
  ;"                  setDebugMessage('Trying to set contentDiv.scrollTop to offsetTop: ' + offsetTop.toString());
  ;"                  contentDiv.scrollTop = offsetTop;
  ;"              }    
  ;"            } catch (error) {
  ;"              setDebugMessage('Error: ' + error.message);
  ;"            }
  ;"              
  ;"        }
  ;"    </script>
  ;"</body>
  ;"</html>
        
    