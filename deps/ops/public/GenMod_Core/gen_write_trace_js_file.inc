!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Write the java-script file which controls display of active call tree
!-------------------------------------------------------------------------------

SUBROUTINE gen_write_trace_js_file (FileName)

USE OpsMod_Kinds, ONLY: &
  integer_default

IMPLICIT NONE

! Subroutine arguments:
CHARACTER (len=*), INTENT(IN) :: FileName

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = "gen_write_trace_js_file"
INTEGER, PARAMETER            :: RecordLength = 1000     ! Record length on json file (lines must not wrap)
INTEGER, PARAMETER            :: nLines = 116            ! Number of lines in the output file
INTEGER(kind=integer_default) :: IOStatus                ! I/O return code
INTEGER                       :: iLine                   ! Loop variable, line number in routine
INTEGER                       :: JSUnit                  ! Unit number for the output file
CHARACTER(len=200)            :: OutString(1:nLines)     ! String to be output

DATA OutString(1:31) / &
   'var CallingTree = {};', &
   '', &
   '$(function() {', &
   '', &
   '// JavaScript for displaying the organisational hierarchy', &
   '    // The default ID for loading the hierarchy UL', &
   '    var ID = "calling-tree";', &
   '    // Switch for "show"', &
   '    var TO_SHOW = "+ ";', &
   '    // Switch for "hide"', &
   '    var TO_HIDE = "- ";', &
   '', &
   '    // Formats the name of a data item', &
   '    var format_name = function(element, item) {', &
   '        if (item.link == null) {', &
   '            element.append(item.name);', &
   '        }', &
   '        else {', &
   '            element.append($("<a/>", {"href": item.link}).text(item.name));', &
   '        }', &
   '    };', &
   '', &
   '    // Formats text for memory usage', &
   '    var format_memory = function(element, item) {', &
   '        if (item.memory == null || item.memory.length == 0) {', &
   '            return;', &
   '        }', &
   '        var div = $("<div/>").addClass("node-memory").appendTo(element);', &
   '        div.append(item.memory);', &
   '    };', &
   '' /

DATA OutString(32:55) / &
   '    // Formats any extra text on entry to routine (in list format)', &
   '    var format_entry = function(element, item) {', &
   '        if (item.entry == null || item.entry.length == 0) {', &
   '            return;', &
   '        }', &
   '        var div = $("<div/>").addClass("node-entry").appendTo(element);', &
   '        $.each(item.entry, function(i, piece) {', &
   '            div.append(', &
   '                  i == 0                       ? ""', &
   '                :                                ", "', &
   '            );', &
   '            div.append(piece.name);', &
   '        });', &
   '    };', &
   '', &
   '    // Formats text from exit of routine', &
   '    var format_exit = function(element, item) {', &
   '        if (item.exit == null || item.exit.length == 0) {', &
   '            return;', &
   '        }', &
   '        var div = $("<div/>").addClass("node-exit").appendTo(element);', &
   '        div.append(item.exit);', &
   '    };', &
   '' /

DATA OutString(56:91) / &
   '    // Creates a unordered list for a data hierarchy', &
   '    var format = function(element, item) {', &
   '        if (item.children == null) {', &
   '            format_name(element, item);', &
   '            format_memory(element, item);', &
   '            format_entry(element, item);', &
   '            format_exit(element, item);', &
   '        }', &
   '        else {', &
   '            $("<a/>", {"href": "javascript:"})', &
   '                .addClass("switch")', &
   '                .text(TO_HIDE)', &
   '                .click(function() {', &
   '                    $("ul", element).toggleClass("hide");', &
   '                    $(this).text($(this).text() == TO_HIDE ? TO_SHOW : TO_HIDE);', &
   '                })', &
   '                .appendTo(element);', &
   '            format_name(element, item);', &
   '            format_memory(element, item);', &
   '            format_entry(element, item);', &
   '            var ul = $("<ul/>").appendTo(element);', &
   '            $.each(item.children, function(i, child) {', &
   '                format($("<li/>").appendTo(ul), child);', &
   '            });', &
   '        }', &
   '    };', &
   '', &
   '    // Displays the default element in the hierarchy, expand all', &
   '    var show_all = function() {', &
   '        $("#" + ID + " a.switch").each(function() {', &
   '            if ($(this).text() == TO_SHOW) {', &
   '                $(this).trigger("click");', &
   '            }', &
   '        });', &
   '    };', &
   '' /

DATA OutString(92:116) / &
   '    // Displays the default element in the hierarchy in default mode', &
   '    var show_default = function() {', &
   '        $("#" + ID + " a.switch").each(function() {', &
   '            if ($(this).text() == TO_HIDE) {', &
   '                $(this).trigger("click");', &
   '            }', &
   '        });', &
   '        $("#" + ID + " a.switch").each(function() {', &
   '            if ($(this).text() == TO_SHOW) {', &
   '                $(this).trigger("click");', &
   '            }', &
   '        });', &
   '        $("#" + ID + " > ul > li > a.switch").each(function() {', &
   '            if ($(this).text() == TO_HIDE) {', &
   '                $(this).trigger("click");', &
   '            }', &
   '        });', &
   '    };', &
   '', &
   '    $("#calling-tree-show-all").click(show_all);', &
   '    $("#calling-tree-show-default").click(show_default);', &
   '', &
   '    format($("#" + ID), CallingTree.Hierarchy);', &
   '    show_default();', &
   '});' /

CALL gen_get_unit (JSUnit)

OPEN (UNIT = JSUnit,       & ! I:
      FILE = FileName,     & ! I:
      STATUS = "REPLACE",  & ! I:
      ACTION = "WRITE",    & ! I:
      RECL = RecordLength, & ! I:
      IOSTAT = IOStatus)     ! O:
IF (IOStatus /= 0) THEN
  CALL gen_fail (RoutineName,                                         &
                 "Cannot open calling tree file " // TRIM (FileName))
END IF

DO iLine = 1, nLines
  WRITE (JSUnit, '(A)', IOSTAT = IOStatus) TRIM (OutString(iLine))
  IF (IOStatus /= 0) THEN
    CALL gen_fail (RoutineName,                                               &
                   "Error when writing java-script file " // TRIM (FileName))
  END IF
END DO

CLOSE (JSUnit)

CALL gen_free_unit (JSUnit)

END SUBROUTINE gen_write_trace_js_file
