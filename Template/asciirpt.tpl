#!------------------------------------------------------------------------------------
#TEMPLATE (ASCII_Reports,'ASCII reports for CW')
#!------------------------------------------------------------------------------------
#!  For "Clarion" template chain
#!  Compatible with all CW versions
#!  Author: Oleg Fomin, E-mail: oleg@fomin.info
#!  http://www.fomintools.com
#!------------------------------------------------------------------------------------
#PROCEDURE(ReportASCII,'Print text (ASCII) report')
  #PROMPT('&Parameters', @s80), %Parameters          #! Retrieve parameters used
  #PROMPT('Input file &Name/!Variable:',@S80),%ASCIIFormName,AT(,12,65),REQ
  #PROMPT('OEM',CHECK),%ASCIIFormOEM,DEFAULT(%FALSE),AT(167,13)
  #PROMPT('Output file &Name/!Variable:',@S80),%ASCIIOutputName,AT(,24,65),REQ,DEFAULT('print.tmp'),PROMPTAT(,24)
  #PROMPT('OEM',CHECK),%ASCIIOutputOEM,DEFAULT(%TRUE),AT(167,25)
  #PROMPT('ASCII Report Width:',SPIN(@N3,80,255)),%ASCIIReportWidth,DEFAULT(255),REQ,AT(,36,30),PROMPTAT(,36)
  #PROMPT('ASCII Preview Procedure:',PROCEDURE),%ASCIIPreviewProcedure
  #PROMPT('Field  ^Names^  CASE sensitive',CHECK),%PrintFieldsCaseSens,AT(10)
  #BUTTON('&Field Assignment'),AT(10,,180),REQ,MULTI(%PrintField,'^ '& %PrintFieldName &' ^'&' = '& %PrintFieldVariable &' ('& %PrintFieldJustType &' '& %PrintFieldPicture &')')
    #PROMPT('Variable:',FIELD),%PrintFieldVariable,REQ
    #VALIDATE((NOT INSTRING('[',%PrintFieldVariable,1)) AND (NOT INSTRING(']',%PrintFieldVariable,1)),'Удалите размерность в квадратных скобках. Массив имен формируется автоматически!')
    #PROMPT('Field  ^Name^ :',@S20),%PrintFieldName,REQ,DEFAULT(%FieldDescription)
    #PROMPT('Print Picture:',PICTURE),%PrintFieldPicture,REQ,DEFAULT('@')
    #PROMPT('Justification:',DROP('Default|Left|Right|Center')),%PrintFieldJustType,REQ,DEFAULT('Default')
  #ENDBUTTON
  #PROMPT('Generate Help field LIST file',CHECK),%EnableListFile,DEFAULT('1'),AT(10)
  #ENABLE(%EnableListFile)
    #PROMPT('Help field LIST file name:',@S64),%ListFileName,DEFAULT(CLIP(SUB(%Procedure,1,8)) &'.LST')
  #ENDENABLE
#LOCALDATA
LocalRequest         LONG,AUTO
OriginalRequest      LONG,AUTO
LocalResponse        LONG,AUTO
FilesOpened          LONG
ASCIIFileSize        LONG
ASCIIBytesThisRead   LONG
ASCIIBytesRead       LONG
ASCIIBytesThisCycle  LONG
ASCIIPercentProgress BYTE
FieldStart           SHORT
FieldFinish          SHORT
FieldLenth           SHORT
ProcessCol           SHORT
#ENDLOCALDATA
#CLASS('Procedure Setup','При входе в процедуру')
#CLASS('Before Assign Values','Перед вычислением полей печати')
#CLASS('After Assign Values','После вычисления полей печати')
#CLASS('Procedure Exit','Перед выходом из процедуры')
#EMBED(%GatherSymbols,'Gather Template Symbols'),HIDE
#IF(%Parameters)                                  #! IF Parameters used
%[20]Procedure %ProcedureType %Parameters         #<! Declare Procedure
#ELSE                                             #! ELSE (IF parameters not used)
%[20]Procedure %ProcedureType                     #<! Declare Procedure
#ENDIF                                            #! END (IF parameters used)
#INSERT(%FileControlInitialize)
#AT(%CustomGlobalDeclarations)
  #INSERT(%FileControlSetFlags)
  #FIX(%Driver,'ASCII')
  #ADD(%UsedDriverDLLs,%DriverDLL)
  #PROJECT(%DriverLIB)
#ENDAT
#FOR(%LocalData)
%[20]LocalData %LocalDataStatement
#ENDFOR
Queue:ASCII          QUEUE,PRE(QUE)
ASCII:String           STRING(%ASCIIReportWidth)
                     END
ASCIIFormName        STRING(80),STATIC
  #IF(%ASCIIFormOEM)
ASCIIFormFile        FILE,PRE(FRM),DRIVER('ASCII'),NAME(ASCIIFormName),OEM
  #ELSE
ASCIIFormFile        FILE,PRE(FRM),DRIVER('ASCII'),NAME(ASCIIFormName)
  #END
ASCIIFormRecord        RECORD
ASCII:String             STRING(%ASCIIReportWidth)
                       END
                     END
ASCIIPrintName       STRING(80),STATIC
  #IF(%ASCIIOutputOEM)
ASCIIPrintFile       FILE,PRE(PRN),DRIVER('ASCII'),NAME(ASCIIPrintName),CREATE,OEM
  #ELSE
ASCIIPrintFile       FILE,PRE(PRN),DRIVER('ASCII'),NAME(ASCIIPrintName),CREATE
  #END
ASCIIPrintRecord       RECORD
ASCII:String             STRING(%ASCIIReportWidth)
                       END
                     END
#COMMENT(22)
#DECLARE(%DimAttribute)
#DECLARE(%DimensionFieldExists)
#CLEAR(%DimensionFieldExists)
#DECLARE(%DimParameter)
#DECLARE(%D1)
#DECLARE(%D2)
#DECLARE(%D3)
#DECLARE(%D4)
#DECLARE(%PrintPictureLenth)
#DECLARE(%PrintPictureLenthStart)
#DECLARE(%PrintPictureLenthFinish)
#FOR(%PrintField)
  #INSERT(%FixPrintField)
  #IF(%DimAttribute)
PRNT:%PrintFieldVariable  #<STRING(%PrintFieldPicture),%DimAttribute
  #ELSE
PRNT:%PrintFieldVariable  #<STRING(%PrintFieldPicture)
  #ENDIF
#ENDFOR
#IF(%DimensionFieldExists)
!----Dimension Field Exists
DimStart             SHORT
DimFinish            SHORT
ValueStart           SHORT
ValueFinish          SHORT
D1                   SHORT
D2                   SHORT
D3                   SHORT
D4                   SHORT
DimNomer             BYTE
#ENDIF
#COMMENT(52)
#EMBED(%DataSection,'Data Section'),DATA          #! Embedded Source Code
#INSERT(%StandardProgressWindow)
  CODE                                            #<! Begin processed code
  PUSHBIND
  LocalRequest = GlobalRequest
  LocalResponse = RequestCancelled
  CLEAR(GlobalRequest)
  CLEAR(GlobalResponse)
  #INSERT(%StandardFormula,'Procedure Setup')
  #INSERT(%FileControlOpen)
  FREE(Queue:ASCII)
  #IF(SUB(%ASCIIFormName,1,1)='!')
    #SET(%ValueConstruct,SUB(%ASCIIFormName,2,LEN(%ASCIIFormName)-1))
  ASCIIFormName = %ValueConstruct
  #ELSE
  ASCIIFormName = '%'ASCIIFormName'
  #ENDIF
  IF NOT ASCIIFormName
    IF MESSAGE('ASCII print form is undefined!','Error',Icon:Hand)
      DO ProcedureReturn
    END
  END
  OPEN(ASCIIFormFile,10h)
  IF ERRORCODE()
    #EMBED(%ASCIIFormFileError,'ASCII file not found') #! Embedded Source Code
    IF StandardWarning(Warn:FileLoadError,ASCIIFormName)
      DO ProcedureReturn
    END
  ELSE
    #EMBED(%ASCIIFormFileFound,'ASCII file opened')    #! Embedded Source Code
  END
  ASCIIFileSize = BYTES(ASCIIFormFile)
  IF ASCIIFileSize = 0
    CLOSE(ASCIIFormFile)
    IF StandardWarning(Warn:FileZeroLength,ASCIIFormName)
      DO ProcedureReturn
    END
  END
  #IF(INLIST('ReportDataProcess(ASCII_Reports)',%ActiveTemplate))
  FREE(Report:ASCII)
  SET(ASCIIFormFile)
  LOOP
    NEXT(ASCIIFormFile)
    IF ERRORCODE()
      LocalResponse = RequestCompleted
      BREAK
    END
    RPT:ASCII:String = FRM:ASCII:String
    #EMBED (%ASCIIFormBeforeAddQueue, 'Before add ASCII form record to Queue')
    ADD(Report:ASCII)
  END
  #ELSE
  OPEN(ProgressWindow)
  ASCIIPercentProgress = 0
  ASCIIBytesRead = 0
  ProgressWindow{Prop:Text} = 'Processing report'
  Progress:Thermometer = 0
  ?Progress:PctText{Prop:Text} = 'Completed 0%%'
  ?Progress:UserString{Prop:Text} = ''
  ACCEPT
    CASE EVENT()
    OF Event:OpenWindow
      SET(ASCIIFormFile)
    OF Event:Timer
      ASCIIBytesThisCycle = 0
      LOOP WHILE ASCIIBytesThisCycle < 200
        NEXT(ASCIIFormFile)
        IF ERRORCODE() OR FRM:ASCII:String[1 : 4] ='#End'
          LocalResponse = RequestCompleted
          BREAK
        END
        ASCIIBytesThisRead = BYTES(ASCIIFormFile)
        ASCIIBytesThisCycle += ASCIIBytesThisRead
        ASCIIBytesRead += ASCIIBytesThisRead
        QUE:ASCII:String = FRM:ASCII:String
        #EMBED (%ASCIIFormBeforeAddQueue, 'Before add ASCII record to Queue')
        DO AssignValues
        DO ProcessString
        ADD(Queue:ASCII)
      END
      IF ASCIIPercentProgress < 100
        ASCIIPercentProgress = (ASCIIBytesRead/ASCIIFileSize)*100
        IF ASCIIPercentProgress > 100
          ASCIIPercentProgress = 100
        END
        IF Progress:Thermometer <> ASCIIPercentProgress THEN
          Progress:Thermometer = ASCIIPercentProgress
          DISPLAY(?Progress:Thermometer)
          DISPLAY(?Progress:PctText)
        END
      END
      IF LocalResponse = RequestCompleted
        LocalResponse = RequestCancelled
        POST(Event:CloseWindow)
      END
    END
    CASE FIELD()
    OF ?Progress:Cancel
      CASE EVENT()
      OF Event:Accepted
        IF StandardWarning(Warn:ConfirmCancelLoad,ASCIIFormName)=Button:OK
          POST(Event:CloseWindow)
        END
      END
    END
  END
  CLOSE(ProgressWindow)
  #ENDIF
  CLOSE(ASCIIFormFile)
  #EMBED(%AfterLoadPrintFormEmbed,'After Load Print Form')  #! Embedded Source Code
#EMBED(%AfterLoadPrintForm,'After Load Print Form'),HIDE
  #IF(SUB(%ASCIIOutputName,1,1)='!')
    #SET(%ValueConstruct,SUB(%ASCIIOutputName,2,LEN(%ASCIIOutputName)-1))
  ASCIIPrintName = %ValueConstruct
  #ELSE
  ASCIIPrintName = '%'ASCIIOutputName'
  #ENDIF
  #EMBED(%BeforeSaveReportQueueToFile,'Before Save Report Queue To File'),WHERE(INLIST('ReportDataProcess(ASCII_Reports)',%ActiveTemplate))     #! Embedded Source Code
  CREATE(ASCIIPrintFile)
  IF ERROR() THEN STOP('Невозможно создать файл '&ASCIIPrintName &': '& ERROR()).
  OPEN(ASCIIPrintFile)
  STREAM(ASCIIPrintFile)
  LOOP I# = 1 TO RECORDS(Queue:ASCII)
    GET(Queue:ASCII,I#)
    PRN:ASCII:String = CLIP(QUE:ASCII:String)
    APPEND(ASCIIPrintFile)
    IF ERROR() THEN STOP('Error writing to file '& ASCIIPrintName &': '& ERROR()).
  END
  FREE(Queue:ASCII)
  FLUSH(ASCIIPrintFile)
  CLOSE(ASCIIPrintFile)
  #IF(%ASCIIPreviewProcedure)
  %ASCIIPreviewProcedure
  #ELSE
  IF MESSAGE('Report has been successfuly prepared! Press OK to print report.','Confirm',|
     ICON:Question,Button:Ok+Button:Cancel,Button:Ok)=Button:Ok
    #EMBED(%BeforePrintFile,'Before Print Report File')  #! Embedded Source Code
    ASCIIFileName = ASCIIPrintName
    START(PrintASCIIFile)
    IF NOT ERROR()
      LocalResponse = RequestCompleted
    END
  END
  #ENDIF
  DO ProcedureReturn
!---------------------------------------------------------------------
ProcedureReturn ROUTINE
  #INSERT(%FileControlClose)
  #EMBED(%EndOfProcedure,'End of Procedure')
  #INSERT(%StandardFormula,'Procedure Exit')
  IF LocalResponse
    GlobalResponse = LocalResponse
  ELSE
    GlobalResponse = RequestCancelled
  END
  POPBIND
  RETURN
!--------------------------------------------------------------------------
AssignValues       ROUTINE
  #EMBED(%BeforeAssignValues,'Before AssignValues ROUTINE')  #! Embedded Source Code
  #INSERT(%StandardFormula,'Before Assign Values')
  #FOR(%PrintField)
    #INSERT(%FixPrintField)
    #IF(%DimAttribute)
      #IF(%D1)
  LOOP D1 = 1 TO %D1
        #IF(%D2)
    LOOP D2 = 1 TO %D2
          #IF(%D3)
      LOOP D3 = 1 TO %D3
            #IF(%D4)
        LOOP D4 = 1 TO %D4
          PRNT:%PrintFieldVariable[D1,D2,D3,D4] = FORMAT(%PrintFieldVariable[D1,D2,D3,D4],%PrintFieldPicture)
        END
      END
    END
  END
            #ELSE
        PRNT:%PrintFieldVariable[D1,D2,D3] = FORMAT(%PrintFieldVariable[D1,D2,D3],%PrintFieldPicture)
      END
    END
  END
            #ENDIF
          #ELSE
      PRNT:%PrintFieldVariable[D1,D2] = FORMAT(%PrintFieldVariable[D1,D2],%PrintFieldPicture)
    END
  END
          #ENDIF
        #ELSE
    PRNT:%PrintFieldVariable[D1] = FORMAT(%PrintFieldVariable[D1],%PrintFieldPicture)
  END
        #ENDIF
      #ENDIF
    #ELSE
  PRNT:%PrintFieldVariable = FORMAT(%PrintFieldVariable,%PrintFieldPicture)
    #ENDIF
  #ENDFOR
  #INSERT(%StandardFormula,'After Assign Values')
  #EMBED(%AfterAssignValues,'After AssignValues ROUTINE')  #! Embedded Source Code
!--------------------------------------------------------------------------
ProcessString      ROUTINE
  ProcessCol = 1
  FieldStart = 0
  FieldFinish = 0
  LOOP WHILE ProcessCol <= LEN(QUE:ASCII:String)
    ProcessCol = INSTRING('^',QUE:ASCII:String,1,ProcessCol)
    IF ProcessCol
      IF FieldStart = 0
        FieldStart  = ProcessCol
      ELSE
        FieldFinish = ProcessCol
        DO ProcessField
        FieldStart = 0
        FieldFinish = 0
      END
      ProcessCol +=1
    ELSE
      IF FieldStart
        IF MESSAGE('Field not enclosed with ^ sign. Line '&POINTER(Queue:ASCII)&' файла '&ASCIIFormName&' position '&FieldStart#&'!')
        END
      END
      BREAK
    END
  END
!--------------------------------------------------------------------------
ProcessField      ROUTINE
  #IF(%DimensionFieldExists)
  CLEAR(DimStart)
  CLEAR(DimFinish)
  DimStart  = INSTRING('[',QUE:ASCII:String,1,FieldStart+1)
  IF DimStart >= FieldFinish THEN CLEAR(DimStart).
  IF DimStart
    DimFinish = INSTRING(']',QUE:ASCII:String,1,FieldStart+1)
    IF DimFinish >= FieldFinish THEN CLEAR(DimFinish).
    IF DimFinish
      ValueFinish = DimStart
      CLEAR(D1);CLEAR(D2);CLEAR(D3);CLEAR(D4)
      LOOP DimNomer = 1 TO 4
        ValueStart = ValueFinish
        ValueFinish = INSTRING(',',QUE:ASCII:String,1,ValueStart+1)
        IF NOT ValueFinish OR ValueFinish > DimFinish
          ValueFinish = DimFinish
        END
        If (ValueFinish - ValueStart) > 2
          EXECUTE DimNomer
            D1 = QUE:ASCII:String[ValueStart+1 : ValueFinish-1]
            D2 = QUE:ASCII:String[ValueStart+1 : ValueFinish-1]
            D3 = QUE:ASCII:String[ValueStart+1 : ValueFinish-1]
            D4 = QUE:ASCII:String[ValueStart+1 : ValueFinish-1]
          END
        ELSE
          EXECUTE DimNomer
            D1 = QUE:ASCII:String[ValueStart+1]
            D2 = QUE:ASCII:String[ValueStart+1]
            D3 = QUE:ASCII:String[ValueStart+1]
            D4 = QUE:ASCII:String[ValueStart+1]
          END
        END
        IF ValueFinish = DimFinish THEN BREAK.
      END
      CLEAR(QUE:ASCII:String[DimStart : DimFinish])
    END
  END
  #ENDIF
  FieldLenth = FieldFinish - FieldStart + 1
  #IF(ITEMS(%PrintField))
    #IF(%PrintFieldsCaseSens)
  CASE LEFT(QUE:ASCII:String[FieldStart+1 : FieldFinish-1])
    #ELSE
  CASE UPPER(LEFT(QUE:ASCII:String[FieldStart+1 : FieldFinish-1]))
    #ENDIF
    #FOR(%PrintField)
      #INSERT(%FixPrintField)
      #IF(%DimAttribute)
        #IF(%D1)
          #IF(%D2)
            #IF(%D3)
              #IF(%D4)
                #SET(%DimParameter,'[D1,D2,D3,D4]')
              #ELSE
                #SET(%DimParameter,'[D1,D2,D3]')
              #ENDIF
            #ELSE
              #SET(%DimParameter,'[D1,D2]')
            #ENDIF
          #ELSE
            #SET(%DimParameter,'[D1]')
          #ENDIF
        #ENDIF
      #ELSE
        #SET(%DimParameter,'')
      #ENDIF
      #IF(%PrintFieldsCaseSens)
  OF '%'PrintFieldName'
      #ELSE
  OF UPPER('%'PrintFieldName')
      #ENDIF
      #CASE(%PrintFieldJustType)
      #OF('Left')
    QUE:ASCII:String[FieldStart : FieldFinish] = LEFT(PRNT:%PrintFieldVariable%DimParameter,FieldLenth)
      #OF('Right')
    QUE:ASCII:String[FieldStart : FieldFinish] = RIGHT(PRNT:%PrintFieldVariable%DimParameter,FieldLenth)
      #OF('Center')
    QUE:ASCII:String[FieldStart : FieldFinish] = CENTER(PRNT:%PrintFieldVariable%DimParameter,FieldLenth)
      #ELSE
        #IF(INSTRING('N',UPPER(%PrintFieldPicture),1,1))
    QUE:ASCII:String[FieldStart : FieldFinish] = RIGHT(PRNT:%PrintFieldVariable%DimParameter,FieldLenth)
        #ELSE
    QUE:ASCII:String[FieldStart : FieldFinish] = PRNT:%PrintFieldVariable%DimParameter
        #ENDIF
      #ENDCASE
    #ENDFOR
  END!CASE
  #ENDIF
#EMBED(%ProcRoutines,'Procedure Routines')
#EMBED(%ProcedureRoutines,'Procedure Routines'),HIDE
#EMBED(%LocalProcedures,'Local Procedures'),HIDE
#IF(NOT INLIST('ReportDataProcess(ASCII_Reports)',%ActiveTemplate))
  #INSERT(%GenerateListFile)
#ENDIF
#!
#!
#!--------------------------------------------------------------------------
#GROUP(%GenerateListFile)
  #IF(%EnableListFile)
    #DECLARE(%ListFileNameTmp)
    #SET(%ListFileNameTmp,'FieldLst.$$$')
    #CREATE(%ListFileNameTmp)
  Application     : %Program
  Procedure       : %Procedure     %ProcedureDescription
  ASCII Form File : %ASCIIFormName
    #IF(INLIST('ReportDataProcess(ASCII_Reports)',%ActiveTemplate))
      #SET(%ValueConstruct,'#Title')
      #INSERT(%ASCIIReportBandSeparator)
%ValueConstruct

      #FOR(%PrintBreaks)
        #SET(%ValueConstruct,'#Header('&%PrintBreakField&')')
        #INSERT(%ASCIIReportBandSeparator)
%ValueConstruct

      #ENDFOR
      #SET(%ValueConstruct,'#Detail')
      #INSERT(%ASCIIReportBandSeparator)
%ValueConstruct

      #FOR(%PrintBreaks),REVERSE
        #SET(%ValueConstruct,'#Footer('&%PrintBreakField&')')
        #INSERT(%ASCIIReportBandSeparator)
%ValueConstruct

      #ENDFOR
      #SET(%ValueConstruct,'#GrandTotals')
      #INSERT(%ASCIIReportBandSeparator)
%ValueConstruct

      #SET(%ValueConstruct,'#End')
      #INSERT(%ASCIIReportBandSeparator)
%ValueConstruct
    #ENDIF
Field Dictionary of the Report form:
--------------------------------------------------------------------------------
Variable (Alignment Picture)   ^ Name ^
--------------------------------------------------------------------------------
    #COMMENT(42)
    #FOR(%PrintField)
    #INSERT(%FixPrintField)
    #LOOP,FOR(%PrintPictureLenthStart,1,LEN(CLIP(%PrintFieldPicture)))
      #IF(INSTRING(SUB(%PrintFieldPicture,%PrintPictureLenthStart,1),'1234567890',1,1))
        #BREAK
      #ENDIF
    #ENDLOOP
    #LOOP,FOR(%PrintPictureLenthFinish,%PrintPictureLenthStart,LEN(CLIP(%PrintFieldPicture))+1)
      #IF(NOT INSTRING(SUB(%PrintFieldPicture,%PrintPictureLenthFinish,1),'1234567890',1,1))
        #BREAK
      #ENDIF
    #ENDLOOP
    #SET(%PrintPictureLenth,SUB(%PrintFieldPicture,%PrintPictureLenthStart,%PrintPictureLenthFinish-%PrintPictureLenthStart))
    #IF(INSTRING('@P',UPPER(%PrintFieldPicture),1,1) OR INSTRING('@K',UPPER(%PrintFieldPicture),1,1))
      #SET(%PrintPictureLenth,LEN(%PrintFieldPicture)-3)
    #ELSE
    #END
    #IF(INSTRING('@D',UPPER(%PrintFieldPicture),1,1))
      #CASE(%PrintPictureLenth)
      #OF(1)                        #!mm/dd/yy  10/31/59
        #SET(%PrintPictureLenth,'8')
      #OF(2)                        #!mm/dd/yyyy    10/31/1959
        #SET(%PrintPictureLenth,'10')
      #OF(3)                        #!mmm dd, yyyy    OCT 31,1959
        #SET(%PrintPictureLenth,'12')
      #OF(4)                        #!mmmmmmmmm dd, yyyy    October 31, 1959
        #SET(%PrintPictureLenth,'18')
      #OF(5)                        #!dd/mm/yy    31/10/59
        #SET(%PrintPictureLenth,'8')
      #OF(6)                        #!dd/mm/yyyy    31/10/1959
        #SET(%PrintPictureLenth,'10')
      #OF(7)                        #!dd mmm yy    31 OCT 59
        #SET(%PrintPictureLenth,'9')
      #OF(8)                        #!dd mmm yyyy    31 OCT 1959
        #SET(%PrintPictureLenth,'11')
      #OF(9)                        #!yy/mm/dd    59/10/31
        #SET(%PrintPictureLenth,'8')
      #OF(10)                       #!yyyy/mm/dd    1959/10/31
        #SET(%PrintPictureLenth,'10')
      #OF(11)                       #!yymmdd    591031
        #SET(%PrintPictureLenth,'6')
      #OF(12)                       #!yyyymmdd    19591031
        #SET(%PrintPictureLenth,'8')
      #OF(13)                       #!mm/yy    10/59
        #SET(%PrintPictureLenth,'5')
      #OF(14)                       #!mm/yyyy    10/1959
        #SET(%PrintPictureLenth,'7')
      #OF(15)                       #!yy/mm    59/10
        #SET(%PrintPictureLenth,'5')
      #OF(16)                       #!yyyy/mm    1959/10
        #SET(%PrintPictureLenth,'7')
      #OF(17)                       #!Windows Control Panel setting for Short Date
        #SET(%PrintPictureLenth,'10')
      #OF(18)                       #!Windows Control Panel setting for Long Date
        #SET(%PrintPictureLenth,'28')
      #ENDCASE
    #ELSIF(INSTRING('@T',UPPER(%PrintFieldPicture),1,1))
      #CASE(%PrintPictureLenth)
      #OF('1')
        #SET(%PrintPictureLenth,'5')
      #OF('2')
        #SET(%PrintPictureLenth,'4')
      #OF('3')
      #OROF('03')
        #SET(%PrintPictureLenth,'7')
      #OF('4')
        #SET(%PrintPictureLenth,'8')
      #OF('5')
        #SET(%PrintPictureLenth,'6')
      #OF('6')
        #SET(%PrintPictureLenth,'10')
      #ENDCASE
    #ENDIF
    #IF(%DimAttribute)
      #SET(%ValueConstruct,'^'&%PrintFieldName&'['&SUB(%DimAttribute,5,LEN(CLIP(%DimAttribute))-5)&']^')
    #ELSE
      #SET(%ValueConstruct,'^'&%PrintFieldName&'^')
    #ENDIF
    #IF(%PrintPictureLenth > LEN(CLIP(%ValueConstruct)))
      #IF(%DimAttribute)
        #SET(%ValueConstruct,%PrintFieldName&'['&SUB(%DimAttribute,5,LEN(CLIP(%DimAttribute))-5)&']')
      #ELSE
        #SET(%ValueConstruct,%PrintFieldName)
      #ENDIF
      #CASE(%PrintFieldJustType)
      #OF('Left')
        #SET(%ValueConstruct,'^'&LEFT(%ValueConstruct,%PrintPictureLenth-2)&'^')
      #OF('Right')
        #INSERT(%RightJustValue,%ValueConstruct,%PrintPictureLenth-2)
        #SET(%ValueConstruct,'^'&SUB(%ValueConstruct,1,%PrintPictureLenth-2)&'^')
      #OF('Center')
        #SET(%ValueConstruct,'^'&CENTER(%ValueConstruct,%PrintPictureLenth-2)&'^')
      #ELSE
        #IF(INSTRING('N',UPPER(%PrintFieldPicture),1,1))
          #SET(%ValueConstruct,%ValueConstruct)
          #INSERT(%RightJustValue,%ValueConstruct,%PrintPictureLenth)
          #SET(%ValueConstruct,'^'&SUB(%ValueConstruct,1,%PrintPictureLenth-2)&'^')
        #ELSE
          #SET(%ValueConstruct,'^'&LEFT(%ValueConstruct,%PrintPictureLenth-2)&'^')
        #ENDIF
      #ENDCASE
    #ENDIF
%PrintFieldVariable (%PrintFieldJustType %PrintFieldPicture)                #<%ValueConstruct
    #ENDFOR
--------------------------------------------------------------------------------
    #COMMENT(52)
    #CLOSE(%ListFileNameTmp)
    #REPLACE(%ListFileName,%ListFileNameTmp)
    #IF(FILEEXISTS(%ListFileNameTmp))
      #REMOVE(%ListFileNameTmp)
    #ENDIF
  #ENDIF
#!
#!
#GROUP(%RightJustValue,*%JustValue,%JustLenth)
  #IF(LEN(CLIP(%JustValue)) < %JustLenth)
    #LOOP,UNTIL(LEN(CLIP(%JustValue))=%JustLenth)
      #SET(%JustValue,' '& %JustValue)
    #ENDLOOP
  #ENDIF
#!
#!
#GROUP(%ASCIIReportBandSeparator)
  #LOOP,UNTIL(LEN(CLIP(%ValueConstruct))=%ASCIIReportWidth)
    #SET(%ValueConstruct,%ValueConstruct & '#')
  #ENDLOOP
#!
#!
#!--------------------------------------------------------------------
#GROUP(%FixPrintField)
  #CLEAR(%DimAttribute)
  #FIX(%LocalData,%PrintFieldVariable)
  #IF(%LocalData)
    #SET(%DimAttribute,EXTRACT(%LocalDataStatement,'DIM'))
    #SET(%D1,EXTRACT(%LocalDataStatement,'DIM',1))
    #SET(%D2,EXTRACT(%LocalDataStatement,'DIM',2))
    #SET(%D3,EXTRACT(%LocalDataStatement,'DIM',3))
    #SET(%D4,EXTRACT(%LocalDataStatement,'DIM',4))
  #ELSE
    #FIX(%GlobalData,%PrintFieldVariable)
    #IF(%GlobalData)
      #SET(%DimAttribute,EXTRACT(%GlobalDataStatement,'DIM'))
      #SET(%D1,EXTRACT(%GlobalDataStatement,'DIM',1))
      #SET(%D2,EXTRACT(%GlobalDataStatement,'DIM',2))
      #SET(%D3,EXTRACT(%GlobalDataStatement,'DIM',3))
      #SET(%D4,EXTRACT(%GlobalDataStatement,'DIM',4))
    #ELSE
      #FOR(%File)
        #FIX(%Field,%PrintFieldVariable)
        #IF(%Field)
          #SET(%DimAttribute,EXTRACT(%FieldStatement,'DIM'))
          #SET(%D1,EXTRACT(%FieldStatement,'DIM',1))
          #SET(%D2,EXTRACT(%FieldStatement,'DIM',2))
          #SET(%D3,EXTRACT(%FieldStatement,'DIM',3))
          #SET(%D4,EXTRACT(%FieldStatement,'DIM',4))
        #ENDIF
      #ENDFOR
    #ENDIF
  #ENDIF
  #IF(%DimAttribute)
    #SET(%DimensionFieldExists,%TRUE)
  #END
#!
#!---------------------------------------------------------------------------
#EXTENSION(ReportDataProcess,'ReportASCII Data processing'),PROCEDURE,PRIMARY('ReportASCII Data processing')
#RESTRICT
  #IF(%ProcedureTemplate='ReportASCII')
    #ACCEPT
  #ELSE
    #REJECT
  #ENDIF
#ENDRESTRICT
#LOCALDATA
RejectRecord         LONG,AUTO
WindowOpened         LONG,AUTO
RecordsToProcess     LONG,AUTO
RecordsProcessed     LONG,AUTO
RecordsPerCycle      LONG,AUTO
RecordsThisCycle     LONG,AUTO
PercentProgress      BYTE
RecordStatus         BYTE,AUTO
BreakLevel           BYTE(0)
NameSection          STRING(64)
FoundSection         BYTE
ReportRow            SHORT
PrintSkipDetails     BYTE
#ENDLOCALDATA
  #SHEET
    #TAB('&General'),HLP('~TPLProcProcess_General')
      #PROMPT('Window Message:',@S40),%WindowMessage
      #PROMPT('Quick-Scan Records',CHECK),%EnableQuickScan,DEFAULT(1)
      #PROMPT('&Record Filter:',@S255),%RecordFilter
      #ENABLE(%RecordFilter OR %RangeField)
        #PROMPT('Approx. Record Count:',@N6),%ApproxRecordCount,REQ
      #ENDENABLE
    #ENDTAB
    #TAB('Range Limits'),WHERE(%PrimaryKey),HLP('~TPLProcProcess_Range_Limits')
      #ENABLE(%PrimaryKey),CLEAR
        #PROMPT('Range Limit &Field:',COMPONENT(%PrimaryKey)),%RangeField
      #ENDENABLE
      #ENABLE(%RangeField)
        #PROMPT('Range Limit &Type:',DROP('Current Value|Single Value|Range of Values|File Relationship')),%RangeLimitType,DEFAULT('Current Value')
        #BOXED('Range Limit Boundary:'),WHERE(%RangeLimitType='Single Value'),AT(,60)
          #PROMPT('&Range Limit Value:',FIELD),%RangeLimit
        #ENDBOXED
        #BOXED('Range Limit Boundaries:'),WHERE(%RangeLimitType='Range of Values'),AT(,60)
          #PROMPT('&Low Limit Value:',FIELD),%RangeLow
          #PROMPT('&High Limit Value:',FIELD),%RangeHigh
        #ENDBOXED
        #BOXED('Range limiting file'),WHERE(%RangeLimitType='File Relationship'),AT(,60)
          #PROMPT('&Related file:',FILE),%RangeFile
        #ENDBOXED
      #ENDENABLE
    #ENDTAB
    #TAB('&Hot Fields'),HLP('~TPLProcProcess_Hot_Fields')
      #BUTTON('Hot Fields'),MULTI(%HotFields,%HotField),INLINE,HLP('~TPLProcProcess_Hot_Fields')
        #PROMPT('Hot Field:',FIELD),%HotField
        #PROMPT('BIND Field',CHECK),%HotFieldBound
      #ENDBUTTON
    #ENDTAB
    #TAB('&Totaling')
      #BUTTON('Group breaks'),MULTI(%PrintBreaks,'Break'&INSTANCE(%PrintBreaks)&'('&%PrintBreakField&')'),AT(,,174)
        #PROMPT('Break Field:',FIELD),%PrintBreakField,REQ
      #ENDBUTTON
      #BUTTON('Print Total Fields'),MULTI(%PrintTotals,%PrintTotalTarget),INLINE,AT(,35,,85)
        #PROMPT('Total Target Field:',FIELD),%PrintTotalTarget,REQ
        #PROMPT('Field To Total:',FIELD),%PrintTotalField,REQ
        #PROMPT('Reset On Field:',FROM(%PrintBreaks,%PrintBreakField,%PrintBreakField)),%PrintTotalResetOn
        #PROMPT('Total Based On:',DROP('Each Record Read|Specified Condition')),%PrintTotalBasedOn
        #ENABLE(%PrintTotalBasedOn = 'Specified Condition'),CLEAR
          #PROMPT('Total Condition:',@S255),%PrintTotalCondition,REQ
        #ENDENABLE
      #ENDBUTTON
    #ENDTAB
  #ENDSHEET
#CLASS('Before Lookups','After Record Retrieved, Before Lookups')
#CLASS('After Lookups','After Record Retrieved, After Lookups')
#CLASS('Before Range Check','In Validate Record ROUTINE, Before Range Limit Code')
#CLASS('Before Filter Check','In Validate Record ROUTINE, Before Filter Code')
#AT(%CustomGlobalDeclarations)
  #INSERT(%FileControlSetFlags)
#ENDAT
#ATSTART
  #INSERT(%FileControlInitialize)
  #DECLARE(%ProcessAction)
  #SET(%ProcessAction,'No record action')
  #DECLARE(%ListView)
  #SET(%ListView,'Process:View')
  #DECLARE(%QueueField),UNIQUE
  #DECLARE(%QueueFieldAssignment,%QueueField)
  #DECLARE(%ProcessFilter)
  #DECLARE(%InstancePrefix)
  #DECLARE(%Temp1)
  #DECLARE(%Temp2)
  #CASE(%RangeLimitType)
  #OF('Range of Values')
    #SET(%Temp1,%RangeLow)
    #SET(%Temp2,%RangeHigh)
  #OF('Single Value')
    #SET(%Temp1,%RangeLimit)
  #OF('File Relationship')
    #SET(%Temp1,%RangeFile)
  #ENDCASE
  #INSERT(%StandardViewFilter,%RecordFilter,%PrimaryKey,%RangeField,%RangeLimitType,'Process:Save:',%Temp1,%Temp2,'Construct')
  #SET(%ProcessFilter,%ValueConstruct)
  #DECLARE(%ListViewBoundField),UNIQUE
  #FOR (%QueueField)
    #SET(%QueueFieldAssignment,%QueueField)
    #ADD(%ListViewBoundField,%QueueField)
  #ENDFOR
  #FOR (%HotFields)
    #ADD(%QueueField,%HotField)
    #SET(%QueueFieldAssignment,%HotField)
    #IF (%HotFieldBound)
      #ADD(%ListViewBoundField,%HotField)
    #ENDIF
  #ENDFOR
  #FOR (%Formula), WHERE( ITEMS(%FormulaField) )
    #FOR (%FormulaField)
      #ADD(%QueueField,%FormulaField)
      #SET(%QueueFieldAssignment,%FormulaField)
    #ENDFOR
  #ENDFOR
  #DECLARE(%TempBreakFields),MULTI,UNIQUE
#ENDAT
#AT(%DataSection)
Report:ASCII         QUEUE,PRE(RPT)
ASCII:String           STRING(%ASCIIReportWidth)
                     END
  #COMMENT(22)
  #FOR(%PrintBreaks)
SAV::%PrintBreakField    #<LIKE(%PrintBreakField)
  #ENDFOR
  #COMMENT(52)
#INSERT(%StandardViewFilter,%RecordFilter,%PrimaryKey,%RangeField,%RangeLimitType,'Process:Save:',%Temp1,%Temp2,'Declare')
#INSERT(%ConstructView)
#ENDAT
#AT(%AfterLoadPrintForm)
  #FOR( %ListViewBoundField )
    #FIND(%Field,%ListViewBoundField)
    #IF (NOT %FieldFile OR %FieldName)
  BIND('%ListViewBoundField',%ListViewBoundField)
    #ENDIF
  #ENDFOR
  #INSERT(%StandardViewFilter,%RecordFilter,%PrimaryKey,%RangeField,%RangeLimitType,'Process:Save:',%Temp1,%Temp2,'Bind')
  #EMBED(%BeforeAccept,'Preparing to Process the Window')
  #INSERT(%StandardViewFilter,%RecordFilter,%PrimaryKey,%RangeField,%RangeLimitType,'Process:Save:',%Temp1,%Temp2,'Save')
  #MESSAGE('Accept Handling',3)
  #IF(%PrimaryKey)
    #IF(%RecordFilter OR %RangeField)
  RecordsToProcess = %ApproxRecordCount
    #ELSE
  RecordsToProcess = RECORDS(%Primary)
    #ENDIF
  RecordsPerCycle = 25
  #ELSE
    #IF(%RecordFilter)
  RecordsToProcess = %ApproxRecordCount
    #ELSE
  RecordsToProcess = BYTES(%Primary)
    #ENDIF
  RecordsPerCycle = 1000
  #ENDIF
  RecordsProcessed = 0
  PercentProgress = 0
  #EMBED(%BeforeOpeningWindow,'Before Opening Progress Window')
  OPEN(ProgressWindow)
  ProgressWindow{Prop:Text} = 'Обработка записей'
  ?Progress:PctText{Prop:Text} = '0% Готово'
  #IF(SUB(%WindowMessage,1,1)='!')
    #SET(%ValueConstruct,SUB(%WindowMessage,2,LEN(%WindowMessage)-1))
  ?Progress:UserString{Prop:Text}=%ValueConstruct
  #ELSE
  ?Progress:UserString{Prop:Text}='%'WindowMessage'
  #ENDIF
  #IF(%EnableQuickScan)
  #EMBED(%BeforeTurnQuickScanOn,'Before Turning QuickScan On'),WHERE(%EnableQuickScan)
  SEND(%Primary,'QUICKSCAN=on')
    #FOR(%Secondary),WHERE(%SecondaryType = '1:MANY')
  SEND(%Secondary,'QUICKSCAN=on')
    #ENDFOR
  #EMBED(%AfterTurnQuickScanOn,'After Turning QuickScan On'),WHERE(%EnableQuickScan)
  #ENDIF
  ACCEPT
    CASE EVENT()
    OF Event:OpenWindow
      #INSERT(%ProcessSaveLimits)
      DO PrintTitle
    OF Event:Timer
      RecordsThisCycle = 0
      LOOP WHILE RecordsThisCycle < RecordsPerCycle
        PrintSkipDetails = FALSE
        #INSERT(%ProcessEventTimer)
        IF PrintSkipDetails = FALSE
          DO PrintDetail
        END
        LOOP
          #EMBED(%BeforeSubsequentRead,'Before subsequent record retrieval')
          DO GetNextRecord
          #EMBED(%AfterSubsequentRead,'After subsequent record retrieval')
          DO ValidateRecord
          CASE RecordStatus
            OF Record:OutOfRange
              LocalResponse = RequestCancelled
              BREAK
            OF Record:OK
              BREAK
          END
        END
        IF LocalResponse = RequestCancelled
          LocalResponse = RequestCompleted
          BREAK
        END
        LocalResponse = RequestCancelled
      END
      IF LocalResponse = RequestCompleted
        BreakLevel = 0
        DO PrintGroupFooters
        NameSection = 'GrandTotals'
        DO PrintSection
        ?Progress:PctText{Prop:Text} = 'Process completed'
        DISPLAY(?Progress:PctText)
        POST(Event:CloseWindow)
      END
    END
    CASE FIELD()
    OF ?Progress:Cancel
      CASE Event()
      OF Event:Accepted
        LocalResponse = RequestCancelled
        POST(Event:CloseWindow)
      END
    END
  END
  CLOSE(ProgressWindow)
#ENDAT
#AT(%ProcedureRoutines)
#INSERT(%ProcessValidateRecord)
#INSERT(%ProcessGetRecords)
!------------------------------------------------------------------------
PrintTitle      ROUTINE
  #FOR(%PrintTotals)
  CLEAR(%PrintTotalTarget)
  #ENDFOR
  DO SaveBreakValues
  DO AssignValues
  NameSection = 'Title'
  DO PrintSection
  DO PrintGroupHeaders
!------------------------------------------------------------------------
PrintDetail        ROUTINE
  BreakLevel = 0
  #FOR(%PrintBreaks)
    #SET(%ValueConstruct,INSTANCE(%PrintBreaks))
    #IF(%ValueConstruct='1')
  IF SAV::%PrintBreakField <> %PrintBreakField
    BreakLevel = %ValueConstruct
    #ELSE
  ELSIF SAV::%PrintBreakField <> %PrintBreakField
    BreakLevel = %ValueConstruct
    #ENDIF
  #ENDFOR
  #IF(ITEMS(%PrintBreaks))
  END
  #ENDIF
  IF BreakLevel
    DO PrintGroupFooters
    DO AssignValues
    DO PrintGroupHeaders
    DO SaveBreakValues
  ELSE
    DO AssignValues
  END
  NameSection = 'Detail'
  DO PrintSection
  #FOR(%PrintTotals)
    #IF(%PrintTotalBasedOn = 'Specified Condition')
  IF %PrintTotalCondition
    %PrintTotalTarget += %PrintTotalField
  END
    #ELSE
  %PrintTotalTarget += %PrintTotalField
    #END
  #ENDFOR
  #IF(ITEMS(%PrintTotals))
  DO AssignValues
  #ENDIF
!------------------------------------------------------------------------
SaveBreakValues    ROUTINE
  #IF(ITEMS(%PrintBreaks))
    #FOR(%PrintBreaks)
  SAV::%PrintBreakField = %PrintBreakField
    #ENDFOR
    #FOR(%PrintBreaks)
      #SET(%ValueConstruct,INSTANCE(%PrintBreaks))
  IF BreakLevel <= %ValueConstruct
      #FOR(%PrintTotals),WHERE(%PrintTotalResetOn=%PrintBreakField)
    CLEAR(%PrintTotalTarget)
      #ENDFOR
  END
    #ENDFOR
  #ENDIF
!------------------------------------------------------------------------
PrintGroupHeaders  ROUTINE
  #IF(ITEMS(%PrintBreaks))
    #FOR(%PrintBreaks)
      #SET(%ValueConstruct,INSTANCE(%PrintBreaks))
  IF BreakLevel <= %ValueConstruct
      #SET(%ValueConstruct,'Header('& %PrintBreakField &')')
    NameSection = '%'ValueConstruct'
    DO PrintSection
  END
    #ENDFOR
  #ENDIF
!------------------------------------------------------------------------
PrintGroupFooters  ROUTINE
  #IF(ITEMS(%PrintBreaks))
    #FOR(%PrintBreaks),REVERSE
      #SET(%ValueConstruct,INSTANCE(%PrintBreaks))
  IF BreakLevel <= %ValueConstruct
      #SET(%ValueConstruct,'Footer('& %PrintBreakField &')')
    NameSection = '%'ValueConstruct'
    DO PrintSection
  END
    #ENDFOR
  #ENDIF
!------------------------------------------------------------------------
PrintSection       ROUTINE
  FoundSection = FALSE
  LOOP ReportRow = 1 TO RECORDS(Report:ASCII)
    GET(Report:ASCII,ReportRow)
    IF RPT:ASCII:String[1] = '#'
      IF INSTRING(UPPER(CLIP(NameSection)),UPPER(CLIP(RPT:ASCII:String)),1,2)
        FoundSection = TRUE
        CYCLE
      END
      IF FoundSection = TRUE
        BREAK
      END
    ELSE
      IF FoundSection = TRUE
        QUE:ASCII:String = RPT:ASCII:String
        DO ProcessString
        ADD(Queue:ASCII)
      END
    END
  END
#ENDAT
#ATEND
  #INSERT(%GenerateListFile)
#ENDAT
!------------------------------------------------------------------------
#EXTENSION(DeclareViewStructure,'Declare a View Structure'),PRIMARY('Declare a View Structure',NOKEY),DESCRIPTION('Declare a View Structure for '& %Primary),MULTI,PROCEDURE
  #BOXED('')
    #PROMPT('View Label:',@S20),%ViewLabel
  #ENDBOXED
  #BOXED('Fields to View in '& %Primary)
    #BUTTON('Field to View'),MULTI(%ViewFields,%ViewField),INLINE
      #PROMPT('&Field:',FIELD(%Primary)),%ViewField
      #PROMPT('&BIND field',CHECK),%ViewFieldBound
    #ENDBUTTON
  #ENDBOXED
  #ATSTART
    #INSERT(%FileControlInitialize)
  #ENDAT
  #AT(%DataSectionBeforeWindow)
%[20]ViewLabel VIEW(%Primary)
    #FIX(%File,%Primary)
    #FOR(%ViewFields)
      #FIX(%Field,%ViewField)
      #IF(%Field)
%[22]Null PROJECT(%Field)
      #ENDIF
    #ENDFOR
%[20]Null END
  #ENDAT
  #AT(%AfterFileOpen)
    #FIX(%File,%Primary)
    #FOR(%ViewFields)
      #FIX(%Field,%ViewField)
      #IF(%Field AND %ViewFieldBound)
BIND('%Field',%Field)
      #ENDIF
    #ENDFOR
  #ENDAT
#!---------------------------------------------------------------------------
#EXTENSION(PrintASCIIFile,'Support direct ASCII printing'),APPLICATION
#DISPLAY('PrintASCIIFile  PROCEDURE  ! ASCIIFileName variable used.')
#DISPLAY('Avalable either thru ''LaunchASCIIPrint'' CODE Template')
#DISPLAY('and Your own embedded code.')
#DISPLAY('')
#BOXED('')
  #DISPLAY('Copyright (C) 1997, Oleg Fomin <<oleg@grain.crimea.com>')
#ENDBOXED
#GLOBALDATA
ASCIIFileName        CSTRING(80)
OEM_ANSI_Print       STRING('OEM ')
#ENDGLOBALDATA
#AT(%CustomGlobalDeclarations)
  #FIX(%Driver,'ASCII')
  #ADD(%UsedDriverDLLs,%DriverDLL)
  #PROJECT(%DriverLIB)
#ENDAT
#AT(%GlobalMap)
  OMIT('***',_WIDTH32_)
MODULE('WinAPI.DLL')
  OpenComm(*CSTRING,SIGNED,SIGNED),SIGNED,PASCAL,RAW
  GetCommError(SIGNED,*COMSTAT),SIGNED,PASCAL,RAW
  WriteComm(SIGNED,*CSTRING,SIGNED),SIGNED,PASCAL,RAW
  CloseComm(SIGNED),PASCAL
END
  !***
#ENDAT
#AT(%GlobalMap)
PrintASCIIFile         !Send file (ASCIIFileName) to LPT1 using Windows API
#ENDAT
#!
#AT(%ProgramProcedures)
PrintASCIIFile    PROCEDURE
  COMPILE('***',_WIDTH32_)
  CODE
  RUN('cmd /c copy "'& CLIP(ASCIIFileName) &'" LPT1')
  !***
  OMIT('***',_WIDTH32_)
COMSTAT             GROUP,PRE(LPT:)
status                BYTE
cbInQue               UNSIGNED
cbOutQue              UNSIGNED
                    END
Port                CSTRING('LPT1')
Output              CSTRING(387)
PortID              LONG
SendAttempt         LONG
CharNumber          LONG
PrintError          LONG
FileSize            LONG
BytesPrinted        LONG
PercentProgress     BYTE
AllDone             BOOL
OutFile             FILE,DRIVER('ASCII'),PRE(PRN:),NAME(ASCIIFileName)
                      RECORD
ASCII:String            STRING(386)
                      END
                    END
PRN::DataChar       STRING(1)
PRN::RecordLenth    SHORT
PRN::CharPointer    SHORT
#INSERT(%StandardProgressWindow)
  CODE
  PortID = OpenComm(Port,0,0)
  IF PortID <= 0
    IF MESSAGE('Error initializing port LPT1. Code:'& PortID,,ICON:Asterisk,Button:Cancel)
    END
    RETURN
  END
  OPEN(OutFile,40h)
  IF ERROR()
    IF MESSAGE('Open file error '''& ASCIIFileName &''' :'& ERROR()).
    RETURN
  END
  FileSize = BYTES(OutFile)
  IF NOT FileSize
    IF MESSAGE('File '''& ASCIIFileName &''' is empty!').
  END
  OPEN(ProgressWindow)
  ProgressWindow{Prop:Timer} = '1'
  PercentProgress = 0
  ProgressWindow{Prop:Text} = 'Printing to LPT1'
  Progress:Thermometer = 0
  ?Progress:PctText{Prop:Text} = 'Completed 0'
  ?Progress:UserString{Prop:Text} = ''
  PRN::RecordLenth = 0
  PRN::CharPointer = 1 !PRN::CharPointer > PRN::RecordLenth : Force NEXT(NEXT(OutFile))
  BytesPrinted = 0
  AllDone = FALSE
  SET(OutFile)
  ACCEPT
    CASE EVENT()
    OF Event:Timer
      IF AllDone = TRUE
        PrintError = GetCommError(PortID,COMSTAT)
        IF LPT::cbOutQue = 0
          POST(Event:CloseWindow)
        END
      ELSE
        IF NOT SendAttempt
          IF    PRN::CharPointer <= PRN::RecordLenth            !Next character
            PRN::CharPointer += 1
            PRN::DataChar = PRN::ASCII:String[PRN::CharPointer]
          ELSE  !PRN::CharPointer > PRN::RecordLenth            !Next line/character
            NEXT(OutFile)
            IF ERRORCODE() = 33
              AllDone = TRUE
              CYCLE
            ELSIF ERRORCODE()
              X# = MESSAGE('Error reading file '& ASCIIFileName &': '& ERROR())
              POST(Event:CloseWindow)
              CYCLE
            ELSE
              PRN::RecordLenth = LEN(CLIP(PRN::ASCII:String)) + 2
              PRN::ASCII:String[PRN::RecordLenth-1] = CHR(0Dh)
              PRN::ASCII:String[PRN::RecordLenth]   = CHR(0Ah)
              PRN::CharPointer = 1
            END
          END
        END
        Output = PRN::ASCII:String[PRN::CharPointer : PRN::RecordLenth]
        IF OEM_ANSI_Print = 'ANSI'
          CONVERTANSITOOEM(Output)
        END
        IF LPT::cbOutQue < 512              !Prevent buffer overflow
          CharNumber = WriteComm(PortID,Output,PRN::RecordLenth - PRN::CharPointer + 1)
        ELSE
          CharNumber = 0
        END
        PrintError = GetCommError(PortID,COMSTAT)
        IF CharNumber = 0
          SendAttempt += 1
          IF SendAttempt > 100
            ?Progress:UserString{Prop:Text} = 'Printer not ready! Code:'& PrintError
          END
        ELSE
          BytesPrinted += CharNumber
          PRN::CharPointer += CharNumber
          SendAttempt = 0
          ?Progress:UserString{Prop:Text} = 'Printing file '& CLIP(ASCIIFileName)
        END
      END
      PercentProgress = 100 * BytesPrinted / FileSize
      Progress:Thermometer = PercentProgress
      ?Progress:PctText{Prop:Text} = 'Completed ' & FORMAT(PercentProgress,@N3) & '% (' & BytesPrinted & ') bytes'
      DISPLAY(?Progress:Thermometer)
      DISPLAY(?Progress:PctText)
      YIELD
    END
    CASE FIELD()
    OF ?Progress:Cancel
      CASE EVENT()
      OF Event:Accepted
        POST(Event:CloseWindow)
      END
    END
  END
  CLOSE(ProgressWindow)
  CloseComm(PortID)
  CLOSE(OutFile)
  RETURN
  !***
#ENDAT
#!
#!---------------------------------------------------------------------------
#CODE(LaunchASCIIPrint,'Print ASCII File on LPT1'),REQ(PrintASCIIFile)
#DISPLAY('Global variable ASCIIFileName contain FileName to print')
#DISPLAY('           (compatible with ''Viewer'' Clarion template)')
#PROMPT('OEM File',CHECK),%PrintOEMFile,AT(10)
#PROMPT('Background printing',CHECK),%BackgroundPrinting,AT(10)
#PROMPT('Confirm print launch',CHECK),%ConfirmPrintLaunch,AT(10)
#BOXED('Confirm message'),WHERE(%ConfirmPrintLaunch)
  #PROMPT('Text',@S80),%ConfirmLaunchText,AT(40,,150),DEFAULT('Are You really want to print on LPT1?')
  #PROMPT('Caption',@S20),%ConfirmLaunchCaption,AT(40,,150),DEFAULT('Confirm printing')
#ENDBOXED
#IF(%ConfirmPrintLaunch)
IF MESSAGE('%'ConfirmLaunchText','%'ConfirmLaunchCaption',|
   Icon:Question,Button:Yes+Button:No,Button:Yes)=Button:Yes
#ENDIF
#IF(%PrintOEMFile)
  OEM_ANSI_Print = 'OEM '
#ELSE
  OEM_ANSI_Print = 'ANSI'
#ENDIF
#IF(%BackgroundPrinting)
  START(PrintASCIIFile)
#ELSE
  PrintASCIIFile
#END
#IF(%ConfirmPrintLaunch)
END
#ENDIF
#INCLUDE('asciirpt.tpw')