program commandoo;

{$mode objfpc}{$H+}
//{ $DEFINE debug}     // do this here or you can define a -dDEBUG in Project Options/Other/Custom Options, i.e. in a build mode so you can set up a Debug with leakview and a Default build mode without it

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ufrmMain, ufrmMsgdlg, unitsharedobj, juusgen, memdslaz, sdflaz, strconst_prog, strconst_en, JiniFiles,
  ufrmAbout, ufrmnolanguage, unitLanguages, ufrmaddlanguage, HintFrame, unitcommands, uSingleInput,
  lazcontrols, ufrmProfiles, unitGlobForm, ufrmListManagerConsolidate, ufrmcmdlineedit, linuxtrix,
  unitfrmcommandsinput, unitfrmCommandsVar, unitfields, ufrmfindtext, unitDBUtils, ufrmListManager, ufrmManageProfile,
  unitDBStructure, unitSearch, ufrmSearch, ufrmBoolExpr, ufrm_search_operator, ufrm_search_double, ufrmSuperUserFile,
  ufrm_search_list, ufrm_search_text, ufrm_search_string, ufrm_search_bool, ufrm_search_enum, ufrm_search_integer,
  {$IFDEF heapdebug}
  SysUtils,
  {$ENDIF}
  ufrmFind, unitDBConstants, ufrmprofilesmerge, unitglob, ufrmOptions, ufrmSimpleSearch;

{$R *.res}

begin
  {$IFDEF heapdebug}
  // Assuming your build mode sets -dDEBUG in Project Options/Other when defining -gh
  // This avoids interference when running a production/default build without -gh

  // Set up -gh output for the Leakview package:
  if FileExists('/home/juus/Downloads/heap.trc') then
    DeleteFile('/home/juus/Downloads/heap.trc');
  SetHeapTraceOutput('/home/juus/Downloads/heap.trc');
  {$ENDIF}

  if Application.HasOption( 'h', 'help' )
     or Application.HasOption( 'v', 'version' )
     or Application.HasOption( 'V', '' ) then
  begin
    writeln( LineEnding
             + 'commandoo: GUI commmand line tester/manager application'
             + LineEnding
             + 'Version: ' + cHandwrittenVersion + cVersionDate
             + LineEnding
             + 'commandoo takes no parameters, help included in application.'
             + LineEnding + LineEnding
             + '  -h, --help : this message'
             + LineEnding
             + '  -v, -V, --version : this message'
             + LineEnding + LineEnding
             );
    exit;
  end;

  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm( TfrmMain, frmMain );
  Application.Run;
end.

