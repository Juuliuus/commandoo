unit unitLanguages;
{
    Commandoo Program: Helper application for Linux commands / CLI
    Copyright (C) 2017 Julius Heinrich Ludwig Schön / Ronald Michael Spicer
    created by Julius Schön / R. Spicer
    Foto.TimePirate.org / TimePirate.org / PaganToday.TimePirate.org
    Julius@TimePirate.org

        This program is free software: you can redistribute it and/or modify
        it under the terms of the GNU General Public License as published by
        the Free Software Foundation, version 3 of the License.

        This program is distributed in the hope that it will be useful,
        but WITHOUT ANY WARRANTY; without even the implied warranty of
        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
        GNU General Public License for more details.

        You should have received a copy of the GNU General Public License
        along with this program.  If not, see <http://www.gnu.org/licenses/>.

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unitsharedobj
  , juusgen
  , controls //this has the definitions of mrYes, mrOk etc.
  , JiniFiles
  ;

type

  { TLanguagesObj }

  TLanguages = class( TSharedObj )
  private
    fLanguageSL : TStringList;
    fLanguageFilesFolder : string;
    fProgramLang : string;
    fSectTabLanguages : string;
    fSectTabFormSettings : string;
    fFormSettingsProgramLangCol : string;
    fIniFile : TJinifile; //pointer only

    procedure SetProgramLang(AValue : string);

  public
    constructor Create; override;
    destructor Destroy; override;
    function Init( const LanguageFilesFolder, LanguagesSectionOrTableName, SectTabFormSettings,
                     FormSettingsProgramLangCol : string; TheIniFile : TJiniFile ) : boolean;

    procedure PrepAndApplyLanguage( FromStartUp : boolean = false );
    function GetInstalledLanguages : string;
    function GetBiDiMode : TBiDiMode;
    function ChooseLanguage( var Cnt : integer ) : string;
    function InstallNewLanguage( aList : TStrings ) : integer;
    function Under18( const LangID : string ) : boolean;
    function UpdateProgramLang( const Lang : string; OnlyEnglish : boolean = false ) : boolean;

    property LanguageSL : TStringList read fLanguageSL;
    property ProgramLang : string read FProgramLang write SetProgramLang;

  end;

var

  Languages : TLanguages;

resourcestring
  ccapOver18 = 'Mature Adult Language';
  cmsgOver18 =
    'The language you have chosen is for mature adult use only. This means you '
    + 'need to be 18 years old or older (or whatever applies in your country)'
    + LineEnding + LineEnding
    + 'Are you over 18 (or whatever)?';
  cNotValidPOFile =
    '"%s" is not a valid .po file for this program. ' + LineEnding + LineEnding
    + 'A valid .po filename '
    + 'looks like "programname.xx.po" (where XX equals some letter combination '
    + 'like "es" or "en" or "pyr", etc.). Everything should be in lowercase. ';
   cLangSymbol = 'Language Symbol: "%s"';
   cmsgTranslation_missingPOFile = 'The language file "%s" is not installed. Maybe it was deleted??';
   cmsgLangIsUninitialized = 'Not available, Languages was not initialized, probable a bad config path.';



const

  cmsgTranslation_missingPOFile_en = 'You have chosen English but the .po file is not installed. The language will change when you have restarted. Install commandoo.en.po if you want it to be automatically changed next time.';
  cLanguageEntryFormat = '%s (%s)';
  cLanguageFileNamePO = '%s.%s.po';
  cLanguageFileNameMO = '%s.%s.mo';

  cLanguageColName = 'LangAbbrev';

  cLangAbbrev_en = 'en';
  cLangStr_en = 'Default - English (en)';

  cLangAbbrev_ar = 'ar';
  cLangAbbrev_ca = 'ca';
  cLangAbbrev_da = 'da';
  cLangAbbrev_nl = 'nl';
  cLangAbbrev_fr = 'fr';
  cLangAbbrev_de = 'de';
  cLangAbbrev_el = 'el';
  cLangAbbrev_hu = 'hu';
  cLangAbbrev_it = 'it';
  cLangAbbrev_ja = 'ja';
  cLangAbbrev_pl = 'pl';
  cLangAbbrev_pt = 'pt';
  cLangAbbrev_ru = 'ru';
  cLangAbbrev_es = 'es';
  cLangAbbrev_sv = 'sv';
  cLangAbbrev_tr = 'tr';
  cLangAbbrev_uk = 'uk';
  //cLangAbbrev_calif = 'califxxx';
  cLangAbbrev_pyr = 'pyrxxx';

  cLangStr_ar = 'Arabic / عربي (ar)';
  cLangStr_ca = 'Catalan / Català (ca)';
  cLangStr_da = 'Danish / Danske (da)';
  cLangStr_nl = 'Dutch / Nederlandse (nl)';
  cLangStr_fr = 'French / Français (fr)';
  cLangStr_de = 'German / Deutsch (de)';
  cLangStr_el = 'Greek / Ελληνική (el)';
  cLangStr_hu = 'Hungarian / Magyar (hu)';
  cLangStr_it = 'Italian / Italiano (it)';
  cLangStr_ja = 'Japanese / 日本語 (ja)';
  cLangStr_pl = 'Polish / Polskie (pl)';
  cLangStr_pt = 'Portuguese / Português (pt)';
  cLangStr_ru = 'Russian / русский (ru)';
  cLangStr_es = 'Spanish / Español (es)';
  cLangStr_sv = 'Swedish / Svensk (sv)';
  cLangStr_tr = 'Turkish / Türk (tr)';
  cLangStr_uk = 'Ukrainian / Український (uk)';
  //cLangStr_calif = 'California (califxxx)';
  cLangStr_pyr = 'Pyrate (pyrxxx)';
  //you can add whatever you want just need a po file that matches
  //some other language stuff is at the bottom after the end

implementation

uses
  ufrmnolanguage
//juus
  //, unitjLCLTranslator
  , LCLTranslator
  , ufrmMsgDlg
  , ufrmAddLanguage
  , strconst_prog
  ;

const
  claLanguageFilesFolderBlankOrInvalid = 'Hey Programmer! TLanguages.Init requires a valid languages po files folder.';
  claSectTabLanguagesInvalid = 'Hey Programmer! TLanguages.Init requires the name of the Table/Section where langages are listed.';
  claSectTabFormSettingsInvalid = 'Hey Programmer! TLanguages.Init requires the name of the Table/Section where FormSettings are stored.';
  claFormSettingsProgramLangColInvalid = 'Hey Programmer! TLanguages.Init requires the name of the Key/Field where the Program Language is stored.';
  claBadInifile = 'Hey Programmer! TLanguages.Init got a nil jinifile.';


{ TLanguages }

procedure TLanguages.SetProgramLang(AValue : string);
begin
  if FProgramLang = AValue then Exit;
  FProgramLang := AValue;
end;

constructor TLanguages.Create;
begin
  inherited;
  fLanguageSL := TStringList.Create;
end;

destructor TLanguages.Destroy;
begin
  fIniFile := nil;
  if Assigned( fLanguageSL ) then
    FreeAndNil( fLanguageSL );

  inherited Destroy;
end;

function TLanguages.Init( const LanguageFilesFolder, LanguagesSectionOrTableName, SectTabFormSettings,
                FormSettingsProgramLangCol : string; TheIniFile : TJiniFile) : boolean;
var
  TrimLanguageFilesFolder : string;
begin
  result := false;

  TrimLanguageFilesFolder := Trim( LanguageFilesFolder );

{$IFNDEF Release}
  fIniFile := TheIniFile;
  if DontRun( IsInitialized, format( csoCallOnce, [ 'TLanguages' ] ) )
     or DontRun( TrimLanguageFilesFolder = '', claLanguageFilesFolderBlankOrInvalid )
     or DontRun( not DirectoryExists( TrimLanguageFilesFolder ), claLanguageFilesFolderBlankOrInvalid )
     or DontRun( Trim( LanguagesSectionOrTableName ) = '', claSectTabLanguagesInvalid )
     or DontRun( Trim( SectTabFormSettings ) = '', claSectTabFormSettingsInvalid )
     or DontRun( not assigned( TheIniFile ), claBadInifile )
     or DontRun( Trim( FormSettingsProgramLangCol ) = '', claFormSettingsProgramLangColInvalid )
     //or DontRun( true, 'Yikes' )
  then
  begin
    fIniFile := nil;
    exit;
  end;
{$ENDIF}

  fLanguageFilesFolder := TrimLanguageFilesFolder;
  fSectTabLanguages := LanguagesSectionOrTableName;
  fFormSettingsProgramLangCol := FormSettingsProgramLangCol;
  fIniFile := TheIniFile;
  fSectTabFormSettings := SectTabFormSettings;

  IsInitialized := true;
  result := true;

end;

procedure TLanguages.PrepAndApplyLanguage( FromStartUp : boolean = false );
var
  FallbackLang : String;
  Cnt : integer;
begin

  if not IsInitialized then
    exit;

  if fIniFile.SectionExists( 'languages' ) then //old section name, needed new list and using new section name.
    fIniFile.EraseSection( 'languages' );

  if not fIniFile.SectionExists( fSectTabLanguages ) then
  with fIniFile do
  begin

    WriteString( fSectTabLanguages, cLangAbbrev_ar, cLangStr_ar );
    WriteString( fSectTabLanguages, cLangAbbrev_ca, cLangStr_ca );
    WriteString( fSectTabLanguages, cLangAbbrev_da, cLangStr_da );
    WriteString( fSectTabLanguages, cLangAbbrev_fr, cLangStr_fr );
    WriteString( fSectTabLanguages, cLangAbbrev_de, cLangStr_de );
    WriteString( fSectTabLanguages, cLangAbbrev_el, cLangStr_el );
    WriteString( fSectTabLanguages, cLangAbbrev_es, cLangStr_es );
    WriteString( fSectTabLanguages, cLangAbbrev_hu, cLangStr_hu );
    WriteString( fSectTabLanguages, cLangAbbrev_it, cLangStr_it );
    WriteString( fSectTabLanguages, cLangAbbrev_ja, cLangStr_ja );
    WriteString( fSectTabLanguages, cLangAbbrev_nl, cLangStr_nl );
    WriteString( fSectTabLanguages, cLangAbbrev_pl, cLangStr_pl );
    WriteString( fSectTabLanguages, cLangAbbrev_pt, cLangStr_pt );
    WriteString( fSectTabLanguages, cLangAbbrev_ru, cLangStr_ru );
    WriteString( fSectTabLanguages, cLangAbbrev_sv, cLangStr_sv );
    WriteString( fSectTabLanguages, cLangAbbrev_tr, cLangStr_tr );
    WriteString( fSectTabLanguages, cLangAbbrev_uk, cLangStr_uk );
//    WriteString( fSectTabLanguages, cLangAbbrev_calif, cLangStr_calif );
    WriteString( fSectTabLanguages, cLangAbbrev_pyr, cLangStr_pyr );

    UpdateFile;

  end;

//juuus this bypasses the big opening choose language form, which makes no sense when there are
//no other languages to choose from.
  fProgramLang := fIniFile.Readstring( fSectTabFormSettings, fFormSettingsProgramLangCol, cLangAbbrev_en );

  FallbackLang := GetInstalledLanguages;

  if FromStartUp and ( fProgramLang = cLangAbbrev_en ) then
    exit;

  if FallbackLang <> '' then
    fProgramLang := FallbackLang;

  Cnt := 1;
  if fProgramLang = '' then
    fProgramLang := ChooseLanguage( Cnt );

  UpdateProgramLang( fProgramLang, Cnt = 1 );

end;

function TLanguages.GetInstalledLanguages : string;
var
  SL : TStringList;
  i : Integer;
begin

  result := '';

  //NO!! == >  GetLanguageIDs(Lang, FallbackLang);//This looks at locale, I want it to use saved settings
  fLanguageSL.Add( cLangStr_en );

  SL := TStringList.Create;
  try

    fIniFile.Readsection( fSectTabLanguages, SL );
    for i := 0 to SL.Count - 1 do
    begin
      if FileExists( fLanguageFilesFolder + format( cLanguageFileNamePO, [ cReferenceProgramName, SL[ i ] ] ) ) then
        fLanguageSL.Add( fIniFile.Readstring( fSectTabLanguages, SL[ i ], '' ) );
    end;
  finally
    SL.Free;
  end;

  if fLanguageSL.Count = 0 then
    result := cLangAbbrev_en;

end;

function TLanguages.UpdateProgramLang( const Lang : string; OnlyEnglish : boolean = false ) : boolean;
var
  POFileName : string;
begin
  result := false;

  if not IsInitialized then
  begin
    MyShowMessage( cmsgLangIsUninitialized, nil );
    exit;
  end;

  result := true;
  fProgramLang := Lang;

  fIniFile.WriteString( fSectTabFormSettings, fFormSettingsProgramLangCol, fProgramLang );
  fIniFile.UpdateFile;

//NOTE: The executable name can be changed (but is that a good idea???). But all ancillary
//files like language files and the main settings file will remain, hard-coded, to
//"commandoo", don't change that. Just sayin'.

  POFileName := fLanguageFilesFolder + format( cLanguageFileNamePO, [ cReferenceProgramName, fProgramLang ] );
  if fileexists ( POFileName ) then
//TRUNK added 3rd string param
//    LCLTranslator.setdefaultlang( fProgramLang, fLanguageFilesFolder, cReferenceProgramName, true )
    LCLTranslator.setdefaultlang( fProgramLang, fLanguageFilesFolder )
  else
  begin
    if pos( '.en.po', POFileName ) > 0 then
    begin
      if not OnlyEnglish then
        InternalMessage( cmsgTranslation_missingPOFile_en );
    end
    else InternalMessage( format( cmsgTranslation_missingPOFile, [ POFileName ] ) );
  end;

end;

function TLanguages.Under18( const LangID : string ) : boolean;
begin
  result := false;

// => xxx <-- used in language PO files to denote "languages" that use adult language.
  if pos( 'xxx', LangId ) > 0 then
  begin
    MsgDlgMessage( ccapOver18, cmsgOver18 );
    if MsgDlgAttentionConfirm( nil ) = mrNo then
      result := true;
  end;

end;


function TLanguages.ChooseLanguage( var Cnt : integer ) : string;
var
  i : Integer;
begin
  with TfrmNoLanguage.Create( nil ) do
  try
//    cbLanguage.Items.Text := fLanguageSL.Text;
    cbLanguage.Items.Assign( fLanguageSL );
    for i := cbLanguage.Items.Count - 1 downto 0 do
      if pos( 'XXX', uppercase( cblanguage.Items[ i ] ) ) > 0 then
        cblanguage.Items.Delete( i );

    Cnt := cbLanguage.Items.Count;
    cbLanguage.ItemIndex := 0;
    ShowModal;
    ShowMessagePtX := left;
    ShowMessagePtY := top;
    result := GetMainLanguageID( cbLanguage.Text );
  finally
    free;
  end;
end;

function TLanguages.InstallNewLanguage( aList : TStrings) : integer;
var
  MenuEntry, Str : String;
  idx : Integer;
  i : Integer;
begin

  result := -1;
  if not IsInitialized then
  begin
    MyShowMessage( cmsgLangIsUninitialized, nil );
    exit;
  end;


  with TfrmAddLanguage.Create( nil ) do
  try

    fIniFile.Readsection( fSectTabLanguages, SymbolList );
    for i := 0 to SymbolList.Count - 1 do
    begin
      Str := fIniFile.Readstring( fSectTabLanguages, SymbolList[ i ], '' );
      Idx := pos( '(', Str );
      if Idx > 0 then
        Str := trim( Copy( Str, 1, Idx - 1 ) );
      MenuList.Add( Str );
    end;

    SaveToPath := fLanguageFilesFolder;

    Showmodal;

    if ModalResult = mrOK then
    begin
      MenuEntry := format( cLanguageEntryFormat,
                           [ CurrLangText,
                             LangSymbol
                            ] );

      idx := fLanguageSL.IndexOf( MenuEntry );
      fIniFile.WriteString( fSectTabLanguages, LangSymbol, MenuEntry );

      if Idx > - 1 then
        fLanguageSL[ Idx ] := MenuEntry
      else idx := fLanguageSL.Add( MenuEntry );

      result := idx;
      AList.Assign( fLanguageSL );

    end;
  finally
    free;
  end;

end;

function TLanguages.GetBiDiMode : TBiDiMode;
begin
  case fProgramLang of
    'ar': Result := bdRightToLeft;// Arabic
    'he': Result := bdRightToLeft;// Hebrew
    'ur': Result := bdRightToLeft;// Urdu
    else
      Result := bdLeftToRight;
  end;
end;




initialization

  Languages := TLanguages.Create;

finalization
  if assigned( Languages ) then
    FreeAndNil( Languages );

end.

  //cLangRestart_en = 'You need to restart the program for this language to be in effect.';
  //cLangRestart_ar = 'تحتاج إلى إعادة تشغيل البرنامج لهذه اللغة لتكون سارية المفعول.';
  //cLangRestart_ca = 'Cal reiniciar el programa per aquest idioma estigui en vigor.';
  //cLangRestart_zh = '您需要重新启动该程序，这种语言生效.';
  //cLangRestart_da = 'Du skal genstarte programmet for dette sprog til at være i kraft.';
  //cLangRestart_nl = 'Je nodig hebt om het programma voor deze taal te zijn in feite opnieuw te starten.';
  //cLangRestart_fr = 'Vous devez redémarrer le programme pour cette langue soit en vigueur.';
  //cLangRestart_de = 'Sie müssen das Programm für diese Sprache neu zu starten.';
  //cLangRestart_el = 'Θα χρειαστεί να κάνετε επανεκκίνηση του προγράμματος για τη γλώσσα αυτή να είναι σε ισχύ.';
  //cLangRestart_hu = 'Újra kell indítani a programot ezen a nyelven is érvényben.';
  //cLangRestart_it = 'È necessario riavviare il programma per questa lingua sia a tutti gli effetti.';
  //cLangRestart_ja = 'あなたは、この言語が有効になるために、プログラムを再起動する必要があります';
  //cLangRestart_pl = 'Musisz ponownie uruchomić program ten język będzie w mocy.';
  //cLangRestart_pt = 'Você precisa reiniciar o programa para essa linguagem para estar em vigor.';
  //cLangRestart_ru = 'Вы должны перезапустить программу для этого языка, чтобы быть в силе.';
  //cLangRestart_es = 'Es necesario reiniciar el programa para este idioma esté en vigor.';
  //cLangRestart_sv = 'Du måste starta om programmet för detta språk för att vara i kraft.';
  //cLangRestart_tr = 'Bu dil geçerli olabilmesi için programı yeniden gerekir.';
  //cLangRestart_uk = 'Ви повинні перезапустити програму для цієї мови, щоб бути в сил.';
  //cLangRestart_vi = 'Bạn cần khởi động lại chương trình cho ngôn ngữ này để có hiệu lực.';
  //cLangRestart_cy = 'Mae angen i chi ailgychwyn y rhaglen ar gyfer yr iaith hon fod yn effeithiol.';
  //cLangRestart_zy = 'Yo homie, restart the fuckin Progam.';
  //cLangRestart_zx = 'Matey, you`se be needin to start this blasted program anew, then the raeadin`ll be eazier.';

  //case frmjuursync.ProgLang of
  //  'ar' : Showmessage( cLangRestart_ar );
  //  'ca' : Showmessage( cLangRestart_ca );
  //  'zh' : Showmessage( cLangRestart_zh );
  //  'da' : Showmessage( cLangRestart_da );
  //  'nl' : Showmessage( cLangRestart_nl );
  //  'fr' : Showmessage( cLangRestart_fr );
  //  'de' : Showmessage( cLangRestart_de );
  //  'el' : Showmessage( cLangRestart_el );
  //  'hu' : Showmessage( cLangRestart_hu );
  //  'it' : Showmessage( cLangRestart_it );
  //  'ja' : Showmessage( cLangRestart_ja );
  //  'pl' : Showmessage( cLangRestart_pl );
  //  'pt' : Showmessage( cLangRestart_pt );
  //  'ru' : Showmessage( cLangRestart_ru );
  //  'es' : Showmessage( cLangRestart_es );
  //  'sv' : Showmessage( cLangRestart_sv );
  //  'tr' : Showmessage( cLangRestart_tr );
  //  'uk' : Showmessage( cLangRestart_uk );
  //  'vi' : Showmessage( cLangRestart_vi );
  //  'cy' : Showmessage( cLangRestart_cy );
  //  'zy' : Showmessage( cLangRestart_zy );
  //  'zx' : Showmessage( cLangRestart_zx );
  //  else Showmessage( cLangRestart_en );
  //end;
{
Language lists

small list
------------------------
Default - English (en)
Arabic (ar)
Catalan (ca)
Chinese (zh)
Danish (da)
Dutch (nl)
French (fr)
German (de)
Greek (el)
Hindi (hi)
Hungarian (hu)
Italian (it)
Japanese (ja)
Polish (pl)
Portuguese (pt)
Russian (ru)
Spanish (es)
Swedish (sv)
Thai (th)
Turkish (tr)
Ukrainian (uk)
Vietnamese (vi)
Welsh (cy)
StreetThug (zy)
Pirate (zx)



full list

Default - English (en)
Afrikaans
Albanian
Amharic
Arabic - Algeria
Arabic - Bahrain
Arabic - Egypt
Arabic - Iraq
Arabic - Jordan
Arabic - Kuwait
Arabic - Lebanon
Arabic - Libya
Arabic - Morocco
Arabic - Oman
Arabic - Qatar
Arabic - Saudi Arabia
Arabic - Syria
Arabic - Tunisia
Arabic - UAE
Arabic - Yemen
Armenian
Assamese
Azeri - Cyrillic
Azeri - Latin
Basque
Belarusian
Bengali
Bengali - India
Bosnian
Bulgarian
Burmese
Catalan
Chinese - China
Chinese - Hong Kong
Chinese - Macau
Chinese - Singapore
Chinese - Taiwan
Croatian
Czech
Danish
Divehi
Dutch - Belgium
Dutch - Netherlands
English - Australia
English - Belize
English - Canada
English - Caribbean
English - Great Britain
English - India
English - Ireland
English - Jamaica
English - New Zealand
English - Phillippines
English - Southern Africa
English - Trinidad
English - United States
English - Zimbabwe
Estonian
Faroese
Farsi
Filipino
Finnish
French - Belgium
French - Cameroon
French - Canada
French - Congo
French - Cote d`Ivoire
French - France
French - Luxembourg
French - Mali
French - Monaco
French - Morocco
French - Senegal
French - Switzerland
French - West Indies
Frisian - Netherlands
FYRO Macedonia
Gaelic - Ireland
Gaelic - Scotland
Galician
Georgian
German - Austria
German - Germany
German - Liechtenstein
German - Luxembourg
German - Switzerland
Greek
Guarani - Paraguay
Gujarati
Hebrew
Hindi
Hungarian
Icelandic
Igbo - Nigeria
Indonesian
Italian - Italy
Italian - Switzerland
Japanese
Kannada
Kashmiri
Kazakh
Khmer
Korean
Lao
Latin
Latvian
Lithuanian
Malay - Brunei
Malay - Malaysia
Malayalam
Maltese
Manipuri
Maori
Marathi
Mongolian
Mongolian
Nepali
Norwegian - Bokml
Norwegian - Nynorsk
Oriya
Polish
Portuguese - Brazil
Portuguese - Portugal
Punjabi
Raeto-Romance
Romanian - Moldova
Romanian - Romania
Russian
Russian - Moldova
Sanskrit
Serbian - Cyrillic
Serbian - Latin
Setsuana
Sindhi
Sinhala; Sinhalese
Slovak
Slovenian
Somali
Sorbian
Spanish - Argentina
Spanish - Bolivia
Spanish - Chile
Spanish - Colombia
Spanish - Costa Rica
Spanish - Dominican Republic
Spanish - Ecuador
Spanish - El Salvador
Spanish - Guatemala
Spanish - Honduras
Spanish - Mexico
Spanish - Nicaragua
Spanish - Panama
Spanish - Paraguay
Spanish - Peru
Spanish - Puerto Rico
Spanish - Spain
Spanish - Uruguay
Spanish - Venezuela
Swahili
Swedish - Finland
Swedish - Sweden
Tajik
Tamil
Tatar
Telugu
Thai
Tibetan
Tsonga
Turkish
Turkmen
Ukrainian
Urdu
Uzbek - Cyrillic
Uzbek - Latin
Vietnamese
Welsh
Xhosa
Yiddish
Zulu
PygLatin


Afrikaans  af  af  1078  436  1252
Albanian  sq  sq  1052    1250
Amharic  am  am  1118
Arabic - Algeria  ar  ar-dz  5121  1401  1256
Arabic - Bahrain  ar  ar-bh  15361    1256
Arabic - Egypt  ar  ar-eg  3073    1256
Arabic - Iraq  ar  ar-iq  2049  801  1256
Arabic - Jordan  ar  ar-jo  11265    1256
Arabic - Kuwait  ar  ar-kw  13313  3401  1256
Arabic - Lebanon  ar  ar-lb  12289  3001  1256
Arabic - Libya  ar  ar-ly  4097  1001  1256
Arabic - Morocco  ar  ar-ma  6145  1801  1256
Arabic - Oman  ar  ar-om  8193  2001  1256
Arabic - Qatar  ar  ar-qa  16385  4001  1256
Arabic - Saudi Arabia  ar  ar-sa  1025  401  1256
Arabic - Syria  ar  ar-sy  10241  2801  1256
Arabic - Tunisia  ar  ar-tn  7169    1256
Arabic - United Arab Emirates  ar  ar-ae  14337  3801  1256
Arabic - Yemen  ar  ar-ye  9217  2401  1256
Armenian  hy  hy  1067
Assamese  as  as  1101
Azeri - Cyrillic  az  az-az  2092    1251
Azeri - Latin  az  az-az  1068    1254
Basque  eu  eu  1069    1252
Belarusian  be  be  1059  423  1251
Bengali - Bangladesh  bn  bn  2117  845
Bengali - India  bn  bn  1093  445
Bosnian  bs  bs  5146
Bulgarian  bg  bg  1026  402  1251
Burmese  my  my  1109  455
Catalan  ca  ca  1027  403  1252
Chinese - China  zh  zh-cn  2052  804
Chinese - Hong Kong SAR  zh  zh-hk  3076
Chinese - Macau SAR  zh  zh-mo  5124  1404
Chinese - Singapore  zh  zh-sg  4100  1004
Chinese - Taiwan  zh  zh-tw  1028  404
Croatian  hr  hr  1050    1250
Czech  cs  cs  1029  405  1250
Danish  da  da  1030  406  1252
Divehi; Dhivehi; Maldivian  dv  dv  1125  465
Dutch - Belgium  nl  nl-be  2067  813  1252
Dutch - Netherlands  nl  nl-nl  1043  413  1252
Edo      1126  466
English - Australia  en  en-au  3081    1252
English - Belize  en  en-bz  10249  2809  1252
English - Canada  en  en-ca  4105  1009  1252
English - Caribbean  en  en-cb  9225  2409  1252
English - Great Britain  en  en-gb  2057  809  1252
English - India  en  en-in  16393  4009
English - Ireland  en  en-ie  6153  1809  1252
English - Jamaica  en  en-jm  8201  2009  1252
English - New Zealand  en  en-nz  5129  1409  1252
English - Phillippines  en  en-ph  13321  3409  1252
English - Southern Africa  en  en-za  7177    1252
English - Trinidad  en  en-tt  11273    1252
English - United States  en  en-us  1033  409  1252
English - Zimbabwe  en    12297  3009  1252
Estonian  et  et  1061  425  1257
Faroese  fo  fo  1080  438  1252
Farsi - Persian  fa  fa  1065  429  1256
Filipino      1124  464
Finnish  fi  fi  1035    1252
French - Belgium  fr  fr-be  2060    1252
French - Cameroon  fr    11276
French - Canada  fr  fr-ca  3084    1252
French - Congo  fr    9228
French - Cote d'Ivoire  fr    12300
French - France  fr  fr-fr  1036    1252
French - Luxembourg  fr  fr-lu  5132    1252
French - Mali  fr    13324
French - Monaco  fr    6156    1252
French - Morocco  fr    14348
French - Senegal  fr    10252
French - Switzerland  fr  fr-ch  4108    1252
French - West Indies  fr    7180
Frisian - Netherlands      1122  462
FYRO Macedonia  mk  mk  1071    1251
Gaelic - Ireland  gd  gd-ie  2108
Gaelic - Scotland  gd  gd  1084
Galician  gl    1110  456  1252
Georgian  ka    1079  437
German - Austria  de  de-at  3079    1252
German - Germany  de  de-de  1031  407  1252
German - Liechtenstein  de  de-li  5127  1407  1252
German - Luxembourg  de  de-lu  4103  1007  1252
German - Switzerland  de  de-ch  2055  807  1252
Greek  el  el  1032  408  1253
Guarani - Paraguay  gn  gn  1140  474
Gujarati  gu  gu  1095  447
Hebrew  he  he  1037    1255
Hindi  hi  hi  1081  439
Hungarian  hu  hu  1038    1250
Icelandic  is  is  1039    1252
Igbo - Nigeria      1136  470
Indonesian  id  id  1057  421  1252
Italian - Italy  it  it-it  1040  410  1252
Italian - Switzerland  it  it-ch  2064  810  1252
Japanese  ja  ja  1041  411
Kannada  kn  kn  1099
Kashmiri  ks  ks  1120  460
Kazakh  kk  kk  1087    1251
Khmer  km  km  1107  453
Konkani      1111  457
Korean  ko  ko  1042  412
Kyrgyz - Cyrillic      1088  440  1251
Lao  lo  lo  1108  454
Latin  la  la  1142  476
Latvian  lv  lv  1062  426  1257
Lithuanian  lt  lt  1063  427  1257
Malay - Brunei  ms  ms-bn  2110    1252
Malay - Malaysia  ms  ms-my  1086    1252
Malayalam  ml  ml  1100
Maltese  mt  mt  1082
Manipuri      1112  458
Maori  mi  mi  1153  481
Marathi  mr  mr  1102
Mongolian  mn  mn  2128  850
Mongolian  mn  mn  1104  450  1251
Nepali  ne  ne  1121  461
Norwegian - Bokml  nb  no-no  1044  414  1252
Norwegian - Nynorsk  nn  no-no  2068  814  1252
Oriya  or  or  1096  448
Polish  pl  pl  1045  415  1250
Portuguese - Brazil  pt  pt-br  1046  416  1252
Portuguese - Portugal  pt  pt-pt  2070  816  1252
Punjabi  pa  pa  1094  446
Raeto-Romance  rm  rm  1047  417
Romanian - Moldova  ro  ro-mo  2072  818
Romanian - Romania  ro  ro  1048  418  1250
Russian  ru  ru  1049  419  1251
Russian - Moldova  ru  ru-mo  2073  819
Sami Lappish      1083
Sanskrit  sa  sa  1103
Serbian - Cyrillic  sr  sr-sp  3098    1251
Serbian - Latin  sr  sr-sp  2074    1250
Setsuana  tn  tn  1074  432
Sindhi  sd  sd  1113  459
Sinhala; Sinhalese  si  si  1115
Slovak  sk  sk  1051    1250
Slovenian  sl  sl  1060  424  1250
Somali  so  so  1143  477
Sorbian  sb  sb  1070
Spanish - Argentina  es  es-ar  11274    1252
Spanish - Bolivia  es  es-bo  16394    1252
Spanish - Chile  es  es-cl  13322    1252
Spanish - Colombia  es  es-co  9226    1252
Spanish - Costa Rica  es  es-cr  5130    1252
Spanish - Dominican Republic  es  es-do  7178    1252
Spanish - Ecuador  es  es-ec  12298    1252
Spanish - El Salvador  es  es-sv  17418    1252
Spanish - Guatemala  es  es-gt  4106    1252
Spanish - Honduras  es  es-hn  18442    1252
Spanish - Mexico  es  es-mx  2058    1252
Spanish - Nicaragua  es  es-ni  19466    1252
Spanish - Panama  es  es-pa  6154    1252
Spanish - Paraguay  es  es-py  15370    1252
Spanish - Peru  es  es-pe  10250    1252
Spanish - Puerto Rico  es  es-pr  20490    1252
Spanish - Spain es  es-es  1034    1252
Spanish - Uruguay  es  es-uy  14346    1252
Spanish - Venezuela  es  es-ve  8202    1252
Swahili  sw  sw  1089  441  1252
Swedish - Finland  sv  sv-fi  2077    1252
Swedish - Sweden  sv  sv-se  1053    1252
Syriac      1114
Tajik  tg  tg  1064  428
Tamil  ta  ta  1097  449
Tatar  tt  tt  1092  444  1251
Telugu  te  te  1098
Thai  th  th  1054
Tibetan  bo  bo  1105  451
Tsonga  ts  ts  1073  431
Turkish  tr  tr  1055    1254
Turkmen  tk  tk  1090  442
Ukrainian  uk  uk  1058  422  1251
Unicode    UTF-8  0
Urdu  ur  ur  1056  420  1256
Uzbek - Cyrillic  uz  uz-uz  2115  843  1251
Uzbek - Latin  uz  uz-uz  1091  443  1254
Venda      1075  433
Vietnamese  vi  vi  1066    1258
Welsh  cy  cy  1106  452
Xhosa  xh  xh  1076  434
Yiddish  yi  yi  1085
Zulu  zu  zu  1077  435

}


