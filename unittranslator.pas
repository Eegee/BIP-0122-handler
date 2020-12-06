unit UnitTranslator;

{ Unit to handle translations of resource strings found in a console
  application. It is similar to LCLTranslator (2004-2015 by
  V.I. Volchenko and Lazarus Developers Team) with the following changes
    - all reference to forms and the LCL are eliminated
    - function FindLocaleFileName has been rewritten
    - default parameter ForceUpdate in SetDefaultLang is not used as it
        applied to Forms. It is kept for compatibility only.
    - directories are searched only if they exist which will reduce
        the number of searches compared to the original code.

  2017 Michel Deslierres

  Below is the copyright notice of LCLTranslator.
}

{ Copyright (C) 2004-2015 V.I.Volchenko and Lazarus Developers Team

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{
This unit translates resource strings in a console program using a .po or .mo
file. For GUI applications (using LCL) use LCLTranslator and DefaultTranslator
instead. The unit searches for translated .po/.mo files in some common places:
  <myAddDir>/<userdir>/<Lang>/<file> or <userdir>/<Lang>/file
  <myAddDir>/languages/<Lang>/<file>
  <myAppDir>/locale/<Lang>/<file>
  <myAppDir>/locale/<Lang>/LC_MESSAGES/<file>
  <myAppDir><file>

  <myAddDir>/<userdir>/<lngfile> or <userdir>/<lngfile>
  <myAddDir>/languages/<lngfile>
  <myAppDir>/locale/<lngfile>
  <myAddDir><lngfile>

where
  userdir: can be a sub directory off the application directory or
           an absolute directory
  Lang: a locale specification
  file: will be app.po and then app.mo
  lngfile: will be app.<Lang>.po or app.<Lang>.mo

In a Unix (Linux) system, /usr/share/locale/<Lang>/LC_MESSAGES/<file>
is also checked.

The search stops as soon as a translation file is found.

By default, SetDefaultLang('', '') searches for the .po or .mo file
corresponding to the system locale. This can be overriden in the application
command line with the '--LANG LangID' switch or its aliases '--lang' and '-l'.

To translate resource strings in a console application, just use this unit in
the project and enable i18n in the project options. Translate the po files
generated by the IDE and place the translated files in one of the above
directory (changing its name if desired).

Dependency: LCLBase, not LCL.
}

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Gettext, Translations, LazUTF8, LazFileUtils;

procedure SetDefaultLang(Const Lang: string; Const Dir: string = ''; ForceUpdate: boolean = true);
function GetDefaultLang: String;

implementation

var
  DefaultLang: String = '';

function GetDefaultLang: String;
begin
  if DefaultLang = '' then SetDefaultLang('');
  GetDefaultLang := DefaultLang;
end;

  {Returns Language Id as found in the following order:
    1. Lang parameter (if not '')
    2. Command line parameter (--Lang | --lang | -l LangId)
    3. Environment variable LANG (if not '')
    4. Other locale environment variables
              'LC_ALL', LC_MESSAGES, 'LANG'}
function GetLang(const Lang: string): string;
var
  T: string;
  i: integer;
begin
  Result := Lang;

  if Result = '' then begin
    for i := 1 to Paramcount - 1 do
      if (ParamStrUTF8(i) = '--LANG')
        or (ParamStrUTF8(i) = '-l')
        or (ParamStrUTF8(i) = '--lang') then
           Result := ParamStrUTF8(i + 1);
  end;

  if Result = '' then
    Result := GetEnvironmentVariableUTF8('LANG');

  if Result = '' then begin
    T := ''; // removes warning about T not initialized
    LazGetLanguageIDs(Result, T);
  end;

  // In Ubuntu Linux, Locale is 'fr_CA.UTF-8' but there
  // is no encoding locale directory of that type
  // inside /usr/share/locale but there are subdirectories
  // with names such as be@latin and ca@valencia.
  // Accordingly, only the first 5 characters will be kept
  // if the third character is a '_'.
  //
  // If this is wrong, then remove these lines and
  // add the commented out block in FindLCFile below.
  //
  if (length(Result) > 5) and (Result[3] = '_') then
    setlength(Result, 5);
end;

function FileFound(const fname: string): boolean;
 begin
   try
     result := FileExistsUTF8(fname);
   except
     result := false;
   end;
 end;

procedure SetDefaultLang(Const Lang: string; Const Dir: string = '';
                           ForceUpdate: boolean = true);
{ Arguments:
  Lang: a locale specification
  Dir: a custom translation files subdirectory (e.g. 'mylng')
  ForceUpdate: ignored, kept for compatibility with
                 LclTranslator.SetDefaultLang.

  The locale specification can be reduced, such as 'fr_CA' or 'fr'.
  It can be a complete specification such as 'fr_CA.UTF-8' or
  'be@latin'. In the first case, only fr_CA is kept. If <file> is
     not found, then Lang is reduced to a 2 letter locale, fr and
     the search is repeated.

  If Dir is not an empty string then the search begins in the
  specified Dir sub directory of the executable's directory.

}
var
  LangID: string;
  AppDir: string;
  UserDir: string;
  LocaleDir: string;
  LanguagesDir: string;

  function FindLCIDLcFile(const LID, LCname: string): string;
  begin
    DefaultLang := LID;

    if UserDir <> '' then begin
      Result := UserDir + LID + DirectorySeparator + LCname;
      if FileFound(Result) then  exit;
    end;

    Result := AppDir + LID + DirectorySeparator + LCname;
    if FileFound(Result) then  exit;

    if LanguagesDir <> '' then begin
      Result := LanguagesDir + LID + DirectorySeparator + LCname;
      if FileFound(Result) then exit;
    end;

    if LocaleDir <> '' then begin
      Result := LocaleDir + LID + DirectorySeparator + LCname;
      if FileFound(Result) then exit;

      Result := LocaleDir + LID + DirectorySeparator
        + 'LC_MESSAGES' + DirectorySeparator + LCname;
      if FileFound(Result) then exit;
    end;

    {$IFDEF UNIX}
    Result := '/usr/share/locale/' + LID + DirectorySeparator
              + 'LC_MESSAGES' + DirectorySeparator + LCname;
    if FileFound(Result) then  exit;
    {$ENDIF}

    // not found
    Result := '';
  end;

  function FindLCFileLCID(const LCname: string): string;
  begin
    if UserDir <> '' then begin
      Result := UserDir + LCName;
      if FileFound(Result) then  exit;
    end;

    Result := AppDir + LCname;
    if FileFound(Result) then  exit;

    if LanguagesDir <> '' then begin
      Result := LanguagesDir + LCname;
      if FileFound(Result) then exit;
    end;

    if LocaleDir <> '' then begin
      Result := LocaleDir + LCname;
      if FileFound(Result) then exit;
    end;

    Result := '';
  end;

  function FindLocaleFileName(const LCExt: string): string;
  var
    LID, LCFilename, BaseFilename: string;
  begin
    BaseFilename := ChangeFileExt(ExtractFileName(ParamStrUTF8(0)), '');
    LCfilename := BaseFileName + LCExt;
    LID := LangID;

    Result := FindLCIDLCFile(LID, LCfilename);
    if Result <> '' then exit;
    Result := FindLCFileLCID(BaseFilename + '.' + LID + LCExt);
    if Result <> '' then exit;

    (*
    // Include this it it is wrong to cut-off language id at
    // 5 characters if the third is a '_' in GetLang.
    // In that case, the previous search was for fr_CA.UTF-8
    // and it is now necessary to do a search for f_CA

    if (length(LID) > 5) and (pos('_', LID) = 3) then begin
      LID := copy(LID, 1, 5);
      Result := FindLCIDLCFile(LID, LCfilename);
      if Result <> '' then exit;
      Result := FindLCFileLCID(BaseFilename + '.' + LID + LCExt);
      if Result <> '' then exit;
    end;
    *)

    if length(LID) > 2 then begin
      LID := copy(LID, 1, 2);
      Result := FindLCIDLCFile(LID, LCfilename);
      if result <> '' then exit;
      Result := FindLCFileLCID(BaseFilename + '.' + LID + LCExt);
      if result <> '' then exit;
    end;

    // last effort, '{AppDir}/{AppName}.po' or ('*.mo'). Even if that
    // file is found, the target language is unknown.
    DefaultLang := '';
    Result := AppDir + BaseFilename + LCExt;
    if not FileFound(result) then
      Result := '';
  end;

var
  lcfn: string;
begin
  LangID := GetLang(Lang);
  if length(LangId) < 2 then
    exit;
  AppDir := ExtractFilePath(ParamStrUTF8(0));
  LocaleDir := AppendPathDelim(AppDir + 'locale');
  LanguagesDir := AppendPathDelim(AppDir + 'languages');
  if not DirectoryExistsUTF8(AppDir) then begin
    AppDir := '';
    LocaleDir := '';
    LanguagesDir := '';
  end
  else begin
    if not DirectoryExistsUTF8(LocaleDir) then
      LocaleDir := '';
    if not DirectoryExistsUTF8(LanguagesDir) then
      LanguagesDir := '';
  end;
  UserDir := '';
  if Dir <> '' then begin
    UserDir := AppendPathDelim(Dir);
    if not (FilenameIsWinAbsolute(UserDir) or FilenameIsUnixAbsolute(UserDir)) then
      UserDir := AppDir + UserDir;
    if not DirectoryExistsUTF8(UserDir) then
      UserDir := '';
  end;

  lcfn := FindLocaleFileName('.po');
  if lcfn <> '' then
    try
      Translations.TranslateResourceStrings(lcfn);
    except
      lcfn := '';
    end;

  if lcfn = '' then begin
    lcfn := FindLocaleFileName('.mo');
    if lcfn <> '' then
      try
        GetText.TranslateResourceStrings(UTF8ToSys(lcfn));
      except
        lcfn := '';
      end;
  end;
  if lcfn = '' then
    DefaultLang := '';
end;

initialization
  SetDefaultLang('', 'lng');
end.
